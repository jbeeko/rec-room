namespace RecRoom

open System
open System.Collections.Generic
open System.Net
open System.Text.RegularExpressions
open Microsoft.Azure.Cosmos
open Microsoft.Azure.Cosmos.Scripts

open SerializationSupport
open QueryHelpers
open Newtonsoft.Json.Linq

module DocDB =

    let dbName = Settings.CosmoDB.dbName

    let options = CosmosClientOptions()
    options.ConnectionMode <- ConnectionMode.Direct
    options.Serializer <- CosmosJsonDotNetSerializer(serializerSettings)

    let dbClient = new CosmosClient(Settings.CosmoDB.connectionString, options)

    // Lazy create the DB
    dbClient.CreateDatabaseIfNotExistsAsync(dbName).Wait()

    let escapeSqlParamPath (paramPath:string) (prefix:string) =
        let pathEls = paramPath.Split('.')
        Array.fold(fun acc (p:string) ->
            acc +
                if ((prefix.Length > 0) || (prefix.Length = 0 && (Array.findIndex((=) p) pathEls) > 0)) then "['" + p + "']"
                else  p) "" pathEls

    let getPartKey (pk: string option) = pk |> Option.map PartitionKey |> Option.defaultValue PartitionKey.None

    let getNullablePartKey (pk: string option) = pk |> Option.map (PartitionKey >> Nullable) |> Option.defaultValue (Nullable())

    type DBQueryCondition = {
        FieldName : string
        ParamName : string
        ParamValue : Object
        Type : QueryConditionType
    }

    let private createStoredProcedure conId name content =
        let sp = StoredProcedureProperties(name, content)
        let res = dbClient.GetContainer(dbName, conId.ToString()).Scripts.CreateStoredProcedureAsync(sp)
        res.Result.Resource

    let createContainer throughPut conId storedProcs =
        let props = ContainerProperties()
        props.Id <- conId.ToString()
        props.PartitionKeyPath <- PartitionKey.SystemKeyPath
        props.PartitionKeyDefinitionVersion <- PartitionKeyDefinitionVersion.V2 |> Nullable
        try
            dbClient.GetDatabase(dbName).CreateContainerIfNotExistsAsync(props, Nullable throughPut).Wait()
            storedProcs |> List.iter (fun storedProc ->
                let name, javaScript = storedProc
                createStoredProcedure conId name javaScript |> ignore)
            Some ()
        with
        | :? System.AggregateException as ex ->
            match ex.GetBaseException() with
            | :? CosmosException as ce ->
                if ce.StatusCode = HttpStatusCode.Conflict
                then None else reraise()
            | _ -> reraise()

    let containers () =
        let iter = dbClient.GetDatabase(dbName).GetContainerQueryIterator<ContainerProperties>()
        seq { while iter.HasMoreResults do yield! iter.ReadNextAsync().Result.Resource }

    let deleteContainer conId =
        try
            dbClient.GetContainer(dbName, conId.ToString()).DeleteContainerAsync().Wait()
            Some "deleted"
        with
        | :? System.AggregateException as ex ->
            match ex.GetBaseException() with
            | :? CosmosException as ce ->
                if ce.StatusCode = HttpStatusCode.NotFound
                then None else reraise()
            | _ -> reraise()

    let deleteContainers () =
        containers ()
        |> Seq.iter (fun c -> deleteContainer c.Id |> ignore)

    /// Create an item. The item does not need to have an ID property
    let insert conId partKey record =
        let pk = getNullablePartKey partKey
        dbClient.GetContainer(dbName, conId.ToString()).CreateItemAsync(record, pk).Result.Resource

    let tryInsert conId partKey record =
        try
            let resp = insert conId partKey record
            Some resp
        with
        | :? System.AggregateException as ex ->
            match ex.GetBaseException() with
            | :? CosmosException as e when e.StatusCode = HttpStatusCode.Conflict -> None
            | _ -> reraise()

    /// Upsert an item. The item should have an "id" property
    let upsert conId partKey record =
        let pk = getNullablePartKey partKey
        dbClient.GetContainer(dbName, conId.ToString()).UpsertItemAsync(record, pk).Wait()

    /// Replace (AKA update the given item.)
    let replace conId partKey itemId record =
        let pk = getNullablePartKey partKey
        dbClient.GetContainer(dbName, conId.ToString()).ReplaceItemAsync(record, itemId, pk).Wait()

    /// Fetch a item using the id property. Return None if the item does not exist.
    let get<'T> conId partKey docId : 'T option =
        let pk = getPartKey partKey
        try
            Some (dbClient.GetContainer(dbName, conId.ToString()).ReadItemAsync<'T>(docId.ToString(), partitionKey = pk).Result.Resource)
        with
        | :? System.AggregateException as ex ->
            match ex.GetBaseException() with
            | :? CosmosException as ce ->
                if ce.StatusCode = HttpStatusCode.NotFound
                then None else reraise()
            | _ -> reraise()

    /// Remove an item using the id property. Return None if the item does not exist.
    let delete conId partKey docId =
        let pk = getPartKey partKey
        try
            dbClient.GetContainer(dbName, conId.ToString()).DeleteItemAsync(docId.ToString(), partitionKey = pk).Wait()
            Some "Deleted"
        with
        | :? System.AggregateException as ex ->
            match ex.GetBaseException() with
            | :? CosmosException as ce ->
                if ce.StatusCode = HttpStatusCode.NotFound
                then None else reraise()
            | _ -> reraise()

    let private indexConditions (conditions: QueryCondition list) : DBQueryCondition list =
        let dbQueryConditions = List<DBQueryCondition>()
        let conditionsMap = new Dictionary<string, List<QueryCondition>>()
        conditions
        |> List.iter (fun c ->
            if conditionsMap.ContainsKey c.Key
            then (conditionsMap.Item c.Key).Add(c)
            else
                let queryConditions = new List<QueryCondition>()
                queryConditions.Add(c)
                conditionsMap.Add(c.Key, queryConditions))
        for entry in conditionsMap do
            if entry.Value.Count = 1 then
                let qc = entry.Value.[0]
                let dbq = { FieldName = qc.Key; ParamName = qc.Key; ParamValue = qc.Value; Type = qc.Type }
                dbQueryConditions.Add(dbq)
            else
                entry.Value
                |> Seq.iteri (fun i qc ->
                    dbQueryConditions.Add({ FieldName = qc.Key; ParamName = qc.Key + i.ToString(); ParamValue = qc.Value; Type = qc.Type }))

        dbQueryConditions |> List.ofSeq

    let private prepareQuery sql (dbConditions : DBQueryCondition list) (sortSql: string option) =
        let (|Match|_|) pattern input =
            if isNull input then None
            else
                let m = Regex.Match(input, pattern)
                if m.Success then Some [for x in m.Groups -> x]
                else None

        let prefix =
            match sql with
            | Match @"VALUE (\S*)" [_; pfx] -> pfx.Value
            | _ -> ""

        let opt (dbcond:DBQueryCondition) : string =
            match dbcond.Type with
            | Equal -> "="
            | LessThan -> "<"
            | MoreThan -> ">"
            | NotEqual -> "!="

        let querySql =
            if sql.ToLower().Contains("where") then
                dbConditions |> List.fold (fun acc e ->
                    acc + (sprintf " AND %s%s %s @%s" prefix (escapeSqlParamPath e.FieldName prefix) (opt e) (e.ParamName.Replace(".", "")) )
                ) sql
            else
                match dbConditions with
                | [] -> sql 
                | [e] -> sql + (sprintf " WHERE %s%s %s @%s" prefix e.FieldName (opt e) (e.ParamName.Replace(".", "")) )
                | e::tail ->
                    let sql' = sql + (sprintf " WHERE %s.%s %s @%s" prefix e.FieldName (opt e) (e.ParamName.Replace(".", "")) )
                    tail |> List.fold (fun acc e ->
                        acc + (sprintf " AND %s%s %s @%s" prefix e.FieldName (opt e) (e.ParamName.Replace(".", "")) )
                    ) sql'
        match sortSql with
        | None -> querySql
        | Some sortSql -> querySql + " " + sortSql

    let private makeQuery sql (dbConditions : DBQueryCondition list) (sortSql: string option) =
        let sql = prepareQuery sql dbConditions sortSql
        let query = QueryDefinition(sql)
        dbConditions |> Seq.fold (fun (q: QueryDefinition) c -> q.WithParameter("@" + c.ParamName.Replace(".", ""), c.ParamValue)) query

    /// Execute the given SQL returning a container of projected items.
    let query<'T> conId partKey sql (conditions: QueryCondition list) =
        let qry = makeQuery sql (indexConditions conditions) None
        let ro = QueryRequestOptions()
        ro.PartitionKey <- getNullablePartKey partKey
        ro.MaxConcurrency <- Nullable -1
        ro.MaxBufferedItemCount <- Nullable -1
        let iter = dbClient.GetContainer(dbName, conId.ToString()).GetItemQueryIterator<'T>(qry, requestOptions = ro)
        seq { while iter.HasMoreResults do yield! iter.ReadNextAsync().Result.Resource }
        |> Seq.map id
        |> Seq.toList

    /// Execute the given SQL returning the raw json result.
    let queryJson<'T> conId partKey sql (sortSql: string option) (conditions: QueryCondition list) =
        let qry = makeQuery sql (indexConditions conditions) sortSql
        let ro = QueryRequestOptions()
        ro.PartitionKey <- getNullablePartKey partKey
        ro.MaxConcurrency <- Nullable -1
        ro.MaxItemCount <- Nullable -1
        let iter = dbClient.GetContainer(dbName, conId.ToString()).GetItemQueryStreamIterator(qry, requestOptions = ro)
        let res = new JArray()

        while iter.HasMoreResults do
            let results = iter.ReadNextAsync().Result
            let stream = results.Content
            use reader = new System.IO.StreamReader(stream)
            let chunkString = reader.ReadToEndAsync().Result
            let chunk = JObject.Parse(chunkString)
            let documents = chunk.Item("Documents") :?> JArray
            documents |> Seq.iter (fun pymnt -> res.Add(pymnt))

        res.ToString() |> Regex.Unescape

    /// Execute the given SQL returning a container of simple values, can be used for aggragate (SUM, COUNT, etc) functions.
    let queryValue<'T> conId partKey sql (conditions : QueryCondition list) : 'T list =
        let qry = makeQuery sql (indexConditions conditions) None
        let ro = QueryRequestOptions()
        ro.PartitionKey <- getNullablePartKey partKey
        ro.MaxConcurrency <- Nullable -1
        ro.MaxBufferedItemCount <- Nullable -1
        let iter = dbClient.GetContainer(dbName, conId.ToString()).GetItemQueryIterator<'T>(qry, requestOptions = ro) // TODO partition for query
        seq { while iter.HasMoreResults do yield! iter.ReadNextAsync().Result.Resource }
        |> Seq.toList

    type DocProxy = {
        id : obj
    }

    let deleteDocs conId partKey =
        let sql = "SELECT Item.id, Item._type from Item"
        query<DocProxy> conId partKey sql []
        |> List.partition (fun p -> (delete conId partKey p.id).IsSome)

    let executeStoredProcedure<'T> conId partKey name input =
        let pk = getPartKey partKey
        let res = dbClient.GetContainer(dbName, conId.ToString()).Scripts.ExecuteStoredProcedureAsync<'T>(name, pk, input)
        res.Result.Resource

    let deleteStoredProcedure conId name =
        let res = dbClient.GetContainer(dbName, conId.ToString()).Scripts.DeleteStoredProcedureAsync(name)
        res.Result.Resource
