namespace RecRoom

open QueryHelpers
open Logger
open Identifiers
open ReflectionSupport

type QueryUniqueResult<'T> =
    | One of 'T
    | Many of 'T List
    | Zero

type InsertResult<'T> =
    | Inserted
    | Duplicate

type UpdateResult<'T> =
    | Updated
    | Conflict

type DeleteResult<'T> =
    | Deleted
    | NotDeleted

exception DomainConstrainViolation of string


module DocDbRepository =

    type ContainerId = ContainerId of string with
        interface IId
        static member IdLength = IdLength.Len4
        override this.ToString() = match this with ContainerId s -> s



    exception ConcurrencyException

    let private getId anObj =
        anObj.GetType().GetProperty("id").GetValue(anObj, null)

    let tryGet<'T> (id: obj) (conId: ContainerId) (partKey: string option) (log:ILogger): Option<'T> =
        match (DocDB.get<'T> conId partKey id) with
        | Some entity -> Some entity
        | None ->
            let docType = typedefof<'T>.Name
            log.Info (sprintf "%s with id %s not found." docType (id.ToString()))
            None

    let get<'T> (id: obj) (conId: ContainerId) (partKey: string option) (log:ILogger): 'T =
        match tryGet<'T> id conId partKey log with
        | Some entity -> entity
        | None -> DomainConstrainViolation "Item not found by Id" |> raise

    let query<'T> (conditions : QueryCondition list) (conId: ContainerId) (partKey: string option) (log: ILogger) (querySql: string option): 'T List =
        let sql =
            match querySql with
            | Some q-> q
            | None ->
                let docType = typedefof<'T>.Name
                sprintf "SELECT VALUE doc FROM doc WHERE doc._type = '%s'"  docType
        DocDB.query<'T> conId partKey sql conditions

    let queryAll<'T> (conditions : QueryCondition list) (conId: ContainerId) (log: ILogger) (querySql: string option): 'T List =
        query<'T> conditions conId None log querySql

    let tryQueryOnlyOne<'T> (conditions : QueryCondition list) (conId: ContainerId) (partKey: string option) (exceptionMsg: string) (log: ILogger) (querySql: string option): 'T option =
        match query<'T> conditions conId partKey log querySql with
        | [] -> None
        | [onlyOne] -> Some onlyOne
        | _ -> DomainConstrainViolation exceptionMsg |> raise

    let queryOnlyOne<'T> (conditions: QueryCondition list) (conId: ContainerId) (partKey: string option) (exceptionMsg: string) (log: ILogger) (querySql: string option): 'T =
        match tryQueryOnlyOne<'T> conditions conId partKey exceptionMsg log querySql with
        | Some i -> i
        | None ->
            log.Info (sprintf "DomainConstraing: %s" exceptionMsg)
            DomainConstrainViolation exceptionMsg |> raise

    let queryValue<'T> (conId: ContainerId) (partKey: string option) (sql: string) (conditions : QueryCondition list) =
        DocDB.queryValue<'T> conId partKey sql conditions

    let insert<'T> (entity: 'T) (conId: ContainerId) (partKey: string option) (log: ILogger) : InsertResult<'T> =
        match DocDB.tryInsert conId partKey entity with
        | Some item ->
            //let docType = typedefof<'T>.Name
            //let id = getId item
            //log.Info (sprintf "%s with id %s was inserted in invocation %s of function %s." docType (id.ToString()) log.InvocationId log.FunctionName)
            Inserted
        | None ->
            Duplicate

    let update<'T> (entity: 'T) (conId: ContainerId) (partKey: string option) (log:ILogger) : UpdateResult<'T> =
        let id = getId entity
        try
            DocDB.upsert conId partKey entity
            //let docType = typedefof<'T>.Name
            //log.Info (sprintf "%s with id %s was updated in invocation %s of function %s." docType (id.ToString()) log.InvocationId log.FunctionName)
            Updated
        with
        | Failure msg ->
            let docType = typedefof<'T>.Name
            log.Info (sprintf "Update of %s with id %s failed with message %s." docType (id.ToString()) msg)
            reraise()

    let updateEntities (entities: 'a list) (conId: ContainerId) (partKey: string option) (log:ILogger)  =
        let ents =
            entities
            |> List.map (fun e ->
                let etag =
                    try
                        e?``_etag``
                    with
                        | _ -> ""

                [|box e; box etag |]
                )
            |> List.toArray
        try
            DocDB.executeStoredProcedure conId partKey "updateEntites" [| ents |] |> ignore
        with
        | _  -> raise ConcurrencyException
        ()

    let insertEntities (entities: 'a list) (conId: ContainerId) (partKey: string option) (log:ILogger)  =
        let ents =
            entities
            |> List.map box
            |> List.toArray
        DocDB.executeStoredProcedure conId partKey "insertEntites" [| ents |] |> ignore

    let updateUoW<'T> (entity: 'T) (conId: ContainerId) (partKey: string option) (log:ILogger) : UpdateResult<'T> =
        let entities = [(box entity)]
        try
            updateEntities entities conId partKey log
            Updated
        with
        | ConcurrencyException -> Conflict
        | Failure msg ->
            let id = getId entity
            let docType = typedefof<'T>.Name
            log.Info (sprintf "Update of %s with id %s failed with message %s." docType (id.ToString()) msg)
            reraise()

    let upsert<'T> (entity: 'T) (conId: ContainerId) (partKey: string option) (log:ILogger) : UpdateResult<'T> =
        DocDB.upsert conId partKey entity
        //let id = getId entity
        //let docType = typedefof<'T>.Name
        //log.Info (sprintf "%s with id %s was upserted in invocation %s of function %s." docType (id.ToString()) log.InvocationId log.FunctionName)
        Updated

    let delete<'T> (id: obj) (conId: ContainerId) (partKey: string option) (log:ILogger) : DeleteResult<'T> =
        match (DocDB.delete conId partKey id) with
        | Some _ ->
            //let docType = typedefof<'T>.Name
            //log.Info (sprintf "%s with id %s was deleted in invocation %s of function %s." docType (id.ToString()) log.InvocationId log.FunctionName)
            Deleted
        | None ->
            let docType = typedefof<'T>.Name
            log.Info (sprintf "%s with id %s not found." docType (id.ToString()))
            NotDeleted
