namespace RecRoom

open System
open System.Globalization
open System.Net.Http
open System.Text.RegularExpressions

open ReflectionSupport

module QueryHelpers =

    type QueryConditionType =
        | Equal
        | NotEqual
        | LessThan
        | MoreThan

    let [<Literal>] private EQUAL = "eq_"
    let [<Literal>] private NOT = "neq_"
    let [<Literal>] private LESS = "lt_"
    let [<Literal>] private MORE = "gt_"

    let (|Int|_|) (str:string) =
        match System.Int32.TryParse(str, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | (true,int) -> Some(int)
        | _ -> None

    let (|Dec|_|) (str:string) =
        match System.Decimal.TryParse(str, NumberStyles.Number, CultureInfo.InvariantCulture) with
        | (true,dec) -> Some(dec)
        | _ -> None

    let (|Bool|_|) (str:string) =
        match System.Boolean.TryParse(str) with
        | (true,bool) -> Some(bool)
        | _ -> None

    let (|Date|_|) str =
        let m = Regex.Match(str, "\d\d\d\d-\d\d-\d\d$")
        if m.Success then Some((DateTime.Parse(str)).ToString("yyyy-MM-ddTHH:mm:ss")) else None

    let paramObjectValue (stringValue: string) : Object =
        match stringValue with
        | Int v -> box v
        | Dec v -> box v
        | Date v -> box v
        | Bool v -> box v
        |_ -> box stringValue

    type QueryCondition = {
        Key : string
        Value : Object
        Type : QueryConditionType } with
        static member CreateEqual (k:string, v:string) =
            {Key = k;Value = paramObjectValue (v.Substring(EQUAL.Length));Type = Equal}
        static member CreateNotEqual (k:string, v:string) =
            {Key = k;Value = paramObjectValue(v.Substring(NOT.Length));Type = NotEqual}
        static member CreateLessThan (k:string, v:string) =
            {Key = k;Value = paramObjectValue(v.Substring(LESS.Length));Type = LessThan}
        static member CreateMoreThan (k:string, v:string) =
            {Key = k;Value = paramObjectValue(v.Substring(MORE.Length));Type = MoreThan}

    type private BadParamName = string
    let private isBadParamName n =
        match n with
        | Choice2Of2 _ -> true
        | _ -> false

    let getQueryParams (req : HttpRequestMessage) =
        let nvc = System.Web.HttpUtility.ParseQueryString(req.RequestUri.Query)
        nvc.AllKeys
        |> Seq.map (fun key -> key, nvc.[key])
        |> Seq.fold (fun acc (key, values) ->
            Seq.append acc (seq
                { for value in values.ToString().Split(',') do
                    yield key, value })) Seq.empty
        |> List.ofSeq

    let tryParseQueryConditions (queryParams : seq<string * string>) =
        let conditions =
            queryParams
            |> Seq.map (fun (k,v) ->
                if v.StartsWith EQUAL then Choice1Of2 (QueryCondition.CreateEqual (k,v))
                elif v.StartsWith NOT then Choice1Of2 (QueryCondition.CreateNotEqual (k,v))
                elif v.StartsWith LESS then Choice1Of2 (QueryCondition.CreateLessThan (k,v))
                elif v.StartsWith MORE then Choice1Of2 (QueryCondition.CreateMoreThan (k,v))
                else Choice2Of2 (k,v))

        if conditions |> Seq.exists isBadParamName then
            Choice2Of2 (conditions
                |> Seq.choose (function
                | Choice2Of2 badCondition -> Some badCondition
                | _ -> None)
                |> Seq.toList)
        else
            Choice1Of2 (conditions
                |> Seq.choose (function
                | Choice1Of2 condition -> Some condition
                | _ -> None)
                |> Seq.toList)


    let tryParseQueryParams<'inputDto> (queryParams : seq<string * string>) =
        if (typeof<'inputDto>.Equals(typeof<string>)) then
            Choice1Of2 queryParams
        else
            let dtoPropNames = getFields typeofType<'inputDto> ""
            let badParamNames :seq<BadParamName> =
                queryParams
                |> Seq.filter (fun (inputParamName,_) -> dtoPropNames |> Array.exists (fun propName -> propName = inputParamName) |> not)
                |> Seq.map fst
            if Seq.isEmpty badParamNames then
                // each query param has a matching dto property, return them
                Choice1Of2 queryParams
            else
                // one or more query params does not have a matching dto property, return the mismatched query param names
                Choice2Of2 (Seq.toList badParamNames)
