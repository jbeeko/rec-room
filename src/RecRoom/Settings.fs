namespace RecRoom
module Settings =
    open System

    let tryGetEnvironmentVariable variable =
        match Environment.GetEnvironmentVariable variable with
        | null -> None
        | s -> Some s

    let getEnvironmentVariable variable =
        match tryGetEnvironmentVariable variable with
        | None -> failwith (sprintf "No Environment setting or KV secret: %s" variable)
        | Some s -> s


    module CosmoDB =
        let dbName = getEnvironmentVariable "_DBName"
        let connectionString = getEnvironmentVariable "_CosmosDBConnectionString"

