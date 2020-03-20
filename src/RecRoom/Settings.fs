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
        let connectionString = "AccountEndpoint=https://rec-room.documents.azure.com:443/;AccountKey=InEFZM1kVX3xxH9UxpHTc2ckYnwylZDshtbDvdToPJ7bguqli4kM2xSXpw4DTDxmQhVGxPn1wFa5J1rhCE1P6Q==;"
        let dbName = getEnvironmentVariable "RecRoomDB"
        // let dbName = getEnvironmentVariable "_DBName"
        // let connectionString = getEnvironmentVariable "_CosmosDBConnectionString"

