module InitializeEnvironment

open System
open System.IO
open Newtonsoft.Json
open RecRoom
open RecRoom.CosmosDB
open RecRoom.RecRoom


type Host ={
    LocalHttpPort: int
    CORS: string
    CORSCredentials: bool
    }

type LocalSettings = {
  IsEncrypted: bool
  Values : Map<string, string>
  Host: Host
  ConnectionStrings: Map<string, string>
}

let readText relPath =
    File.ReadAllText (Path.Combine(AppDomain.CurrentDomain.BaseDirectory, relPath))

// Since ConfigurationManager is NOT recomended on Azure Functions v2
// and Test programs can't access local.settings.json we
// load the relevant values environment variables brute force.

// Note this only loads the "Values" sections other sections such as the
// "ConnectionStrings" section are not loaded.

// test.settings.json contains those env variables automatically set by
// functions runtime and hence can't be set in local.settings.json
let copyFromSettings () =

    ["local.settings.json"; "test.settings.json"]
    |> List.map (fun s -> readText s)
    |> List.iter (fun input ->
        let settings = JsonConvert.DeserializeObject<LocalSettings>(input)
        settings.Values |> Map.iter (fun k v -> Environment.SetEnvironmentVariable(k, v, EnvironmentVariableTarget.Process))
    )



let initializeCosmosDB () =
    let dbName = Settings.CosmoDB.dbName
    printfn "[i] CosmosDB deleting containers in '%s'" dbName
    deleteContainers ()

    printfn "[i] Creating Collection"
    createContainer 400 "Testing" [] |> (printfn "Result %A") 

    // InitializeProfiles.initializeProfiles logger |> ignore

    // printfn "[i] Creating ORGS"
    // Clients.Repo.createCollection Constants.Seymour.defaultRUs Constants.Seymour.orgsId |> ignore

    // printfn "[i] Creating TRUK"
    // createTruk logger
    ()