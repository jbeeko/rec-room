module InitializeEnvironment

open System
open Newtonsoft.Json
open FixtureSupport



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
