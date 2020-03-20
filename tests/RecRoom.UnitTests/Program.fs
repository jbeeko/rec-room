open System
open Expecto
open Tests

[<EntryPoint>]
let main args =
    InitializeEnvironment.initializeCosmosDB()
    runTestsWithArgs defaultConfig args tests
