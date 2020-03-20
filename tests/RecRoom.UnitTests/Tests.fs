module Tests

open Expecto
open RecRoom.CosmosDB
//open RecRoom.RecRoom
open RecRoom.Logger

type TestRec = {
    id: string
    Name: string
}
let conId = "Testing"
let log = getNullLogger "testing" "1234"
let tests =
    testList "Group of tests" [

        test "List containers"{
            let containers = containers() |> List.ofSeq
            Expect.equal containers.Length 1 "not one container"          
        }        
        test "A simple store, fetch update delete test" {
            let recId = "1234"
            let record = {id = recId; Name = "John Smith"}
            let record' = insert  conId None record
            Expect.equal record record' "not equal"

            let record'' = get conId None recId
            Expect.equal record record'' "not equal"

            let record = {record with Name = "Bill Smith"}
            replace conId None recId record
            let record'' = get conId None recId
            Expect.equal record record'' "not equal"
            Expect.equal "Bill Smith" record''.Name "not equal"



        }
        test "A simple test" {
            let subject = "Hello World"
            Expect.equal subject "Hello World" "The strings should equal"
        }

        testProperty "Reverse of reverse of a list is the original list" (
            fun (xs:list<int>) -> List.rev (List.rev xs) = xs
        )
    ]
