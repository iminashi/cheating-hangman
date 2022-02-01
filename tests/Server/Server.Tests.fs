module Server.Tests

open Expecto

open Shared
open Server

let server = testList "Server" [
    testCase "Words with given length can be retrieved" <| fun _ ->
        let length = 8

        let words = Hangman.getWordsWithLength length

        Expect.all words (fun w -> w.Length = length) "Result should be true"
]

let all =
    testList "All"
        [
            Tests.shared
            server
        ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args all
