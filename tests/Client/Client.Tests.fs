module Client.Tests

open Fable.Mocha

open Index
open Shared

let client = testList "Client" [
    testCase "Guess was wrong" <| fun _ ->
        let result = WrongAnswer('a')
        let model, _ = init ()

        let model, _ = update (GuessProcessed result) model

        Expect.equal model.WrongAnswers.Count 1 "There should be 1 wrong answer"

    testCase "Guess was correct" <| fun _ ->
        let result = CorrectAnswer('a', [ 0 ])
        let model, _ = init ()

        let model, _ = update (GuessProcessed result) model

        Expect.equal model.CorrectAnswers.[0] (Some 'a') "Correct answer at index 0 is a"
]

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Shared.Tests.shared
#endif
            client
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all
