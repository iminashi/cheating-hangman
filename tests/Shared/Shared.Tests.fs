module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared

let shared = testList "Shared" [
    testCase "String contains returns true for character in string" <| fun _ ->
        let actual = String.contains 'o' "word"
        Expect.equal actual true "Should be true"

    testCase "String contains returns false for character not in string" <| fun _ ->
        let actual = String.contains 'W' "word"
        Expect.equal actual false "Should be false"
]
