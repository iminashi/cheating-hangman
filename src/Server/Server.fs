module Server

open Fable.Remoting.Giraffe
open Fable.Remoting.Server
open Saturn
open System.IO

open Shared

let wordLists =
    File.ReadAllLines("wordlist.txt")
    |> List.ofArray
    |> List.groupBy String.length
    |> readOnlyDict

let countWordsWithoutLetter (wordList: string list) (letter: char) =
    wordList
    |> List.filter (fun x -> not (x.Contains(letter)))
    |> List.length

let numberInPattern (number: int) = List.contains number

let matchesPattern word letter pattern =
    let rec testPos pos =
        if pos = String.length word then
            true
        elif word.[pos] = letter then
            if not (numberInPattern pos pattern) then
                false
            else
                testPos (pos + 1)
        else
            if numberInPattern pos pattern then
                false
            else
                testPos (pos + 1)

    testPos 0

let removeWordsWithoutLetter (requiredLetter: char) =
    List.filter (fun (w: string) -> w.Contains(requiredLetter))

let removeWordsWithLetter (forbiddenLetter: char) =
    List.filter (fun (w: string) -> not (w.Contains(forbiddenLetter)))

let mostFreqPatternByLetter wordList letter =
    let rec findMaxPattern wList maxPattern maxPatternCount =
        match wList with
        | [] ->
            maxPattern, maxPatternCount
        | word :: tail ->
            let currentPattern =
                [ for i = 0 to String.length word - 1 do
                    if word.[i] = letter then yield i ]

            // Remove all the words that match this pattern
            let lst = List.filter (fun w -> not (matchesPattern w letter currentPattern)) tail
            let currentPatternCount = List.length wList - List.length lst

            if currentPatternCount > maxPatternCount then
                findMaxPattern lst currentPattern currentPatternCount
            else
                findMaxPattern lst maxPattern maxPatternCount

    let wordsWithTheLetter = removeWordsWithoutLetter letter wordList
    findMaxPattern wordsWithTheLetter [] 0

let prepareWordList guessData =
    wordLists.[guessData.WordLength]
    |> List.filter (fun word ->
        guessData.WrongAnswers
        |> List.forall (word.Contains >> not)
        &&
        guessData.CorrectAnswers
        |> Array.groupBy snd
        |> Array.forall (fun (letter, group) ->
            let pattern = group |> Array.map fst |> Array.toList
            matchesPattern word letter pattern))

let makeGuess (guessData: GuessData) =
    let wordList = prepareWordList guessData
    let missingCount = countWordsWithoutLetter wordList guessData.CurrentGuess
    let nextPattern, nextPatternCount = mostFreqPatternByLetter wordList guessData.CurrentGuess

    if missingCount > nextPatternCount then
        WrongAnswer(guessData.CurrentGuess)
    else
        CorrectAnswer(guessData.CurrentGuess, nextPattern)

let getCorrectWord guessData =
    prepareWordList guessData
    |> List.head

let hangmanApi =
    { makeGuess = fun data -> async { return makeGuess data }
      getCorrectWord = fun data -> async { return getCorrectWord data } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue hangmanApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
