module Hangman

open System.IO

open Shared

let wordLists =
    File.ReadAllLines("wordlist.txt")
    |> List.ofArray
    |> List.groupBy String.length
    |> readOnlyDict

let countWordsWithoutLetter (wordList: string list) (letter: char) =
    wordList
    |> List.sumBy (fun x -> if x.Contains(letter) then 0 else 1)

let numberInPattern (number: int) = Set.contains number

let matchesPattern word (letter, pattern) =
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

let mostFreqPatternByLetter wordList letter =
    let rec findMaxPattern maxPattern maxPatternCount wList =
        match wList with
        | [] ->
            maxPattern, maxPatternCount
        | word :: tail ->
            let currentPattern =
                [ for i = 0 to String.length word - 1 do
                    if word.[i] = letter then yield i ]
                |> Set.ofList

            // Remove all the words that match this pattern
            let lst = List.filter (fun w -> not (matchesPattern w (letter, currentPattern))) tail
            let currentPatternCount = List.length wList - List.length lst

            if currentPatternCount > maxPatternCount then
                findMaxPattern currentPattern currentPatternCount lst
            else
                findMaxPattern maxPattern maxPatternCount lst

    wordList
    |> removeWordsWithoutLetter letter
    |> findMaxPattern Set.empty 0

let prepareWordList guessData =
    let patterns =
        guessData.CorrectAnswers
        |> Array.groupBy snd
        |> Array.map (fun (letter, group) ->
            let pattern =
                group
                |> Array.map fst
                |> Set.ofArray

            letter, pattern)

    wordLists.[guessData.WordLength]
    |> List.filter (fun word ->
        guessData.WrongAnswers |> Array.forall (word.Contains >> not)
        && patterns |> Array.forall (matchesPattern word))

let makeGuess (guessData: GuessData) =
    let wordList = prepareWordList guessData
    let missingCount = countWordsWithoutLetter wordList guessData.CurrentGuess
    let bestPattern, bestPatternCount = mostFreqPatternByLetter wordList guessData.CurrentGuess

    if missingCount > bestPatternCount then
        WrongAnswer(guessData.CurrentGuess)
    else
        CorrectAnswer(guessData.CurrentGuess, bestPattern |> Set.toList)

let getCorrectWord guessData =
    prepareWordList guessData
    |> List.head
