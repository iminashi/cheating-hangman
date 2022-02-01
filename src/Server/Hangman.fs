module Hangman

open System
open System.IO

open Shared

/// Returns a list of words that all have the given length.
let getWordsWithLength =
    let wordLists =
        Path.Combine(AppContext.BaseDirectory, "wordlist.txt")
        |> File.ReadAllLines
        |> List.ofArray
        |> List.groupBy String.length
        |> readOnlyDict

    fun length -> wordLists.[length]

/// Returns the number of words without the letter in the list.
let countWordsWithoutLetter (wordList: string list) (letter: char) =
    wordList
    |> List.sumBy (fun x -> if x.Contains(letter) then 0 else 1)

/// Returns true if the number is included in the pattern.
let numberInPattern (number: int) = Set.contains number

/// Tests if the word matches the pattern for the given letter.
let matchesPattern word (letter, pattern) =
    let rec testIndex index =
        if index = String.length word then
            true
        elif word.[index] = letter then
            // If this index should not be in the pattern, it is not a match
            if not (numberInPattern index pattern) then
                false
            else
                testIndex (index + 1)
        else
            // If this index should be in the pattern, it is not a match
            if numberInPattern index pattern then
                false
            else
                testIndex (index + 1)

    testIndex 0

/// Removes words from the given list that do not include the given letter.
let removeWordsWithoutLetter (requiredLetter: char) =
    List.filter (String.contains requiredLetter)

/// Finds the pattern that has the most possible matches.
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

/// Prepares a word list based on the guess data.
let prepareWordList guessData =
    // Convert the correct guesses into letter, index patterns
    let patterns =
        guessData.CorrectAnswers
        |> Array.groupBy snd
        |> Array.map (fun (letter, group) ->
            let pattern =
                group
                |> Array.map fst
                |> Set.ofArray

            letter, pattern)

    // Remove words that contain wrong guesses or that do not match the patterns
    getWordsWithLength guessData.WordLength
    |> List.filter (fun word ->
        guessData.WrongAnswers |> Array.forall (word.Contains >> not)
        && patterns |> Array.forall (matchesPattern word))

/// Determines the result for a guessed letter.
let makeGuess (guessData: GuessData) =
    let guess = guessData.CurrentGuess
    let wordList = prepareWordList guessData
    let missingCount = countWordsWithoutLetter wordList guess
    let bestPattern, bestPatternCount = mostFreqPatternByLetter wordList guess

    // Check if there are more possible wrong answers than correct ones
    if missingCount > bestPatternCount then
        WrongAnswer(guess)
    else
        CorrectAnswer(guess, bestPattern |> Set.toList)

/// Returns the correct word based on the guess data.
let getCorrectWord guessData =
    prepareWordList guessData
    |> List.head
