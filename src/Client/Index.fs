module Index

open Elmish
open Fable.Remoting.Client
open Shared
open Feliz
open Feliz.Bulma
open Browser.Types
open System

type Model =
    { WordLength: int
      MaxGuesses: int
      WrongAnswers: Set<char>
      GuessedLetters: Set<char>
      CorrectAnswers: char option array
      WaitingForResult: bool }

type Msg =
    | MakeGuess of char
    | NewGame
    | GuessProcessed of GuessResult
    | WordLengthChanged of int
    | MaxGuessesChanged of int

let hangmanApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IHangManApi>

let createModel wordLength maxGuesses =
    { WordLength = wordLength
      MaxGuesses = maxGuesses
      WrongAnswers = Set.empty
      GuessedLetters = Set.empty
      CorrectAnswers = Array.replicate wordLength None
      WaitingForResult = false }

let init () : Model * Cmd<Msg> =
    createModel 10 10, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | NewGame ->
        init ()

    | WordLengthChanged wordLength ->
        createModel wordLength model.MaxGuesses, Cmd.none

    | MaxGuessesChanged maxGuesses ->
        { model with MaxGuesses = maxGuesses }, Cmd.none

    | MakeGuess letter ->
        let correct =
            model.CorrectAnswers
            |> Array.mapi (fun i opt -> opt |> Option.map (fun x -> i, x))
            |> Array.choose id

        let guessData =
            { WordLength = model.WordLength
              WrongAnswers = model.WrongAnswers |> Set.toList
              CorrectAnswers = correct
              CurrentGuess = letter }

        let cmd =
            Cmd.OfAsync.perform hangmanApi.makeGuess guessData GuessProcessed

        { model with
            GuessedLetters = model.GuessedLetters.Add(letter)
            WaitingForResult = true }, cmd

    | GuessProcessed result ->
        match result with
        | WrongAnswer letter ->
            { model with
                WrongAnswers = model.WrongAnswers.Add(letter)
                WaitingForResult = false }, Cmd.none
        | CorrectAnswer (letter, indexes) ->
            let correct =
                model.CorrectAnswers
                |> Array.mapi (fun i x ->
                    if List.contains i indexes then
                        Some letter
                    else
                        x)

            { model with
                CorrectAnswers = correct
                WaitingForResult = false }, Cmd.none

let containerBox (model: Model) (dispatch: Msg -> unit) =
    let guesses = model.GuessedLetters.Count
    let gameWon = model.CorrectAnswers |> Array.forall Option.isSome
    let isGameOver = (guesses >= model.MaxGuesses || gameWon) && not model.WaitingForResult

    Bulma.box [
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Html.div [
                    prop.text "Max Guesses"
                ]
                Bulma.input.number [
                    prop.value model.MaxGuesses
                    prop.max 26
                    prop.min 5
                    prop.onInput (fun e -> (e.currentTarget :?> HTMLInputElement).value |> int |> MaxGuessesChanged |> dispatch)
                ]
            ]
        ]

        Bulma.field.div [
            field.isGrouped
            prop.children [
                Html.div [
                    prop.text "Word Length"
                ]
                Bulma.input.number [
                    prop.value model.WordLength
                    prop.max 22
                    prop.min 2
                    prop.onInput (fun e -> (e.currentTarget :?> HTMLInputElement).value |> int |> WordLengthChanged |> dispatch)
                ]
            ]
        ]

        Bulma.field.div [
            prop.children [
                for a in model.CorrectAnswers do
                    Bulma.button.span [
                        prop.style [ style.fontSize 18 ]
                        prop.text (a |> Option.defaultValue '_' |> string)
                        text.isUppercase
                        if model.WaitingForResult && a.IsNone then button.isLoading
                    ]
            ]
        ]

        Bulma.columns [
            columns.isMobile
            columns.isMultiline
            columns.isVariable
            column.is1
            prop.children (
                [ 'a' .. 'z' ]
                |> List.map (fun letter ->
                    let isDisabled = model.GuessedLetters.Contains(letter) || isGameOver

                    Bulma.column [
                        prop.children [
                            Bulma.button.span [
                                if model.WrongAnswers.Contains(letter) then color.isDanger else color.isPrimary
                                prop.disabled isDisabled
                                prop.text (string letter)
                                text.isUppercase
                                if not isDisabled && not model.WaitingForResult then prop.onClick (fun _ -> dispatch (MakeGuess letter))
                            ]
                        ]
                    ])
            )
        ]


        Html.div [
            prop.text $"Guess {guesses} / {model.MaxGuesses}"
        ]

        if gameWon then
            Bulma.title [
                prop.style [ style.color color.black ]
                prop.text "YOU WIN!"
            ]
        elif isGameOver then
            Bulma.title [
                prop.style [ style.color color.black ]
                prop.text "YOU LOSE!"
            ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is12
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Cheating Hangman"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]
