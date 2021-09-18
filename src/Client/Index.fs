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
      WaitingForResult: bool
      CorrectWord: string option }

type Msg =
    | MakeGuess of char
    | NewGame
    | GuessProcessed of GuessResult
    | WordLengthChanged of int
    | MaxGuessesChanged of int
    | SetCorrectWord of string

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
      WaitingForResult = false
      CorrectWord = None }

let createGuessData letter model =
    let correct =
        model.CorrectAnswers
        |> Array.mapi (fun i opt -> opt |> Option.map (fun x -> i, x))
        |> Array.choose id

    { WordLength = model.WordLength
      WrongAnswers = model.WrongAnswers |> Set.toList
      CorrectAnswers = correct
      CurrentGuess = letter }

let init () : Model * Cmd<Msg> =
    createModel 10 10, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | NewGame ->
        createModel model.WordLength model.MaxGuesses, Cmd.none

    | SetCorrectWord word ->
        { model with CorrectWord = Some word }, Cmd.none

    | WordLengthChanged wordLength ->
        createModel wordLength model.MaxGuesses, Cmd.none

    | MaxGuessesChanged maxGuesses ->
        { model with MaxGuesses = maxGuesses }, Cmd.none

    | MakeGuess letter ->
        let guessData = createGuessData letter model

        let cmd =
            Cmd.OfAsync.perform hangmanApi.makeGuess guessData GuessProcessed

        { model with
            GuessedLetters = model.GuessedLetters.Add(letter)
            WaitingForResult = true }, cmd

    | GuessProcessed result ->
        let model =
            match result with
            | WrongAnswer letter ->
                { model with WrongAnswers = model.WrongAnswers.Add(letter) }
            | CorrectAnswer (letter, indexes) ->
                let correctAnswers =
                    model.CorrectAnswers
                    |> Array.mapi (fun i x ->
                        if List.contains i indexes then
                            Some letter
                        else
                            x)

                { model with CorrectAnswers = correctAnswers }

        let cmd =
            if model.GuessedLetters.Count = model.MaxGuesses then
                let guessData = createGuessData 'x' model

                Cmd.OfAsync.perform hangmanApi.getCorrectWord guessData SetCorrectWord
            else
                Cmd.none

        { model with WaitingForResult = false }, cmd

let containerBox (model: Model) (dispatch: Msg -> unit) =
    let guesses = model.GuessedLetters.Count
    let gameWon = model.CorrectAnswers |> Array.forall Option.isSome
    let isGameOver = (guesses >= model.MaxGuesses || gameWon) && not model.WaitingForResult

    Bulma.box [
        Bulma.columns [
            Bulma.column [
                column.isOneQuarter
                prop.children [
                    Bulma.button.a [
                        button.isOutlined
                        button.isFullWidth
                        color.isInfo
                        prop.onClick (fun _ -> dispatch NewGame)
                        prop.children [
                            Bulma.icon [
                                Html.i [
                                    prop.classes [ "fa"; "fa-sync-alt" ]
                                ]
                            ]
                            Html.span [ prop.text "New Game" ]
                        ]
                    ]
                ]
            ]
            Bulma.column [
                Bulma.field.div [
                    field.isHorizontal
                    prop.children [
                        Bulma.fieldLabel [
                            prop.text "Max Guesses"
                        ]
                        Bulma.fieldBody [
                            Bulma.input.number [
                                prop.title "The number of guesses allowed."
                                prop.value model.MaxGuesses
                                prop.max 26
                                prop.min 5
                                prop.onInput (fun e -> (e.currentTarget :?> HTMLInputElement).value |> int |> MaxGuessesChanged |> dispatch)
                            ]
                        ]
                    ]
                ]
            ]

            Bulma.column [
                Bulma.field.div [
                    field.isHorizontal
                    prop.children [
                        Bulma.fieldLabel [
                            prop.text "Word Length"
                        ]
                        Bulma.fieldBody [
                            Bulma.input.number [
                                prop.title "The length of the word to guess."
                                prop.value model.WordLength
                                prop.max 22
                                prop.min 2
                                prop.onInput (fun e -> (e.currentTarget :?> HTMLInputElement).value |> int |> WordLengthChanged |> dispatch)
                            ]
                        ]
                    ]
                ]
            ]
        ]

        Bulma.columns [
            columns.isMobile
            columns.isMultiline
            columns.isGapless
            columns.isCentered
            prop.children [
                for a in model.CorrectAnswers do
                    Bulma.column [
                        column.isNarrow
                        prop.children [
                            Bulma.button.a [
                                prop.style [ style.fontSize 18; style.width 40 ]
                                prop.text (a |> Option.defaultValue '_' |> string)
                                text.isUppercase
                                button.isStatic
                                if model.WaitingForResult && a.IsNone then button.isLoading
                            ]
                        ]
                    ]
            ]
        ]

        Bulma.columns [
            columns.isMobile
            columns.isMultiline
            prop.children (
                [ 'a' .. 'z' ]
                |> List.map (fun letter ->
                    let isDisabled = model.GuessedLetters.Contains(letter) || isGameOver

                    Bulma.column [
                        column.isNarrow
                        prop.children [
                            Bulma.button.a [
                                prop.style [ style.width 50 ]
                                if isDisabled then
                                    if model.WrongAnswers.Contains(letter) then
                                        color.isDanger
                                    elif model.GuessedLetters.Contains(letter) then
                                        color.isPrimary
                                    else
                                        color.isDark
                                else
                                    color.isLink
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
            match model.CorrectWord with
            | Some correctWord ->
                Html.div [
                    prop.text $"I was thinking of the word '{correctWord}'"
                ]
            | None ->
                ()
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
