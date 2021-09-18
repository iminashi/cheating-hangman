namespace Shared

type GuessData =
    { WordLength: int
      WrongAnswers: char list
      CorrectAnswers: (int * char) array
      CurrentGuess: char }

type GuessResult =
    | WrongAnswer of guess: char
    | CorrectAnswer of guess: char * indexes: int list

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IHangManApi =
    { makeGuess: GuessData -> Async<GuessResult> }
