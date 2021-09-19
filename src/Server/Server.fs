module Server

open Fable.Remoting.Giraffe
open Fable.Remoting.Server
open Saturn
open Shared

let hangmanApi =
    { makeGuess = fun data -> async { return Hangman.makeGuess data }
      getCorrectWord = fun data -> async { return Hangman.getCorrectWord data } }

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
