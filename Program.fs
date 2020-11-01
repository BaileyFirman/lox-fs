namespace LoxFs

open System
open System.IO
open Token
open TokenType
open Scanner

module LoxFs =
    [<EntryPoint>]
    let main argv =
        let mutable hadError = false

        let report line where message =
            printfn "[line %s] Error%s:%s" line where message

        let error line message = report line "" message

        let usage code =
            printfn "Usage: dotnet run [script]"
            Environment.Exit code

        let run source =
            let scanner = Scanner (source)
            let tokens = scanner.ScanTokens
            printfn "%s" <| tokens.ToString()


        let runFile path =
            let file = File.ReadAllText path
            match hadError with
            | true -> Environment.Exit 65
            | false -> Environment.Exit 64
            ()

        let rec runPrompt code =
            printf "> "
            let line = Console.ReadLine()
            printfn "%s" line
            hadError <- false
            runPrompt code

        let exitCode = 64

        match argv.Length with
        | 3 -> usage exitCode
        | 2 -> runFile argv.[0]
        | _ -> runPrompt exitCode
        0 // return an integer exit code
