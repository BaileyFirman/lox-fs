namespace LoxFs

open System
open System.IO
open Scanner
open Error
open Parser
open Interpreter
open System.Diagnostics

module LoxFs =
    [<EntryPoint>]
    let main argv =
        let errorHandler = ErrorHandler(false)

        let usage code =
            printfn "Usage: dotnet run [script]"
            Environment.Exit code

        let run source =
            let sw = Stopwatch()
            sw.Start()

            let scanner = Scanner(source, errorHandler)
            let tokens = scanner.ScanTokens
            sw.Stop()
            printfn $"DEBUG Scanned {tokens.Length} Token(s) In {sw.ElapsedMilliseconds}ms"

            sw.Reset()
            sw.Start()

            let parser = Parser(tokens)
            let statements = parser.Start()
            sw.Stop()
            printfn $"DEBUG Parsed {statements.Length} Statements(s) In {sw.ElapsedMilliseconds}ms"

            sw.Reset()
            sw.Start()

            let interpreter = Interpreter()
            interpreter.Interpret statements |> ignore
            sw.Stop()
            printfn $"DEBUG Interpreted In {sw.ElapsedMilliseconds}ms"

            ()

        let runFile path =
            let file = File.ReadAllText path

            match errorHandler.HadError with
            | true -> Environment.Exit 65
            | false -> run file


        let rec runPrompt code =
            printf "> "
            let line = Console.ReadLine()

            run line

            errorHandler.SetError false
            runPrompt code

        let exitCode = 64

        match argv.Length with
        | 2 -> usage exitCode
        | 1 -> runPrompt ""
        | _ -> runFile "test.lox"

        0
