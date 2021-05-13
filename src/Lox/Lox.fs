namespace LoxFs

open System
open System.IO
open Scanner
open Error
open AstPrinter
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
            sw.Start() |> ignore

            let scanner = Scanner(source, errorHandler)
            let tokens = scanner.ScanTokens

            let parser = Parser(tokens)
            let statements = parser.Start()

            printfn $"Parsed In |> {sw.ElapsedMilliseconds}ms"

            let astPrinter = AstPrinter()

            sw.Reset() |> ignore
            sw.Start() |> ignore

            let interpreter = Interpreter()

            let returnValue = interpreter.Interpret statements

            sw.Stop() |> ignore
            printfn $"Interpreted In |> {sw.ElapsedMilliseconds}ms"

            // printfn $"-> {returnValue}"

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
