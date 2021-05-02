namespace LoxFs

open System
open System.IO
open Scanner
open Error
open AstPrinter
open Parser
open Interpreter

module LoxFs =
    [<EntryPoint>]
    let main argv =
        let errorHandler = ErrorHandler(false)

        let usage code =
            printfn "Usage: dotnet run [script]"
            Environment.Exit code

        let run source =
            let scanner = Scanner(source, errorHandler)
            let tokens = scanner.ScanTokens

            let parser = Parser(tokens)
            let expression = parser.Start()

            let astPrinter = AstPrinter()
            let interpreter = Interpreter()

            let astString = astPrinter.PrintAst expression
            let returnValue = interpreter.Interpret expression

            printfn $"{astString} -> {returnValue}"

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
