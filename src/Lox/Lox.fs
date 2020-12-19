namespace LoxFs

open System
open System.IO
open Token
open TokenType
open Scanner
open Error
open AstPrinter

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
            printfn "%s" <| tokens.ToString()

        let runFile path =
            let file = File.ReadAllText path
            match errorHandler.HadError with
            | true -> Environment.Exit 65
            | false -> Environment.Exit 64
            ()

        let rec runPrompt code =
            printf "> "
            let line = Console.ReadLine()
            let scanner = Scanner(line, errorHandler)
            let tokens = scanner.ScanTokens
            tokens
            |> Seq.iter (fun x -> printfn "%s" <| x.ToString())
            |> ignore
            printfn ""
            errorHandler.SetError false
            runPrompt code

        let astPrintTest () =
            let x = AstPrinter()
            printf "%s" <| x.print(x.testExpr ())

        let exitCode = 64

        match argv.Length with
        | 3 -> usage exitCode
        | 2 -> runFile argv.[0]
        | 1 -> astPrintTest ()
        | _ -> runPrompt exitCode
        0 // return an integer exit code
