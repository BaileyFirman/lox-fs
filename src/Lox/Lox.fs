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
            let parser = Parser(tokens |> List.toArray)
            printfn $"{tokens}"
            let expression = parser.Start()
            let i = Interpreter() 
            i.Interpret expression
            // let x = AstPrinter()
            // printf "%s" <| x.print (expression)

        let runFile path =
            let file = File.ReadAllText path
            match errorHandler.HadError with
            | true -> Environment.Exit 65
            | false -> run file
            

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
            let x = Interpreter()
            let z = AstPrinter()
            printf "%s" <| z.print (x.testExpr ())
            
            x.Interpret (x.testExpr ())


        let exitCode = 64

        match argv.Length with
        | 3 -> usage exitCode
        | 2 -> runFile argv.[0]
        | 1 -> astPrintTest ()
        | _ -> runFile "test.lox"
        0 // return an integer exit code
