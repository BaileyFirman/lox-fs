open System
open System.IO


[<EntryPoint>]
let main argv =
    let usage code =
        printfn "Usage: dotnet run [script]"
        Environment.Exit code
    
    let runFile path =
        let file = File.ReadAllText path
        ()

    let rec runPrompt code =
        printf "> "
        let line = Console.ReadLine()
        printfn "%s" line
        runPrompt code

    let exitCode = 64

    match argv.Length with
    | 3 -> usage exitCode
    | 2 -> runFile argv.[0]
    | _ -> runPrompt exitCode
    0 // return an integer exit code