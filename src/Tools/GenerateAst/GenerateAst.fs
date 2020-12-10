open System
open System.Text

let exitWithMessage message =
    printfn "%s" message
    Environment.Exit 64

let defineAst outputDir baseName types =
    let path = $"{outputDir}/{baseName}/.java"

    let program =
        [|
            "package com.craftinginterpreters.lox;"
            "\n"
            "import java.util.List;"
            "\n"
            "abstract class "
            baseName
            " {"
            "}"
            "\n"
        |]
        |> String.Concat

    printf "%s" program 
    0

[<EntryPoint>]
let main argv =
    let outputDir = if argv.Length <> 1 then String.Empty else argv.[0]
    defineAst outputDir "TEST" () |> ignore
    if outputDir = String.Empty
        then exitWithMessage "Usage: generate_ast <output directory>"
        else ()
    0