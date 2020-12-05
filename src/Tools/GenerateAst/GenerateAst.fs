open System

let exitWithMessage message =
    printfn "%s" message
    Environment.Exit 64

let defineAst outputDir baseName types =
    let path = $"{outputDir}/{baseName}/.java"
    0

[<EntryPoint>]
let main argv =
    let outputDir = if argv.Length <> 1 then String.Empty else argv.[0]
    if outputDir = String.Empty then exitWithMessage "Usage: generate_ast <output directory>" else ()
    0