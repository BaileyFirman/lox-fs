open System

let exitWithMessage message =
    printfn "%s" message
    Environment.Exit 64

let defineType baseName className (fieldList: string) =
    let openType = $"  static class {className} extends {baseName}" + " {\n"
    let constructor = $"    {className}({fieldList})" + "{\n"
    let fieldParams =
        fieldList.Split ", "
        |> Array.map(fun f ->
                        let name = (f.Split " ").[1]
                        $"      this.{name} = {name};\n")
        |> String.Concat
    let close = "    }\n\n"
    ""

let defineAst outputDir baseName (types: string[]) =
    let path = $"{outputDir}/{baseName}/.java"

    let astClasses =
        types
        |> Array.map (fun t ->
                        let splitType = t.Split ":"
                        let className = splitType.[0].Trim()
                        let field = splitType.[1].Trim()
                        defineType baseName className field)
        |> String.Concat

    let program =
        [|
            "package com.craftinginterpreters.lox;"
            "\n"
            "import java.util.List;"
            "\n"
            "abstract class "
            baseName
            astClasses
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
    defineAst outputDir "TEST" [| "TEST:TYPE" |] |> ignore
    if outputDir = String.Empty
        then exitWithMessage "Usage: generate_ast <output directory>"
        else ()
    0