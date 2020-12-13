open System

let exitWithMessage message =
    printfn "%s" message
    Environment.Exit 64

let defineVisitor (baseName: string) (types: string[]) =
    let typeVisits =
        types
        |> Array.map(fun t ->
            let typeName = (t.Split ":").[0].Trim()
            $"  R visit{typeName}{baseName}({typeName} {baseName.ToLower()});")
        |> String.Concat

    let visitor =
        [|
            " interface Visitor<R> {"
            typeVisits
            " }"
        |]
        |> String.Concat

    visitor

let defineType baseName className (fieldList: string) =
    let openType = $"  static class {className} extends {baseName}" + " {\n"
    let constructor = $"    {className}({fieldList})" + "{\n"

    let fields = fieldList.Split ", "

    let fieldParams =
        fields
        |> Array.map(fun f ->
                        let name = (f.Split " ").[1]
                        $"      this.{name} = {name};\n")
        |> String.Concat
    
    let close = "    }\n\n"

    let finalFields =
        fields
        |> Array.map(fun f -> $"  final {f};")
    let closeType = "  }"

    $"{openType}{constructor}{fields}{fieldParams}{close}{finalFields}{closeType}"

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
            " {"
            defineVisitor baseName types
            astClasses
            "\n"
            " abstract <R> R accept(Visitor<R> visitor);"
            "}"
            "\n"
        |]
        |> String.Concat

    printf "%s" program 
    0

[<EntryPoint>]
let main argv =
    let typeDescriptions =
        [|
            "Binary   : Expr left, Token operator, Expr right"
            "Grouping : Expr expression"
            "Literal  : Object value"
            "Unary    : Token operator, Expr right"
        |]

    let outputDir = if argv.Length <> 1 then String.Empty else argv.[0]
    defineAst outputDir "Expr" typeDescriptions |> ignore
    if outputDir = String.Empty
        then exitWithMessage "Usage: generate_ast <output directory>"
        else ()
    0