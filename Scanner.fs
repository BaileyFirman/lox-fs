namespace LoxFs

open System
open Token
open TokenType
open Error

module Scanner =
    type Scanner(source, errorHandler) =
        let errorHandler : ErrorHandler = errorHandler
        let source : string = source
        let mutable tokens : List<Token> = []
        let mutable current = 0
        let mutable line = 1
        let mutable start = 0

        let isAtEnd () = current >= source.Length

        let _peek offset =
            match current + offset with
            | co when co >= source.Length -> '\u0004'
            | _ -> source.[current + offset]

        let peek () = _peek 0
        let peekNext () = _peek 1

        let advance () =
            current <- current + 1
            source.[current - 1]

        let addLiteralToken tokenType literal =
            let text = source.[start..(current - 1)]
            let token = Token(tokenType, text, literal, line)
            tokens <- (token :: tokens)
            ()

        let addToken tokenType = addLiteralToken tokenType null

        let matchToken expected =
            match current with
            | c when c >= source.Length -> false
            | c when source.[c] <> expected -> false
            | _ ->
                current <- current + 1
                true

        let number () =
            while peek () |> Char.IsDigit do
                advance () |> ignore

            match peek () = '.' && peekNext () |> Char.IsDigit with
            | true ->
                advance () |> ignore
                while peekNext () |> Char.IsDigit do
                    advance () |> ignore
            | false -> ()

            let double =
                Double.Parse source.[start..(current - 1)]

            addLiteralToken NUMBER double

        let identifier () =
            while peek () |> Char.IsLetterOrDigit do
                advance () |> ignore

            let text = source.[start..(current - 1)]

            let tokenType =
                match text with
                | "and" -> AND
                | "class" -> CLASS
                | "else" -> ELSE
                | "false" -> FALSE
                | "for" -> FOR
                | "fun" -> FUN
                | "if" -> IF
                | "nil" -> NIL
                | "or" -> OR
                | "print" -> PRINT
                | "return" -> RETURN
                | "super" -> SUPER
                | "this" -> THIS
                | "true" -> TRUE
                | "var" -> VAR
                | "while" -> WHILE
                | _ -> IDENTIFIER

            addToken tokenType

        let scanToken () =
            let c = advance ()

            let addCompoundToken compoundType normalType =
                match matchToken '=' with
                | true -> addToken compoundType
                | false -> addToken normalType

            let addSlashToken () =
                match matchToken '/' with
                | true ->
                    while peek () <> '\n' && isAtEnd () |> not do
                        advance () |> ignore
                | false -> addToken SLASH

            let addCommaToken () =
                while peek () <> '"' && isAtEnd () |> not do
                    match peek () with
                    | '\n' -> line <- line + 1
                    | _ -> ()
                    advance () |> ignore

                advance () |> ignore

                let value = source.[(start + 1)..(current - 2)] // maybe bug
                addLiteralToken STRING value
                ()

            match c with
            | '(' -> addToken LEFTPAREN
            | ')' -> addToken RIGHTPAREN
            | '{' -> addToken LEFTBRACE
            | '}' -> addToken RIGHTBRACE
            | ',' -> addToken COMMA
            | '.' -> addToken DOT
            | '-' -> addToken MINUS
            | '+' -> addToken PLUS
            | ';' -> addToken SEMICOLON
            | '*' -> addToken STAR
            | '!' -> addCompoundToken BANGEQUAL BANG
            | '=' -> addCompoundToken EQUALEQUAL EQUAL
            | '<' -> addCompoundToken LESSEQUAL LESS
            | '>' -> addCompoundToken GREATEREQUAL GREATER
            | '/' -> addSlashToken ()
            | ' '
            | '\r'
            | '\t' -> ()
            | '\n' -> line <- line + 1
            | '"' -> addCommaToken ()
            | c when Char.IsDigit c -> number ()
            | c when Char.IsLetter c -> identifier ()
            | _ -> ()

        member __.ScanTokens =
            while not (isAtEnd ()) do
                start <- current
                scanToken ()

            (Token(EOF, "", null, 0) :: tokens)
            |> List.rev
            |> List.toArray
