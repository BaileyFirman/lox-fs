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
        let mutable start = 0
        let mutable current = 0
        let mutable line = 1

        let isAtEnd () = current >= source.Length

        let peek () =
            if isAtEnd () then
                '\u0004'
            else
                source.[current]

        let peekNext () =
            if (current + 1 >= source.Length) then
                '\u0004'
            else
                source.[current + 1]


        let advance () =
            let c = source.[current]
            current <- current + 1
            c

        let addLiteralToken tokenType literal =
            let text = source.[start..(current - 1)]
            let token = Token(tokenType, text, literal, line)
            tokens <- (token :: tokens)
            ()

        let addToken tokenType = addLiteralToken tokenType null

        let matchToken expected : bool =
            if isAtEnd () then
                false
            else if source.[current] <> expected then
                false
            else
                current <- current + 1
                true

        let number () =
            while (Char.IsDigit(peek ())) do
                advance () |> ignore

            if (peek () = '.' && (Char.IsDigit(peekNext ()))) then
                advance () |> ignore

                while (Char.IsDigit(peekNext ())) do
                    advance () |> ignore
            else
                ()

            let double =
                Double.Parse source.[start..(current - 1)]

            addLiteralToken NUMBER double

        let identifier () =
            while (Char.IsLetterOrDigit(peek ())) do
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
                let kind =
                    if matchToken '=' then
                        compoundType
                    else
                        normalType

                addToken kind

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
            | '/' ->
                if matchToken '/' then
                    while (peek () <> '\n' && not (isAtEnd ())) do
                        advance () |> ignore
                else
                    addToken SLASH
            | ' '
            | '\r'
            | '\t' -> ()
            | '\n' -> line <- line + 1
            | '"' ->
                while (peek () <> '"' && not (isAtEnd ())) do
                    if peek () = '\n' then line <- line + 1

                    advance () |> ignore

                advance () |> ignore

                let value = source.[(start + 1)..(current - 2)] // maybe bug
                addLiteralToken STRING value

                ()
            | c ->
                if (Char.IsDigit c) then
                    number ()
                else if (Char.IsLetter c) then
                    identifier ()
                else
                    ()

        member __.ScanTokens : seq<Token> =
            while not (isAtEnd ()) do
                start <- current
                scanToken ()

            (Token(EOF, "", null, 0) :: tokens)
            |> List.rev
            |> List.toSeq
