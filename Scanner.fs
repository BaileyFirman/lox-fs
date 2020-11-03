namespace LoxFs

open System
open System.IO
open Token
open TokenType
open Error

module Scanner =
    type Scanner(source, errorHandler) =
        let errorHandler: ErrorHandler = errorHandler
        let source: string = source
        let mutable tokens: List<Token> = []
        let mutable start = 0
        let mutable current = 0
        let mutable line = 1

        member __.IsAtEnd = current >= source.Length

        member __.Advance () =
            current <- current + 1
            source.[current - 1]

        member __.AddToken tokenType literal =
            let text = source.[start..(current - 1)]
            let newToken = Token(tokenType, text, literal, line)
            tokens <- tokens @ [ newToken ]

        member __.MatchChar expected =
            match __.IsAtEnd || source.[current] <> expected with
            | true -> false
            | false ->
                current <- current + 1
                true

        member __.Peek () =
            match __.IsAtEnd with
            | true -> '\u0004'
            | false -> source.[current]

        member __.ScanToken() =
            let c = __.Advance ()

            let matchEqual t f = if __.MatchChar '=' then t else f

            let matchDivision () =
                let rec comment () =
                    match __.Peek () <> '\n' && (not __.IsAtEnd) with
                    | true ->
                        __.Advance () |> ignore
                        comment ()
                    | false -> COMMENT

                if __.MatchChar '/' then comment () else SLASH

            let matchString () =
                let rec string () =
                    match __.Peek () <> '"' && (not __.IsAtEnd) with
                    | true ->
                        match __.Peek () = '\n' with
                        | true -> line <- line + 1
                        | false -> ()
                        __.Advance () |> ignore
                        string ()
                    | false -> ()
                
                string ()

                match __.IsAtEnd with
                | true ->
                    errorHandler.Error line "Unterminated String."
                    ()
                | false ->
                    __.Advance () |> ignore
                    ()
                
                let value = source.[(start + 1)..(current - 1)]
                __.AddToken STRING value

                STRING

            let newline () =
                line <- line + 1
                WHITESPACE

            let error line =
                errorHandler.Error line "Unexpected Character"
                ERROR

            let tokenType =
                match c with
                | '(' -> LEFTPAREN
                | ')' -> RIGHTPAREN
                | '{' -> LEFTBRACE
                | '}' -> RIGHTBRACE
                | ',' -> COMMA
                | '.' -> DOT
                | '-' -> MINUS
                | '+' -> PLUS
                | ';' -> SEMICOLON
                | '*' -> STAR
                | '!' -> matchEqual BANGEQUAL BANG
                | '=' -> matchEqual EQUALEQUAL EQUAL
                | '<' -> matchEqual LESSEQUAL LESS
                | '>' -> matchEqual GREATEREQUAL GREATER
                | '/' -> matchDivision ()
                | '"' -> matchString ()
                | ' '
                | '\r'
                | '\t' -> WHITESPACE
                | '\n' -> newline ()
                | _ -> error line


            match tokenType with
            | WHITESPACE
            | ERROR
            | STRING -> ()
            | _ -> __.AddToken tokenType tokenType

        member __.ScanTokens: List<Token> =
            let rec scanLoop () =
                match __.IsAtEnd with
                | true -> ()
                | false ->
                    start <- current
                    __.ScanToken()
                    scanLoop ()

            scanLoop ()
            tokens <- tokens @ [ Token(EOF, "", null, 0) ]
            tokens
