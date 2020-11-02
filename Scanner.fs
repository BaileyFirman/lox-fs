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
        
        member __.Peek =
            match __.IsAtEnd with
            | true -> '\u0004'
            | false -> source.[current]

        member __.ScanToken () =
            let c = __.Advance ()

            let matchEqual t f =
                if __.MatchChar '=' then t else f

            let matchDivision () =
                let rec comment () =
                    match __.Peek <> '\n' && (not __.IsAtEnd) with
                    | true ->
                        __.Advance () |> ignore
                        comment ()
                    | false -> COMMENT
                if __.MatchChar '/'
                    then comment () else SLASH

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
                | ' ' | '\r' | '\t' -> WHITESPACE
                | _ ->
                    errorHandler.Error line "Unexpected Character"
                    ERROR

            match tokenType with
            | WHITESPACE
            | ERROR -> ()
            | _ -> __.AddToken tokenType tokenType

        member __.ScanTokens: List<Token> =
            let rec scanLoop () =
                match __.IsAtEnd with
                | true -> ()
                | false ->
                    start <- current
                    __.ScanToken ()
                    scanLoop ()
            scanLoop ()
            tokens <- tokens @ [ Token(EOF, "", null, 0) ]
            tokens
