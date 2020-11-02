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

        member __.Advance =
            current <- current + 1
            source.[current - 1]

        member __.AddToken tokenType literal =
            let text = source.[start..current]
            let newToken = Token(tokenType, text, literal, line)
            tokens <- tokens @ [ newToken ]

        member __.MatchChar expected =
            match __.IsAtEnd || source.[current] <> expected with
            | true -> false
            | false ->
                current <- current + 1
                true

        member __.ScanToken =
            let c = ' ' // advance

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
                | _ ->
                    errorHandler.Error line "Unexpected Character"
                    ERROR

            match tokenType <> ERROR with
            | true -> __.AddToken tokenType null
            | _ -> ()

        member __.ScanTokens: List<Token> =
            let rec scanLoop tokens =
                match __.IsAtEnd with
                | true -> ()
                | false ->
                    start <- current
                    // scanToken
                    scanLoop tokens

            tokens <- tokens @ [ Token(EOF, "", null, 0) ]
            tokens
