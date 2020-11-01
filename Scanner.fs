namespace LoxFs

open System
open System.IO
open Token
open TokenType

module Scanner =
    type Scanner(source) =
        let source: string = source
        let mutable tokens: List<Token> = []
        let mutable start = 0
        let mutable current = 0
        let mutable line = 1

        member __.Advance =
            current <- current + 1
            source.[current - 1]

        member __.AddToken tokenType literal =
            let text = source.[start..current]
            let newToken = Token(tokenType, text, literal, line)
            tokens <- tokens @ [ newToken ]

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
                | _ -> failwith "ERROR Lexem not recognised"

            __.AddToken tokenType null

        member __.IsAtEnd = current >= source.Length

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
