namespace LoxFs

open Token
open TokenType

module Parser =
    type Parser(tokens) =
        let tokens: Token[] = tokens
        let mutable current = 0

        let peek () =
            tokens.[current]

        let previous () =
            tokens.[current - 1]

        let isAtEnd () =
            peek().tokenType = EOF

        let advance () =
            let atEnd = isAtEnd ()
            if not atEnd
            then current <- current + 1
            else ()
            previous ()

        let check tokenType =
            if isAtEnd ()
            then false
            else peek().tokenType = tokenType

        let rec matchToken (tokens: TokenType[]): bool =
            let head = tokens.[0]
            match check head with
            | true ->
                advance () |> ignore
                true
            | false -> matchToken tokens.[1..]
            |> ignore
            false

        let equality (): Expr =
            let expr = comparison()

        let rec matchEquality () =
            match BANGEQUAL

        member __.Expression = equality()