namespace LoxFs

open Token
open TokenType

module Parser =
    type Parser(tokens) =
        let tokens: Token[] = tokens
        let mut current = 0

        let rec matchToken (tokens: TokenType[]): bool =
            let head = tokens.[0]
            match check head with
            | true ->
                advance ()
                true
            | false -> matchToken tokens.[1..]
            |> ignore
            false

        let equality (): Expr =
            let expr = comparison()

        let rec matchEquality () =
            match BANGEQUAL

        member __.Expression = equality()