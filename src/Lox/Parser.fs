namespace LoxFs

open Token
open TokenType
open Expr

module Parser =
    type Parser(tokens) =
        let tokens: Token [] = tokens
        let mutable current = 0

        let peek () = tokens.[current]

        let previous () = tokens.[current - 1]

        let isAtEnd () = peek().tokenType = EOF

        let advance () =
            let atEnd = isAtEnd ()
            if not atEnd then current <- current + 1 else ()
            previous ()

        let check tokenType =
            if isAtEnd () then false else peek().tokenType = tokenType

        let rec matchToken (tokens: TokenType []): bool =
            let head = tokens.[0]
            match check head with
            | true ->
                advance () |> ignore
                true
            | false -> matchToken tokens.[1..]
            |> ignore
            false

        let factor (): IExpr = Literal("FACTOR") :> IExpr

        let term (): IExpr =
            let mutable expr = factor ()
            let matchTokens = [| MINUS; PLUS |]

            let rec innerTerm (): unit =
                match matchToken matchTokens with
                | true ->
                    let operator = previous ()
                    let right = factor ()
                    expr <- Binary(expr, operator, right)
                    innerTerm ()
                | false -> ()

            innerTerm ()
            expr


        let comparison (): IExpr =
            let mutable expr = term ()

            let matchTokens =
                [| GREATER
                   GREATEREQUAL
                   LESS
                   LESSEQUAL |]

            let rec innerComparison (): unit =
                match matchToken matchTokens with
                | true ->
                    let operator = previous ()
                    let right = term ()
                    expr <- Binary(expr, operator, right)
                    innerComparison ()
                | false -> ()

            innerComparison ()
            expr
