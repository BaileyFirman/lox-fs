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

        let report line at message =
            $"{line}{at}{message}"

        let error (token: Token) message =
            if token.tokenType = EOF
            then report token.line " at end" message
            else report token.line " at " $"{token.lexeme}'{message}"

        let rec expression () = equality () :> IExpr

        and consume tokenType message =
            if check tokenType
            then advance () |> ignore
            else ()

        and primary (): IExpr =
            match matchToken [| FALSE |] with
            | true -> Literal(false) :> IExpr
            | false ->
                match matchToken [| TRUE |] with
                | true -> Literal(true) :> IExpr
                | false ->
                    match matchToken [| NIL |] with
                    | true -> Literal(null) :> IExpr
                    | false ->
                        match matchToken [| NUMBER; STRING |] with
                        | true -> Literal(previous().literal) :> IExpr
                        | false ->
                            match matchToken [| LEFTPAREN |] with
                            | true ->
                                let expr = expression ()
                                consume RIGHTPAREN "Expect ')' after expression."
                                Grouping(expr) :> IExpr
                            | false -> Literal("<ERROR>") :> IExpr

        and unary () =
            let matchTokens = [| BANG; MINUS |]

            let rec innerUnary () =
                match matchToken matchTokens with
                | true ->
                    let operator = previous ()
                    let right = innerUnary ()
                    Unary(operator, right) :> IExpr
                | false -> primary ()

            innerUnary ()


        and factor (): IExpr =
            let mutable expr = unary ()

            let matchTokens = [| SLASH; STAR |]

            let rec innerFactor () =
                match matchToken matchTokens with
                | true ->
                    let operator = previous ()
                    let right = unary ()
                    expr <- Binary(expr, operator, right)
                | false -> ()

            innerFactor ()
            expr

        and term (): IExpr =
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


        and comparison (): IExpr =
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

        and equality () =
            let mutable expr = comparison ()

            let matchTokens = [| BANGEQUAL; EQUALEQUAL |]

            let rec innerEquality () =
                match matchToken matchTokens with
                | true ->
                    let operator = previous ()
                    let right = comparison ()
                    expr <- Binary(expr, operator, right)
                    innerEquality ()
                | false -> ()

            innerEquality ()
            expr
        
        and parse () =
            expression ()

        member __.Start () = parse ()
