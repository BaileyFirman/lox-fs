namespace LoxFs

open Expr
open Stmt
open Token
open TokenType

module Parser =
    type Parser(tokens: Token[]) =
        let mutable current = 0

        let report line at message = failwith $"{line}{at}{message}"
        let peek () = tokens.[current]
        let previous () = tokens.[current - 1]
        let isAtEnd () = peek().TokenType = EOF

        let advance () =
            match peek().TokenType with
            | EOF -> ()
            | _ -> current <- current + 1

            previous ()

        let check tokenType =
            match peek().TokenType with
            | EOF -> false
            | t -> t = tokenType

        let rec matchToken token =
            match token with
            | t when check t ->
                advance () |> ignore
                true
            | _ -> false

        let rec matchTokens tokens =
            match tokens with
            | t when t |> Array.exists check ->
                advance () |> ignore
                true
            | _ -> false

        let error (token: Token) message =
            let reportLine = report token.Line

            match token.TokenType with
            | EOF -> reportLine " at end" message
            | _ -> reportLine " at " $"{token.Lexeme}'{message}"

        let rec expression () = assignment () :> IExpr
        and assignment () =
            let expr : IExpr = orExpression ()

            match matchToken EQUAL with
            | true ->
                let equals = previous ()
                let value = assignment ()

                match expr with
                | :? Variable ->
                    let name = (expr :?> Variable).Name
                    Assign(name, value) :> IExpr
                | _ -> error equals "Invalid assignment target."
            | false -> expr
        and orExpression () =
            let mutable expr = andExpression ()

            while matchToken OR do
                expr <- Logical(expr, previous (), andExpression ())

            expr
        and andExpression () : IExpr =
            let mutable expr = equality ()

            while matchToken AND do
                expr <- Logical(expr, previous (), equality ())

            expr
        and varDeclaration () =
            let name : Token =
                consume IDENTIFIER "Expect variable name"

            let intializer =
                match matchToken EQUAL with
                | true -> expression ()
                | false -> Literal(null) :> IExpr

            consume SEMICOLON "Expect ';' after variable declaration."
            |> ignore

            Var(name, intializer) :> IStmt
        and declaration () =
            if matchToken FUN then
                func ("function")
            else if matchToken VAR then
                varDeclaration ()
            else
                statement ()
        and consume tokenType message =
            match tokenType with
            | t when check t -> advance ()
            | _ ->
                let next = peek ()
                error next message
                next
        and func kind : IStmt =
            let name =
                consume IDENTIFIER $"Expect {kind} name."

            consume LEFTPAREN $"Expect '(' after {kind} name."
            |> ignore

            let mutable parameters = []

            let appendParameter () =
                parameters <-
                    consume IDENTIFIER $"Expect parameter name."
                    :: parameters

            match check RIGHTPAREN |> not with
            | true ->
                appendParameter ()

                while matchToken COMMA do
                    appendParameter ()
            | false -> ()

            parameters <- List.rev <| parameters

            consume RIGHTPAREN $"Expect ')' after parameters name."
            |> ignore

            consume LEFTBRACE $"Expect '{{' before {kind} body."
            |> ignore

            Func(name, parameters, block ()) :> IStmt
        and statement () =
            if matchToken FOR then
                forStatement ()
            else if matchToken IF then
                ifStatement ()
            else if matchToken PRINT then
                printStatement ()
            else if matchToken RETURN then
                returnStatement ()
            else if matchToken WHILE then
                whileStatement ()
            else if matchToken LEFTBRACE then
                Block(block ()) :> IStmt
            else
                expressionStatement ()
        and returnStatement () =
            let keyword = previous ()

            let value =
                match check SEMICOLON |> not with
                | true -> Some(expression ())
                | false -> None

            consume SEMICOLON "Expect ';' after return value"
            |> ignore

            Return(keyword, value) :> IStmt
        and forStatement () =
            consume LEFTPAREN "Expect '(' after 'for'."
            |> ignore

            let initializer =
                if matchToken SEMICOLON then
                    None
                else if matchToken VAR then
                    Some(varDeclaration ())
                else
                    Some(expressionStatement ())

            let condition =
                match check SEMICOLON |> not with
                | true -> Some(expression ())
                | false -> None

            consume SEMICOLON "Expect ';' after loop condition."
            |> ignore

            let increment =
                match check RIGHTPAREN |> not with
                | true -> Some(expression ())
                | false -> None

            consume RIGHTPAREN "Expect ')' after for clauses."
            |> ignore

            let mutable body = statement ()

            match increment with
            | Some i -> body <- (Block([ body; Expression(i) ]))
            | None -> ()

            match condition with
            | Some c -> body <- While(c, body)
            | None -> body <- While(Literal(true), body)

            match initializer with
            | Some i -> body <- Block([ i; body ])
            | None -> ()

            body
        and ifStatement () =
            consume LEFTPAREN "Expect '(' after if." |> ignore

            let condition = expression ()

            consume RIGHTPAREN "Expect ')' after if condition."
            |> ignore

            let thenBranch = statement ()

            let elseBranch =
                match matchToken ELSE with
                | true -> Some(statement ())
                | false -> None

            If(condition, thenBranch, elseBranch) :> IStmt
        and printStatement () =
            let value = expression ()

            consume SEMICOLON "Expect ';' after value."
            |> ignore

            Print value :> IStmt
        and whileStatement () : IStmt =
            consume LEFTPAREN "Expect '(' after 'while'."
            |> ignore

            let condition = expression ()

            consume RIGHTPAREN "Expect ')' after condition."
            |> ignore

            While(condition, statement ()) :> IStmt
        and expressionStatement () =
            let value = expression ()

            consume SEMICOLON "Expect ';' after expression."
            |> ignore

            Expression(value) :> IStmt
        and block () =
            let mutable statements = []

            while check RIGHTBRACE |> not && isAtEnd () |> not do
                statements <- declaration () :: statements

            consume RIGHTBRACE "Expect '}' after block."
            |> ignore

            statements |> List.rev
        and primary () =
            if matchToken FALSE then
                Literal(false) :> IExpr
            else if matchToken TRUE then
                Literal(true) :> IExpr
            else if matchToken NIL then
                Literal(null) :> IExpr
            else if matchTokens [| NUMBER; STRING |] then
                Literal(previous().Literal) :> IExpr
            else if matchToken IDENTIFIER then
                Variable(previous ()) :> IExpr
            else
                Literal(false) :> IExpr
        and unary () =
            match matchTokens [| BANG; MINUS |] with
            | true -> Unary(previous (), unary ()) :> IExpr
            | false -> call ()
        and call () =
            let mutable expr = primary ()
            let mutable breakWhile = false

            while not breakWhile do
                match matchToken LEFTPAREN with
                | true -> expr <- finishCall (expr)
                | false -> breakWhile <- true

            expr
        and finishCall callee =
            let mutable arguments = []

            let appendArgument () = arguments <- expression () :: arguments

            match check RIGHTPAREN |> not with
            | true ->
                match arguments.Length with
                | a when a >= 255 -> error (peek ()) "Can't have more than 255 arguments."
                | _ -> appendArgument ()

                while matchToken COMMA do
                    appendArgument ()
            | false -> ()

            arguments <- List.rev <| arguments

            let paren =
                consume RIGHTPAREN "Expect ')' after arguments"

            Call(callee, paren, arguments)
        and factor () =
            let mutable expr = unary ()

            while matchTokens [| SLASH; STAR |] do
                let operator = previous ()
                let right = unary ()
                expr <- Binary(expr, operator, right)

            expr
        and term () =
            let mutable expr = factor ()

            while matchTokens [| MINUS; PLUS |] do
                let operator = previous ()
                let right = factor ()
                expr <- Binary(expr, operator, right)

            expr
        and comparison () =
            let mutable expr = term ()

            let matchableTokens =
                [| GREATER
                   GREATEREQUAL
                   LESS
                   LESSEQUAL |]

            while matchTokens matchableTokens do
                let operator = previous ()
                let right = term ()
                expr <- new Binary(expr, operator, right)

            expr
        and equality () : IExpr =
            let mutable expr = comparison ()

            while matchTokens [| BANGEQUAL; EQUALEQUAL |] do
                let operator = previous ()
                let right = comparison ()
                expr <- Binary(expr, operator, right)

            expr
        and parse () =
            let mutable statements = []

            while isAtEnd () |> not do
                statements <- declaration () :: statements

            statements |> List.rev

        member __.Start() = parse ()
