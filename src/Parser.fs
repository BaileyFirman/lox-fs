namespace LoxFs

open Token
open TokenType
open Expr
open Stmt

module Parser =
    type Parser(tokens: seq<Token>) =
        let tokens : Token [] = tokens |> Seq.toArray

        let mutable current = 0

        let report line at message = failwith $"{line}{at}{message}"
        let peek () = tokens.[current]
        let previous () = tokens.[current - 1]
        let isAtEnd () = peek().tokenType = EOF

        let advance () =
            match peek().tokenType with
            | EOF -> ()
            | _ -> current <- current + 1

            previous ()

        let check tokenType =
            match peek().tokenType with
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
            let reportLine = report token.line

            match token.tokenType with
            | EOF -> reportLine " at end" message
            | _ -> reportLine " at " $"{token.lexeme}'{message}"

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

            if not (check RIGHTPAREN) then
                parameters <-
                    parameters
                    @ [ (consume IDENTIFIER $"Expect parameter name.") ]

                while matchTokens [| COMMA |] do
                    parameters <-
                        parameters
                        @ [ (consume IDENTIFIER $"Expect parameter name.") ]
            else
                ()

            consume RIGHTPAREN $"Expect ')' after parameters name."
            |> ignore

            consume LEFTBRACE $"Expect '{{' before {kind} body."
            |> ignore

            let body = block ()

            Func(name, parameters, body) :> IStmt
        and statement () : IStmt =
            if matchTokens [| FOR |] then
                forStatement ()
            else if matchTokens [| IF |] then
                ifStatement ()
            else if matchTokens [| PRINT |] then
                printStatement ()
            else if matchTokens [| RETURN |] then
                returnStatement ()
            else if matchTokens [| WHILE |] then
                whileStatement ()
            else if matchTokens [| LEFTBRACE |] then
                Block(block ()) :> IStmt
            else
                expressionStatement ()
        and returnStatement () : IStmt =
            let keyword = previous ()
            let mutable value : IExpr option = None

            if not (check SEMICOLON) then
                value <- Some(expression ())
            else
                ()

            consume SEMICOLON "Expect ';' after return value"
            |> ignore

            Return(keyword, value) :> IStmt
        and forStatement () =
            consume LEFTPAREN "Expect '(' after 'for'."
            |> ignore

            let initializer =
                if matchTokens [| SEMICOLON |] then
                    None
                else if matchTokens [| VAR |] then
                    Some(varDeclaration ())
                else
                    Some(expressionStatement ())

            let condition =
                if not (check SEMICOLON) then
                    Some(expression ())
                else
                    None

            consume SEMICOLON "Expect ';' after loop condition."
            |> ignore

            let increment =
                if not (check RIGHTPAREN) then
                    Some(expression ())
                else
                    None

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
            // let mutable elseBrach = null
            let elseBranch =
                if matchTokens [| ELSE |] then
                    Some(statement ())
                else
                    None

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

            let body = statement ()

            While(condition, body) :> IStmt
        and expressionStatement () =
            let value = expression ()

            consume SEMICOLON "Expect ';' after expression."
            |> ignore

            Expression(value) :> IStmt
        and block () =
            let mutable statements : list<IStmt> = []

            while ((not (check RIGHTBRACE)) && (not (isAtEnd ()))) do
                statements <- statements @ [ declaration () ]

            consume RIGHTBRACE "Expect '}' after block."
            |> ignore

            statements
        and primary () : IExpr =
            let mutable ret : IExpr = Literal(false) :> IExpr

            if matchTokens [| FALSE |] then
                ret <- Literal(false) :> IExpr
            else
                ()

            if matchTokens [| TRUE |] then
                ret <- Literal(true) :> IExpr
            else
                ()

            if matchTokens [| NIL |] then
                ret <- Literal(null) :> IExpr
            else
                ()

            if matchTokens [| NUMBER; STRING |] then
                ret <- Literal(previous().literal) :> IExpr
            else
                ()

            if matchTokens [| IDENTIFIER |] then
                ret <- Variable(previous ()) :> IExpr
            else
                ()

            // if matchToken [| LEFTPAREN |] then
            //     let expr = expression ()

            //     consume RIGHTPAREN "Expect ')' after expression."
            //     |> ignore

            //     ret <- Grouping(expr) :> IExpr
            // else
            //     ()

            ret
        and unary () =

            if matchTokens [| BANG; MINUS |] then
                let operator = previous ()
                let right = unary ()
                Unary(operator, right) :> IExpr
            else
                call ()
        and call () =
            let mutable expr = primary ()
            let mutable breakWhile = false

            while not breakWhile do
                if matchTokens [| LEFTPAREN |] then

                    expr <- finishCall (expr)
                else
                    breakWhile <- true

            expr
        and finishCall callee =
            let mutable arguments = []

            if not (check RIGHTPAREN) then
                if arguments.Length >= 255 then
                    error (peek ()) "Can't have more than 255 arguments."
                else
                    arguments <- arguments @ [ expression () ]

                while matchTokens [| COMMA |] do
                    arguments <- arguments @ [ expression () ]
            else
                ()

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

            let matchTokenz =
                [| GREATER
                   GREATEREQUAL
                   LESS
                   LESSEQUAL |]

            while matchTokens matchTokenz do
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

            while not (isAtEnd ()) do
                statements <-
                    // printfn "Adding Statement"
                    // statements @ [ statement () ]
                    statements @ [ declaration () ]

            // tokens
            // |> Seq.iter(fun x -> printfn $"{x.lexeme} {x.line} {x.literal} {x.tokenType}")
            // expression ()

            statements

        member __.Start() = parse ()
