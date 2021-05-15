namespace LoxFs

open Token
open TokenType
open Expr
open Stmt

module Parser =
    type Parser(tokens: seq<Token>) =
        let tokens : Token [] = tokens |> Seq.toArray

        let mutable current = 0

        let peek () = tokens.[current]
        let previous () = tokens.[current - 1]
        let isAtEnd () = peek().tokenType = EOF

        let advance () =
            let offset =
                if peek().tokenType = EOF then
                    current
                else
                    current + 1

            current <- offset
            previous ()

        let check tokenType =
            let next = peek ()

            if isAtEnd () then
                false
            else
                // printfn $"{next.tokenType} {tokenType}"
                next.tokenType = tokenType

        let rec matchToken (tokens: TokenType []) : bool =
            let existsMatch =
                tokens |> Array.exists (fun x -> check x)

            if existsMatch then
                advance () |> ignore
                true
            else
                false

        let report line at message = failwith $"{line}{at}{message}"

        let error (token: Token) message =
            if token.tokenType = EOF then
                report token.line " at end" message
            else
                report token.line " at " $"{token.lexeme}'{message}"

        let rec expression () = assignment () :> IExpr
        and assignment () =
            let expr : IExpr = orEx ()

            if matchToken [| EQUAL |] then
                let equals = previous ()
                let value = assignment ()

                if (expr :? Variable) then
                    let name = (expr :?> Variable).Name
                    Assign(name, value) :> IExpr
                else
                    error equals "Invalid assignment target."
            else
                expr
        and orEx () =
            let mutable expr = andEx ()

            while matchToken [| OR |] do
                let operator = previous ()
                let right = andEx ()
                expr <- Logical(expr, operator, right)

            expr
        and andEx () : IExpr =
            let mutable expr = equality ()

            while matchToken [| AND |] do
                let operator = previous ()
                let right = equality ()
                expr <- Logical(expr, operator, right)

            expr
        and varDeclaration () =
            let name : Token =
                consume IDENTIFIER "Expect variable name"

            let mutable intializer : IExpr = Literal(null) :> IExpr

            if matchToken [| EQUAL |] then
                intializer <- expression ()
            else
                ()

            consume SEMICOLON "Expect ';' after variable declaration."
            |> ignore

            Var(name, intializer) :> IStmt
        and declaration () =
            if matchToken [| FUN |] then
                func("function")
            else if matchToken [| VAR |] then
                varDeclaration ()
            else
                statement ()
        and consume tokenType (message: string) : Token =
            if check tokenType then
                advance ()
            else
                let next = peek ()
                error next message
                next
        and func kind: IStmt =
            let name = consume IDENTIFIER $"Expect {kind} name."
            consume LEFTPAREN $"Expect '(' after {kind} name."
            |> ignore

            let mutable parameters = []

            if not(check RIGHTPAREN)
            then
                parameters <- parameters @ [ (consume IDENTIFIER $"Expect parameter name.") ]

                while matchToken [| COMMA |] do
                    parameters <- parameters @ [ (consume IDENTIFIER $"Expect parameter name.") ]
            else ()

            consume RIGHTPAREN $"Expect ')' after parameters name."
            |> ignore

            consume LEFTBRACE $"Expect '{{' before {kind} body."
            |> ignore

            let body = block ()

            Func(name, parameters, body) :> IStmt
        and statement () : IStmt =
            if matchToken [| FOR |] then
                forStatement ()
            else if matchToken [| IF |] then
                ifStatement ()
            else if matchToken [| PRINT |] then
                printStatement ()
            else if matchToken [| WHILE |] then
                whileStatement ()
            else if matchToken [| LEFTBRACE |] then
                Block(block ()) :> IStmt
            else
                expressionStatement ()
        and forStatement () =
            consume LEFTPAREN "Expect '(' after 'for'."
            |> ignore

            let initializer =
                if matchToken [| SEMICOLON |] then
                    None
                else if matchToken [| VAR |] then
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
                if matchToken [| ELSE |] then
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

            Expression value :> IStmt
        and block () =
            let mutable statements : list<IStmt> = []

            while ((not (check RIGHTBRACE)) && (not (isAtEnd ()))) do
                statements <- statements @ [ declaration () ]

            consume RIGHTBRACE "Expect '}' after block."
            |> ignore

            statements
        and primary () : IExpr =
            let mutable ret : IExpr = Literal(false) :> IExpr

            if matchToken [| FALSE |] then
                ret <- Literal(false) :> IExpr
            else
                ()

            if matchToken [| TRUE |] then
                ret <- Literal(true) :> IExpr
            else
                ()

            if matchToken [| NIL |] then
                ret <- Literal(null) :> IExpr
            else
                ()

            if matchToken [| NUMBER; STRING |] then
                ret <- Literal(previous().literal) :> IExpr
            else
                ()

            if matchToken [| IDENTIFIER |] then
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
            let matchTokens = [| BANG; MINUS |]

            if matchToken matchTokens then
                let operator = previous ()
                let right = unary ()
                Unary(operator, right) :> IExpr
            else
                call ()
        and call () =
            let mutable expr = primary ()
            let mutable breakWhile = false

            while not breakWhile do
                if matchToken [| LEFTPAREN |] then
                    
                    expr <- finishCall (expr)
                else
                    breakWhile <- true
            expr
        and finishCall callee =
            let mutable arguments = []

            if not (check RIGHTPAREN) then
                if arguments.Length >= 255
                then
                    error (peek ()) "Can't have more than 255 arguments."
                else
                    arguments <- arguments @ [ expression () ]

                while matchToken [| COMMA |] do
                    arguments <- arguments @ [ expression () ]
            else
                ()

            let paren =
                consume RIGHTPAREN "Expect ')' after arguments"
            
            Call(callee, paren, arguments)
        and factor () =
            let mutable expr = unary ()

            let matchTokens = [| SLASH; STAR |]

            while matchToken matchTokens do
                let operator = previous ()
                let right = unary ()
                expr <- Binary(expr, operator, right)

            expr
        and term () =
            let mutable expr = factor ()
            let matchTokens = [| MINUS; PLUS |]

            while matchToken matchTokens do
                let operator = previous ()
                let right = factor ()
                expr <- Binary(expr, operator, right)

            expr
        and comparison () =
            let mutable expr = term ()

            let matchTokens =
                [| GREATER
                   GREATEREQUAL
                   LESS
                   LESSEQUAL |]

            while matchToken matchTokens do
                let operator = previous ()
                let right = term ()
                expr <- new Binary(expr, operator, right)

            expr
        and equality () : IExpr =
            let mutable expr = comparison ()

            while matchToken [| BANGEQUAL; EQUALEQUAL |] do
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
