namespace LoxFs

open TokenType
open Expr
open Stmt
open Env
open Microsoft.FSharp.Core
open System
open Ex

module Interpreter =
    let globalEnv = Env(None)

    type ILoxCallable =
        abstract member Arity : int
        abstract member Call : Interpreter -> list<obj> -> obj

    and LoxFunction(declaration, closure) =
        member __.Closure : Env = closure
        member __.Declaration : Func = declaration

        override __.ToString() = $"<fn {__.Declaration.Name.lexeme}>"

        interface ILoxCallable with
            member __.Arity : int = __.Declaration.Fparams.Length

            member __.Call (interpreter: Interpreter) (arguments: obj list) : obj =
                let environment = Env(Some(closure))

                declaration.Fparams
                |> Seq.iteri (fun i t -> environment.Define t.lexeme arguments.[i])

                try
                    interpreter.ExecuteBlock declaration.Body environment
                    null
                with ex ->
                    match ex with
                    | :? ReturnEx as returnEx ->
                        match returnEx.Data0 with
                        | Some r -> r
                        | None -> null
                    | _ -> null

    and Interpreter() as this =
        let mutable env = globalEnv

        let evaluate (expr: IExpr) = expr.Accept(this)

        let execute (stmt: IStmt) =
            stmt.Accept(this) |> ignore
            null

        let executeBlock (statements: list<IStmt>) (blockEnv: Env) : unit =
            let previousEnv = env

            try
                env <- blockEnv
                statements |> List.map execute |> ignore
            finally
                env <- previousEnv

        let isEqual left right =
            match left, right with
            | null, null -> true
            | null, _ -> false
            | _, _ -> left = right

        let isTruthy (x: obj) =
            match x with
            | :? bool as Bool -> Convert.ToBoolean Bool
            | null -> false
            | _ -> true

        let stringify (value: obj) =
            match value with
            | :? double as Double ->
                let dString = Double.ToString()

                match dString with
                | ds when ds.EndsWith(".0") -> ds.[0..(ds.Length - 2)]
                | _ -> dString
            | _ -> value.ToString()

        let interpret statements =
            statements |> Seq.toArray |> Array.map execute

        member public __.Env = Some(env)
        member public __.Globals = Some(globalEnv)

        member __.ExecuteBlock = executeBlock

        member __.Interpret(statements: seq<IStmt>) = interpret statements

        interface Expr.IVisitor<obj> with
            member __.VisitBinaryExpr(expr: Binary) : obj =
                let left : obj = evaluate expr.Left
                let right : obj = evaluate expr.Right

                match expr.Operator.tokenType with
                | MINUS -> (left :?> double) - (right :?> double) :> obj
                | SLASH -> (left :?> double) / (right :?> double) :> obj
                | STAR -> (left :?> double) * (right :?> double) :> obj
                | GREATER -> (left :?> double) > (right :?> double) :> obj
                | GREATEREQUAL -> (left :?> double) >= (right :?> double) :> obj
                | LESS -> (left :?> double) < (right :?> double) :> obj
                | LESSEQUAL -> (left :?> double) <= (right :?> double) :> obj
                | EQUALEQUAL -> (isEqual left right) :> obj
                | BANGEQUAL -> (not (isEqual left right)) :> obj
                | PLUS ->
                    if ((left :? double) && (right :? double)) then
                        (left :?> double) + (right :?> double) :> obj
                    else
                        (if ((left :? string) && (right :? string)) then
                             (left :?> string) + (right :?> string) :> obj
                         else
                             new obj ())
                | _ -> new obj () // Unreachable

            member __.VisitGroupingExpr(expr: Grouping) = evaluate expr.Expression

            member __.VisitLiteralExpr(expr: Literal) = expr.Value

            member __.VisitUnaryExpr(expr: Unary) : obj =
                let rightObj : obj = evaluate expr.Right

                let right =
                    // We can't implicitly take an obj with the underlying type int and autocast AFAIK
                    if (rightObj.GetType() = int.GetType()) then
                        (float (rightObj :?> int))
                    else
                        rightObj :?> float

                match expr.Operator.tokenType with
                | BANG -> (not (isTruthy right)) :> obj
                | MINUS -> (-(right)) :> obj
                | _ -> new obj () // Unreachable

            member __.VisitVariableExpr(expr: Variable) = env.Get expr.Name

            member __.VisitAssignExpr(expr: Assign) =
                let value = evaluate expr.Value

                if value.GetType() = typeof<double> then
                    env.Assign(expr.Name, (value :?> double))
                else if value.GetType() = typeof<string> then
                    env.Assign(expr.Name, (value :?> double))
                else
                    env.Assign(expr.Name, value)

                value

            member __.VisitLogicalExpr(expr: Logical) =
                let left = evaluate expr.Left

                if expr.Operator.tokenType = OR then
                    if isTruthy left then
                        left
                    else
                        evaluate expr.Right
                else if isTruthy left then
                    left
                else
                    evaluate expr.Right

            member __.VisitCallExpr(expr: Call) =
                let callee = evaluate (expr.Callee)

                let arguments = expr.Argurments |> List.map evaluate

                let loxfn = callee :?> ILoxCallable
                loxfn.Call this arguments

        interface Stmt.IStmtVisitor<obj> with
            member __.VisitExpressionStmt(stmt: Expression) =
                evaluate stmt.Expression |> ignore
                null

            member __.VisitPrintStmt(stmt) =
                let result = evaluate stmt.Expression

                match result with
                | null -> printfn ""
                | _ -> printfn $"{stringify result}"
                null

            member __.VisitVarStmt(stmt) =
                stmt.Initializer
                |> evaluate
                |> env.Define stmt.Name.lexeme
                null

            member __.VisitWhileStmt(stmt) =
                while stmt.Condition |> evaluate |> isTruthy do
                    execute stmt.Body |> ignore
                null

            member __.VisitBlockStmt(stmt) =
                Env(Some(env)) |> executeBlock stmt.Statements
                null

            member __.VisitIfStmt(stmt) =
                if stmt.Condition |> evaluate |> isTruthy then
                    execute stmt.ThenBranch
                else
                    match stmt.ElseBranch with
                    | Some eb -> execute eb
                    | None -> null

            member __.VisitFuncStmt(stmt) =
                LoxFunction(stmt, env)
                |> env.Define stmt.Name.lexeme

                null

            member __.VisitReturnStmt(stmt) =
                match stmt.Value with
                | Some v -> Some(evaluate v) |> ReturnEx |> raise
                | None -> ReturnEx None |> raise
