namespace LoxFs

open Env
open Ex
open Expr
open Microsoft.FSharp.Core
open Stmt
open System
open TokenType

module Interpreter =
    let globalEnv = Env(None)

    type ILoxCallable =
        abstract member Arity : int
        abstract member Call : Interpreter -> list<obj> -> obj

    and LoxFunction(declaration, closure) =
        member __.Closure : Env = closure
        member __.Declaration : Func = declaration

        override __.ToString() = $"<fn {__.Declaration.Name.Lexeme}>"

        interface ILoxCallable with
            member __.Arity : int = __.Declaration.Fparams.Length

            member __.Call (interpreter: Interpreter) (arguments: obj list) : obj =
                let environment = Env(Some(closure))

                declaration.Fparams
                |> Seq.iteri (fun i t -> environment.Define t.Lexeme arguments.[i])

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

        member __.Interpret(statements) = interpret statements

        interface Expr.IVisitor<obj> with
            member __.VisitBinaryExpr(expr) =
                let l = evaluate expr.Left
                let r = evaluate expr.Right

                match expr.Operator.TokenType with
                | MINUS -> (l :?> double) - (r :?> double) :> obj
                | SLASH -> (l :?> double) / (r :?> double) :> obj
                | STAR -> (l :?> double) * (r :?> double) :> obj
                | GREATER -> (l :?> double) > (r :?> double) :> obj
                | GREATEREQUAL -> (l :?> double) >= (r :?> double) :> obj
                | LESS -> (l :?> double) < (r :?> double) :> obj
                | LESSEQUAL -> (l :?> double) <= (r :?> double) :> obj
                | EQUALEQUAL -> (isEqual l r) :> obj
                | BANGEQUAL -> (not (isEqual l r)) :> obj
                | PLUS ->
                    match l, r with
                    | l, r when (l :? double) && (r :? double) -> ((l :?> double) + (r :?> double)) :> obj
                    | l, r when (l :? string) && (r :? string) -> ((l :?> string) + (r :?> string)) :> obj
                    | _ -> null
                | _ -> null

            member __.VisitGroupingExpr(expr) = evaluate expr.Expression

            member __.VisitLiteralExpr(expr) = expr.Value

            member __.VisitUnaryExpr(expr) =
                let rightObj = evaluate expr.Right

                let right = // We can't implicitly take an obj with the underlying type int and autocast AFAIK
                    match rightObj with
                    | :? int as Int -> Int |> float
                    | _ -> rightObj :?> float

                match expr.Operator.TokenType with
                | BANG -> right |> isTruthy |> not :> obj
                | MINUS -> (-(right)) :> obj
                | _ -> null

            member __.VisitVariableExpr(expr) = env.Get expr.Name

            member __.VisitAssignExpr(expr) =
                let value = evaluate expr.Value
                env.Assign(expr.Name, value)
                value

            member __.VisitLogicalExpr(expr) =
                let left = evaluate expr.Left

                match expr.Operator.TokenType with
                | OR ->
                    match left with
                    | l when isTruthy l -> left
                    | _ -> evaluate expr.Right
                | _ when isTruthy left -> left
                | _ -> evaluate expr.Right

            member __.VisitCallExpr(expr) =
                let loxfn = evaluate (expr.Callee) :?> ILoxCallable

                expr.Argurments
                |> List.map evaluate
                |> loxfn.Call this

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
                |> env.Define stmt.Name.Lexeme

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
                |> env.Define stmt.Name.Lexeme

                null

            member __.VisitReturnStmt(stmt) =
                match stmt.Value with
                | Some v -> Some(evaluate v) |> ReturnEx |> raise
                | None -> ReturnEx None |> raise
