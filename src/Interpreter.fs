namespace LoxFs

open TokenType
open Expr
open Stmt
open Env
open Microsoft.FSharp.Core
open System
open Ex

module Interpreter =
    let globalsOut = Env(None)

    type ILoxCallable =
        abstract member Arity : int
        abstract member Call : Interpreter -> list<obj> -> obj

    and LoxFunction(declaration, closure) =

        member __.Declaration : Func = declaration
        member __.Closure : Env = closure

        override __.ToString() = $"<fn {__.Declaration.Name.lexeme}>"

        interface ILoxCallable with
            member __.Arity : int = __.Declaration.Fparams.Length

            member __.Call (interpreter: Interpreter) (arguments: obj list) : obj =
                let environment = Env(Some(closure))

                declaration.Fparams
                |> Seq.iteri (fun i x -> environment.Define x.lexeme arguments.[i])

                let retVal =
                    try
                        interpreter.ExecuteBlock declaration.Body environment
                        None
                    with ex ->
                        if (ex :? ReturnEx) then
                            let ez = (ex :?> ReturnEx)
                            ez.Data0
                        else
                            None

                let retData =
                    match retVal with
                    | Some r -> r
                    | None -> null

                retData

    and Interpreter() as this =
        // let globals = globalsOut
        let mutable env = globalsOut

        member public __.Env = Some(env)
        member public __.Globals = Some(globalsOut)

        member private __.evaluate(expr: IExpr) : obj = expr.Accept(this)

        member private __.parenthesize (exprs: seq<IExpr>) (name: string) : string =
            let formattedExprs =
                exprs
                |> Seq.map (fun expr -> $" {expr.Accept(this)}")
                |> String.Concat

            [| "("; name; formattedExprs; ")" |]
            |> String.Concat

        member private __.print(expr: IExpr) = expr.Accept(this)

        member private __.isTruthy(value: obj) : bool =
            match value with
            | null -> false
            | value when value.GetType() = typeof<bool> -> Convert.ToBoolean value
            | _ -> true

        member private __.isEqual left right =
            if left = null && right = null then true
            else if left = null then false
            else left = right

        member __.InterpretExpression expression = __.evaluate expression

        member __.Execute(stmt: IStmt) =
            stmt.Accept(this) |> ignore
            null

        member __.ExecuteBlock (statements: list<IStmt>) (environment: Env) : unit =
            let previous = env

            try
                env <- environment
                let executions = statements |> List.map __.Execute
                ()
            finally
                env <- previous

            ()

        member __.Interpret(statements: seq<IStmt>) =
            statements |> Seq.toArray |> Array.map __.Execute

        member __.Stringify value =
            match value with
            // | null -> "nil"
            | value when value.GetType() = typeof<double> ->
                let text = value.ToString()

                if text.EndsWith(".0") then
                    text.[0..(text.Length - 2)]
                else
                    text
            | _ -> value.ToString()

        interface Expr.IVisitor<obj> with
            member __.VisitBinaryExpr(expr: Binary) : obj =
                let left : obj = __.evaluate expr.Left
                let right : obj = __.evaluate expr.Right

                match expr.Operator.tokenType with
                | MINUS -> (left :?> double) - (right :?> double) :> obj
                | SLASH -> (left :?> double) / (right :?> double) :> obj
                | STAR -> (left :?> double) * (right :?> double) :> obj
                | GREATER -> (left :?> double) > (right :?> double) :> obj
                | GREATEREQUAL -> (left :?> double) >= (right :?> double) :> obj
                | LESS -> (left :?> double) < (right :?> double) :> obj
                | LESSEQUAL -> (left :?> double) <= (right :?> double) :> obj
                | EQUALEQUAL -> (__.isEqual left right) :> obj
                | BANGEQUAL -> (not (__.isEqual left right)) :> obj
                | PLUS ->
                    // printfn $"{left} {right}"
                    if ((left :? double) && (right :? double)) then
                        (left :?> double) + (right :?> double) :> obj
                    else
                        (if ((left :? string) && (right :? string)) then
                             (left :?> string) + (right :?> string) :> obj
                         else
                             new obj ())
                | _ -> new obj () // Unreachable

            member __.VisitGroupingExpr(expr: Grouping) = __.evaluate expr.Expression

            member __.VisitLiteralExpr(expr: Literal) = expr.Value

            member __.VisitUnaryExpr(expr: Unary) : obj =
                let rightObj : obj = __.evaluate expr.Right

                let right =
                    // We can't implicitly take an obj with the underlying type int and autocast AFAIK
                    if (rightObj.GetType() = int.GetType()) then
                        (float (rightObj :?> int))
                    else
                        rightObj :?> float

                match expr.Operator.tokenType with
                | BANG -> (not (__.isTruthy right)) :> obj
                | MINUS -> (-(right)) :> obj
                | _ -> new obj () // Unreachable

            member __.VisitVariableExpr(expr: Variable) = env.Get expr.Name

            member __.VisitAssignExpr(expr: Assign) =
                let value = __.evaluate expr.Value

                // printfn $"XYZ {(value :?> Literal).Value.GetType()}"

                if value.GetType() = typeof<double> then
                    env.Assign(expr.Name, (value :?> double))
                else if value.GetType() = typeof<string> then
                    env.Assign(expr.Name, (value :?> double))
                else
                    env.Assign(expr.Name, value)

                // env.Assign(expr.Name, value)
                value

            member __.VisitLogicalExpr(expr: Logical) =
                let left = __.evaluate expr.Left

                if expr.Operator.tokenType = OR then
                    if __.isTruthy (left) then
                        left
                    else
                        __.evaluate expr.Right
                else if __.isTruthy (left) then
                    left
                else
                    __.evaluate expr.Right

            member __.VisitCallExpr(expr: Call) =
                let callee = __.evaluate (expr.Callee)

                let arguments = expr.Argurments |> List.map __.evaluate

                let loxfn = callee :?> ILoxCallable
                loxfn.Call this arguments

            member __.VisitVoidExpr(expr: VoidExpr) = null

        interface Stmt.IStmtVisitor<obj> with
            member __.VisitExpressionStmt(stmt: Expression) =
                __.evaluate stmt.Expression |> ignore
                null

            member __.VisitPrintStmt(stmt: Print) =
                // printfn $"Interpreter::Stmt::VisitPrintStmt {stmt}"
                let value = __.evaluate stmt.Expression

                if value = null then
                    printf ""
                else if value.GetType() = typeof<double> then
                    let r = (value :?> double)
                    printfn $"{__.Stringify r}"
                else if value.GetType() = typeof<string> then
                    let r = (value :?> string)
                    printfn $"{__.Stringify r}"
                else
                    printfn $"{__.Stringify value}"
                null

            member __.VisitVarStmt(stmt: Var) =
                let mutable value : obj = null

                value <- __.evaluate stmt.Initializer

                if value.GetType() = typeof<double> then
                    // printfn $"FUCK YOU {(value :?> double)}"
                    env.Define stmt.Name.lexeme (value :?> double)
                else if value.GetType() = typeof<string> then
                    // printfn $"FUCK YOU {(value :?> string)}"
                    env.Define stmt.Name.lexeme (value :?> string)
                else
                    env.Define stmt.Name.lexeme value

                null

            member __.VisitWhileStmt(stmt: While) =
                while (__.isTruthy (__.evaluate (stmt.Condition))) do
                    __.Execute stmt.Body |> ignore

                null

            member __.VisitBlockStmt(stmt: Block) =
                let mutable newEnv = Env(Some(env))
                __.ExecuteBlock stmt.Statements newEnv
                null

            member __.VisitIfStmt(stmt: If) =
                if __.isTruthy (__.evaluate (stmt.Condition)) then
                    __.Execute stmt.ThenBranch
                else
                    match stmt.ElseBranch with
                    | Some eb -> __.Execute eb
                    | None -> null

            member __.VisitFuncStmt(stmt: Func) =
                let func = LoxFunction(stmt, env)
                env.Define stmt.Name.lexeme func
                null

            member __.VisitReturnStmt(stmt: Return) =
                let value : obj option =
                    match stmt.Value with
                    | Some v -> Some(__.evaluate v)
                    | None -> None

                raise (ReturnEx value)
                null

            member __.VisitVoidStmt(expr: VoidStmt) = null
