namespace LoxFs

open TokenType
open Expr
open Stmt
open Microsoft.FSharp.Core
open System

module Interpreter =
    type Interpreter() as this =
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
            | value when value.GetType() = typeof<bool> -> Convert.ToBoolean obj
            | _ -> true

        member private __.isEqual left right =
            if left = null && right = null
            then
                true
            else
                if left = null
                then
                    false
                else
                    left = right

        member __.InterpretExpression expression = __.evaluate expression

        member __.Execute (stmt: IStmt) =
            stmt.Accept(this) |> ignore
            null

        member __.Interpret(statements: seq<IStmt>) =
            statements
            |> Seq.toArray
            |> Array.map __.Execute

        member __.Stringify value =
            match value with
            | value when value.GetType() = typeof<double> ->
                let text = value.ToString()

                if text.EndsWith(".0") then
                    text.[0..(text.Length - 2)]
                else
                    text
            | null -> "nil"
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

        interface Stmt.IStmtVisitor<obj> with
            member __.VisitExpressionStmt(stmt: Expression) =
                __.evaluate stmt.Expression |> ignore
                null

            member __.VisitPrintStmt(stmt: Print) =
                let value = __.evaluate stmt.Expression
                printfn $"{__.Stringify value}"
                null
