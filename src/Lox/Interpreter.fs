namespace LoxFs

open Token
open TokenType
open Expr
open Microsoft.FSharp.Core
open System

module Interpreter =
    type Interpreter() as this =
        member __.evaluate (expr: IExpr): obj =
            expr.Accept(this)

        member __.parenthesize (exprs: IExpr []) (name: string): string =
            let formattedExprs =
                exprs
                |> Array.map (fun expr -> $" {expr.Accept(this)}")
                |> String.Concat

            [| "("; name; formattedExprs; ")" |]
            |> String.Concat

        member __.testExpr() =
            Binary(Unary((Token(MINUS, "-", (), 1)), (Literal 123)), Token(STAR, "*", (), 1), Grouping(Literal(45.67)))

        member __.print(expr: IExpr) = expr.Accept(this)

        member __.isTruthy (value: obj): bool =
            match value with
            | nil -> false
            | value when value.GetType() = typeof<bool> -> Convert.ToBoolean obj
            | _ -> true

        member __.isEqual left right =
            left = right

        interface IVisitor<obj> with
            member __.VisitBinaryExpr(expr: Binary): obj =
                let left: obj = __.evaluate expr.Left
                let right: obj = __.evaluate expr.Right

                let result = 
                    match expr.Operator.tokenType with
                    | MINUS -> (left :?> double) - (right :?> double) :> obj
                    | SLASH -> (left :?> double) / (right :?> double) :> obj
                    | STAR -> (left :?> double) * (right :?> double) :> obj
                    | GREATER -> (left :?> double) > (right :?> double) :> obj
                    | GREATEREQUAL -> (left :?> double) >= (right :?> double) :> obj
                    | LESS -> (left :?> double) < (right :?> double) :> obj
                    | LESSEQUAL -> (left :?> double) <= (right :?> double) :> obj
                    | BANGEQUAL -> (__.isEqual left right) :> obj
                    | EQUALEQUAL -> (not (__.isEqual left right)) :> obj
                    | PLUS ->
                        if ((left :? double) && (right :? double))
                        then (left :?> double) + (right :?> double) :> obj
                        else (
                            if ((left :? string) && (right :? string))
                            then (left :?> string) + (right :?> string) :> obj
                            else new obj())
                    | _ -> new obj() // Unreachable

                result

            member __.VisitGroupingExpr(expr: Grouping) =
                __.evaluate expr.Expression

            member __.VisitLiteralExpr(expr: Literal) =
                expr.Value

            member __.VisitUnaryExpr(expr: Unary): obj =
                let right: obj = __.evaluate expr.Right
                match expr.Operator.tokenType with
                | NOT -> (not (__.isTruthy right)) :> obj
                | MINUS -> (- (right :?> float)) :> obj
                | _ -> new obj() // Unreachable
