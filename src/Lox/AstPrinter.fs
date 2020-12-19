namespace LoxFs

open Token
open TokenType
open Expr
open Microsoft.FSharp.Core
open System

module AstPrinter =
    type AstPrinter() as this =
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

        interface IVisitor<string> with
            member __.VisitBinaryExpr(expr: Binary): string =
                let exprs = [| expr.Left; expr.Right |]
                __.parenthesize exprs expr.Operator.lexeme

            member __.VisitGroupingExpr(expr: Grouping): string =
                let exprs = [| expr.Expression |]
                __.parenthesize exprs "group"

            member __.VisitLiteralExpr(expr: Literal): string =
                if expr.Value = null then "nil" else expr.Value.ToString()

            member __.VisitUnaryExpr(expr: Unary): string =
                let exprs = [| expr.Right |]
                __.parenthesize exprs expr.Operator.lexeme
