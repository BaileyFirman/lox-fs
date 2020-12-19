namespace LoxFs

open Token

module Expr =
    type IVisitor<'T> =
        abstract VisitBinaryExpr: Binary -> 'T
        abstract VisitGroupingExpr: Grouping -> 'T
        abstract VisitLiteralExpr: Literal -> 'T
        abstract VisitUnaryExpr: Unary -> 'T

    and Expr() =
        // abstract Accept: IVisitor<'T> -> 'T
        member __.Accept (visitor: IVisitor<'T>) = visitor

    and Binary(left, operator, right) as this =
        inherit Expr()
        let left: Expr = left
        let literal: Token = operator
        let right: Expr = right
        member __.Accept (visitor: IVisitor<'T>) = visitor.VisitBinaryExpr(this)

    and Grouping(expression) as this =
        inherit Expr()
        let expression: Expr = expression
        member __.Accept (visitor: IVisitor<'T>) = visitor.VisitGroupingExpr(this)

    and Literal(value) as this =
        inherit Expr()
        let value: Expr = value
        member __.Accept (visitor: IVisitor<'T>) = visitor.VisitLiteralExpr(this)

    and Unary(operator, right) as this =
        inherit Expr()
        let operator: Token = operator
        let right: Expr = right
        member __.Accept (visitor: IVisitor<'T>) = visitor.VisitUnaryExpr(this)