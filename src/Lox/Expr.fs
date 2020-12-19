namespace LoxFs

open Token

module Expr =
    type IVisitor<'T> =
        abstract member VisitBinaryExpr: Binary -> 'T
        abstract member VisitGroupingExpr: Grouping -> 'T
        abstract member VisitLiteralExpr: Literal -> 'T
        abstract member VisitUnaryExpr: Unary -> 'T

    and IExpr =
        // inherit IVisitor<obj>
        abstract member Accept :IVisitor<'T> -> 'T

    and Binary(left, operator, right) as this =
        member __.left: IExpr = left
        member __.operator: Token = operator
        member __.right: IExpr = right
        interface IExpr with
            member __.Accept (visitor: IVisitor<'T>) = visitor.VisitBinaryExpr(this)

    and Grouping(expression) as this =
        member __.expression: IExpr = expression
        interface IExpr with
            member __.Accept (visitor: IVisitor<'T>) = visitor.VisitGroupingExpr(this)

    and Literal(value: obj) as this =
        member __.value: obj = value
        interface IExpr with
            member __.Accept (visitor: IVisitor<'T>) = visitor.VisitLiteralExpr(this)

    and Unary(operator, right) as this =
        member __.operator: Token = operator
        member __.right: IExpr = right
        interface IExpr with
            member __.Accept (visitor: IVisitor<'T>) = visitor.VisitUnaryExpr(this)