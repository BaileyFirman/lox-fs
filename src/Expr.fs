namespace LoxFs

open Token

module Expr =
    type IVisitor<'T> =
        abstract VisitAssignExpr : Assign -> 'T
        abstract VisitBinaryExpr : Binary -> 'T
        abstract VisitCallExpr : Call -> 'T
        abstract VisitGroupingExpr : Grouping -> 'T
        abstract VisitLiteralExpr : Literal -> 'T
        abstract VisitLogicalExpr : Logical -> 'T
        abstract VisitUnaryExpr : Unary -> 'T
        abstract VisitVariableExpr : Variable -> 'T

    and IExpr =
        abstract Accept : IVisitor<'T> -> 'T

    and Assign(name: Token, value: IExpr) =
        member __.Name = name
        member __.Value = value

        interface IExpr with
            member __.Accept v = v.VisitAssignExpr __

    and Binary(left: IExpr, operator: Token, right: IExpr) =
        member __.Left = left
        member __.Operator = operator
        member __.Right = right

        interface IExpr with
            member __.Accept v = v.VisitBinaryExpr __

    and Call(callee: IExpr, paren: Token, arguments: list<IExpr>) =
        member __.Callee = callee
        member __.Paren = paren
        member __.Argurments = arguments

        interface IExpr with
            member __.Accept v = v.VisitCallExpr __

    and Grouping(expression: IExpr) =
        member __.Expression = expression

        interface IExpr with
            member __.Accept v = v.VisitGroupingExpr __

    and Literal(value: obj) =
        member __.Value = value

        interface IExpr with
            member __.Accept v = v.VisitLiteralExpr __

    and Logical(left: IExpr, operator: Token, right: IExpr) =
        member __.Left = left
        member __.Operator = operator
        member __.Right = right

        interface IExpr with
            member __.Accept v = v.VisitLogicalExpr __

    and Unary(operator: Token, right: IExpr) =
        member __.Operator = operator
        member __.Right = right

        interface IExpr with
            member __.Accept v = v.VisitUnaryExpr __

    and Variable(name: Token) =
        member __.Name = name

        interface IExpr with
            member __.Accept v = v.VisitVariableExpr __
