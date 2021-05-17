namespace LoxFs

open Token

module Expr =
    type IVisitor<'T> =
        abstract VisitBinaryExpr : Binary -> 'T
        abstract VisitGroupingExpr : Grouping -> 'T
        abstract VisitLiteralExpr : Literal -> 'T
        abstract VisitUnaryExpr : Unary -> 'T
        abstract VisitVariableExpr : Variable -> 'T
        abstract VisitAssignExpr : Assign -> 'T
        abstract VisitLogicalExpr : Logical -> 'T
        abstract VisitCallExpr : Call -> 'T

    and IExpr =
        abstract Accept : IVisitor<'T> -> 'T

    and Binary(left, operator, right) as this =
        member __.Left : IExpr = left
        member __.Operator : Token = operator
        member __.Right : IExpr = right

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitBinaryExpr(this)

    and Grouping(expression) as this =
        member __.Expression : IExpr = expression

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitGroupingExpr(this)

    and Literal(value: obj) as this =
        member __.Value : obj = value

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitLiteralExpr(this)

    and Unary(operator, right) as this =
        member __.Operator : Token = operator
        member __.Right : IExpr = right

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitUnaryExpr(this)

    and Variable(name) as this =
        member __.Name : Token = name

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitVariableExpr(this)

    and Assign(name, value) as this =
        member __.Name : Token = name
        member __.Value : IExpr = value

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitAssignExpr(this)

    and Logical(left, operator, right) as this =
        member __.Left : IExpr = left
        member __.Operator : Token = operator
        member __.Right : IExpr = right

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) = visitor.VisitLogicalExpr(this)

    and Call(callee, paren, arguments) as this =
        member __.Callee : IExpr = callee
        member __.Paren : Token = paren
        member __.Argurments : list<IExpr> = arguments

        interface IExpr with
            member __.Accept(visitor: IVisitor<'T>) =
                visitor.VisitCallExpr(this)
