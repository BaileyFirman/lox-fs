namespace LoxFs

open Expr

module Stmt =
    type IStmtVisitor<'T> =
        abstract VisitExpressionStmt : Expression -> 'T
        abstract VisitPrintStmt : Print -> 'T

    and IStmt =
        abstract Accept : IStmtVisitor<'T> -> 'T

    and Expression(expression) as this =
        member __.Expression : IExpr = expression

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitExpressionStmt(this)

    and Print(expression) as this =
        member __.Expression : IExpr = expression

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitPrintStmt(this)
