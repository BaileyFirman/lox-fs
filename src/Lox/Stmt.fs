namespace LoxFs

open Token
open Expr

module Stmt =
    type IStmtVisitor<'T> =
        abstract VisitExpressionStmt : Expression -> 'T
        abstract VisitPrintStmt : Print -> 'T
        abstract VisitVarStmt : Var -> 'T
        abstract VisitBlockStmt : Block -> 'T

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

    and Var(token, initializer) as this =
        member __.Name : Token = token
        member __.Initializer : IExpr = initializer

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitVarStmt(this)

    and Block(statements) as this =
        member __.Statements : list<IStmt> = statements

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitBlockStmt(this)
