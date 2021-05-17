namespace LoxFs

open Token
open Expr

module Stmt =
    type IStmtVisitor<'T> =
        abstract VisitExpressionStmt : Expression -> 'T
        abstract VisitPrintStmt : Print -> 'T
        abstract VisitVarStmt : Var -> 'T
        abstract VisitBlockStmt : Block -> 'T
        abstract VisitIfStmt : If -> 'T
        abstract VisitWhileStmt : While -> 'T
        abstract VisitVoidStmt : VoidStmt -> 'T
        abstract VisitFuncStmt : Func -> 'T
        abstract VisitReturnStmt : Return -> 'T

    and IStmt =
        abstract Accept : IStmtVisitor<'T> -> 'T

    and VoidStmt() as this =
        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitVoidStmt(this)

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

    and If(condition, thenBranch, elseBranch) as this =
        member __.Condition : IExpr = condition
        member __.ThenBranch : IStmt = thenBranch
        member __.ElseBranch : IStmt option = elseBranch

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitIfStmt(this)

    and While(condition, body) as this =
        member __.Condition : IExpr = condition
        member __.Body : IStmt = body

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitWhileStmt(this)

    and Func(name, fparams, body) as this =
        member __.Name : Token = name
        member __.Fparams : list<Token> = fparams
        member __.Body : list<IStmt> = body

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitFuncStmt(this)

    and Return(keyword, value) as this =
        member __.Keyword : Token = keyword
        member __.Value : IExpr option = value

        interface IStmt with
            member __.Accept(visitor: IStmtVisitor<'T>) = visitor.VisitReturnStmt(this)
