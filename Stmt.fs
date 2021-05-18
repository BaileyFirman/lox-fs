namespace LoxFs

open Token
open Expr

module Stmt =
    type IStmtVisitor<'T> =
        abstract VisitBlockStmt : Block -> 'T
        abstract VisitExpressionStmt : Expression -> 'T
        abstract VisitFuncStmt : Func -> 'T
        abstract VisitIfStmt : If -> 'T
        abstract VisitPrintStmt : Print -> 'T
        abstract VisitReturnStmt : Return -> 'T
        abstract VisitVarStmt : Var -> 'T
        abstract VisitWhileStmt : While -> 'T

    and IStmt =
        abstract Accept : IStmtVisitor<'T> -> 'T

    and Block(statements: list<IStmt>) =
        member __.Statements = statements

        interface IStmt with
            member __.Accept v = v.VisitBlockStmt __

    and Expression(expression: IExpr) =
        member __.Expression = expression

        interface IStmt with
            member __.Accept v = v.VisitExpressionStmt __

    and Func(name: Token, fparams: list<Token>, body: list<IStmt>) =
        member __.Name = name
        member __.Fparams = fparams
        member __.Body = body

        interface IStmt with
            member __.Accept v = v.VisitFuncStmt __

    and If(condition: IExpr, thenBranch: IStmt, elseBranch: IStmt option) =
        member __.Condition = condition
        member __.ThenBranch = thenBranch
        member __.ElseBranch = elseBranch

        interface IStmt with
            member __.Accept v = v.VisitIfStmt __

    and Print(expression: IExpr) =
        member __.Expression = expression

        interface IStmt with
            member __.Accept v = v.VisitPrintStmt __

    and Return(keyword: Token, value: IExpr option) =
        member __.Keyword = keyword
        member __.Value = value

        interface IStmt with
            member __.Accept v = v.VisitReturnStmt __

    and Var(token: Token, initializer: IExpr) =
        member __.Name = token
        member __.Initializer = initializer

        interface IStmt with
            member __.Accept v = v.VisitVarStmt __

    and While(condition: IExpr, body: IStmt) =
        member __.Condition = condition
        member __.Body = body

        interface IStmt with
            member __.Accept v = v.VisitWhileStmt __
