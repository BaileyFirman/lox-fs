namespace LoxFs

open Token
open TokenType
open Expr

module AstPrinter =
    type AstPrinter() =
        let testExpr = Binary (
                            Unary (
                                (Token (MINUS, "-", (), 1)),
                                (Literal 123)),
                            Token(STAR, "*", (), 1),
                            Grouping
                                (Literal(45.67)))
