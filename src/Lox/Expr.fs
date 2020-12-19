namespace LoxFs

open Token
open TokenType

module Expr =
    type Expr() = class end

    type Binary(left, operator, right)=
        inherit Expr()
        let left: Expr = left
        let literal: Token = operator
        let right: Expr = right
