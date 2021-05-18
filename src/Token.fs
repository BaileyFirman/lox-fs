namespace LoxFs

open TokenType

module Token =
    type Token(tokenType: TokenType, lexeme: string, literal: obj, line: int) =
        member __.TokenType = tokenType
        member __.Lexeme = lexeme
        member __.Literal = literal
        member __.Line = line

        override __.ToString() = $"{tokenType} {lexeme} {literal}"
