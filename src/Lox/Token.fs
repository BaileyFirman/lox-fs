namespace LoxFs

open TokenType

module Token =
    type Token(tokenType, lexeme, literal, line) =
        member __.tokenType : TokenType = tokenType
        member __.lexeme : string = lexeme
        member __.literal : obj = literal
        member __.line : int = line

        override __.ToString() = $"{tokenType} {lexeme}"
