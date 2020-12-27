namespace LoxFs

open TokenType

module Token =
    type Token(tokenType, lexeme, literal, line) =
        let tr = tokenType
        member __.tokenType: TokenType = tokenType
        member __.lexeme: string = lexeme
        member __.literal: obj = literal
        member __.line: int = line

        override this.ToString() =
            let tokenTypeString = tokenType.ToString()
            $"{tokenTypeString} {lexeme}"
