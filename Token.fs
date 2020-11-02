namespace LoxFs

open TokenType

module Token =
    type Token(tokenType, lexeme, literal, line) =
        let tokenType: TokenType = tokenType
        let lexeme: string = lexeme
        let literal: obj = literal
        let line: int = line

        member this.ToString() =
            let tokenTypeString = tokenType.ToString()
            let literalString = literal.ToString()
            printfn "%s %s %s" tokenTypeString lexeme literalString
