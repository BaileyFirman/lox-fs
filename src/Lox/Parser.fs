namespace LoxFs

open Token

module Parser =
    type Parser(tokens) =
        let tokens: Token[] = tokens
        let mut current = 0