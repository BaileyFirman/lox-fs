namespace LoxFs

module TokenType =
    type TokenType =
        // Single-character tokens.
        | LEFTPAREN
        | RIGHTPAREN
        | LEFTBRACE
        | RIGHTBRACE
        | COMMA
        | DOT
        | MINUS
        | PLUS
        | SEMICOLON
        | SLASH
        | STAR
        // One or two character tokens.
        | BANG
        | BANGEQUAL
        | EQUAL
        | EQUALEQUAL
        | GREATER
        | GREATEREQUAL
        | LESS
        | LESSEQUAL
        // Literals.
        | IDENTIFIER
        | STRING
        | NUMBER
        // Keywords
        | AND
        | CLASS
        | ELSE
        | FALSE
        | FUN
        | FOR
        | IF
        | NIL
        | OR
        | PRINT
        | RETURN
        | SUPER
        | THIS
        | TRUE
        | VAR
        | WHILE
        | EOF
        // Implementation specific
        | ERROR
        | COMMENT
        | WHITESPACE

