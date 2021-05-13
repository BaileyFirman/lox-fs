namespace LoxFs

open System
open Token
open TokenType
open Error

module Scanner =
    type Scanner(source, errorHandler) =
        let errorHandler : ErrorHandler = errorHandler
        let source : string = source
        let mutable tokens : List<Token> = []
        let mutable start = 0
        let mutable current = 0
        let mutable line = 1
        
        let isAtEnd () =
            current >= source.Length

        let peek () =
            if isAtEnd () then '\u0004'
            else source.[current]

        let peekNext () =
            if(current + 1 >= source.Length)
            then '\u0004'
            else source.[current + 1]


        let advance () =
            let c = source.[current]
            current <- current + 1
            c

        let addToken2 tokenType literal =
            let text = source.[start..(current - 1)]
            let token = Token(tokenType, text, literal, line)
            tokens <- (token :: tokens)
            ()

        let addToken tokenType =
            addToken2 tokenType null

        let matchToken expected: bool =
            if isAtEnd ()
            then false
            else
                if source.[current] <> expected
                then false
                else
                    current <- current + 1
                    true

        let number () =
            while (Char.IsDigit (peek ())) do
                advance ()

            if (peek () = '.' && (Char.IsDigit(peekNext())))
            then
                advance ()

                while (Char.IsDigit(peekNext())) do
                    advance ()
            else
                ()

            let double =
                    Double.Parse source.[start..(current - 1)]

            addToken2 NUMBER double

        let identifier () =
            while (Char.IsLetterOrDigit (peek ())) do
                advance ()

            let text = source.[start..(current - 1)]
            // printfn $">>>>>>> {text}"

            let tokenType =
                match text with
                | "and" -> AND
                | "class" -> CLASS
                | "else" -> ELSE
                | "false" -> FALSE
                | "for" -> FOR
                | "fun" -> FUN
                | "if" -> IF
                | "nil" -> NIL
                | "or" -> OR
                | "print" -> PRINT
                | "return" -> RETURN
                | "super" -> SUPER
                | "this" -> THIS
                | "true" -> TRUE
                | "var" -> VAR
                | "while" -> WHILE
                | _ -> IDENTIFIER

            addToken tokenType

        let scanToken () =
            let c = advance()

            match c with
            | '(' -> addToken LEFTPAREN
            | ')' -> addToken RIGHTPAREN
            | '{' -> addToken LEFTBRACE
            | '}' -> addToken RIGHTBRACE
            | ',' -> addToken COMMA
            | '.' -> addToken DOT
            | '-' -> addToken MINUS
            | '+' -> addToken PLUS
            | ';' -> addToken SEMICOLON
            | '*' -> addToken STAR
            | '!' ->
                let kind = if matchToken '=' then BANGEQUAL else BANG
                addToken kind
            | '=' ->
                let kind = if matchToken '=' then EQUALEQUAL else EQUAL
                addToken kind
            | '<' ->
                let kind = if matchToken '=' then LESSEQUAL else LESS
                addToken kind
            | '>' ->
                let kind = if matchToken '=' then GREATEREQUAL else GREATER
                addToken kind
            | '/' ->
                if matchToken '/'
                then
                    while (peek() <> '\n' && not(isAtEnd ())) do
                        advance ()
                else
                    addToken SLASH
            | ' '
            | '\r'
            | '\t' -> ()
            | '\n' ->
                line <- line + 1
            | '"' ->
                while (peek() <> '"' && not(isAtEnd ())) do
                    if peek () = '\n'
                    then line <- line + 1
                    else ()

                    advance ()

                // if isAtEnd () then

                advance ()

                let value = source.[(start + 1)..(current - 2)]  // maybe bug
                addToken2 STRING value

                ()
            | c ->
                if (Char.IsDigit c)
                then
                    number ()
                else
                    if (Char.IsLetter c)
                    then
                        identifier ()
                    else
                        ()

        member __.ScanTokens : seq<Token> =
            while not (isAtEnd ()) do
                start <- current
                scanToken ()

            (Token(EOF, "", null, 0) :: tokens)
            |> List.rev
            |> List.toSeq
