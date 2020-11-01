namespace LoxFs

open System
open System.IO
open Token
open TokenType

module Scanner =
   type Scanner(source) =
      let source: string = source
      let mutable tokens: List<Token> = []
      let mutable start = 0
      let mutable current = 0
      let mutable line = 1
      member this.IsAtEnd =
         current >= source.Length
      member this.ScanTokens: List<Token> =
         let rec scanLoop tokens =
            match this.IsAtEnd with
            | true -> ()
            | false ->
               start <- current
               // scanToken
               scanLoop tokens
         tokens <- tokens @ [Token(EOF, "", null, 0)]
         tokens
         

