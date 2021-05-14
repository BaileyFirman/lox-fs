namespace LoxFs

open Microsoft.FSharp.Core
open System
open System.Collections.Generic
open Token

module Env =
    type Env(enclosingEnv: Env option) =
        let mutable values = new Dictionary<string, obj>()

        let mutable enclosing: Env option = enclosingEnv

        member public __.Get (name: Token) =
            match values.ContainsKey name.lexeme with
            | true -> values.[name.lexeme]
            | false -> 
                match enclosing with
                | Some e -> e.Get name
                | None -> new obj() // Unreachable or error

        member public __.Define (name: string) value =
            values.[name] <- value

        member public __.Assign(name: Token, value: obj) =
            match values.ContainsKey name.lexeme with
            | true -> values.[name.lexeme] <- value
            | false ->
                match enclosing with
                | Some e -> e.Assign(name, value)
                | None -> ()

        member public __.Assign (name: Token, value: obj option) =
            let actual =
                match value with
                | Some v -> v
                | None -> new obj() // should be unreachable

            __.Assign (name, actual)