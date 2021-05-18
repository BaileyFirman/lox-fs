namespace LoxFs

open Microsoft.FSharp.Core
open System.Collections.Generic
open Token

module Env =
    type Env(enclosingEnv: Env option) =
        let mutable values = new Dictionary<string, obj>()

        let mutable enclosing = enclosingEnv

        member public __.Assign(name: Token, value) =
            match values.ContainsKey name.lexeme with
            | true -> values.[name.lexeme] <- value
            | false ->
                match enclosing with
                | Some e -> e.Assign(name, value)
                | None -> ()

        member public __.Assign(name: Token, value: obj option) =
            match value with
            | Some v -> __.Assign(name, v)
            | None -> failwith "Could not assign variable with null value"

        member public __.Define (name) value = values.[name] <- value

        member public __.Get(name: Token) =
            match values.ContainsKey name.lexeme with
            | true -> values.[name.lexeme]
            | false ->
                match enclosing with
                | Some e -> e.Get name
                | None -> failwith "Could not find variable in global scope"
