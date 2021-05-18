namespace LoxFs

module Error =
    type public ErrorHandler(hasError: bool) =
        let mutable hadError = hasError

        member __.Error line message = __.Report line "" message

        member __.HadError = hadError

        member __.Report line where message =
            printfn $"[line {line}] Error{where}{message}"

        member __.SetError error = hadError <- error
