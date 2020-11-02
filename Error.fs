namespace LoxFs

module Error =
    type public ErrorHandler(hasError) =
        let mutable hadError: bool = hasError

        member __.Report line where message =
            printfn "[line %d] Error%s:%s" line where message

        member __.Error line message = __.Report line "" message

        member __.HadError = hadError
        member __.SetError = hadError <- true
