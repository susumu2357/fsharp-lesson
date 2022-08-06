open Eval

[<EntryPoint>]
let main args =
    // printfn "first argument: %s" args[0]
    eval args[0]
    0
