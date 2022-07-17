// For more information see https://aka.ms/fsharp-console-apps

open System

let checkHello argArray =
    argArray
    |> List.ofArray
    |> List.map (fun s -> s="hello")
    |> List.fold (fun result b -> result || b) false

// let arg = Environment.GetCommandLineArgs()
// printfn "%A" arg

// let isHello = checkHello arg

// if isHello then 
//     printfn "Hello World!!"
// else
//     printfn "I don't know"

[<EntryPoint>]
let main args =
    printfn "%A" args
    
    let isHello = checkHello args
    if isHello then 
        printfn "Hello World!!"
    else
        printfn "I don't know"
    // Return 0. This indicates success.
    0
