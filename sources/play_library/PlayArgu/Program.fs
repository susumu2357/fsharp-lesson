// For more information see https://aka.ms/fsharp-console-apps

open System

let checkHello argArray =
    argArray
    |> List.ofArray
    |> List.map (fun s -> s="hello")
    |> List.fold (fun result b -> result || b) false

let arg = Environment.GetCommandLineArgs()
printfn "%A" arg

let isHello = checkHello arg

if isHello then 
    printfn "Hello World!!"
else
    printfn "I don't know"
