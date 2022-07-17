// For more information see https://aka.ms/fsharp-console-apps

open System
open Argu

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

[<CliPrefix(CliPrefix.Dash)>]
type Arguments =
    | Hello
    | OtherArgument of string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Hello -> "Print Hello World."
            | OtherArgument _ -> "Dummy argument."

let parser = ArgumentParser.Create<Arguments>(programName="Hello")

[<EntryPoint>]
let main args =
    printfn "raw arguments: %A" args

    let inputs = (parser.Parse args).GetAllResults()

    let checkHello2 input=
        match input with
        | Hello -> printfn "Hello World!!"
        | OtherArgument x -> printfn "Other argument: %s" x

    inputs 
    |> List.map (fun elm -> checkHello2 elm)
    |> ignore

    0
