// For more information see https://aka.ms/fsharp-console-apps

open System
open Argu
open Deedle

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


type DeedleArguments =
    | Filter of string * string
    | Projection of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Filter _ -> "The first argument should be a column name, the second argument should be a value to be used in the filter function."
            | Projection _ -> "Columns to be projected."

type Row = ObjectSeries<string>
and ConditionFunc = Row -> bool
and FilterFunc = string -> string -> Frame<int, string> -> Frame<int, string>
and ProjectFunc = string list -> Frame<int, string> -> Frame<int, string>

let deedleParser = ArgumentParser.Create<DeedleArguments>(programName="Deedle")

[<EntryPoint>]
let main2 args =
    printfn "raw arguments: %A" args
    let df = Frame.ReadCsv "C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\data\\シラバス.csv"
    let inputs = (deedleParser.Parse args).GetAllResults()
    
    let filter: FilterFunc =
        fun column value df_ ->
        let conditionFunc: ConditionFunc = 
            fun row -> row.GetAs<string>(column)=value
        df_.RowsDense 
        |> Series.filterValues( conditionFunc )
        |> Frame.ofRows

    let project: ProjectFunc =
        fun columns df -> df.Columns[columns]

    let applyFunc input df_=
        match input with
        | Filter (column, value) -> filter column value df_
        | Projection colList -> project colList df_

    let transformedDf = 
        inputs
        |> List.fold (fun tmp_df elm -> applyFunc elm tmp_df) df
    
    transformedDf.Print()

    0