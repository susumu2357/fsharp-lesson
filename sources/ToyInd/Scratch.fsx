open System
open System.IO

#load "SimpleSearch.fs"
open SimpleSearch

// Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyInd"
// Environment.CurrentDirectory <- @"C:\Users\susum\Documents\fsharp-lesson\sources\ToyInd"

searchWordUnderDir "pipe3" "test_target"

#load "executeProcess.fs"
open executeProcess

type Command =
    | Ag
    | Grep
    | FsharpSimple // SimpleSearch

// Prepare labels
let createLabels datasetName =
    [Ag; Grep; FsharpSimple]
    |> List.map (fun case ->
        match case with
        | Ag -> datasetName + "_ag"
        | Grep -> datasetName + "_grep"
        | FsharpSimple -> datasetName + "_fsharpSimple"
    )
let labels = createLabels "fparsec"


let createCommand targetWord targetDir commandType =
    let addArguments c =
        c + " " + targetWord + " " + targetDir
        
    match commandType with
    | Ag -> addArguments "ag"
    | Grep -> addArguments "grep -R"
    | FsharpSimple -> addArguments "dotnet run -c Release"

// Bake in targetWord and targetDir
let command = createCommand "pipe3" "test_target/fparsec"

// Measure time and memory usage for each command.
let results =
    [Ag; Grep; FsharpSimple]
    |> List.map command
    |> List.map (fun commandString -> "-f \"%e,%M\" " + commandString)
    |> List.map (executeProcess "/usr/bin/time")
    |> List.map (fun result -> result.StdErr)

// Add the header
let resultsWithLabels =
    ["label,time(sec),memory(kB)"] @ (
        (labels, results) 
        ||> List.map2 (fun label results -> label + "," + results)
        )

File.WriteAllLines("benchmarkResults.csv", resultsWithLabels)
