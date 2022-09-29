open System
open System.IO

#load "ExecuteProcess.fs"
open ExecuteProcess

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

// Need to prepare the binary file by executing "dotnet build -c Release"
let createCommand targetWord targetDir commandType =
    let addArguments c =
        c + " " + targetWord + " " + targetDir
        
    match commandType with
    | Ag -> addArguments "ag"
    | Grep -> addArguments "grep -R"
    | FsharpSimple -> addArguments "bin/Release/net6.0/ToyInd"

// Bake in targetWord and targetDir
let command = createCommand "pipe3" (Environment.CurrentDirectory + "/test_target/fparsec")

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
