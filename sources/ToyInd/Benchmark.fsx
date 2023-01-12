open System
open System.IO
open Microsoft.FSharp.Reflection

#load "ExecuteProcess.fs"
open ExecuteProcess

type Command =
    | Ag
    | Grep
    | FsharpSimple // open all files and search the word one by one
    | FsharpNaiveTrigram // look up the trigram index to narrow down possibility

let commandCases = [Ag; Grep; FsharpSimple; FsharpNaiveTrigram]

// Prepare labels
let createLabels datasetName =
    commandCases
    |> List.map (fun case ->
        match case with
        | Ag -> datasetName + "_ag"
        | Grep -> datasetName + "_grep"
        | FsharpSimple -> datasetName + "_fsharpSimple"
        | FsharpNaiveTrigram -> datasetName + "_fsharpNaiveTrigram"
    )

// Set the dataset name and the search word
let datasetName = "fsharp"
let searchWord = "generics"

let labels = createLabels datasetName

// Need to prepare the binary file by executing "dotnet build -c Release"
// FsharpNaiveTrigram was prepared with "dotnet build -c Release -o tmp/Release/net6.0/"
let createCommand targetWord targetDir commandType =
    let addArguments c =
        c + " " + targetWord + " " + targetDir
        
    match commandType with
    | Ag -> addArguments "ag"
    | Grep -> addArguments "grep -R"
    | FsharpSimple -> addArguments "bin/Release/net6.0/ToyInd"
    | FsharpNaiveTrigram -> addArguments "tmp/ToyInd"

// Bake in targetWord and targetDir
let command = createCommand searchWord (Environment.CurrentDirectory + sprintf "/test_target/%s" datasetName)

// Measure time and memory usage for each command.
let results =
    commandCases
    |> List.map command
    |> List.map (fun commandString -> "-f \"%e,%M\" " + commandString)
    |> List.map (executeProcess "/usr/bin/time")
    |> List.map (fun result -> result.StdErr)

let previousResults = File.ReadAllLines("newBenchmarkResults.csv") |> Seq.toList
let resultsWithLabels =
    if List.length previousResults = 0 then
        // Add the header
        ["label,time(sec),memory(kB)"] @ (
            (labels, results) 
            ||> List.map2 (fun label results -> label + "," + results)
            )
    else
        // Append the results
        previousResults @ (
            (labels, results) 
            ||> List.map2 (fun label results -> label + "," + results)
            )

File.WriteAllLines("newBenchmarkResults.csv", resultsWithLabels)
