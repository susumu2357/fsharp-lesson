open System
open System.IO

let excludingExtensions = [".png"; ".jpg"; ".jpeg"] |> Collections.Generic.HashSet

/// <summary>Search the targetWord in the files located in the targetDir.</summary>
/// <returns>A sequence of "filepath: line" where the line contains the targetWord.</returns>
let searchWord targetDir targetWord =
    let files = Directory.EnumerateFiles(targetDir)

    // Keep lines which contain the targetWord for each file.
    let filteredLines =
        files
        |> Seq.filter ( fun file -> not (excludingExtensions.Contains (FileInfo(file).Extension)))
        |> Seq.map File.ReadLines
        |> Seq.map (fun lines -> lines |> Seq.cast<string>)
        |> Seq.map (fun lines -> 
            lines |>
                Seq.filter (
                    fun line -> line.Contains (targetWord:string)
        ))
    
    // Combine file names and filtered lines.
    (files, filteredLines)
    ||> Seq.map2 (fun file lines ->
        lines
        |> Seq.map (fun line -> file + ": " + line)
     )
    |> Seq.collect id

[<EntryPoint>]
let main args =
    printfn "%A" (searchWord args[0] args[1])
    0
