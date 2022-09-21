module SimpleSearch

open System
open System.IO

let excludingExtensions = [".png"; ".jpg"; ".jpeg"] |> Collections.Generic.HashSet

/// <summary>Search the targetWord in the files located in the targetDir.</summary>
/// <returns>A sequence of "filepath: line" where the line contains the targetWord.</returns>
let searchWordWithinDir (targetWord:string) (targetDir: string) =
    let files = Directory.EnumerateFiles(targetDir)

    // Keep lines which contain the targetWord for each file.
    let filteredLines =
        files
        |> Seq.filter ( fun file -> not (excludingExtensions.Contains (FileInfo(file).Extension)))
        |> Seq.map File.ReadLines
        |> Seq.map (fun lines -> 
            lines |>
                Seq.filter (
                    fun line -> line.Contains targetWord
        ))
    
    // Combine file names and filtered lines.
    (files, filteredLines)
    ||> Seq.map2 (fun file lines ->
        lines
        |> Seq.map (fun line -> file + ": " + line)
     )
    |> Seq.collect id

let rec searchWordUnderDir (targetWord:string) (targetDir: string) =
    let currentDirResult = searchWordWithinDir targetWord targetDir

    let childrenDirs = Directory.EnumerateDirectories(targetDir)

    if childrenDirs |> Seq.isEmpty then
        currentDirResult
    else
        let childrenDirsResults =
            childrenDirs
            |> Seq.map (fun dir -> 
                searchWordUnderDir targetWord dir)
            |> Seq.collect id
        (currentDirResult, childrenDirsResults)
        ||> Seq.append
