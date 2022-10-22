module SimpleSearch

open System
open System.IO

let excludingExtensions = [".png"; ".jpg"; ".jpeg"] |> Collections.Generic.HashSet

let searchWordFromFiles (targetWord:string) (targetFilePaths: string seq) =
    // Keep lines which contain the targetWord for each file.
    let filteredLines =
        targetFilePaths
        |> Seq.filter ( fun file -> not (excludingExtensions.Contains (FileInfo(file).Extension)))
        |> Seq.map File.ReadLines
        |> Seq.map (fun lines -> 
            lines |>
                Seq.filter (
                    fun line -> line.Contains targetWord
        ))

    // Combine file names and filtered lines.
    (targetFilePaths, filteredLines)
    ||> Seq.map2 (fun file lines ->
        lines
        |> Seq.map (fun line -> file + ": " + line)
        )
    |> Seq.collect id

/// <summary>Search the targetWord in the files located in the targetDir and subdirectories under the targetDir.</summary>
/// <returns>A sequence of "filepath: line" where the line contains the targetWord.</returns>
let searchWordUnderDir (targetWord:string) (targetDir: string) =
    let files = Directory.EnumerateFiles(targetDir, "*", SearchOption.AllDirectories)

    searchWordFromFiles targetWord files
