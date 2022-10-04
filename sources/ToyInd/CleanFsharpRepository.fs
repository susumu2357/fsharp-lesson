module CleanFsharpRepository

open System
open System.IO

let targetExtensions = [".fsproj"; ".csproj"; ".props"; "targets"] |> Collections.Generic.HashSet

/// <summary>Delete .fsproj and similar to avoid loading too many projects.</summary>
let deleteFsprojUnderDir (targetDir: string) =
    let files = Directory.EnumerateFiles(targetDir, "*", SearchOption.AllDirectories)

    let deletefiles =
        files
        |> Seq.filter ( fun file -> (targetExtensions.Contains (FileInfo(file).Extension)))
        |> Seq.map (fun file -> File.Delete(file))
    
    Seq.length deletefiles
