module FileIndex

open System.IO

module FileIndex =
    type T = FileIndex of Map<string, int>

    let createFileIndex (targetDir: string) =
        let dirName = "test_index/" + (targetDir.Split [|'/'|])[1]
        Directory.CreateDirectory(dirName)
        |> ignore

        let fileName = dirName + "/path_list.txt"

        let files = Directory.EnumerateFiles(targetDir, "*", SearchOption.AllDirectories)

        File.WriteAllLines(fileName, files)
        |> ignore

        printfn "FileIndex %s created" fileName

    let loadFileIndex (targetDir: string) =
        let fileName: string = targetDir + "/path_list.txt"
        if File.Exists fileName then
            File.ReadAllLines(fileName) 
            |> Seq.toList
            |> List.mapi (fun i elm -> (elm, i+1))
            |> Map
            |> FileIndex
        else
            failwithf "File does not exist"

    let lookupFileId (FileIndex fileIndex) (filePath: string) =
        fileIndex.[filePath]
