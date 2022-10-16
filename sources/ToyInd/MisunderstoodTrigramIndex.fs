module TrigramIndex

open System
open System.IO

open SimpleSearch
open FileIndex

module TrigramIndex =
    type T = TrigramIndex of Map<string, List<int>>

    let allChars = ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']

    let createDir (targetDir: string) (firstChar: char) =
        let firstString =
            if (Char.IsUpper firstChar) then "_" + string (Char.ToLower firstChar)
            else string firstChar
        let dirName = "test_index/" + (targetDir.Split [|'/'|])[1] + "/" + firstString
        Directory.CreateDirectory(dirName)
        |> ignore
        dirName

    let searchTrigram (targetDir: string) (firstChar: char) =
        let indexDir = targetDir.Replace("target", "index")
        let fileLookUp = FileIndex.lookupFileId (FileIndex.loadFileIndex indexDir)

        let chars = 
            allChars
            |> List.collect (fun x -> 
                allChars |> List.map (fun y -> (Char.ToString x) + (Char.ToString y))
                )
        let searchWord = searchWordUnderDir targetDir

        let searchResults =
            chars
            |> List.map (fun twoChars -> (Char.ToString firstChar) + twoChars )
            |> List.map (fun trigram -> searchWord trigram)
            |> List.map (fun searchResults -> 
                searchResults 
                |> Seq.map (fun (fileLine: string) -> (fileLine.Split [|':'|])[0])
                |> Seq.map (fun file -> fileLookUp file |> string)
                |> Seq.toList
                )

        (chars, searchResults)
        ||> List.map2 (fun twoChars searchResults ->
            (twoChars, searchResults)
        )
        |> Map

    let createTrigramIndex (targetDir: string) =
        let dirNames =
            allChars
            |> List.map (createDir targetDir)

        let maps =
            allChars
            |> List.map (searchTrigram targetDir)

        (dirNames, maps)
        ||> List.map2 (fun dirName map ->
            map
            |> Map.iter (fun k v -> 
                let fileName = dirName + (sprintf "/%s.txt" k)
                File.WriteAllLines(fileName, v)
                |> ignore
            )
        )
        |> ignore
        printfn "TrigramIndex for %s created" targetDir


