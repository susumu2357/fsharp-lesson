module TrigramIndex

open System
open System.IO
open System.Text.RegularExpressions

open SimpleSearch
open FileIndex

module TrigramIndex =
    type T = TrigramIndex of Map<string, string list>

    let ngram (n: int) (line: string) =
        let rec recNgram (n: int) (line: string) (acc: string list) =
            if (Seq.length line) >= n then
                let nLetters = line[0..n-1]
                if Regex.IsMatch(nLetters, (sprintf "[a-zA-Z0-9]{%d}" n)) then
                    recNgram n line[1..] (nLetters::acc)
                else
                    recNgram n line[1..] acc
            else
                acc

        recNgram n line []

    // distinctTrigrams looks like 
    // seq[ seq["abc"; "def"; ...]; seq["Abc"; "1ab"]; ...]
    // The outer seq index corresponds to the file id
    let createDistinctTrigrams files =
        let excludingExtensions = [".png"; ".jpg"; ".jpeg"] |> Collections.Generic.HashSet

        async {
            let distinctTrigrams =
                files
                |> Seq.filter ( fun file -> not (excludingExtensions.Contains (FileInfo(file).Extension)))
                |> Seq.map File.ReadLines
                |> Seq.map (fun lines -> 
                    lines 
                    |> Seq.map (ngram 3)
                    |> Seq.concat
                    |> Seq.distinct
                )
            return distinctTrigrams
        }

    let updateMap (i: int) (accMap: Map<string, string list>) (trigrams: seq<string>) =
        let updatedtrigramMap =
            trigrams
            |> Seq.fold (fun accMap trigram ->
                if (Map.containsKey trigram accMap) then 
                    Map.add trigram (accMap[trigram]@[string i]) accMap
                else 
                    Map.add trigram [string i] accMap
            ) accMap
        (i+1, updatedtrigramMap)

    // trigramMap looks like 
    // map[ ("abc", ["1"; "20"; ...]); ("Abc", ["100"; "123"; ...]); ...]
    // Mapping from a trigram to a list of file ids which contain the trigram
    let createTrigramMap (distinctTrigrams: seq<seq<string>>) =
        async {
            let (_, trigramMap) =        
                distinctTrigrams
                |> Seq.fold (fun (i, accMap) trigrams ->
                    updateMap i accMap trigrams
                ) (1, Map.empty<string, string list>)
            return trigramMap
        }

    let writingTrigramMap (targetDir: string) (trigramMap: Map<string, string list>) =
        async {
            trigramMap
            |> Map.iter (fun trigram fileIds ->
                    let firstChar =
                        if Char.IsUpper trigram[0] then "_" + string (Char.ToLower trigram[0])
                        else string trigram[0]
                    let dirName = "test_index/" + (targetDir.Split [|'/'|] |> Array.last) + "/" + firstChar
                    
                    Directory.CreateDirectory(dirName)
                    |> ignore            

                    let fileName = dirName + (sprintf "/%s.txt" trigram[1..])
                    File.WriteAllLines(fileName, fileIds)
                    |> ignore            
            )
        }
        
    let createTrigramIndex (targetDir: string) =
        let dirName = "test_index/" + (targetDir.Split [|'/'|] |> Array.last)
        Directory.CreateDirectory(dirName)
        |> ignore

        let files = Directory.EnumerateFiles(targetDir, "*", SearchOption.AllDirectories)
            
        let trigramMap =
            files
            |> createDistinctTrigrams
            |> Async.RunSynchronously
            |> createTrigramMap
            |> Async.RunSynchronously

        printfn "Writing TrigramIndex..."

        trigramMap
        |> writingTrigramMap targetDir
        |> Async.StartImmediate

        printfn "TrigramIndex for %s created" targetDir

    
    let loadWholeTrigramIndex (indexDir: string) =
        async {
            let files = Directory.EnumerateFiles(indexDir, "*", SearchOption.AllDirectories)
            let trigramIndex =
                files
                |> Seq.filter (fun fileName ->
                    Regex.IsMatch(FileInfo(fileName).Name.Replace(".txt", ""), "[a-zA-Z0-9]{2}")
                )
                |> Seq.map (fun fileName ->
                    let firstChar = 
                        let dirName = FileInfo(fileName).DirectoryName.Split [|'/'|] |> Array.last
                        if dirName.StartsWith "_" then dirName.ToUpper()
                        else dirName
                    let trigram = firstChar + FileInfo(fileName).Name.Replace(".txt", "")

                    let fileIds = File.ReadAllLines fileName |> Array.toList

                    (trigram, fileIds)
                )
                |> Seq.fold (fun accMap keyVal ->
                    let (key, value) = keyVal
                    Map.add key value accMap
                ) Map.empty<string, string list>
            return trigramIndex
        }
     
    let fetchTrigramIndex (indexDir: string) (searchWord: string) (showFileIds: bool) =
        let trigrams = 
            ngram 3 searchWord
            |> List.distinct

        let files =
            trigrams
            |> List.map (fun trigram ->
                let firstChar =
                    if Char.IsUpper trigram[0] then "_" + string (Char.ToLower trigram[0])
                    else string trigram[0]
                indexDir + (sprintf "/%s/" firstChar) + trigram[1..] + ".txt"
                )
        
        let trigramIndex = 
            (trigrams, files)
            ||> Seq.map2 (fun trigram fileName ->
                let fileIds = File.ReadAllLines fileName |> Array.toList

                (trigram, fileIds)
            )
            |> Seq.fold (fun accMap keyVal ->
                let (key, value) = keyVal
                Map.add key value accMap
            ) Map.empty<string, string list>

        if showFileIds then 
            printfn "The number of file ids for the first trigram: %i" (List.length trigramIndex[trigrams[0]])

        trigramIndex

    let searchWord (searchWord: string) (targetDir: string) (showFileIds: bool) =
        let indexDir = Environment.CurrentDirectory + "/test_index/" + (targetDir.Split [|'/'|] |> Array.last)
        let fileIndex = FileIndex.loadInverseFileIndex indexDir
        let trigramMap = fetchTrigramIndex indexDir searchWord showFileIds
        let firstKey = trigramMap.Keys |> Seq.toList |> List.head

        let filePaths =
            trigramMap
            |> Map.fold (fun accIds trigram ids ->
                accIds
                |> Set.ofList
                |> Set.intersect (Set.ofList ids)
                |> Set.toList
            ) trigramMap[firstKey]
            |> List.map (fun fileId ->
                FileIndex.lookupFilePath fileIndex (int fileId)
            )
            |> List.toSeq
        
        if showFileIds then
            printfn "The number of file ids after taking intersection: %i" (Seq.length filePaths)

        SimpleSearch.searchWordFromFiles searchWord filePaths

        