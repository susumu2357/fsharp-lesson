module TrigramIndex

open System
open System.IO
open System.Text.RegularExpressions

module TrigramIndex =
    type T = TrigramIndex of Map<string, string list>

    let createTrigramIndex (targetDir: string) =
        let excludingExtensions = [".png"; ".jpg"; ".jpeg"] |> Collections.Generic.HashSet

        let dirName = "test_index/" + (targetDir.Split [|'/'|])[1]
        Directory.CreateDirectory(dirName)
        |> ignore

        let files = Directory.EnumerateFiles(targetDir, "*", SearchOption.AllDirectories)

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
        let (_, trigramMap) =        
            distinctTrigrams
            |> Seq.fold (fun (i, accMap) trigrams ->
                updateMap i accMap trigrams
            ) (1, Map.empty<string, string list>)

        printfn "Writing TrigramIndex..."

        trigramMap
        |> Map.iter (fun trigram fileIds ->
            let firstChar =
                if Char.IsUpper trigram[0] then "_" + string (Char.ToLower trigram[0])
                else string trigram[0]
            let dirName = "test_index/" + (targetDir.Split [|'/'|])[1] + "/" + firstChar
            
            Directory.CreateDirectory(dirName)
            |> ignore            

            let fileName = dirName + (sprintf "/%s.txt" trigram[1..])
            File.WriteAllLines(fileName, fileIds)
            |> ignore            
        )

        printfn "TrigramIndex for %s created" targetDir
