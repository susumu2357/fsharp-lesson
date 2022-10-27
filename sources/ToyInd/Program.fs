open SimpleSearch
open TrigramIndex

open System.Resources.Extensions

[<EntryPoint>]
let main args =
    let searchResult = 
        // searchWordUnderDir args[0] args[1]
        TrigramIndex.searchWord args[0] args[1] false

    // printfn "%A" searchResult

    searchResult
    |> Seq.length
    |> printfn "%d" 
    0
