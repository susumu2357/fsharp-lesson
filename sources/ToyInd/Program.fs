open SimpleSearch

[<EntryPoint>]
let main args =
    let searchResult = searchWordUnderDir args[0] args[1]

    // printfn "%A" searchResult

    searchResult
    |> Seq.length
    |> printfn "%d" 
    0
