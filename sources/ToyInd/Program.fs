open System.IO

let path = DirectoryInfo("C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\ToyInd\\test_target")

let files = path.EnumerateFiles()


files
|> Seq.cast<string>
|> Seq.map File.ReadLines 
|> printfn "%A"