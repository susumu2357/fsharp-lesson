#r "nuget:Deedle"

open Deedle

Frame.ReadCsv "C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\data\\シラバス.csv"

// #load "Deedle.fsx"
// Frame.ReadCsv "C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\data\\シラバス.csv"

let df = Frame.ReadCsv "C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\data\\シラバス.csv"
df.Print()

// 課題1: GetとGetAsの違いを調べよう。
let row = df.Rows.GetAt(0)
row.Get("専門")
row.GetAs("専門")
row.GetAs<string>("専門")

// 課題2: 専門が数学の行だけを残そう
df.RowsDense
|> Series.filterValues( fun row -> row.Get("専門")="数学")

// 課題3: 専門が数学の行だけを持ったFrameを作ろう
let math_df = 
    df.RowsDense 
    |> Series.filterValues( fun row -> row.Get("専門")="数学")
    |> Frame.ofRows

math_df.Print()