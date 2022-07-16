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

// 課題4: 場所と学年だけのFrameを作ろう
let selected_df = df.Columns[["場所"; "学年"]]
selected_df.Print()

// 課題5: フィルタとプロジェクションを関数にしよう
type Row = ObjectSeries<string>
and ConditionFunc = Row -> bool
and ColumnsList = string list

let filter (conditionFunc: ConditionFunc) (df: Frame<int, string>) =
    df.RowsDense 
    |> Series.filterValues( conditionFunc )
    |> Frame.ofRows

let conditionFunc = 
    fun row: Row -> row.Get("専門")="物理"


let project df: FrameData columns: ColumnsList =
    df.Columns[columns]