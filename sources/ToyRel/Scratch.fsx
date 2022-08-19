#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"

open FParsec

#r "nuget:RadLine"

open RadLine

open System

Environment.CurrentDirectory
// Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyRel"
Environment.CurrentDirectory <- @"C:\Users\susum\Documents\fsharp-lesson\sources\ToyRel"

#load "Common.fs"
open Common
#load "Parser.fs"
open Parser
#load "Relation.fs"
open Relation
#load "Eval.fs"
open Eval
#load "Interpreter.fs"
open Interpreter

// #load "StateM.fs"
// open StateM

// eval "print シラバス"
// eval "project (シラバス) 名前, 学年"
// eval "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
// eval "シラバス"

// eval "test_relation = project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
// eval "list"

// let state1 = State()

// let useDB dbName =
//     state1 {
//         let db = Database dbName
//         do! setValue db
//     }

// let getDB: StateM<Database, Database> =
//     state1 {
//         let! dbName = getValue
//         return dbName
//     }

// let dbInit = Database "master"
// let _, dbState = run (useDB "default") dbInit
// let currentDB, currentState = run getDB dbState

// let evalM str =
//     state1 {
//         let! (Database currentDB) = getDB
//         let openRelation = Relation.openRelation currentDB
//         let saveRelation = Relation.save currentDB
//         let saveAsRelation = Relation.saveAs currentDB

//         match paserResult pStmt str with
//         | PrintStmt printStmt -> evalPrintStmt printStmt openRelation
//         | AssignStmt (basename, expression) -> evalAssignStmt basename expression openRelation saveAsRelation
//         | ListingStmt _ ->
//             let databasePath = ".\\database\\" + currentDB
//             listing databasePath
//         | QuitStmt _ -> Environment.Exit 1
//         | Expression exp ->
//             match exp with
//             | ProjectExpression projectExpression ->
//                 let rel = evalProjectExpression projectExpression openRelation
//                 printfn "Relation %s returned." (saveRelation rel)
//             | Identifier identifier ->
//                 printfn "Only relation name is provided."
//                 printfn "Do you mean \"print\" ?"
//     }

// run (evalM "testM = project (index) author, title") dbInit
// run (evalM "print testM") dbInit

// evalM "list"

// differenceの実装、エラー処理無し
eval "use wikipedia"
eval "r2 = (project (Employee) DeptName) difference (project (Dept) DeptName)"
eval "print r2"

// difference revised
eval "use wikipedia"
// ColumnsNotMatch
eval "r2 = (project (Employee) DeptName) difference (project (Dept) Manager)"
// columnsOrderNotMatch
eval "r2 = (project (Employee) Name, DeptName) difference (project (Employee) DeptName, Name)"

eval "test = Employee"


eval "use wikipedia"
eval "(Employee) product (Dept)"
eval "(Employee) product (project (Dept) DeptName)"

let rel1 = Relation.openRelation "Employee"
let rel2 = Relation.openRelation "Dept"

let df1 =
    match rel1 with
    | Result.Ok r -> Relation.value r
    | Result.Error e -> failwithf "%A" e

let df2 =
    match rel2 with
    | Result.Ok r -> Relation.value r
    | Result.Error e -> failwithf "%A" e

df1.Print()
df2.Print()

let cols1 = df1.ColumnKeys |> Collections.Generic.HashSet
let cols2 = df2.ColumnKeys |> Seq.toList
let prefix = "test"

let renamedCols2 =
    [ for col in cols2 do
          if cols1.Contains col then
              prefix + "." + col
          else
              col ]

let renaming col =
    if cols1.Contains col then
        prefix + "." + col
    else
        col

let renamedDf2 = df2 |> Frame.mapColKeys renaming
renamedDf2.Print()

[ for col in cols1 do
      df1.Columns.[col].Values |> Seq.toList ]

let a = df1.IndexRowsWith({ 0 .. df1.RowCount })
let df3 = Frame.join JoinKind.Outer df1 renamedDf2
df3.Print()

let RowsAsList =
    df1.Rows.Values
    |> Seq.toList
    |> List.map (fun elm -> Series.values elm |> Seq.toList)

let expandRow (row: obj list) =
    List.map
        (fun elm ->
            List.replicate (Frame.countRows df2) elm
            |> List.toSeq
            |> Series.ofValues)
        row
    |> List.toSeq

expandRow RowsAsList[0]
// Frame(df1.ColumnKeys, expandRow RowsAsList[0])

let df4 =
    df1.Columns
    |> Series.mapValues (fun series ->
        Series.values series
        |> Seq.toList
        |> List.map (fun elm -> List.replicate (Frame.countRows df2) elm)
        |> List.concat
        |> List.toSeq
        |> Series.ofValues)
    |> Frame.ofColumns

df4.Print()

let df5 =
    renamedDf2.Columns
    |> Series.mapValues (fun series ->
        Series.values series
        |> Seq.toList
        |> List.replicate (Frame.countRows df1)
        |> List.concat
        |> List.toSeq
        |> Series.ofValues)
    |> Frame.ofColumns

df5.Print()

let df6 = df4.Join(df5)
df6.Print()

eval "use wikipedia"
eval "test = join (Employee) (Dept) (Employee.DeptName = Dept.DeptName)"
eval "test = join (Employee) (Dept) (Dept.DeptName = Employee.DeptName)"
eval "test = join (project (Employee) DeptName, Name) (Dept) (DeptName = Dept.DeptName)"
eval "test = join (Employee) (Dept) ((Employee.DeptName = Dept.DeptName) and (Employee.Name <> Dept.Manager))"
eval "test = join (Employee) (Dept) ((Employee.DeptName = Dept.DeptName) and (Employee.DeptName = \"Finance\"))"
eval "test = join (Employee) (Dept) ((Employee.DeptName = Dept.DeptName) and (Employee.DeptName = \"Sales\"))"
eval "test = join (project (Employee) DeptName, Name) (Dept) (Name = \"Harry\")"

let rel1Name = Some (Identifier "Employee")
let rel2Name = Some (Identifier "Dept")

let cond = 
    { Column1 = "DeptName"
      Column2 = "DeptName"
      Operator = Equal
      Relation1 = Some "Employee"
      Relation2 = Some "Dept"}
    |> ColumnColumn
    |> Condition.SingleCondition

let renamedDf (df: Frame<int, string>) relName =
    let cols = df.ColumnKeys |> Seq.toList
    let rename col =
        relName + "." + col
    df |> Frame.mapColKeys rename

let df3 = renamedDf df1 "testRel"
df3.Print()

let df4 = renamedDf df2 "testRel2"
df4.Print()

