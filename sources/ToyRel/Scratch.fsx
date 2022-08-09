#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"

open FParsec

open System

Environment.CurrentDirectory
Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyRel"

#load "Common.fs"
open Common
#load "Parser.fs"
open Parser
#load "Relation.fs"
open Relation
#load "Eval.fs"
open Eval
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
