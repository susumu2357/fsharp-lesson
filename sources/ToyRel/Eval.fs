module Eval

open System

open Common
open Relation
open Parser

type EvalExpression = Expression -> (Identifier -> Relation.T) -> Relation.T
type EvalProjectExpression = ProjectExpression -> (Identifier -> Relation.T) -> Relation.T

let rec evalExpression: EvalExpression =
    fun exp openRelation ->
        match exp with
        | Identifier identifier -> openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression openRelation

and evalProjectExpression: EvalProjectExpression =
    fun projectExp openRelation ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp openRelation
        let (ColumnList columns) = columnList
        Relation.project rel columns

let evalPrintStmt identifier openRelation =
    let rel = openRelation (identifier: Identifier)
    Relation.print rel


let evalAssignStmt basename expression openRelation saveAsRelation =
    let rel = evalExpression expression openRelation
    saveAsRelation rel basename

let listing path =
    let files = System.IO.Directory.GetFiles(path, "*.csv")

    let names =
        [ for x in files do
              System.IO.Path.GetFileNameWithoutExtension x ]

    List.iteri (printfn "%2i: %s") names

// let eval str =
//     match paserResult pStmt str with
//     | PrintStmt printStmt -> evalPrintStmt printStmt
//     | AssignStmt (basename, expression) -> evalAssignStmt (basename, expression)
//     | ListingStmt _ -> listing databasePath
//     | QuitStmt _ -> Environment.Exit 1
//     | Expression exp ->
//         match exp with
//         | ProjectExpression projectExpression ->
//             let rel = evalProjectExpression projectExpression
//             printfn "Relation %s returned." (Relation.save rel)
//         | Identifier identifier ->
//             printfn "Only relation name is provided."
//             printfn "Do you mean \"print\" ?"
