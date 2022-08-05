module Eval

open Common
open Relation
open Parser

type EvalExpression = Expression -> Relation.T
type EvalProjectExpression = ProjectExpression -> Relation.T

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList
        Relation.project rel columns

let evalPrintStmt identifier =
    let rel = Relation.openRelation identifier
    Relation.print rel


let evalAssignStmt (basename, expression) =
    let rel = evalExpression expression
    Relation.saveAs rel basename

let listing path =
    let files = System.IO.Directory.GetFiles(path, "*.csv")

    let names =
        [ for x in files do
              System.IO.Path.GetFileNameWithoutExtension x ]

    List.iteri (printfn "%2i: %s") names

let eval str =
    match paserResult pStmt str with
    | PrintStmt printStmt -> evalPrintStmt printStmt
    | AssignStmt (basename, expression) -> evalAssignStmt (basename, expression)
    | ListingStmt _ -> listing databasePath
    | Expression exp ->
        match exp with
        | ProjectExpression projectExpression ->
            let rel = evalProjectExpression projectExpression
            printfn "Relation %s returned." (Relation.save rel)
        | Identifier identifier ->
            printfn "Only relation name is provided."
            printfn "Do you mean \"print\" ?"
