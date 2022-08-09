module Eval

open System
open Deedle

open Common
open Relation
open Parser

type EvalExpression = Expression -> Relation.T
type EvalProjectExpression = ProjectExpression -> Relation.T
type EvalDifferenceExpression = Expression -> Expression -> Relation.T

type Difference = Relation.T -> Relation.T -> Result<Relation.T, ComparabilityError>

and ComparabilityError =
    | ColumnsNotMatch of string
    | ColumnTypesNotMatch of string
    | ColumnsOrderNotMatch of string

and Comparability =
    | Comparable of string
    | ComparabilityError of ComparabilityError

let getColsAndTypes: Frame<int, string> -> string list * Type list =
    fun df ->
        let cols = df.ColumnKeys |> Seq.toList
        let types = df.ColumnTypes |> Seq.toList
        cols, types

let validateComparability df1 df2 =
    let col1, colType1 = getColsAndTypes df1
    let col2, colType2 = getColsAndTypes df2

    let checkColumns = (col1 = col2)
    let checkTypes = (colType1 = colType2)

    match (checkColumns, checkTypes) with
    | (true, true) -> Comparable "comparable"
    | (true, false) ->
        ColumnTypesNotMatch "columnTypesNotMatch"
        |> ComparabilityError
    | (false, _) ->
        if List.sort col1 = List.sort col2 then
            ColumnsOrderNotMatch "columnsOrderNotMatch"
            |> ComparabilityError
        else
            ColumnsNotMatch "columnsNotMatch"
            |> ComparabilityError

let difference: Difference =
    fun rel1 rel2 ->
        let df1 = Relation.value rel1
        let df2 = Relation.value rel2

        match validateComparability df1 df2 with
        | Comparable _ ->
            let reference = df2.Rows.Values |> Seq.distinct

            df1.RowsDense
            |> Series.filterValues (fun row -> not (reference |> Seq.contains row))
            |> Frame.ofRows
            |> Relation.create
            |> Result.Ok
        | ComparabilityError err -> err |> Result.Error

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | DifferenceExpression (exp1, exp2) -> evalDifferenceExpression exp1 exp2

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList
        Relation.project rel columns

and evalDifferenceExpression: EvalDifferenceExpression =
    fun exp1 exp2 ->
        let rel1 = evalExpression exp1
        let rel2 = evalExpression exp2

        match difference rel1 rel2 with
        | Result.Ok result -> result
        | Result.Error err -> failwithf "Not comparable!! %A" err



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
    | ListingStmt _ -> listing (databaseBase + dbPath)
    | QuitStmt _ -> Environment.Exit 1
    | UseStmt newDBName ->
        let newdbPath = newDBName + @"\\"
        printfn "changed database from %s to %s" dbPath newdbPath
        dbPath <- newdbPath
    | Expression exp ->
        match exp with
        | ProjectExpression projectExpression ->
            let rel = evalProjectExpression projectExpression
            printfn "Relation %s returned." (Relation.save rel)
        | DifferenceExpression (exp1, exp2) ->
            let rel = evalDifferenceExpression exp1 exp2
            printfn "Relation %s returned." (Relation.save rel)

        | Identifier identifier ->
            printfn "Only relation name is provided."
            printfn "Do you mean \"print\" ?"
