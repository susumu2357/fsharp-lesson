#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"

open FParsec

open System

Environment.CurrentDirectory
Environment.CurrentDirectory <- @"C:\Users\susum\OneDrive\Documents\fsharp\fsharp-lesson\sources\ToyRel"

#load "Common.fs"
open Common
#load "Relation.fs"
open Relation
#load "Parser.fs"
open Parser

type EvalExpression = Expression -> Result<Relation.T, ExecutionError>
and EvalProjectExpression = ProjectExpression -> Result<Relation.T, ExecutionError>
and EvalDifferenceExpression = Expression -> Expression -> Result<Relation.T, ExecutionError>

and Difference = Relation.T -> Relation.T -> Result<Relation.T, ComparabilityError>

and ExecutionError =
    | IncorrectPathError of IncorrectPathError
    | ProjectionError of ProjectionError
    | ComparabilityError of ComparabilityError

and ProjectionError = | ColumnNotFound

and ComparabilityError =
    | ColumnsMismatch
    | ColumnTypesMismatch
    | ColumnsOrderMismatch

and Comparability =
    | Comparable
    | ComparabilityError of ComparabilityError

type EvaluationError =
    | ParseError of ParseError
    | ExecutionError of ExecutionError

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
    | (true, true) -> Comparable
    | (true, false) -> ColumnTypesMismatch |> ComparabilityError
    | (false, _) ->
        if List.sort col1 = List.sort col2 then
            ColumnsOrderMismatch |> ComparabilityError
        else
            ColumnsMismatch |> ComparabilityError

let difference: Difference =
    fun rel1 rel2 ->
        let df1 = Relation.value rel1
        let df2 = Relation.value rel2

        match validateComparability df1 df2 with
        | Comparable ->
            let reference = df2.Rows.Values |> Collections.Generic.HashSet

            df1.RowsDense
            |> Series.filterValues (fun row -> not (reference.Contains row))
            |> Frame.ofRows
            |> Relation.create
            |> Result.Ok
        | ComparabilityError err -> err |> Result.Error

let validateColumnNames rel columnNames =
    let refColumnNames = (Relation.value rel).ColumnKeys |> Seq.toList

    columnNames
    |> List.map (fun elm -> refColumnNames |> List.contains elm)
    |> List.fold (fun acc elm -> acc && elm) true

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier ->
            Relation.openRelation identifier
            |> Result.mapError IncorrectPathError
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | DifferenceExpression (exp1, exp2) -> evalDifferenceExpression exp1 exp2

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList

        let project columns rel =
            if validateColumnNames rel columns then
                Relation.project rel columns |> Result.Ok
            else
                ColumnNotFound |> ProjectionError |> Result.Error

        rel |> Result.bind (project columns)



and evalDifferenceExpression: EvalDifferenceExpression =
    fun exp1 exp2 ->
        let rel1 = evalExpression exp1
        let rel2 = evalExpression exp2

        match rel1, rel2 with
        | Result.Ok rel1, Result.Ok rel2 ->
            difference rel1 rel2
            |> Result.mapError ExecutionError.ComparabilityError
        | Result.Ok rel1, Result.Error err2 -> err2 |> Result.Error
        | Result.Error err1, Result.Ok rel2 -> err1 |> Result.Error
        // When both relations are illegal, only the first error will be raise.
        | Result.Error err1, Result.Error err2 -> err1 |> Result.Error



let evalPrintStmt identifier =
    let rel = Relation.openRelation identifier

    rel
    |> Result.mapError IncorrectPathError
    |> Result.map Relation.print


let evalAssignStmt (basename, expression) =
    let rel = evalExpression expression
    rel |> Result.map (Relation.saveAs basename)

let listing path =
    let files = IO.Directory.GetFiles(path, "*.csv")

    let names =
        [ for x in files do
              IO.Path.GetFileNameWithoutExtension x ]

    List.iteri (printfn "%2i: %s") names

let printRelationName rel = printfn "Relation %s returned." rel

let eval str =
    let evalAdapted paserResult =
        match paserResult with
        | PrintStmt printStmt -> evalPrintStmt printStmt
        | AssignStmt (basename, expression) -> evalAssignStmt (basename, expression)
        | ListingStmt _ -> listing (databaseBase + dbPath) |> Result.Ok
        | QuitStmt _ -> Environment.Exit 1 |> Result.Ok
        | UseStmt newDBName ->
            let newdbPath = newDBName + @"\\"
            printfn "changed database from %s to %s" dbPath newdbPath
            dbPath <- newdbPath
            () |> Result.Ok
        | Expression exp ->
            match exp with
            | ProjectExpression projectExpression ->
                let rel = evalProjectExpression projectExpression

                rel
                |> Result.map Relation.save
                |> Result.map printRelationName

            | DifferenceExpression (exp1, exp2) ->
                let rel = evalDifferenceExpression exp1 exp2

                rel
                |> Result.map Relation.save
                |> Result.map printRelationName

            | Identifier identifier ->
                printfn "Only relation name is provided."
                printfn "Do you mean \"print\" ?" |> Result.Ok

        |> Result.mapError ExecutionError

    let paserResultAdapted =
        paserResult pStmt str
        |> Result.mapError ParseError

    paserResultAdapted |> Result.bind evalAdapted
