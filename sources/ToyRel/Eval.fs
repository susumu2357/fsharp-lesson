module Eval

open System
open Deedle

open Common
open Relation
open Parser

type EvalExpression = Expression -> Result<Relation.T, ExecutionError>
type EvalProjectExpression = ProjectExpression -> Result<Relation.T, ExecutionError>
type EvalDifferenceExpression = Expression -> Expression -> Result<Relation.T, ExecutionError>
type Difference = Relation.T -> Relation.T -> Result<Relation.T, ComparabilityError>
type Row = ObjectSeries<string>
type EvalCondition = Condition -> (Row -> bool)

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
    | (true, true) -> Comparable colType1
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
        | Comparable _ ->
            let reference = df2.Rows.Values |> Collections.Generic.HashSet

            df1.RowsDense
            |> Series.filterValues (fun row -> not (reference.Contains row))
            |> Frame.ofRows
            |> Relation.create
            |> Result.Ok
        | ComparabilityError err -> err |> Result.Error

let evalOperator a b op =
    match op with
    | NotEqual -> a <> b
    | LessOrEqual -> a <= b
    | GreaterOrEqual -> a >= b
    | Less -> a < b
    | Greater -> a > b
    | Equal -> a = b

let rec evalCondition: EvalCondition =
    fun cond ->
        match cond with
        | ANDCondition (cond1, cond2) ->
            fun row ->
                (evalCondition cond1) row
                && (evalCondition cond2) row
        | ORCondition (cond1, cond2) ->
            fun row ->
                (evalCondition cond1) row
                || (evalCondition cond2) row
        | SingleCondition cond ->
            match cond with
            | ColumnColumn { Column1 = col1
                             Column2 = col2
                             Operator = op } ->
                fun row ->
                    let testRow = row.TryGetAs<float>(col1)

                    match testRow with
                    | OptionalValue.Present _ -> evalOperator (row.GetAs<float>(col1)) (row.GetAs<float>(col2)) op
                    | OptionalValue.Missing -> evalOperator (row.GetAs<string>(col1)) (row.GetAs<string>(col2)) op

            | ColumnValue { Column = col
                            Value = value
                            Operator = op } ->
                match value with
                | Float floatValue -> fun row -> evalOperator (row.GetAs<float>(col)) floatValue op
                | String stringValue -> fun row -> evalOperator (row.GetAs<string>(col)) stringValue op

let rec validateCondition (condition: Condition, df: Frame<int, string>) =
    match condition with
    | ANDCondition (cond1, cond2) ->
        validateCondition (cond1, df)
        + validateCondition (cond2, df)
    | ORCondition (cond1, cond2) ->
        validateCondition (cond1, df)
        + validateCondition (cond2, df)
    | SingleCondition cond ->
        match cond with
        | ColumnColumn { Column1 = col1
                         Column2 = col2
                         Operator = op } ->
            let df1 = df.Columns[[ col1 ]]
            let df2 = df.Columns[[ col2 ]]

            let (_, t1) = getColsAndTypes df1
            let (_, t2) = getColsAndTypes df2

            if t1 = t2 then
                let tp = t1.[0]

                match tp with
                | t when (t = typeof<float> || t = typeof<int32>) -> 0
                | t when
                    (t = typeof<string>
                     && ((op = Equal) || (op = NotEqual)))
                    ->
                    0
                | _ -> 2
            else
                2

        | ColumnValue { Column = col
                        Value = value
                        Operator = op } ->

            let df1 = df.Columns[[ col ]]

            match validateComparability df1 df1 with
            | Comparable colTypes ->
                let tp = colTypes.[0]

                match tp with
                | t when (t = typeof<float> || t = typeof<int32>) ->
                    match value with
                    | Float _ -> 0
                    | _ -> 2
                | t when
                    (t = typeof<string>
                     && ((op = Equal) || (op = NotEqual)))
                    ->
                    match value with
                    | String _ -> 0
                    | _ -> 2
                | _ -> 2
            | _ -> 1



let restriction rel cond =
    let df = Relation.value rel
    let validateInt = validateCondition (cond, df)

    if validateInt = 0 then
        let condition = evalCondition cond

        df.RowsDense
        |> Series.filterValues (condition)
        |> Frame.ofRows
        |> Relation.create
        |> Result.Ok
    elif validateInt % 2 = 1 then
        ColumnTypesMismatch
        |> Result.Error
        |> Result.mapError ExecutionError.ComparabilityError
    else
        ConditionError
        |> Result.Error
        |> Result.mapError ExecutionError.ConditionError

let validateColumnNames rel columnNames =
    let refColumnNames = (Relation.value rel).ColumnKeys |> Seq.toList

    columnNames
    |> List.map (fun elm -> refColumnNames |> List.contains elm)
    |> List.fold (fun acc elm -> acc && elm) true

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | DifferenceExpression (exp1, exp2) -> evalDifferenceExpression exp1 exp2
        | RestrictExpression (exp, cond) -> evalRestrictExpression exp cond

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
        // When both relations are illegal, only raise the first error.
        | Result.Error err1, Result.Error err2 -> err1 |> Result.Error

and evalRestrictExpression exp cond =
    let rel = evalExpression exp

    match rel with
    | Result.Ok rel -> restriction rel cond
    | Result.Error err -> err |> Result.Error

let evalPrintStmt identifier =
    let rel = Relation.openRelation identifier

    rel |> Result.map Relation.print


let evalAssignStmt (basename, expression) =
    let rel = evalExpression expression
    printfn "%A" rel
    rel |> Result.map (Relation.saveAs basename)

let listing path =
    let files = IO.Directory.GetFiles(path, "*.csv")

    let names =
        [ for x in files do
              IO.Path.GetFileNameWithoutExtension x ]

    List.iteri (printfn "%2i: %s") names |> Result.Ok

let printRelationName rel = printfn "Relation %s returned." rel

let eval str =
    let evalAdapted paserResult =
        match paserResult with
        | PrintStmt printStmt -> evalPrintStmt printStmt
        | AssignStmt (basename, expression) -> evalAssignStmt (basename, expression)
        | ListingStmt _ -> listing (databaseBase + dbPath)
        | QuitStmt _ -> Environment.Exit 1 |> Result.Ok
        | UseStmt newDBName ->
            let newdbPath = newDBName + @"\\"
            printfn "changed database from %s to %s" dbPath newdbPath
            dbPath <- newdbPath
            Result.Ok()
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

            | RestrictExpression (exp, cond) ->
                let rel = evalRestrictExpression exp cond

                rel
                |> Result.map Relation.save
                |> Result.map printRelationName

            | Identifier identifier ->
                printfn "Only relation name is provided."
                printfn "Do you mean \"print\" ?" |> Result.Ok

        |> Result.mapError ExecutionError

    let paserResultAdapted =
        paserResult pStmt str
        |> Result.mapError EvaluationError.ParseError

    paserResultAdapted |> Result.bind evalAdapted
