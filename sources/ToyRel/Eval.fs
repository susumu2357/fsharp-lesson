module Eval

open System
open Deedle

open Common
open Relation

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

/// <summary>Validate single column of Deedle Frame.</summary>
/// <param name="df">The dataframe to be validated.</param>
/// <param name="col">The column name. Should be a single string.</param>
/// <returns>If the column of the dataframe is valid, returns ValidColumn.
/// Float and Int columns are treated as valid Float column.
/// If the column is invalid, returns one of cases of ColumnValidity.ConditionError.
/// For example, if the column name is not found in the dataframe, raise ColumnNotFound error.</returns>
let validateColumn: Frame<int, string> -> string -> ColumnValidity =
    fun df col ->
        let colDf = df.Columns[[ col ]]

        if Frame.countRows colDf >= 1 then
            let (_, t) = getColsAndTypes colDf

            match t.[0] with
            | t when (t = typeof<float> || t = typeof<int32>) -> Float |> ValidColumn
            | t when t = typeof<string> -> String |> ValidColumn
            | _ ->
                UnsupportedColumnType
                |> ColumnValidity.ConditionError
        else
            ColumnNotFound |> ColumnValidity.ConditionError

/// <summary>Validate union comparability.
/// If the relations have the same column names, the same column types, and the same order of columns, these are called union comparable.</summary>
/// <param name="df1">The Deedle Frame of the left hand side of 'difference'.</param>
/// <param name="df2">The Deedle Frame of the right hand side of 'difference'.</param>
/// <returns>If the inputs are union comparable, returns Comparable of Comparability type.
/// Otherwise, returns one of cases of ComparabilityError.</returns>
/// For example, if the column names do not match, raise ColumnsMismatch error.</returns>
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

/// <summary>Evaluate Condition of 'restrict'.
/// AND and OR conditions are recursively evaluated.</summary>
/// <param name="cond">The Condition which should be validated before executing this function.</param>
/// <returns>The function to be used in Series.filterValues.</returns>
let rec evalCondition: EvalCondition =
    fun cond ->
        match cond with
        | InfixCondition ((cond1, op), cond2) ->
            match op with
            | And ->
                fun row ->
                    (evalCondition cond1) row
                    && (evalCondition cond2) row
            | Or ->
                fun row ->
                    (evalCondition cond1) row
                    || (evalCondition cond2) row
        | NOTCondition cond -> fun row -> not ((evalCondition cond) row)
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
                | Value.Float floatValue -> fun row -> evalOperator (row.GetAs<float>(col)) floatValue op
                | Value.String stringValue -> fun row -> evalOperator (row.GetAs<string>(col)) stringValue op

/// <summary>Validate Condition will be used in 'restrict'.
/// AND and OR conditions are recursively evaluated.</summary>
/// <param name="condition">The Condition parsed from 'restrict' Expression.</param>
/// <param name="df">The Deedle Frame used in 'restrict' Expression.</param>
/// <returns>If the input Condition is valid, returns ValidCondition of ConditionValidity type.
/// Otherwise, returns one of cases of ConditionError.
/// For example, if the operators other than equal or not equal are used for conditioning a string column,
/// raise IlldifinedOperatorForStrings error.
/// If there are multiple errors, only the left most error will be raised .</returns>
let rec validateCondition condition df =
    match condition with
    | InfixCondition ((cond1, op), cond2) ->
        match op with
        | And -> combineValidity (validateCondition cond1 df) (validateCondition cond2 df)
        | Or -> combineValidity (validateCondition cond1 df) (validateCondition cond2 df)
    | NOTCondition cond -> validateCondition cond df
    | SingleCondition cond ->
        match cond with
        | ColumnColumn { Column1 = col1
                         Column2 = col2
                         Operator = op } ->
            let v1 = validateColumn df col1
            let v2 = validateColumn df col2

            match (v1, v2) with
            | (ValidColumn c1, ValidColumn c2) ->
                match (c1, c2) with
                | (Float, Float) -> ValidCondition
                | (String, String) ->
                    match op with
                    | Equal -> ValidCondition
                    | NotEqual -> ValidCondition
                    | _ -> IlldifinedOperatorForStrings |> ConditionError
                | _ -> TypesMismatch |> ConditionError
            | (ColumnValidity.ConditionError e, ValidColumn _) -> e |> ConditionError
            | (ValidColumn _, ColumnValidity.ConditionError e) -> e |> ConditionError
            | (ColumnValidity.ConditionError e1, ColumnValidity.ConditionError e2) -> e1 |> ConditionError

        | ColumnValue { Column = col
                        Value = value
                        Operator = op } ->

            let v1 = validateColumn df col

            match v1 with
            | ValidColumn c1 ->
                match (c1, value) with
                | (Float, Value.Float _) -> ValidCondition
                | (String, Value.String _) ->
                    match op with
                    | Equal -> ValidCondition
                    | NotEqual -> ValidCondition
                    | _ -> IlldifinedOperatorForStrings |> ConditionError
                | _ -> TypesMismatch |> ConditionError
            | ColumnValidity.ConditionError e -> e |> ConditionError


let restriction rel cond =
    let df = Relation.value rel
    let conditionValidity = validateCondition cond df

    match conditionValidity with
    | ValidCondition _ ->
        let condition = evalCondition cond

        df.RowsDense
        |> Series.filterValues (condition)
        |> Frame.ofRows
        |> Relation.create
        |> Result.Ok

    | ConditionError err ->
        err
        |> Result.Error
        |> Result.mapError ExecutionError.ConditionError

/// <summary>Validate column name existence, which is used in 'project'.</summary>
/// <param name="rel">The Relation parsed from 'project' Expression.</param>
/// <param name="columnNames">The string list of column names parsed from 'project' Expression.</param>
/// <returns>If the columnNames exsit in the input relation, returns true.
/// Otherwise, returns false.</returns>
let validateColumnNames rel columnNames =
    let refColumnNames = (Relation.value rel).ColumnKeys |> Seq.toList

    columnNames
    |> List.map (fun elm -> refColumnNames |> List.contains elm)
    |> List.fold (fun acc elm -> acc && elm) true

let tryGetRelName exp =
    match exp with
    | Identifier identifier -> identifier
    | _ -> Identifier "Right"

let product rel1 rel2 prefix =
    let df1 = Relation.value rel1
    let df2 = Relation.value rel2
    let N1 = Frame.countRows df1
    let N2 = Frame.countRows df2

    let cols1 = df1.ColumnKeys |> Collections.Generic.HashSet

    let renaming col =
        if cols1.Contains col then
            prefix + "." + col
        else
            col

    let renamedDf2 = df2 |> Frame.mapColKeys renaming

    let expandedDf1 =
        df1.Columns
        |> Series.mapValues (fun series ->
            Series.values series
            |> Seq.toList
            |> List.map (fun elm -> List.replicate N2 elm)
            |> List.concat
            |> Series.ofValues)
        |> Frame.ofColumns

    let expandedDf2 =
        renamedDf2.Columns
        |> Series.mapValues (fun series ->
            Series.values series
            |> Seq.toList
            |> List.replicate N1
            |> List.concat
            |> Series.ofValues)
        |> Frame.ofColumns

    expandedDf1.Join(expandedDf2) |> Relation.create


let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | InfixExpression ((exp1, op), exp2) ->
            match op with
            | Difference -> evalDifferenceExpression exp1 exp2
            | Product -> evalProductExpression exp1 exp2
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
                ProjectionError.ColumnNotFound
                |> ProjectionError
                |> Result.Error

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

and evalProductExpression exp1 exp2 =
    let rel1 = evalExpression exp1
    let rel2 = evalExpression exp2
    let prefix = tryGetRelName exp2

    match rel1, rel2 with
    | Result.Ok rel1, Result.Ok rel2 -> product rel1 rel2 prefix |> Result.Ok
    | Result.Ok rel1, Result.Error err2 -> err2 |> Result.Error
    | Result.Error err1, Result.Ok rel2 -> err1 |> Result.Error
    // When both relations are illegal, only raise the first error.
    | Result.Error err1, Result.Error err2 -> err1 |> Result.Error

let evalPrintStmt identifier =
    let rel = Relation.openRelation identifier

    rel |> Result.map Relation.print


let evalAssignStmt (basename, expression) =
    let rel = evalExpression expression
    rel |> Result.map (Relation.saveAs basename)

let listing path =
    let files = IO.Directory.GetFiles(path, "*.csv")

    let names =
        [ for x in files do
              IO.Path.GetFileNameWithoutExtension x ]

    List.iteri (printfn "%2i: %s") names |> Result.Ok

let printRelationName rel = printfn "Relation %s returned." rel

let evalAdapted paserResult =
    match paserResult with
    | PrintStmt printStmt -> evalPrintStmt printStmt
    | AssignStmt (basename, expression) ->
        evalAssignStmt (basename, expression)
        |> Result.map (fun () -> printRelationName basename)
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

        | InfixExpression ((exp1, op), exp2) ->
            match op with
            | Difference ->
                let rel = evalDifferenceExpression exp1 exp2

                rel
                |> Result.map Relation.save
                |> Result.map printRelationName
            | Product ->
                let rel = evalProductExpression exp1 exp2

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
