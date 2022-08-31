module Eval

open System
open Deedle

open Common
open Relation

type EvalExpression = Expression -> Result<Relation.T, ExecutionError>
type EvalProjectExpression = ProjectExpression -> Result<Relation.T, ExecutionError>
type EvalDifferenceExpression = Expression -> Expression -> Result<Relation.T, ExecutionError>
// type Difference = Relation.T -> Relation.T -> Result<Relation.T, ComparabilityError>
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
/// Decimal and Int columns are treated as valid Decimal column.
/// If the column is invalid, returns one of cases of ColumnValidity.ConditionError.
/// For example, if the column name is not found in the dataframe, raise ColumnNotFound error.</returns>
let validateColumn: Frame<int, string> -> string -> ColumnValidity =
    fun df col ->
        let colDf = df.Columns[[ col ]]

        if Frame.countRows colDf >= 1 then
            let (_, t) = getColsAndTypes colDf

            match t.[0] with
            | t when (t = typeof<decimal> || t = typeof<int32>) -> Decimal |> ValidColumn
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

let infixOperation rel1 rel2 computeRelation =
    let df1 = Relation.value rel1
    let df2 = Relation.value rel2

    match validateComparability df1 df2 with
    | Comparable _ -> computeRelation df1 df2 |> Result.Ok
    | ComparabilityError err ->
        ExecutionError.ComparabilityError err
        |> Result.Error

let difference rel1 rel2 =
    let computeRelation (df1: Frame<int, string>) (df2: Frame<int, string>) =
        let reference = df2.Rows.Values |> Collections.Generic.HashSet

        df1.RowsDense
        |> Series.filterValues (fun row -> not (reference.Contains row))
        |> Frame.ofRows
        |> Relation.create

    infixOperation rel1 rel2 computeRelation

let union rel1 rel2 =
    let computeRelation (df1: Frame<int, string>) (df2: Frame<int, string>) =
        Seq.append df1.RowsDense.Values df2.RowsDense.Values
        |> Series.ofValues
        |> Frame.ofRows
        |> Relation.create

    infixOperation rel1 rel2 computeRelation

let intersection rel1 rel2 =
    let computeRelation (df1: Frame<int, string>) (df2: Frame<int, string>) =
        let reference = df2.Rows.Values |> Collections.Generic.HashSet

        df1.RowsDense
        |> Series.filterValues (fun row -> reference.Contains row)
        |> Frame.ofRows
        |> Relation.create

    infixOperation rel1 rel2 computeRelation

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
                    try
                        match row.TryGetAs<decimal>(col1) with
                        | OptionalValue.Present _ ->
                            evalOperator (row.GetAs<decimal>(col1)) (row.GetAs<decimal>(col2)) op
                        | OptionalValue.Missing -> false
                    with
                    | :? System.FormatException -> evalOperator (row.GetAs<string>(col1)) (row.GetAs<string>(col2)) op

            | ColumnValue { Column = col
                            Value = value
                            Operator = op } ->
                match value with
                | Value.Decimal decimalValue -> fun row -> evalOperator (row.GetAs<decimal>(col)) decimalValue op
                | Value.String stringValue -> fun row -> evalOperator (row.GetAs<string>(col)) stringValue op

let rec validationCatamorphism fColumnColumn fColumnValue input condition =
    let recurse = validationCatamorphism fColumnColumn fColumnValue input

    match condition with
    | InfixCondition ((cond1, op), cond2) ->
        match op with
        | And -> combineValidity (recurse cond1) (recurse cond2)
        | Or -> combineValidity (recurse cond1) (recurse cond2)
    | NOTCondition cond -> recurse cond
    | SingleCondition cond ->
        match cond with
        | ColumnColumn { Column1 = col1
                         Column2 = col2
                         Operator = op
                         Relation1 = rel1
                         Relation2 = rel2 } -> fColumnColumn col1 col2 op rel1 rel2 input

        | ColumnValue { Column = col
                        Value = value
                        Operator = op
                        Relation = rel } ->

            fColumnValue col value op rel input

let evalCatamorphism rel1 rel2 resultf =
    match rel1, rel2 with
    | Result.Ok rel1, Result.Ok rel2 -> resultf rel1 rel2
    | Result.Ok rel1, Result.Error err2 -> err2 |> Result.Error
    | Result.Error err1, Result.Ok rel2 -> err1 |> Result.Error
    // When both relations are illegal, only raise the first error.
    | Result.Error err1, Result.Error err2 -> err1 |> Result.Error

/// <summary>Validate Condition will be used in 'restrict'.
/// AND and OR conditions are recursively evaluated.</summary>
/// <param name="condition">The Condition parsed from 'restrict' Expression.</param>
/// <param name="df">The Deedle Frame used in 'restrict' Expression.</param>
/// <returns>If the input Condition is valid, returns ValidCondition of ConditionValidity type.
/// Otherwise, returns one of cases of ConditionError.
/// For example, if the operators other than equal or not equal are used for conditioning a string column,
/// raise IlldifinedOperatorForStrings error.
/// If there are multiple errors, only the left most error will be raised .</returns>
let validateCondition condition df =
    let fColumnColumn col1 col2 op dummy1 dummy2 df =
        let v1 = validateColumn df col1
        let v2 = validateColumn df col2

        match (v1, v2) with
        | (ValidColumn c1, ValidColumn c2) ->
            match (c1, c2) with
            | (Decimal, Decimal) -> ValidCondition
            | (String, String) ->
                match op with
                | Equal -> ValidCondition
                | NotEqual -> ValidCondition
                | _ -> IlldifinedOperatorForStrings |> ConditionError
            | _ -> TypesMismatch |> ConditionError
        | (ColumnValidity.ConditionError e, ValidColumn _) -> e |> ConditionError
        | (ValidColumn _, ColumnValidity.ConditionError e) -> e |> ConditionError
        | (ColumnValidity.ConditionError e1, ColumnValidity.ConditionError e2) -> e1 |> ConditionError

    let fColumnValue col value op dummy df =
        let v1 = validateColumn df col

        match v1 with
        | ValidColumn c1 ->
            match (c1, value) with
            | (Decimal, Value.Decimal _) -> ValidCondition
            | (String, Value.String _) ->
                match op with
                | Equal -> ValidCondition
                | NotEqual -> ValidCondition
                | _ -> IlldifinedOperatorForStrings |> ConditionError
            | _ -> TypesMismatch |> ConditionError
        | ColumnValidity.ConditionError e -> e |> ConditionError

    validationCatamorphism fColumnColumn fColumnValue df condition



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
let validateColumnNames columnNames rel =
    let refColumnNames = (Relation.value rel).ColumnKeys |> Seq.toList

    columnNames
    |> List.map (fun elm -> refColumnNames |> List.contains elm)
    |> List.fold (fun acc elm -> acc && elm) true

let tryGetRelName exp =
    match exp with
    | Identifier identifier -> Some identifier
    | _ -> None

let resolveName (name: Identifier option) defaultName =
    match name with
    | Some n -> n
    | None -> defaultName

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

/// <summary>Validate columns used in 'join'.</summary>
/// <param name="df1">The Deedle Frame used in the left hand side of the 'join' Expression.</param>
/// <param name="df2">The Deedle Frame used in the right hand side of the 'join' Expression.</param>
/// <param name="rel1Name">An optional relation name of the left hand side of the 'join' Expression.</param>
/// <param name="rel2Name">An optional relation name of the right hand side of the 'join' Expression.</param>
/// <param name="rel1">An optional relation name which appears in the qualifier of the col1.</param>
/// <param name="rel2">An optional relation name which appears in the qualifier of the col2.</param>
/// <param name="col1">The column name used in the left hand side of the 'join' Condition.</param>
/// <param name="col2">The column name used in the right hand side of the 'join' Condition.</param>
/// <returns>If the column of the dataframe is valid, returns ValidColumn.
/// Decimal and Int columns are treated as valid Decimal column.
/// If the column is invalid, returns one of cases of ColumnValidity.ConditionError.
/// For example, if the column name is not found in the dataframe, raise ColumnNotFound error.</returns>
let validateJoinColumns df1 df2 rel1Name rel2Name rel1 rel2 col1 col2 =

    // If the qualifier rel matches with one of the relation names (name1, name2),
    // validate the column with the matched relation df1 or df2.
    // If the qualifier does not match with the relation names,
    // raise IncorrectRelationName error.
    // If the qualifier is None, validate the column with both relations.
    let testNames name1 name2 rel col =
        if rel |> Option.contains name1 then
            validateColumn df1 col
        elif rel |> Option.contains name2 then
            validateColumn df2 col
        elif rel |> Option.isSome then
            IncorrectRelationName
            |> ColumnValidity.ConditionError
        else
            combineORColumnValidity (validateColumn df1 col) (validateColumn df2 col)

    let testNoName rel col =
        if rel |> Option.isSome then
            RelationNameUnavailable
            |> ColumnValidity.ConditionError
        else
            combineORColumnValidity (validateColumn df1 col) (validateColumn df2 col)

    // Qualifiers rel1, rel2 could match with either name1 or name2.
    // Validate the column name corresponding to the qualifier.
    let relationNamesCase name1 name2 =
        let v1 = testNames name1 name2 rel1 col1
        let v2 = testNames name1 name2 rel2 col2

        combineANDColumnValidity v1 v2

    let leftRelationNameCase name1 =
        let v1 = testNames name1 "" rel1 col1
        let v2 = testNames name1 "" rel2 col2

        combineANDColumnValidity v1 v2

    let rightRelationNameCase name2 =
        let v1 = testNames "" name2 rel1 col1
        let v2 = testNames "" name2 rel2 col2

        combineANDColumnValidity v1 v2

    // Because there are no name1 and name2 in this case,
    // Some r1 and/or Some r2 raise RelationNameUnavailable error.
    let noRelationNamesCase =
        let v1 = testNoName rel1 col1
        let v2 = testNoName rel2 col2

        combineANDColumnValidity v1 v2


    match (rel1Name, rel2Name) with
    | Some name1, Some name2 -> relationNamesCase name1 name2
    | Some name1, None -> leftRelationNameCase name1
    | None, Some name2 -> rightRelationNameCase name2
    | None, None -> noRelationNamesCase

/// <summary>Validate Condition will be used in 'join'.
/// AND and OR conditions are recursively evaluated.</summary>
/// <param name="df1">The Deedle Frame used in the left hand side of the 'join' Expression.</param>
/// <param name="df2">The Deedle Frame used in the right hand side of the 'join' Expression.</param>
/// <param name="rel1Name">An optional relation name of the left hand side of the 'join' Expression.</param>
/// <param name="rel2Name">An optional relation name of the right hand side of the 'join' Expression.</param>
/// <param name="condition">The Condition parsed from 'restrict' Expression.</param>
/// <returns>If the input Condition is valid, returns ValidCondition of ConditionValidity type.
/// Otherwise, returns one of cases of ConditionError.
/// For example, if the operators other than equal or not equal are used for conditioning a string column,
/// raise IlldifinedOperatorForStrings error.
/// If there are multiple errors, only the left most error will be raised .</returns>
let validateJoinCondition df1 df2 rel1Name rel2Name condition =
    let fColumnColumn col1 col2 op rel1 rel2 (df1, df2, rel1Name, rel2Name) =
        let validity = validateJoinColumns df1 df2 rel1Name rel2Name rel1 rel2 col1 col2

        match validity with
        | ValidColumn c ->
            match c with
            | Decimal -> ValidCondition
            | String ->
                match op with
                | Equal -> ValidCondition
                | NotEqual -> ValidCondition
                | _ -> IlldifinedOperatorForStrings |> ConditionError
        | ColumnValidity.ConditionError e -> e |> ConditionError

    let fColumnValue col value op rel (df1, df2, rel1Name, rel2Name) =
        let validity = validateJoinColumns df1 df2 rel1Name rel2Name rel None col col

        match validity with
        | ValidColumn c1 ->
            match (c1, value) with
            | (Decimal, Value.Decimal _) -> ValidCondition
            | (String, Value.String _) ->
                match op with
                | Equal -> ValidCondition
                | NotEqual -> ValidCondition
                | _ -> IlldifinedOperatorForStrings |> ConditionError
            | _ -> TypesMismatch |> ConditionError
        | ColumnValidity.ConditionError e -> e |> ConditionError

    validationCatamorphism fColumnColumn fColumnValue (df1, df2, rel1Name, rel2Name) condition

let rec evalJoinCondition (rel1Name, rel2Name, cond) =
    match cond with
    | InfixCondition ((cond1, op), cond2) ->
        match op with
        | And ->
            fun (row: Row) ->
                (evalJoinCondition (rel1Name, rel2Name, cond1)) row
                && (evalJoinCondition (rel1Name, rel2Name, cond2)) row
        | Or ->
            fun row ->
                (evalJoinCondition (rel1Name, rel2Name, cond1)) row
                || (evalJoinCondition (rel1Name, rel2Name, cond2)) row
    | NOTCondition cond -> fun row -> not ((evalJoinCondition (rel1Name, rel2Name, cond)) row)
    | SingleCondition cond ->
        match cond with
        | ColumnColumn { Column1 = col1
                         Column2 = col2
                         Operator = op
                         Relation1 = rel1
                         Relation2 = rel2 } ->
            let renamedCol1 =
                if col1 = col2 then
                    match (rel1Name, rel2Name, rel1) with
                    | Some _, Some name2, Some r1 ->
                        if name2 = r1 then
                            name2 + "." + col1
                        else
                            col1
                    | Some _, None, Some r1 -> col1
                    | None, Some name2, Some r1 ->
                        if name2 = r1 then
                            name2 + "." + col1
                        else
                            col1
                    | _, _, _ -> col1
                else
                    col1

            let renamedCol2 =
                if col1 = col2 then
                    match (rel1Name, rel2Name, rel2) with
                    | Some _, Some name2, Some r2 ->
                        if name2 = r2 then
                            name2 + "." + col2
                        else
                            col2
                    | Some _, None, Some r2 -> col2
                    | None, Some name2, Some r2 ->
                        if name2 = r2 then
                            name2 + "." + col2
                        else
                            col2
                    | _, _, _ -> col2
                else
                    col2

            fun row ->
                try
                    match row.TryGetAs<decimal>(renamedCol1) with
                    | OptionalValue.Present _ ->
                        evalOperator (row.GetAs<decimal>(renamedCol1)) (row.GetAs<decimal>(renamedCol2)) op
                    | OptionalValue.Missing -> false
                with
                | :? System.FormatException ->
                    evalOperator (row.GetAs<string>(renamedCol1)) (row.GetAs<string>(renamedCol2)) op

        | ColumnValue { Column = col
                        Value = value
                        Operator = op
                        Relation = rel } ->

            let prefix =
                match rel with
                | Some r -> r + "."
                | None -> ""

            match value with
            | Value.Decimal decimalValue ->
                fun row ->
                    try
                        match row.TryGetAs<decimal>(col) with
                        | OptionalValue.Present _ -> evalOperator (row.GetAs<decimal>(col)) decimalValue op
                        | OptionalValue.Missing -> false
                    with
                    | :? System.FormatException -> evalOperator (row.GetAs<decimal>(prefix + col)) decimalValue op
            | Value.String stringValue ->
                fun row ->
                    try
                        match row.TryGetAs<string>(col) with
                        | OptionalValue.Present _ -> evalOperator (row.GetAs<string>(col)) stringValue op
                        | OptionalValue.Missing -> false
                    with
                    | :? System.FormatException -> evalOperator (row.GetAs<string>(prefix + col)) stringValue op



let joinRow df2 rel1Name rel2Name cond (row1: ObjectSeries<string>) =
    let rowDf =
        Frame(row1.Keys, Seq.map (fun elm -> Series([ 0 ], [ elm ])) row1.Values)

    let prodDf =
        match rel2Name with
        | Some name2 -> product (Relation.create rowDf) (Relation.create df2) name2
        | None -> product (Relation.create rowDf) (Relation.create df2) "Right"

    let condition = evalJoinCondition (rel1Name, rel2Name, cond)

    (Relation.value prodDf).RowsDense
    |> Series.filterValues (condition)
    |> Frame.ofRows
    |> Frame.indexRowsOrdinally


let concatFrames (frames: Frame<int, string> list) =
    let N = Frame.countRows frames[0]

    frames
    |> List.mapi (fun i frame -> (Frame.mapRowKeys (fun num -> num + N * i) frame))
    |> List.map (fun frame -> Frame.transpose frame)
    |> Frame.mergeAll
    |> Frame.transpose

let dropDuplicateColumn rel2Name (frame: Frame<int, string>) =
    match rel2Name with
    | Some name2 ->
        let cols = frame.ColumnKeys |> Seq.toList

        let duplicateNames =
            List.filter (fun elm -> List.contains (name2 + "." + elm) cols) cols

        if List.length duplicateNames > 0 then
            List.fold
                (fun s col ->
                    if frame.GetColumn(col) = frame.GetColumn(name2 + "." + col) then
                        Frame.dropCol (name2 + "." + col) s
                    else
                        s)
                frame
                duplicateNames
        else
            frame
    | None -> frame


let join rel1 rel2 rel1Name rel2Name cond =
    let df1 = Relation.value rel1
    let df2 = Relation.value rel2

    let conditionValidity = validateJoinCondition df1 df2 rel1Name rel2Name cond

    match conditionValidity with
    | ValidCondition _ ->
        df1.Rows.Values
        |> Seq.toList
        |> List.map (joinRow df2 rel1Name rel2Name cond)
        |> concatFrames
        |> dropDuplicateColumn rel2Name
        |> Relation.create
        |> Result.Ok

    | ConditionError err ->
        err
        |> Result.Error
        |> Result.mapError ExecutionError.ConditionError


let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | InfixExpression ((exp1, op), exp2) ->
            match op with
            | Difference -> evalDifferenceExpression exp1 exp2
            | Product -> evalProductExpression exp1 exp2
            | Union -> evalUnionExpression exp1 exp2
            | Intersection -> evalIntersectionExpression exp1 exp2
        | RestrictExpression (exp, cond) -> evalRestrictExpression exp cond
        | JoinExpression ((exp1, exp2), cond) -> evalJoinExpression exp1 exp2 cond
        | RenameExpression ((qualifier, col), newCol) -> evalRenameExpression qualifier col newCol

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList

        let project columns rel =
            if validateColumnNames columns rel then
                Relation.project rel columns |> Result.Ok
            else
                ColumnError.ColumnNotFound
                |> ColumnError
                |> Result.Error

        rel |> Result.bind (project columns)


and evalDifferenceExpression: EvalDifferenceExpression =
    fun exp1 exp2 ->
        let rel1 = evalExpression exp1
        let rel2 = evalExpression exp2

        evalCatamorphism rel1 rel2 difference

and evalUnionExpression exp1 exp2 =
    let rel1 = evalExpression exp1
    let rel2 = evalExpression exp2

    evalCatamorphism rel1 rel2 union

and evalIntersectionExpression exp1 exp2 =
    let rel1 = evalExpression exp1
    let rel2 = evalExpression exp2

    evalCatamorphism rel1 rel2 intersection

and evalRestrictExpression exp cond =
    let rel = evalExpression exp

    match rel with
    | Result.Ok rel -> restriction rel cond
    | Result.Error err -> err |> Result.Error

and evalProductExpression exp1 exp2 =
    let rel1 = evalExpression exp1
    let rel2 = evalExpression exp2
    let prefix = resolveName (tryGetRelName exp2) "Right"

    match rel1, rel2 with
    | Result.Ok rel1, Result.Ok rel2 -> product rel1 rel2 prefix |> Result.Ok
    | Result.Ok rel1, Result.Error err2 -> err2 |> Result.Error
    | Result.Error err1, Result.Ok rel2 -> err1 |> Result.Error
    // When both relations are illegal, only raise the first error.
    | Result.Error err1, Result.Error err2 -> err1 |> Result.Error

and evalJoinExpression exp1 exp2 cond =
    let rel1 = evalExpression exp1
    let rel2 = evalExpression exp2
    let rel1Name = tryGetRelName exp1
    let rel2Name = tryGetRelName exp2

    match rel1, rel2 with
    | Result.Ok rel1, Result.Ok rel2 -> join rel1 rel2 rel1Name rel2Name cond
    | Result.Ok rel1, Result.Error err2 -> err2 |> Result.Error
    | Result.Error err1, Result.Ok rel2 -> err1 |> Result.Error
    // When both relations are illegal, only raise the first error.
    | Result.Error err1, Result.Error err2 -> err1 |> Result.Error

and evalRenameExpression qualifier col newCol =
    let relation = Relation.openRelation qualifier

    let rename col newCol rel =
        if validateColumnNames [ col ] rel then
            Relation.rename rel col newCol |> Result.Ok
        else
            ColumnError.ColumnNotFound
            |> ColumnError
            |> Result.Error

    relation |> Result.bind (rename col newCol)

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

let saveAndPrint rel =
    rel
    |> Result.map Relation.save
    |> Result.map printRelationName


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
            evalProjectExpression projectExpression
            |> saveAndPrint

        | InfixExpression ((exp1, op), exp2) ->
            match op with
            | Difference -> evalDifferenceExpression exp1 exp2 |> saveAndPrint
            | Product -> evalProductExpression exp1 exp2 |> saveAndPrint
            | Union -> evalUnionExpression exp1 exp2 |> saveAndPrint
            | Intersection ->
                evalIntersectionExpression exp1 exp2
                |> saveAndPrint

        | RestrictExpression (exp, cond) -> evalRestrictExpression exp cond |> saveAndPrint

        | JoinExpression ((exp1, exp2), cond) -> evalJoinExpression exp1 exp2 cond |> saveAndPrint

        | RenameExpression ((qualifier, col), newCol) ->
            evalRenameExpression qualifier col newCol
            |> saveAndPrint

        | Identifier identifier ->
            printfn "Only relation name is provided."
            printfn "Do you mean \"print\" ?" |> Result.Ok

    |> Result.mapError ExecutionError
