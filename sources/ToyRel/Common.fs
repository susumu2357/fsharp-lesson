module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
let databaseBase = ".\\database\\"
let mutable dbPath = "master\\"
let mutable latestRelationName = ""

type Value =
    | Decimal of decimal
    | String of string

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | InfixExpression of ((Expression * InfixOperator) * Expression)
    | RestrictExpression of Expression * Condition
    | JoinExpression of ((Expression * Expression) * Condition)
    | RenameExpression of ((string * string) * string)

and Identifier = string

and ProjectExpression = Expression * ColumnList

and Condition =
    | SingleCondition of SingleCondition
    | InfixCondition of ((Condition * LogicalOperator) * Condition)
    | NOTCondition of Condition

and SingleCondition =
    | ColumnColumn of ColumnColumn
    | ColumnValue of ColumnValue

and ColumnColumn =
    { Column1: string
      Column2: string
      Operator: Operator
      Relation1: string option
      Relation2: string option }

and ColumnValue =
    { Column: string
      Value: Value
      Operator: Operator
      Relation: string option }

and Operator =
    | NotEqual
    | LessOrEqual
    | GreaterOrEqual
    | Less
    | Greater
    | Equal

and InfixOperator =
    | Difference
    | Product
    | Union
    | Intersection

and LogicalOperator =
    | And
    | Or

and ColumnList = ColumnList of Identifier list

type ColOrVal =
    | Column of Column
    | Value of Value

and Column =
    | SingleIdentifier of string
    | QualifiedIdentifier of string * string

type Statement =
    | PrintStmt of Identifier
    | AssignStmt of AssignStmt
    | ListingStmt of string
    | QuitStmt of string
    | UseStmt of Identifier
    | Expression of Expression

and AssignStmt = Identifier * Expression


type ExecutionError =
    | IncorrectPathError
    | EmptyCSVError
    | ColumnError of ColumnError
    | CompatibilityError of CompatibilityError
    | ConditionError of ConditionError

and ColumnError = ColumnNotFound

and CompatibilityError =
    | ColumnsMismatch
    | ColumnTypesMismatch
    | ColumnsOrderMismatch

and ConditionError =
    | TypesMismatch
    | IlldifinedOperatorForStrings
    | UnsupportedColumnType
    | ColumnNotFound
    | IncorrectRelationName
    | RelationNameUnavailable

type ColumnValidity =
    | ValidColumn of ValidColumn
    | ConditionError of ConditionError

and ValidColumn =
    | Decimal
    | String

type ConditionValidity =
    | ValidCondition
    | ConditionError of ConditionError

type Compatibility =
    | Compatible
    | CompatibilityError of CompatibilityError

type EvaluationError =
    | ParseError of ParseError
    | ExecutionError of ExecutionError

and ParseError = ParseError of string

let combineValidity (v1: ConditionValidity) (v2: ConditionValidity) =
    match (v1, v2) with
    | (ValidCondition, ValidCondition) -> ValidCondition
    | (ValidCondition, ConditionError e) -> ConditionError e
    | (ConditionError e, ValidCondition) -> ConditionError e
    // When both conditions are invalid, only raise the first error.
    | (ConditionError e1, ConditionError e2) -> ConditionError e1

let combineANDColumnValidity (v1: ColumnValidity) (v2: ColumnValidity) =
    match (v1, v2) with
    | (ValidColumn t1, ValidColumn t2) ->
        if t1 = t2 then
            ValidColumn t1
        else
            TypesMismatch |> ColumnValidity.ConditionError
    | (ValidColumn t1, ColumnValidity.ConditionError e) -> ColumnValidity.ConditionError e
    | (ColumnValidity.ConditionError e, ValidColumn t2) -> ColumnValidity.ConditionError e
    // When both conditions are invalid, only raise the first error.
    | (ColumnValidity.ConditionError e1, ColumnValidity.ConditionError e2) -> ColumnValidity.ConditionError e1

let combineORColumnValidity (v1: ColumnValidity) (v2: ColumnValidity) =
    match (v1, v2) with
    // When both columns are valid, pass the first column.
    | (ValidColumn t1, ValidColumn t2) -> ValidColumn t1
    | (ValidColumn t1, ColumnValidity.ConditionError e) -> ValidColumn t1
    | (ColumnValidity.ConditionError e, ValidColumn t2) -> ValidColumn t2
    // When both columns are invalid, only raise the first error.
    | (ColumnValidity.ConditionError e1, ColumnValidity.ConditionError e2) -> ColumnValidity.ConditionError e1