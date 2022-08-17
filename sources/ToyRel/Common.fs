module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
let databaseBase = ".\\database\\"
let mutable dbPath = "master\\"

type ColumnList = ColumnList of string list

type Value =
    | Float of float
    | String of string

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | DifferenceExpression of Expression * Expression
    | RestrictExpression of Expression * Condition
    | ProductExpression of Expression * Expression

and Identifier = string
and ProjectExpression = Expression * ColumnList

and Condition =
    | SingleCondition of SingleCondition
    | ANDCondition of Condition * Condition
    | ORCondition of Condition * Condition
    | NOTCondition of Condition

and SingleCondition =
    | ColumnColumn of ColumnColumn
    | ColumnValue of ColumnValue

and ColumnColumn =
    { Column1: string
      Column2: string
      Operator: Operator }

and ColumnValue =
    { Column: string
      Value: Value
      Operator: Operator }

and Operator =
    | NotEqual
    | LessOrEqual
    | GreaterOrEqual
    | Less
    | Greater
    | Equal


type ColOrVal =
    | Column of string
    | Value of Value

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
    | ProjectionError of ProjectionError
    | ComparabilityError of ComparabilityError
    | ConditionError of ConditionError

and ProjectionError = ColumnNotFound

and ComparabilityError =
    | ColumnsMismatch
    | ColumnTypesMismatch
    | ColumnsOrderMismatch

and ConditionError =
    | TypesMismatch
    | IlldifinedOperatorForStrings
    | UnsupportedColumnType
    | ColumnNotFound

type ColumnValidity =
    | ValidColumn of ValidColumn
    | ConditionError of ConditionError

and ValidColumn =
    | Float
    | String

type ConditionValidity =
    | ValidCondition
    | ConditionError of ConditionError

type Comparability =
    | Comparable
    | ComparabilityError of ComparabilityError

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
