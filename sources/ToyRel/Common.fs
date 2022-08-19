module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
let databaseBase = ".\\database\\"
let mutable dbPath = "master\\"

type Value =
    | Float of float
    | String of string

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | InfixExpression of ((Expression * InfixOperator) * Expression)
    | RestrictExpression of Expression * Condition
    | JoinExpression of ((Expression * Expression) * Condition)

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

and LogicalOperator =
    | And
    | Or

and ColumnList = ColumnList of Identifier list

type ColOrVal =
    | Column of DotColumn
    | Value of Value

and DotColumn =
    | SingleIdentifier of string
    | DoubleIdentifier of string * string

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
