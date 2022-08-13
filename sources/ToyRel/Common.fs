module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
let databaseBase = ".\\database\\"
let mutable dbPath = "master\\"

type ColumnList = ColumnList of string list

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression
    | DifferenceExpression of Expression * Expression
    | RestrictExpression of Expression * Condition

and Identifier = string
and ProjectExpression = Expression * ColumnList

and Condition =
    | SingleCondition of SingleCondition
    | ANDCondition of Condition * Condition
    | ORCondition of Condition * Condition

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

and Value =
    | Float of float
    | String of string

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

and ConditionError = ConditionError

type Comparability<'a> =
    | Comparable of 'a list
    | ComparabilityError of ComparabilityError

type EvaluationError =
    | ParseError of ParseError
    | ExecutionError of ExecutionError

and ParseError = ParseError of string
