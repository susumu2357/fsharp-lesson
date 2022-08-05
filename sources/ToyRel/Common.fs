module Common

let databasePath = ".\\sources\\ToyRel\\database\\master\\"

type ColumnList = ColumnList of string list

type Expression =
    | Identifier of IdentifierType
    | ProjectExpression of ProjectExpression

and IdentifierType = string
and ProjectExpression = Expression * ColumnList

type Statement =
    | PrintStmt of string
    | AssignStmt of AssignStmt
    | ListingStmt of string
    | Expression of Expression

and AssignStmt = IdentifierType * Expression
