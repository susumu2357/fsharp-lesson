module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
let databaseBase = ".\\database\\"
let mutable dbPath = "master\\"

type ColumnList = ColumnList of string list

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression

and Identifier = string
and ProjectExpression = Expression * ColumnList

type Statement =
    | PrintStmt of Identifier
    | AssignStmt of AssignStmt
    | ListingStmt of string
    | QuitStmt of string
    | UseStmt of Identifier
    | Expression of Expression

and AssignStmt = Identifier * Expression
