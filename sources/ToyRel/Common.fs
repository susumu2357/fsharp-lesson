module Common

// let databasePath = ".\\sources\\ToyRel\\database\\master\\"
// let databasePath = ".\\database\\master\\"

type ColumnList = ColumnList of string list

type Expression =
    | Identifier of Identifier
    | ProjectExpression of ProjectExpression

and Identifier = string
and ProjectExpression = Expression * ColumnList

type Statement =
    | PrintStmt of string
    | AssignStmt of AssignStmt
    | ListingStmt of string
    | QuitStmt of string
    | Expression of Expression

and AssignStmt = Identifier * Expression

type StateM<'s, 'r> = StateM of ('s -> 'r * 's)
type Database = Database of string
