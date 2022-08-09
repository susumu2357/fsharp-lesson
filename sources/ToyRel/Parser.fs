module Parser

open FParsec
open Common

let ws = manyChars (anyOf [ ' '; '　' ])

let identifierRegex =
    "^([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})+([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}|[0-9])*"

let pIdentifier = regex identifierRegex .>> ws
let notSBracket s = s <> '[' && s <> ']'

let pSBracketColumn =
    (pstring "[") >>. many1Satisfy notSBracket
    .>> (pstring "]")

let pColumn = pIdentifier <|> pSBracketColumn

let str_ws s = pstring s .>> ws

let pColumnList = sepBy pColumn (str_ws ",") |>> ColumnList


let pExpression, pExpressionRef = createParserForwardedToRef ()

let pProjectExpression =
    let expression =
        (str_ws "project")
        >>. (str_ws "(")
        >>. pExpression
        .>> (str_ws ")")

    expression .>>. pColumnList |>> ProjectExpression

let pIdentifierExpression = pIdentifier |>> Expression.Identifier

pExpressionRef.Value <- pProjectExpression <|> pIdentifierExpression

let pIdentifierStmt = pIdentifierExpression |>> Expression
let pProjectStmt = pProjectExpression |>> Expression

let pPrintStmt =
    let stmt = (str_ws "print") >>. pIdentifier

    stmt |>> PrintStmt

let pAssignStmt =
    pIdentifier .>>. ((str_ws "=") >>. pExpression)
    |>> AssignStmt

let pListingStmt =
    let stmt = str_ws "list"

    stmt |>> ListingStmt

let pQuitStmt =
    let stmt = str_ws "quit"

    stmt |>> QuitStmt

let pUseStmt =
    let stmt = str_ws "use" >>. pIdentifier

    stmt |>> UseStmt

let pStmt =
    pPrintStmt
    <|> pUseStmt
    <|> pListingStmt
    <|> pQuitStmt
    <|> pProjectStmt
    <|> pAssignStmt
    <|> pIdentifierStmt

let paserResult parser str =
    match run parser str with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

// To avoid Value restriction error
paserResult pStmt "print abc" |> ignore
