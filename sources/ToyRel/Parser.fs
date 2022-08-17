module Parser

open FParsec
open Common

let ws = manyChars (anyOf [ ' '; '　' ])
let str_ws s = pstring s .>> ws

let identifierRegex =
    "^([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})+([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}|[0-9])*"

let pIdentifier = regex identifierRegex .>> ws
let notSBracket s = s <> '[' && s <> ']'

let pSBracketColumn =
    (pstring "[") >>. many1Satisfy notSBracket
    .>> (str_ws "]")

let pColumn = pIdentifier <|> pSBracketColumn


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

let toTwoExpressionsType ((exp1, operator), exp2) =
    match operator with
    | "difference" -> DifferenceExpression(exp1, exp2)
    | "product" -> ProductExpression(exp1, exp2)
    | _ -> failwithf "operator should be one of 'difference' and 'product'"

let pTwoExpressions =
    let expression1 = (str_ws "(") >>. pExpression .>> (str_ws ")")
    let expression2 = (str_ws "(") >>. pExpression .>> (str_ws ")")

    (expression1
     .>>. (str_ws ("difference") <|> str_ws ("product")))
    .>>. expression2
    |>> toTwoExpressionsType

let pOperator =
    (str_ws "<>" >>% NotEqual)
    <|> (str_ws "<=" >>% LessOrEqual)
    <|> (str_ws ">=" >>% GreaterOrEqual)
    <|> (str_ws "<" >>% Less)
    <|> (str_ws ">" >>% Greater)
    <|> (str_ws "=" >>% Equal)

let pValue =
    let notQuotation = satisfy (fun c -> c <> '\"')

    (pfloat .>> ws |>> Value.Float |>> ColOrVal.Value)
    <|> (str_ws "\"" >>. (manyChars notQuotation)
         .>> str_ws "\""
         |>> Value.String
         |>> ColOrVal.Value)

let toColumnColumn ((col1, op), col2) =
    { Column1 = col1
      Column2 = col2
      Operator = op }

let toColumnValue ((col, op), value) =
    { Column = col
      Value = value
      Operator = op }

let toConditionType ((col, op), colOrVal) =
    match colOrVal with
    | ColOrVal.Column col2 -> toColumnColumn ((col, op), col2) |> ColumnColumn
    | ColOrVal.Value value -> toColumnValue ((col, op), value) |> ColumnValue

let pCondition, pConditionRef = createParserForwardedToRef ()

let pSingleCondition =
    pColumn
    .>>. pOperator
    .>>. (pColumn |>> ColOrVal.Column <|> pValue)
    |>> toConditionType
    |>> SingleCondition



let toAndOrType ((cond1, andOr), cond2) =
    match andOr with
    | "and" -> ANDCondition(cond1, cond2)
    | "or" -> ORCondition(cond1, cond2)
    | _ -> failwithf "logical operator should be either 'and' or 'or'"

let pAndOr =
    let cond1 = (str_ws "(") >>. pCondition .>> (str_ws ")")
    let cond2 = (str_ws "(") >>. pCondition .>> (str_ws ")")

    (cond1 .>>. (str_ws ("and") <|> str_ws ("or")))
    .>>. cond2
    |>> toAndOrType

let pNOTCondition =
    let cond = (str_ws "(") >>. pCondition .>> (str_ws ")")
    str_ws ("not") >>. cond |>> NOTCondition


pConditionRef.Value <- pNOTCondition <|> pSingleCondition <|> pAndOr

let pRestrictExpression =
    let expression =
        (str_ws "restrict")
        >>. (str_ws "(")
        >>. pExpression
        .>> (str_ws ")")

    let condition = (str_ws "(") >>. pCondition .>> (str_ws ")")

    expression .>>. condition |>> RestrictExpression


pExpressionRef.Value <-
    pProjectExpression
    <|> pRestrictExpression
    <|> pIdentifierExpression
    <|> pTwoExpressions


let pIdentifierStmt = pIdentifierExpression |>> Expression
let pProjectStmt = pProjectExpression |>> Expression
let pRestrictStmt = pRestrictExpression |>> Expression
let pTwoExpressionsStmt = pTwoExpressions |>> Expression

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
    <|> pRestrictStmt
    <|> pAssignStmt
    <|> pTwoExpressionsStmt
    <|> pIdentifierStmt

let paserResult =
    fun parser str ->
        match run parser str with
        | Success (result, _, _) -> Result.Ok result
        | Failure (errorMsg, _, _) -> Result.Error(ParseError errorMsg)

// To avoid Value restriction error
paserResult pStmt "print abc" |> ignore
