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

let pQualifiedIdentifier =
    ((regex identifierRegex) .>> pstring ".")
    .>>. pColumn


let pDotColumn =
    (attempt pQualifiedIdentifier
     |>> QualifiedIdentifier)
    <|> (pColumn |>> SingleIdentifier)

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


let pInfixExpression =
    let expression1 = (str_ws "(") >>. pExpression .>> (str_ws ")")
    let expression2 = (str_ws "(") >>. pExpression .>> (str_ws ")")

    (expression1
     .>>. ((str_ws ("difference") >>% Difference)
           <|> (str_ws ("product") >>% Product)
           <|> (str_ws ("union") >>% Union)
           <|> (str_ws ("intersect") >>% Intersection)))
    .>>. expression2
    |>> InfixExpression

let pOperator =
    (str_ws "<>" >>% NotEqual)
    <|> (str_ws "<=" >>% LessOrEqual)
    <|> (str_ws ">=" >>% GreaterOrEqual)
    <|> (str_ws "<" >>% Less)
    <|> (str_ws ">" >>% Greater)
    <|> (str_ws "=" >>% Equal)

let pValue =
    let notQuotation = satisfy (fun c -> c <> '\"')

    (pfloat .>> ws
     |>> decimal
     |>> Value.Decimal
     |>> ColOrVal.Value)
    <|> (str_ws "\"" >>. (manyChars notQuotation)
         .>> str_ws "\""
         |>> Value.String
         |>> ColOrVal.Value)

let toColumnColumn ((col1, op), col2) =
    match (col1, col2) with
    | (QualifiedIdentifier (r1, c1), QualifiedIdentifier (r2, c2)) ->
        { Column1 = c1
          Column2 = c2
          Operator = op
          Relation1 = Some r1
          Relation2 = Some r2 }
    | (QualifiedIdentifier (r1, c1), SingleIdentifier c2) ->
        { Column1 = c1
          Column2 = c2
          Operator = op
          Relation1 = Some r1
          Relation2 = None }
    | (SingleIdentifier c1, QualifiedIdentifier (r2, c2)) ->
        { Column1 = c1
          Column2 = c2
          Operator = op
          Relation1 = None
          Relation2 = Some r2 }
    | (SingleIdentifier c1, SingleIdentifier c2) ->
        { Column1 = c1
          Column2 = c2
          Operator = op
          Relation1 = None
          Relation2 = None }

let toColumnValue ((col, op), value) =
    match col with
    | QualifiedIdentifier (r, c) ->
        { Column = c
          Value = value
          Operator = op
          Relation = Some r }
    | SingleIdentifier c ->
        { Column = c
          Value = value
          Operator = op
          Relation = None }

let toConditionType ((col, op), colOrVal) =
    match colOrVal with
    | ColOrVal.Column col2 -> toColumnColumn ((col, op), col2) |> ColumnColumn
    | ColOrVal.Value value -> toColumnValue ((col, op), value) |> ColumnValue

let pCondition, pConditionRef = createParserForwardedToRef ()

let pSingleCondition =
    pDotColumn
    .>>. pOperator
    .>>. (pDotColumn |>> ColOrVal.Column <|> pValue)
    |>> toConditionType
    |>> SingleCondition



let pAndOr =
    let cond1 = (str_ws "(") >>. pCondition .>> (str_ws ")")
    let cond2 = (str_ws "(") >>. pCondition .>> (str_ws ")")

    (cond1
     .>>. ((str_ws ("and") >>% And)
           <|> (str_ws ("or") >>% Or)))
    .>>. cond2
    |>> InfixCondition

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

let pJoinExpression =
    let joinLeft =
        (str_ws "join") >>. (str_ws "(") >>. pExpression
        .>> (str_ws ")")

    let joinRight = (str_ws "(") >>. pExpression .>> (str_ws ")")

    let condition = (str_ws "(") >>. pCondition .>> (str_ws ")")

    joinLeft .>>. joinRight .>>. condition
    |>> JoinExpression

let pRenameExpression =
    (str_ws "rename"
     >>. str_ws "("
     >>. pQualifiedIdentifier
     .>> str_ws ")")
    .>>. pColumn
    |>> RenameExpression

pExpressionRef.Value <-
    pProjectExpression
    <|> pRestrictExpression
    <|> pRenameExpression
    <|> pJoinExpression
    <|> pIdentifierExpression
    <|> pInfixExpression


let pIdentifierStmt = pIdentifierExpression |>> Expression
let pProjectStmt = pProjectExpression |>> Expression
let pRestrictStmt = pRestrictExpression |>> Expression
let pInfixStmt = pInfixExpression |>> Expression
let pJoinStmt = pJoinExpression |>> Expression
let pRenameStmt = pRenameExpression |>> Expression

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
    <|> pRenameStmt
    <|> pJoinStmt
    <|> pAssignStmt
    <|> pInfixStmt
    <|> pIdentifierStmt

let paserResult =
    fun parser str ->
        match run parser str with
        | Success (result, _, _) -> Result.Ok result
        | Failure (errorMsg, _, _) -> Result.Error(ParseError errorMsg)

// To avoid Value restriction error
paserResult pStmt "print abc" |> ignore
