// 課題1: pExpressionとpProjectExpressionをここまでの仕様で完成させよ
// 課題0のブランチで回答したが、続きを進めるために必要なので再掲
#r "nuget:FParsec"
open FParsec

let identifierRegex =
    "^([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})+([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}|[0-9])*"

let pIdentifier = regex identifierRegex

let notSBracket s = s <> '[' && s <> ']'

let pSBracketColumn =
    (pstring "[") >>. many1Satisfy notSBracket
    .>> (pstring "]")

let pColumn = pIdentifier <|> pSBracketColumn
run pColumn "[abc_123]"

let ws = manyChars (anyOf [ ' '; '　' ])
let str_ws s = pstring s .>> ws

type ColumnList = ColumnList of string list

let pColumnList = sepBy pColumn (str_ws ",") |>> ColumnList

run pColumnList "a, b, [123_abc]"

type Expression =
    | Identifier of string
    | ProjectExpression of Expression * ColumnList

let pExpression, pExpressionRef = createParserForwardedToRef ()

let pProjectExpression =
    let expression =
        (str_ws "project")
        >>. (str_ws "(")
        >>. pExpression
        .>> (str_ws ")")

    expression .>>. pColumnList |>> ProjectExpression

let pIdentifierwithType = pIdentifier |>> Identifier

pExpressionRef.Value <- pProjectExpression <|> pIdentifierwithType

// ProjectExpression (Identifier "シラバス", ColumnList ["専門"; "学年"; "場所"])
run pProjectExpression "project (シラバス) 専門, 学年, 場所"

// ProjectExpression (ProjectExpression (Identifier "シラバス", ColumnList ["専門"; "学年"; "場所"]), ColumnList ["専門"; "学年"])
run pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"

// ProjectExpression (ProjectExpression (Identifier "シラバス", ColumnList ["123_専門"]), ColumnList ["専門"; "学年"])
run pProjectExpression "project (project (シラバス) [123_専門]) 専門, 学年"
