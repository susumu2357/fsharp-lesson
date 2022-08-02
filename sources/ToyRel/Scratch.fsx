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
    | Identifier of Identifier: string
    | ProjectExpression of ProjectExpression

and ProjectExpression = Expression * ColumnList

let pExpression, pExpressionRef = createParserForwardedToRef ()

let pProjectExpression =
    let expression =
        (str_ws "project")
        >>. (str_ws "(")
        >>. pExpression
        .>> (str_ws ")")

    expression .>>. pColumnList |>> ProjectExpression

let pIdentifierExpression = pIdentifier |>> Identifier

pExpressionRef.Value <- pProjectExpression <|> pIdentifierExpression

// ProjectExpression (Identifier "シラバス", ColumnList ["専門"; "学年"; "場所"])
run pProjectExpression "project (シラバス) 専門, 学年, 場所"

// ProjectExpression (ProjectExpression (Identifier "シラバス", ColumnList ["専門"; "学年"; "場所"]), ColumnList ["専門"; "学年"])
run pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"

// ProjectExpression (ProjectExpression (Identifier "シラバス", ColumnList ["123_専門"]), ColumnList ["専門"; "学年"])
run pProjectExpression "project (project (シラバス) [123_専門]) 専門, 学年"

// 第一回と同じ感じのprojectでdfを返す所まで実装してみよう
#r "nuget:Deedle"
open Deedle

type EvalExpression = Expression -> Frame<int, string>
and EvalProjectExpression = ProjectExpression -> Frame<int, string>

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier ->
            let filepath =
                ".\\sources\\ToyRel\\database\\master\\"
                + identifier
                + ".csv"

            Frame.ReadCsv filepath
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let df = evalExpression exp
        let (ColumnList columns) = columnList
        df.Columns[columns]

// 以下、動作確認

let paserResult parser str =
    match run parser str with
    | CharParsers.Success (result, _, _) ->
        match result with
        | ProjectExpression projectExpression -> projectExpression
        | Identifier _ -> failwithf "Try to test on ProjectExpression"
    | CharParsers.Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let test1ProjectExpression =
    paserResult pProjectExpression "project (シラバス) 専門, 学年, 場所"

let test1Df = evalProjectExpression test1ProjectExpression
test1Df.Print()

let test2ProjectExpression =
    paserResult pProjectExpression "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"

let test2Df = evalProjectExpression test2ProjectExpression
test2Df.Print()
