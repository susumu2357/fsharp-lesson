// 課題3: Relationの型をつくれ
#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"
open FParsec

// 前回までにつくったevalProjectExpressionを再掲

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

// ここから本題
type Relation = Relation of Frame<int, string>

let distinct (df: Frame<int, string>) =
    df.Rows.Values
    |> Seq.distinct
    |> Series.ofValues
    |> Frame.ofRows
    |> Relation

let df = Frame.ReadCsv ".\\sources\\ToyRel\\database\\master\\シラバス.csv"
let test1Relation = distinct df

// test1Relation.Print()
// The type 'Relation' does not define the field, constructor or member 'Print'.

let (Relation backToDf) = test1Relation
backToDf.Print()

// 以前つくったpaserResultを変形
let paserResult parser str =
    match run parser str with
    | CharParsers.Success (result, _, _) ->
        match result with
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression
        | Identifier identifier -> evalExpression (Identifier identifier)
    | CharParsers.Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

// partial application
let project = paserResult pProjectExpression

let test2Df = project "project (シラバス) 専門, 学年, 場所"
test2Df.Print()
let test3Df = project "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
test3Df.Print()

// partial application and function composition
let openRelation = paserResult pExpression >> distinct

let test2Relation = openRelation "シラバス"
let (Relation backToDf2) = test2Relation
backToDf2.Print()
