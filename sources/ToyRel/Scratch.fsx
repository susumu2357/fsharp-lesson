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

// ここから本題
type Relation = Relation of Frame<int, string>

// 以前つくったpaserResultを変形
let paserResult parser str =
    match run parser str with
    | CharParsers.Success (result, _, _) ->
        match result with
        | ProjectExpression projectExpression -> (ProjectExpression projectExpression)
        | Identifier identifier -> (Identifier identifier)
    | CharParsers.Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let distinct (df: Frame<int, string>) =
    df.Rows.Values
    |> Seq.distinct
    |> Series.ofValues
    |> Frame.ofRows
    |> Relation

let openRelation str =
    match paserResult pIdentifierExpression str with
    | Identifier identifier ->
        let filepath =
            ".\\sources\\ToyRel\\database\\master\\"
            + identifier
            + ".csv"

        Frame.ReadCsv filepath |> distinct
    | _ -> failwithf "Failure"

let test1Rel = openRelation "シラバス"
test1Rel

type EvalExpression = Expression -> Relation
type EvalProjectExpression = ProjectExpression -> Relation

let toDf rel =
    let (Relation df) = rel
    df

// let toRel df = Relation df

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList
        let df = toDf rel
        distinct df.Columns.[columns]


let project str =
    let exp = paserResult pProjectExpression str

    match exp with
    | ProjectExpression projectExpression -> evalProjectExpression projectExpression
    | _ -> failwithf "Failure"


let test2Df = project "project (シラバス) 専門, 学年, 場所"
(toDf test2Df).Print()
let test3Df = project "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
(toDf test3Df).Print()
