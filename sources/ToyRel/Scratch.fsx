#r "nuget:Deedle"
open Deedle

#r "nuget:FParsec"
open FParsec

// 前回の再掲
let ws = manyChars (anyOf [ ' '; '　' ])

let identifierRegex =
    "^([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})+([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}|[0-9])*"

let pIdentifier = regex identifierRegex .>> ws
let notSBracket s = s <> '[' && s <> ']'

let pSBracketColumn =
    (pstring "[") >>. many1Satisfy notSBracket
    .>> (pstring "]")

let pColumn = pIdentifier <|> pSBracketColumn
run pColumn "[abc_123]"

let str_ws s = pstring s .>> ws

type ColumnList = ColumnList of string list
let pColumnList = sepBy pColumn (str_ws ",") |>> ColumnList

type Expression =
    | Identifier of Identifier: string
    | ProjectExpression of ProjectExpression

and ProjectExpression = Expression * ColumnList

// Statement型を導入
type Statement =
    | PrintStmt of string
    | AssignStmt of AssignStmt
    | Expression of Expression

and AssignStmt = Expression * Expression

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

let paserResult parser str =
    match run parser str with
    | CharParsers.Success (result, _, _) -> result
    | CharParsers.Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

// Statement型を返すパーサー
let pIdentifierStmt = pIdentifierExpression |>> Expression
let pProjectStmt = pProjectExpression |>> Expression

let databasePath = ".\\sources\\ToyRel\\database\\master\\"

open System

let r = Random()

let randName unit =
    let randomChars =
        [ for x in 0..3 do
              string (char (r.Next((int 'a'), (int 'z') + 1))) ]

    "zz"
    // + List.fold (fun acc elem -> acc + string elem) "" randomChars
    + String.concat "" randomChars

module Relation =
    type T = Relation of Frame<int, string>

    let create (df: Frame<int, string>) =
        df.Rows.Values
        |> Seq.distinct
        |> Series.ofValues
        |> Frame.ofRows
        |> Relation

    let value (Relation rel) = rel

    let print rel = (value rel).Print()

    let openRelation relationName =
        match paserResult pIdentifierStmt relationName with
        | Expression exp ->
            match exp with
            | Identifier identifier ->
                let filepath =
                    ".\\sources\\ToyRel\\database\\master\\"
                    + identifier
                    + ".csv"

                Frame.ReadCsv filepath |> create
            | _ -> failwithf "Failure"
        | _ -> failwithf "Failure"

    let project (Relation df) (columns: string list) = create df.Columns.[columns]

    let saveAs rel basename =
        let df = value rel
        df.SaveCsv(databasePath + basename + ".csv")

    // ランダムなファイル名でRelationを保存
    let save rel =
        let rndName = randName ()
        saveAs rel rndName
        rndName

type EvalExpression = Expression -> Relation.T
type EvalProjectExpression = ProjectExpression -> Relation.T

let rec evalExpression: EvalExpression =
    fun exp ->
        match exp with
        | Identifier identifier -> Relation.openRelation identifier
        | ProjectExpression projectExpression -> evalProjectExpression projectExpression

and evalProjectExpression: EvalProjectExpression =
    fun projectExp ->
        let (exp, columnList) = projectExp
        let rel = evalExpression exp
        let (ColumnList columns) = columnList
        Relation.project rel columns


// プリント
let pPrintStmt =
    let stmt = (str_ws "print") >>. pIdentifier

    stmt |>> PrintStmt

let evalPrintStmt identifier =
    let rel = Relation.openRelation identifier
    Relation.print rel

// 開発の方針
// 新しいStatement(or Expression)を実装する場合、そのケースをStatement(or Expression)型に追加して、
// 新しいケースをStatement型として返すパーサーと、その型を評価するeval...関数を書く。
// 下記のpStmtにパーサーを足す。
// 下記のevalに新しい分岐を加えて、eval...関数による処理を足す。
// なお、RelationのI/O周りはRelation moduleに入れる。

// 課題8: 左辺を含むExpressionのパース
// 以下をパースする場合、右辺の結果をtest_relation.csvとして保存するようにしたい。
// test_relation = project (project (シラバス) 専門, 学年, 場所) 専門, 学年
// assignStmtと呼ぶことにする。

let passignStmt =
    pExpression .>>. ((str_ws "=") >>. pExpression)
    |>> AssignStmt

let evalAssignStmt (basename, expression) =
    match basename with
    | Identifier identifier ->
        let rel = evalExpression expression
        Relation.saveAs rel identifier
    | _ -> failwithf "Failure"

let pStmt =
    pPrintStmt
    <|> pProjectStmt
    <|> passignStmt
    <|> pIdentifierStmt

let eval str =
    match paserResult pStmt str with
    | PrintStmt printStmt -> evalPrintStmt printStmt
    | AssignStmt (basename, expression) -> evalAssignStmt (basename, expression)
    | Expression exp ->
        match exp with
        | ProjectExpression projectExpression ->
            let rel = evalProjectExpression projectExpression
            printfn "Relation %s returned." (Relation.save rel)
        | Identifier identifier ->
            printfn "Only relation name is provided."
            printfn "Do you mean \"print\" ?"

eval "print シラバス"
eval "project (シラバス) 名前, 学年"
eval "project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
eval "シラバス"

eval "test_relation = project (project (シラバス) 専門, 学年, 場所) 専門, 学年"
