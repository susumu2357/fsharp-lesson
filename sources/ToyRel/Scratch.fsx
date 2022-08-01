open System.Text.RegularExpressions

Regex.IsMatch("123abc", "[0-9]+")
Regex.Match("123abc", "[0-9]+")

Regex.IsMatch("あ", "\p{IsHiragana}")
Regex.IsMatch("ア", "\p{IsHiragana}")

Regex.IsMatch("あ", "\p{IsHiragana}|\p{IsKatakana}")
Regex.IsMatch("ア", "\p{IsHiragana}|\p{IsKatakana}")
Regex.Match("あアほげいか123", "(\p{IsHiragana}|\p{IsKatakana})+")

// 課題0: identifierにマッチする正規表現を書け
let identifierRegex =
    "^([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs})+([a-zA-Z_]|\p{IsHiragana}|\p{IsKatakana}|\p{IsCJKUnifiedIdeographs}|[0-9])*"

// 以下は全体がマッチ
Regex.Match("abc", identifierRegex)
Regex.Match("_abc123", identifierRegex)
Regex.Match("abc_123", identifierRegex)
Regex.Match("専門", identifierRegex)
Regex.Match("フロア", identifierRegex)


Regex.Match("123", identifierRegex)

// abcのみマッチ
Regex.Match("abc.def", identifierRegex)

// abcのみマッチ
Regex.Match("abc*", identifierRegex)

// abcのみマッチ
Regex.Match("abc:def", identifierRegex)

// abcのみマッチ
Regex.Match("abc def", identifierRegex)

// マッチ無し
Regex.Match("(abc)", identifierRegex)

// abcのみマッチ
Regex.Match("abc+def", identifierRegex)

// pColumnの実装
#r "nuget:FParsec"
open FParsec

let pIdentifier = regex identifierRegex
run pIdentifier "abc"

let notSBracket s = s <> '[' && s <> ']'

let pSBracketColumn =
    (pstring "[") >>. many1Satisfy notSBracket
    .>> (pstring "]")

let pColumn = pIdentifier <|> pSBracketColumn
run pColumn "[abc_123]"

// 角括弧の中身全体をパース
run pColumn "[123_abc]"

// Failure
run pColumn "123_abc"


// expressionの暫定的な仕様

// pColumnListを定義していなかったので、そちらを先に準備
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
