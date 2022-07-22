#r "nuget: FParsec"
open FParsec

let ws = manySatisfy (fun c -> c = ' ' || c = '　')
run ws "　 abc"

let str_ws s = pstring s .>> ws
let notBracketWhite = many1Satisfy (fun c -> c <> ']' &&  c <> ' ' &&  c <> '　') .>> ws
let pColumn = ws >>. between (str_ws "[") (str_ws "]") (manyStrings notBracketWhite) 

run pColumn "[学年]"

// 全角と半角スペースを無視する
run pColumn " 　[ 　学年  　] 　"

// カラム名の間にあるスペースも無視
run pColumn "[学 年]"
run pColumn "[学　年]"

let ws2 = manyChars (anyOf [' '; '　'])
run ws2 " 　abc"

let str_ws2 s = pstring s .>> ws2

let notBracket = satisfy (fun c -> c <> '[' && c <> ']')
let tmpParser = (pstring "[") >>. (manyChars notBracket) .>> (str_ws2 "]")
let pColumn2 = ws2 >>. tmpParser

// カッコの前後のスペースは無視
run pColumn2 " 　[名前]  　"

// カッコの内側のスペースはそのままパース
run pColumn2 " 　[名 前]  　"
run pColumn2 " 　[名前　学年]  　"


// 課題11: projectのパーサーを書こう
let eachColumn = sepBy pColumn2 (pstring ",")
let pProjcet = ws2 >>. (str_ws2 "project") >>. (str_ws2 "(") >>. eachColumn .>> (str_ws2 ")")

run pProjcet "project([場所], [学年])"
run pProjcet " 　project ( 　[場所]　 , [学年] 　) "

// 「返す型もちゃんと作りましょう。」というリクエストがよく分からず、特に実装していないです。
// pProjectのreturnは、ParserResult<string list,unit>ですが、Successの場合に新しく定義したProject型で値を返すという感じでしょうか？

// 課題 10.1 pidentifierを書け
// 後から追加された課題なので、課題11の後に解いています。
let letterOrDigit c = isLetter c || isDigit c
let pidentifier = many1Satisfy2 isLetter letterOrDigit
run pidentifier "abc123"
run pidentifier "123abc"
run pidentifier "a_b"
run pidentifier "_a"

// 課題12: filterのパーサーを書こう
// 仕切り直しで、wsとstr_wsを新たに定義しています。
let ws = manyChars (anyOf [' '; '　'])
let str_ws s = pstring s .>> ws

let notBracket = satisfy (fun c -> c <> ']')
let colParser = (pstring "[") >>. (manyChars notBracket) .>> (str_ws "]")

let notQuotation = satisfy (fun c -> c <> '\"')
let valParser = (pstring "\"") >>. (manyChars notQuotation) .>> (str_ws "\"")

let filterArgs = colParser .>>. (str_ws "=" >>. valParser)

let pFilter = ws >>. (str_ws "filter") >>. (str_ws "(") >>. filterArgs .>> (str_ws ")")

// returnはstring*string
run pFilter "filter([専門] = \"物理\")"

// カッコの中身、引用符の中身以外の空白は無視
run pFilter " 　filter ( 　[専門] 　=   \"物理\"　) 　"

// カッコの中身、引用符の中身の空白はそのままパース
run pFilter "filter([専 　門] = \"物理　　数学　\")"

// 課題13: pFilterのパーサーの返す型を作ろう
type FilterExpression = 
    {
        Column: string
        Value: string
    }

let toFilterExpression result =
        let column, value = result
        {Column=column; Value=value}

let pFilterwithType = 
    pFilter
    |>> toFilterExpression

run pFilterwithType "filter([専門] = \"物理\")"
run pFilterwithType " 　filter ( 　[専門] 　=   \"物理\"　) 　"

run pFilterwithType "filter([専門])"

// 課題14: pProjectも型を作って返すようにし、projectとfilterの両方をパースするpExpressionを作る
// 仕切り直しで、pProjcetを新たに定義しています。内容は課題11と同じです。
let notBracket = satisfy (fun c -> c <> ']')
let columns = (pstring "[") >>. (manyChars notBracket) .>> (str_ws "]")
let pColumn = ws >>. columns

let columnList = sepBy pColumn (pstring ",")
let pProjcet = ws >>. (str_ws "project") >>. (str_ws "(") >>. columnList .>> (str_ws ")")

type ProjectExpression = 
    SelectedColumns of string list

let toProjectExpression result =
    SelectedColumns result

let pProjcetwithType = 
    pProjcet
    |>> toProjectExpression

run pProjcetwithType "project([学年], [名前])"

type Expression =
    | Filter of FilterExpression
    | Project of ProjectExpression

// The type 'FilterExpression' does not match the type 'ProjectExpression'
let pExpression = pFilterwithType <|> pProjcetwithType

// The type 'Expression' does not match the type 'FilterExpression'
let pExpressio: Parser<Expression, Unit> = pFilterwithType <|> pProjcetwithType

let pFilterwithType = 
    pFilterwithType
    |>> Filter

let pProjcetwithType = 
    pProjcetwithType
    |>> Project

let pExpression = pFilterwithType <|> pProjcetwithType

run pExpression "project([学年], [名前])"
run pExpression "filter([専門] = \"物理\")"
