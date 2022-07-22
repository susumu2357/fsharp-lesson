// 課題15: 両者のパースをくっつけて課題5のPlayDeedleとくっつけよう
#r "nuget: FParsec"
open FParsec

#r "nuget:Deedle"
open Deedle


// 課題12の抜粋
let ws = manyChars (anyOf [' '; '　'])
run ws " "
let str_ws s = pstring s .>> ws

let notBracket = satisfy (fun c -> c <> ']')
run notBracket "a"
let colParser = (pstring "[") >>. (manyChars notBracket) .>> (str_ws "]")

let notQuotation = satisfy (fun c -> c <> '\"')
run notQuotation "a"
let valParser = (pstring "\"") >>. (manyChars notQuotation) .>> (str_ws "\"")

let filterArgs = colParser .>>. (str_ws "=" >>. valParser)

let pFilter = ws >>. (str_ws "filter") >>. (str_ws "(") >>. filterArgs .>> (str_ws ")")

// 課題13, 14の抜粋
let columns = (pstring "[") >>. (manyChars notBracket) .>> (str_ws "]")
let pColumn = ws >>. columns

let columnList = sepBy pColumn (pstring ",")
let pProjcet = ws >>. (str_ws "project") >>. (str_ws "(") >>. columnList .>> (str_ws ")")


type FilterExpression = 
    {
        Column: string
        Value: string
    }
and ProjectExpression = SelectedColumns of string list
and Expression =
    | Filter of FilterExpression
    | Project of ProjectExpression

let toProjectExpression result =
    Project (SelectedColumns result)

let pProjcetwithType = 
    pProjcet
    |>> toProjectExpression

let toFilterExpression result =
        let column, value = result
        Filter {Column=column; Value=value}

let pFilterwithType = 
    pFilter
    |>> toFilterExpression

let pExpression = pFilterwithType <|> pProjcetwithType

run pExpression "project([学年], [名前])"
run pExpression "filter([専門] = \"物理\")"


// 課題5のFilterを変形
type Row = ObjectSeries<string>
and FilterFunc = FilterExpression ->  Frame<int, string> -> Frame<int, string>

let filter: FilterFunc =
    fun filterArgs df -> 
        let column = filterArgs.Column
        let value = filterArgs.Value
        let condition = fun (row: Row) -> row.GetAs<string>(column)=value
        df.RowsDense 
        |> Series.filterValues( condition )
        |> Frame.ofRows

// 課題5のProjectを変形
type ProjectFunc = ProjectExpression -> Frame<int, string> -> Frame<int, string>

let project: ProjectFunc=
    fun (SelectedColumns projectArgs) df -> 
        df.Columns[projectArgs]

// runExprを定義
let runExpr inputString df =
    match run pExpression inputString with
    | FParsec.CharParsers.ParserResult.Success (res, _, _) -> 
        match res with
        | Project projectArgs -> project projectArgs df
        | Filter filterArgs -> filter filterArgs df
    | Failure (errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

let df = Frame.ReadCsv "C:\\Users\\susum\\Documents\\fsharp-lesson\\sources\\data\\シラバス.csv"

let test1 = runExpr "project([学年], [名前])" df
test1.Print()

let test2 = runExpr "filter([専門] = \"物理\")" df
test2.Print()

runExpr "test([専門])" df
runExpr "filter([専門])" df
