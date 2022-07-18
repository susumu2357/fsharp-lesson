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