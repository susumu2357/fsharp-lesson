#r "nuget: FParsec"
open FParsec

run pfloat "1.25"

let ws = spaces
let float_ws = pfloat .>> ws
run float_ws "1.25 "

// FParsec Tutorial
// http://www.quanttec.com/fparsec/tutorial.html

// 4.2 Parsing a single float
let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test pfloat "1.25"
test pfloat "1.25E 3"
run pfloat "1.25E 3"

// 4.3 Parsing a float between brackets
let str s = pstring s
let floatBetweenBrackets = str "[" >>. pfloat .>> str "]"

run floatBetweenBrackets "[1234.567]"
run floatBetweenBrackets "[abc]"
run floatBetweenBrackets "[1234.567"

// 4.4 Abstracting parsers
let betweenStrings s1 s2 p = str s1 >>. p .>> str s2
let floatBetweenBrackets2 = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"

run floatBetweenBrackets2 "[123.4]"
run floatBetweenDoubleBrackets "[[1234.5]]"
run floatBetweenDoubleBrackets "[[1234.5]"

let between pBegin pEnd p  = pBegin >>. p .>> pEnd
let betweenStrings2 s1 s2 p = p |> between (str s1) (str s2)

let floatBetweenBrackets3 =  pfloat |> betweenStrings2 "[" "]"
run floatBetweenBrackets3 "[1.234]"

// 4.5 Parsing a list of floats
run (many floatBetweenBrackets) ""
run (many floatBetweenBrackets) "[1.2]"
run (many floatBetweenBrackets) "[1][1.2][3.4]"
run (many floatBetweenBrackets) "[1][1.2E]"
run (many1 floatBetweenBrackets) ""
run (many1 floatBetweenBrackets) "[]"

run (skipMany floatBetweenBrackets) "[1][1.2][3.4]"
run (skipMany1 floatBetweenBrackets) "[]"

let floatList = str "[" >>. sepBy pfloat (str ",") .>> str "]"
run floatList "[1.0,2.0,3.4]"
run floatList "[1,2,3]"
run floatList "[1,2,3,]"

// 4.6 Handling whitespace
let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
run float_ws "123  "

let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"
run numberList "[1, 2, 3]  \n"
run numberList @"[1, 
2, 3"

let numberListFile = ws >>. numberList .>> eof
run numberListFile "  [1, 2, 3]  "
run numberList "  [1, 2, 3]  "
run numberListFile "[1, 2, 3] [4]"
run numberList "[1, 2, 3] [4]"

// 4.7 Parsing string data
