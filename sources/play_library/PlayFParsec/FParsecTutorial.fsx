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
run (many (str "a" <|> str "b")) "abba"

run (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"

let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace

run identifier "id_1"
run identifier "_1"
run identifier "1abc"
run identifier ""

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"") (manyChars (normalChar <|> escapedChar))

run stringLiteral "\"abc\""
run stringLiteral "\"ab\t\""

let stringLiteral2 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                           | 'n' -> "\n"
                                                           | 'r' -> "\r"
                                                           | 't' -> "\t"
                                                           | c   -> string c)
    between (pstring "\"") (pstring "\"") (manyStrings (normalCharSnippet <|> escapedChar))

run stringLiteral2 "\"abc\""
run stringLiteral2 "\"abc\n\""

let stringLiteral3 =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                           | 'n' -> "\n"
                                                           | 'r' -> "\r"
                                                           | 't' -> "\t"
                                                           | c   -> string c)
    between (pstring "\"") (pstring "\"")  (stringsSepBy normalCharSnippet escapedChar)

run stringLiteral3 "\"abc\""
run stringLiteral3 "\"abc\n\""

// 4.8 Sequentially applying parsers
let product = pipe2 float_ws (str_ws "*" >>. float_ws) (fun x y -> x * y)
run product "2*3"
run product "10*3"

type StringConstant = StringConstant of string * string
let stringConstant = pipe3 identifier (str_ws "=") stringLiteral (fun id _ str -> StringConstant(id, str))
run stringConstant "myString = \"stringValue\""
run stringConstant "myString = \"stringValue123\""

run (float_ws .>>. (str_ws "," >>. float_ws)) "123, 456"

// 4.9 Parsing alternatives
let boolean = (stringReturn "true"  true) <|> (stringReturn "false" false)
run boolean "false"
run boolean "fal"

run  ((ws >>. str "a") <|> (ws >>. str "b")) " b"
run  (ws >>.  (str "a" <|> str "b")) " b"
