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
