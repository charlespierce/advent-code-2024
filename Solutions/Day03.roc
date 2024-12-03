module [solution]

solution = { day: 3, part1, part2 }

import Parser exposing [Parser, parseStr, anyVal, map, delimited, separatedPair, tag, tagged, oneOf, many1, number]

part1 : Str -> _
part1 = \input ->
    parseStr? input parseTokens
        |> List.map evaluateTokenPart1
        |> List.sum
        |> Ok

part2 : Str -> _
part2 = \input ->
    parseStr? input parseTokens
        |> List.walk (Bool.true, 0) walkTokensPart2
        |> .1
        |> Ok

Token : [Trivia, Mul U64 U64, Enable, Disable]

parseTrivia : Parser _
parseTrivia = anyVal Trivia

parseMul : Parser _
parseMul =
    delimited
        (tag "mul(")
        (separatedPair number (tag ",") number)
        (tag ")")
    |> map \(first, second) -> Mul first second

parseEnable : Parser _
parseEnable = tagged Enable "do()"

parseDisable : Parser _
parseDisable = tagged Disable "don't()"

parseToken : Parser Token
parseToken = oneOf [parseMul, parseEnable, parseDisable, parseTrivia]

parseTokens : Parser (List Token)
parseTokens = many1 parseToken

evaluateTokenPart1 : Token -> U64
evaluateTokenPart1 = \token ->
    when token is
        Trivia -> 0
        Enable -> 0
        Disable -> 0
        Mul first second -> first * second

walkTokensPart2 : (Bool, U64), Token -> (Bool, U64)
walkTokensPart2 = \(enabled, sum), token ->
    when token is
        Enable -> (Bool.true, sum)
        Disable -> (Bool.false, sum)
        Mul first second if enabled -> (enabled, sum + first * second)
        _ -> (enabled, sum)

example1 : Str
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect part1 example1 == Ok 161

example2 : Str
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

expect part2 example2 == Ok 48
