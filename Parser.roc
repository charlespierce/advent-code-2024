module [
    Utf8,
    Parser,
    ParseResult,
    parse,
    parseStr,
    tag,
    tagged,
    oneOf,
    many,
    many1,
    separatedList,
    delimited,
    pair,
    triple,
    separatedPair,
    digit,
    number,
    signed,
    any,
    anyVal,
    map,
]

Utf8 : List U8

Parser output : Utf8 -> ParseResult output

ParseResult output : Result (output, Utf8) [ParseError Str]

parse : Utf8, Parser output -> Result output _
parse = \input, parser ->
    when parser input is
        Ok (output, []) -> Ok output
        Ok (_, rest) -> Err (ParsingIncomplete rest)
        Err (ParseError msg) -> Err (ParseError msg)

parseStr : Str, Parser output -> Result output _
parseStr = \input, parser ->
    Str.toUtf8 input |> parse parser

tag : Str -> Parser Utf8
tag = \value ->
    valueBytes = Str.toUtf8 value
    \input ->
        { before, others } = List.splitAt input (List.len valueBytes)

        if before == valueBytes then
            Ok (before, others)
        else
            Err (ParseError "did not find expected string $(value)")

tagged : output, Str -> Parser output
tagged = \value, tagStr -> tag tagStr |> map \_ -> value

oneOf : List (Parser output) -> Parser output
oneOf = \parsers ->
    \input ->
        List.walkUntil parsers (Err (ParseError "no parsers provided")) \_, parser ->
            when parser input is
                Ok (output, rest) -> Break (Ok (output, rest))
                Err msg -> Continue (Err msg)

manyInner : List output, Utf8, Parser output -> ParseResult (List output)
manyInner = \results, remaining, parser ->
    when parser remaining is
        Err _ -> Ok (results, remaining)
        Ok (next, rest) -> manyInner (List.append results next) rest parser

many : Parser output -> Parser (List output)
many = \parser ->
    \input ->
        manyInner [] input parser

many1 : Parser output -> Parser (List output)
many1 = \parser ->
    \input ->
        (first, rest) = parser? input
        manyInner [first] rest parser

separatedList : Parser output, Parser _ -> Parser (List output)
separatedList = \item, separator ->
    \input ->
        combined =
            pair separator item
            |> map .1

        (first, rest) = item? input
        manyInner [first] rest combined

delimited : Parser _, Parser output, Parser _ -> Parser output
delimited = \opening, value, closing ->
    \input ->
        (_, rest) = opening? input
        (output, rest2) = value? rest
        (_, rest3) = closing? rest2

        Ok (output, rest3)

pair : Parser output1, Parser output2 -> Parser (output1, output2)
pair = \first, second ->
    \input ->
        (firstValue, rest) = first? input
        (secondValue, rest2) = second? rest

        Ok ((firstValue, secondValue), rest2)

triple : Parser output1, Parser output2, Parser output3 -> Parser (output1, output2, output3)
triple = \first, second, third ->
    \input ->
        (firstValue, rest) = first? input
        (secondValue, rest2) = second? rest
        (thirdValue, rest3) = third? rest2

        Ok ((firstValue, secondValue, thirdValue), rest3)

separatedPair : Parser output1, Parser _, Parser output2 -> Parser (output1, output2)
separatedPair = \first, separator, second ->
    \input ->
        (firstValue, rest) = first? input
        (_, rest2) = separator? rest
        (secondValue, rest3) = second? rest2

        Ok ((firstValue, secondValue), rest3)

digit : Parser U64
digit = \input ->
    when input is
        [] -> Err (ParseError "Unexpected EOF")
        [first, .. as rest] if '0' <= first && first <= '9' ->
            Ok (Num.toU64 (first - '0'), rest)

        _ -> Err (ParseError "Not a digit")

number : Parser U64
number = \input ->
    (digits, rest) = (many digit)? input
    value = List.walk digits 0 \total, d -> total * 10 + d

    Ok (value, rest)

signed : Parser I64
signed = \input ->
    (sign, rest) =
        when (tag "-") input is
            Ok (_, left) -> (-1, left)
            Err _ -> (1, input)

    (unsigned, rest2) = number? rest

    value = sign * (Num.toI64 unsigned)

    Ok (value, rest2)

any : Parser U8
any = \input ->
    when input is
        [] -> Err (ParseError "Unexpected EOF")
        [value, .. as rest] -> Ok (value, rest)

anyVal : output -> Parser output
anyVal = \value -> any |> map \_ -> value

map : Parser a, (a -> b) -> Parser b
map = \parser, func ->
    \input ->
        (valueA, rest) = parser? input
        Ok (func valueA, rest)
