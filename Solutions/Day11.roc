module [solution]

solution = { day: 11, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parse input
    |> repeatBlink 25
    |> countStones

part2 : Str -> _
part2 = \input ->
    parse input
    |> repeatBlink 75
    |> countStones

Stones : Dict U64 U64

parse : Str -> Stones
parse = \input ->
    Str.trim input
    |> Str.splitOn " "
    |> List.mapTry Str.toU64
    |> Result.withDefault []
    |> List.walk (Dict.empty {}) \dict, value -> Dict.insert dict value 1

countStones : Stones -> U64
countStones = \stones ->
    Dict.values stones
    |> List.sum

repeatBlink : Stones, U64 -> Stones
repeatBlink = \stones, count ->
    if count == 0 then
        stones
    else
        repeatBlink (blink stones) (count - 1)

blink : Stones -> Stones
blink = \stones ->
    Dict.toList stones
    |> List.joinMap \(stone, count) ->
        applyRules stone
        |> List.map \v -> (v, count)
    |> mergeCounts

mergeCounts : List (a, U64) -> Dict a U64
mergeCounts = \list ->
    List.walk list (Dict.empty {}) \dict, (key, count) ->
        increaseCount dict key count

increaseCount : Dict a U64, a, U64 -> Dict a U64
increaseCount = \dict, key, count ->
    Dict.update dict key \entry ->
        when entry is
            Err Missing -> Ok count
            Ok old -> Ok (old + count)

applyRules : U64 -> List U64
applyRules = \stone ->
    applyZero stone
    |> Result.onErr \_ -> applyEvenDigits stone
    |> Result.onErr \_ -> Ok (applyDefault stone)
    |> Result.withDefault []

applyZero : U64 -> Result (List U64) [NotApplicable]
applyZero = \stone ->
    if stone == 0 then
        Ok [1]
    else
        Err NotApplicable

applyEvenDigits : U64 -> Result (List U64) [NotApplicable]
applyEvenDigits = \stone ->
    digits = toDigits stone
    len = List.len digits
    if len % 2 == 0 then
        { before, others } = List.splitAt digits (len // 2)
        Ok [
            fromDigits before,
            fromDigits others,
        ]
    else
        Err NotApplicable

applyDefault : U64 -> List U64
applyDefault = \stone -> [stone * 2024]

toDigits : U64 -> List U64
toDigits = \num ->
    toDigitsInner num []

toDigitsInner : U64, List U64 -> List U64
toDigitsInner = \num, digits ->
    if num == 0 then
        digits
    else
        next = num % 10
        rest = num // 10

        toDigitsInner rest (List.prepend digits next)

fromDigits : List U64 -> U64
fromDigits = \digits ->
    fromDigitsInner digits 0

fromDigitsInner : List U64, U64 -> U64
fromDigitsInner = \digits, num ->
    when digits is
        [] -> num
        [next, .. as rest] ->
            newNum = (num * 10) + next
            fromDigitsInner rest newNum

example : Str
example = "125 17"

expect part1 example == 55312
