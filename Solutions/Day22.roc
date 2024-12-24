module [solution]

solution = { day: 22, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parse input
    |> List.map \num -> iterateSecret num 2000
    |> List.sum

part2 : Str -> _
part2 = \input ->
    parse input
    |> List.walk (Dict.empty {}) \totals, secret ->
        generatePricesDiffs secret [] (Set.empty {}) totals 2001
        |> .1
    |> Dict.walk 0 \maxSale, _, sale -> Num.max sale maxSale

generatePricesDiffs : I64, List I64, Set (List I64), Dict (List I64) I64, U64 -> (Set (List I64), Dict (List I64) I64)
generatePricesDiffs = \secret, diffs, seenDiffs, diffTotals, iterations ->
    if iterations == 0 then
        (seenDiffs, diffTotals)
    else
        curPrice = price secret
        nextSecret = genSecret secret
        nextPrice = price nextSecret

        nextDiff = nextPrice - curPrice

        updatedDiffs = appendDiff diffs nextDiff

        if List.len updatedDiffs == 4 then
            if Set.contains seenDiffs updatedDiffs then
                generatePricesDiffs nextSecret updatedDiffs seenDiffs diffTotals (iterations - 1)
            else
                updatedSeen = Set.insert seenDiffs updatedDiffs
                updatedTotals = Dict.update diffTotals updatedDiffs \existing ->
                    when existing is
                        Ok sales -> Ok (sales + nextPrice)
                        Err Missing -> Ok nextPrice

                generatePricesDiffs nextSecret updatedDiffs updatedSeen updatedTotals (iterations - 1)
        else
            generatePricesDiffs nextSecret updatedDiffs seenDiffs diffTotals (iterations - 1)

appendDiff : List I64, I64 -> List I64
appendDiff = \diffs, diff ->
    List.append diffs diff
    |> List.takeLast 4

parse : Str -> List I64
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map \line ->
        Str.toI64 line
        |> Result.withDefault 0

iterateSecret : I64, I64 -> I64
iterateSecret = \secret, iterations ->
    if iterations == 0 then
        secret
    else
        genSecret secret
        |> iterateSecret (iterations - 1)

genSecret : I64 -> I64
genSecret = \secret ->
    first =
        Num.mul secret 64
        |> mixPrune secret

    second =
        Num.divTrunc first 32
        |> mixPrune first

    Num.mul second 2048
    |> mixPrune second

mixPrune : I64, I64 -> I64
mixPrune = \new, old ->
    Num.bitwiseXor new old
    |> Num.rem 16777216

price : I64 -> I64
price = \secret -> Num.rem secret 10

example : Str
example =
    """
    1
    10
    100
    2024
    """

expect part1 example == 37327623

example2 : Str
example2 =
    """
    1
    2
    3
    2024
    """

expect part2 example2 == 23
