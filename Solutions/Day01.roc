module [solution]

solution = { day: 1, part1, part2 }

part1 : Str -> _
part1 = \input ->
    (left, right) = parse? input
    sortedLeft = List.sortAsc left
    sortedRight = List.sortAsc right

    List.map2 sortedLeft sortedRight Num.absDiff
    |> List.sum
    |> Ok

part2 : Str -> _
part2 = \input ->
    (left, right) = parse? input
    rightFrequency = toFrequencyMap right

    List.map left \num ->
        multiplier = toSimilarity (Dict.get rightFrequency num)
        num * multiplier
    |> List.sum
    |> Ok

parse : Str -> Result (List U64, List U64) _
parse = \input ->
    Str.trim input
        |> Str.splitOn "\n"
        |> List.mapTry? parseLine
        |> List.walk ([], []) \lists, items ->
            (List.append lists.0 items.0, List.append lists.1 items.1)
        |> Ok

parseLine : Str -> Result (U64, U64) _
parseLine = \line ->
    Str.splitOn line "   "
        |> List.mapTry? Str.toU64
        |> intoTuple

intoTuple : List U64 -> Result (U64, U64) _
intoTuple = \list ->
    when list is
        [first, second] -> Ok (first, second)
        [..] -> Err WrongNumberOfElements

toFrequencyMap : List U64 -> Dict U64 U64
toFrequencyMap = \list ->
    List.walk list (Dict.empty {}) \dict, num ->
        Dict.update dict num incrementFrequency

incrementFrequency : Result U64 [Missing] -> Result U64 [Missing]
incrementFrequency = \possible ->
    when possible is
        Err Missing -> Ok 1
        Ok count -> Ok (count + 1)

toSimilarity : Result U64 _ -> U64
toSimilarity = \possible ->
    when possible is
        Ok value -> value
        Err _ -> 0

example : Str
example =
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

expect part1 example == Ok 11

expect part2 example == Ok 31
