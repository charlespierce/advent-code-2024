module [solution]

solution = { day: 2, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parse? input
        |> List.keepIf (reportChecker walkValue)
        |> List.len
        |> Ok

part2 : Str -> _
part2 = \input ->
    parse? input
        |> List.keepIf (reportChecker walkValueWithDampener)
        |> List.len
        |> Ok

reportChecker : _ -> (List U8 -> Bool)
reportChecker = \walker ->
    \report ->
        forward =
            List.walkTry report Initial walker
            |> Result.isOk
        backward =
            List.reverse report
            |> List.walkTry Initial walker
            |> Result.isOk

        forward || backward

walkValue : _, U8 -> Result _ _
walkValue = \state, value ->
    when state is
        Initial -> Ok (Start value)
        Start prev if validIncreasing prev value -> Ok (Increasing value)
        Start prev if validDecreasing prev value -> Ok (Decreasing value)
        Increasing prev if validIncreasing prev value -> Ok (Increasing value)
        Decreasing prev if validDecreasing prev value -> Ok (Decreasing value)
        _ -> Err Unsafe

walkValueWithDampener : _, U8 -> Result _ _
walkValueWithDampener = \state, value ->
    when state is
        Initial -> Ok (Start value)
        Start prev if validIncreasing prev value -> Ok (Increasing value)
        Start prev if validDecreasing prev value -> Ok (Decreasing value)
        Increasing prev if validIncreasing prev value -> Ok (Increasing value)
        Increasing prev -> Ok (IncreasingDamped prev)
        Decreasing prev if validDecreasing prev value -> Ok (Decreasing value)
        Decreasing prev -> Ok (DecreasingDamped prev)
        IncreasingDamped prev if validIncreasing prev value -> Ok (IncreasingDamped value)
        DecreasingDamped prev if validDecreasing prev value -> Ok (DecreasingDamped value)
        _ -> Err Unsafe

validIncreasing : U8, U8 -> Bool
validIncreasing = \prev, curr ->
    prev < curr && curr <= prev + 3

validDecreasing : U8, U8 -> Bool
validDecreasing = \prev, curr ->
    curr < prev && curr + 3 >= prev

parse : Str -> Result (List (List U8)) _
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.mapTry parseReport

parseReport : Str -> Result (List U8) _
parseReport = \line ->
    Str.splitOn line " "
        |> List.walkTry [] \report, level ->
            parsed = Str.toU8? level

            Ok (List.append report parsed)

example : Str
example =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

expect part1 example == Ok 2

expect part2 example == Ok 4
