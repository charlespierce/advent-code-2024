module [solution]

solution = { day: 7, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parse? input
        |> List.keepIf checkEquation
        |> List.map .0
        |> List.sum
        |> Ok

part2 : Str -> _
part2 = \input ->
    parse? input
        |> List.keepIf checkEquationWithConcat
        |> List.map .0
        |> List.sum
        |> Ok

parse : Str -> Result (List (U64, List U64)) _
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.mapTry parseTest

parseTest : Str -> Result (U64, List U64) _
parseTest = \line ->
    when Str.splitOn line ": " is
        [target, values] ->
            targetVal = Str.toU64? target
            numValues =
                values
                    |> Str.splitOn " "
                    |> List.mapTry? Str.toU64

            Ok (targetVal, numValues)

        _ -> Err Invalid

checkEquation : (U64, List U64) -> Bool
checkEquation = \(target, values) ->
    possibleResults values
    |> Set.contains target

possibleResults : List U64 -> Set U64
possibleResults = \values ->
    when values is
        [single] -> Set.single single
        [.. as rest, next] ->
            subValues = possibleResults rest
            plus = Set.map subValues \a -> a + next
            times = Set.map subValues \a -> a * next

            Set.union plus times

        [] -> crash "Unreachable"

checkEquationWithConcat : (U64, List U64) -> Bool
checkEquationWithConcat = \(target, values) ->
    possibleResultsWithConcat values
    |> Set.contains target

possibleResultsWithConcat : List U64 -> Set U64
possibleResultsWithConcat = \values ->
    when values is
        [single] -> Set.single single
        [.. as rest, next] ->
            subValues = possibleResultsWithConcat rest
            plus = Set.map subValues \a -> a + next
            times = Set.map subValues \a -> a * next
            concat = Set.map subValues \a -> concatenate a next

            Set.union plus times
            |> Set.union concat

        _ -> crash "Unreachable"

concatenate : U64, U64 -> U64
concatenate = \left, right ->
    Num.toStr left
    |> Str.concat (Num.toStr right)
    |> Str.toU64
    |> Result.withDefault 0

example : Str
example =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """

expect part1 example == Ok 3749

expect part2 example == Ok 11387
