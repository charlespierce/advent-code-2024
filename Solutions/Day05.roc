module [solution]

solution = { day: 5, part1, part2 }

part1 : Str -> _
part1 = \input ->
    (reqs, manuals) = parse input

    List.keepIf manuals \manual -> validateManual manual reqs
    |> List.map getMiddlePage
    |> List.sum

part2 : Str -> _
part2 = \input ->
    (reqs, manuals) = parse input

    List.dropIf manuals \manual -> validateManual manual reqs
    |> List.map \manual -> orderManual manual reqs
    |> List.map getMiddlePage
    |> List.sum

getMiddlePage : Manual -> U64
getMiddlePage = \manual ->
    middleIndex = (List.len manual) // 2
    when List.get manual middleIndex is
        Ok value -> value
        Err OutOfBounds -> crash "Passed a list with no elements"

validateManual : Manual, Requirements -> Bool
validateManual = \manual, reqs ->
    List.mapWithIndex manual \page, index ->
        prevPages = List.takeFirst manual index
        validatePage page prevPages reqs
    |> List.all \b -> b

validatePage : U64, List U64, Requirements -> Bool
validatePage = \page, prevPages, reqs ->
    when Dict.get reqs page is
        Ok mustComeAfter ->
            List.all prevPages \prevPage ->
                !(List.contains mustComeAfter prevPage)

        Err KeyNotFound -> Bool.true

orderManual : Manual, Requirements -> Manual
orderManual = \manual, reqs ->
    List.walkWithIndexUntil manual [] \_, page, index ->
        otherPages = List.dropAt manual index
        if validatePage page otherPages reqs then
            Break
                (
                    orderManual otherPages reqs
                    |> List.append page
                )
        else
            Continue []

Requirements : Dict U64 (List U64)

Manual : List U64

parse : Str -> (Requirements, List Manual)
parse = \input ->
    when Str.splitOn input "\n\n" is
        [reqs, manuals] -> (parseRequirements reqs, parseManuals manuals)
        _ -> crash "Invalid input"

parseRequirements : Str -> Requirements
parseRequirements = \input ->
    Str.splitOn input "\n"
    |> List.walk (Dict.empty {}) parseSingleRequirement

parseSingleRequirement : Requirements, Str -> Requirements
parseSingleRequirement = \dict, entry ->
    values = Str.splitOn entry "|" |> List.mapTry Str.toU64

    when values is
        Ok [prev, later] ->
            Dict.update dict prev \exists ->
                when exists is
                    Ok list -> Ok (List.append list later)
                    _ -> Ok [later]

        _ -> dict

parseManuals : Str -> List Manual
parseManuals = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map parseSingleManual

parseSingleManual : Str -> Manual
parseSingleManual = \input ->
    Str.splitOn input ","
    |> List.mapTry Str.toU64
    |> Result.withDefault []

example : Str
example =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """

expect part1 example == 143

expect part2 example == 123
