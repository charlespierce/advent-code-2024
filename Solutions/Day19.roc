module [solution]

solution = { day: 19, part1, part2 }

part1 : Str -> _
part1 = \input ->
    (available, needed) = parse input

    isPossible = isPossibleBuilder available

    List.walk needed ([], Dict.empty {}) \(acc, memo), design ->
        (found, updatedMemo) = isPossible design memo

        if found then
            (List.append acc design, updatedMemo)
        else
            (acc, updatedMemo)
    |> .0
    |> List.len

part2 : Str -> _
part2 = \input ->
    (available, needed) = parse input

    countPossible = countPossibilitiesBuilder available

    List.walk needed (0, Dict.empty {}) \(acc, memo), design ->
        (count, updatedMemo) = countPossible design memo

        (count + acc, updatedMemo)
    |> .0

Memo : Dict Str Bool

isPossibleBuilder : List Str -> (Str, Memo -> (Bool, Memo))
isPossibleBuilder = \available ->
    isPossibleInner : Str, Memo -> (Bool, Memo)
    isPossibleInner = \design, memo ->
        when Dict.get memo design is
            Ok value -> (value, memo)
            Err _ if Str.isEmpty design -> (Bool.true, memo)
            _ ->
                (matched, updatedMemo) =
                    List.walkUntil available (Bool.false, memo) \(_, m), pattern ->
                        when stripPrefix design pattern is
                            Err NoMatch -> Continue (Bool.false, m)
                            Ok remaining ->
                                (matchRecur, memoRecur) = isPossibleInner remaining m
                                if matchRecur then
                                    Break (Bool.true, memoRecur)
                                else
                                    Continue (Bool.false, memoRecur)

                (matched, Dict.insert updatedMemo design matched)

    isPossibleInner

CountMemo : Dict Str U64

countPossibilitiesBuilder : List Str -> (Str, CountMemo -> (U64, CountMemo))
countPossibilitiesBuilder = \available ->
    countPossibleInner : Str, CountMemo -> (U64, CountMemo)
    countPossibleInner = \design, memo ->
        when Dict.get memo design is
            Ok value -> (value, memo)
            Err _ if Str.isEmpty design -> (1, memo)
            _ ->
                (count, updatedMemo) =
                    List.walk available (0, memo) \(c, m), pattern ->
                        when stripPrefix design pattern is
                            Err NoMatch -> (c, m)
                            Ok remaining ->
                                (countRecur, memoRecur) = countPossibleInner remaining m
                                (countRecur + c, memoRecur)

                (count, Dict.insert updatedMemo design count)

    countPossibleInner

stripPrefix : Str, Str -> Result Str [NoMatch]
stripPrefix = \full, prefix ->
    stripped = Str.dropPrefix full prefix

    if stripped == full then
        Err NoMatch
    else
        Ok stripped

parse : Str -> (List Str, List Str)
parse = \input ->
    separated =
        Str.trim input
        |> Str.splitOn "\n\n"

    when separated is
        [available, designs] -> (parseAvailable available, parseDesigns designs)
        _ -> crash "Invalid input"

parseAvailable : Str -> List Str
parseAvailable = \line ->
    Str.splitOn line ", "

parseDesigns : Str -> List Str
parseDesigns = \lines ->
    Str.splitOn lines "\n"

example : Str
example =
    """
    r, wr, b, g, bwu, rb, gb, br

    brwrr
    bggr
    gbbr
    rrbgbr
    ubwu
    bwurrg
    brgr
    bbrgwb
    """

expect part1 example == 6

expect part2 example == 16
