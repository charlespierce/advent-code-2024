module [solution]

solution = { day: 25, part1, part2 }

part1 : Str -> _
part1 = \input ->
    (locks, keys) = parse input

    countPairs locks keys

part2 : Str -> _
part2 = \_input ->
    "Merry Christmas!"

countPairs : Set (List U8), Set (List U8) -> U64
countPairs = \locks, keys ->
    Set.walk locks 0 \count, lock ->
        Set.walk keys count \acc, key ->
            if fit lock key then
                acc + 1
            else
                acc

fit : List U8, List U8 -> Bool
fit = \lock, key ->
    List.map2 lock key \a, b -> (a, b)
    |> List.walkUntil Bool.true \_, (lockHeight, keyHeight) ->
        if lockHeight + keyHeight > 5 then
            Break Bool.false
        else
            Continue Bool.true

Item : [Lock (List U8), Key (List U8)]

parse : Str -> (Set (List U8), Set (List U8))
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n\n"
    |> List.map parseItem
    |> List.walk (Set.empty {}, Set.empty {}) \(locks, keys), item ->
        when item is
            Lock a -> (Set.insert locks a, keys)
            Key a -> (locks, Set.insert keys a)

parseItem : Str -> Item
parseItem = \input ->
    lines = Str.splitOn input "\n"
    heights =
        List.sublist lines { start: 1, len: 5 }
        |> parseColumnHeights

    if List.get lines 0 == Ok "#####" then
        Lock heights
    else
        Key heights

parseColumnHeights : List Str -> List U8
parseColumnHeights = \lines ->
    lines
    |> List.map Str.toUtf8
    |> List.map \points ->
        List.map points \point ->
            when point is
                '.' -> 0
                '#' -> 1
                _ -> crash "Invalid input"
    |> sumColumns

sumColumns : List (List U8) -> List U8
sumColumns = \cols ->
    List.walk cols [0, 0, 0, 0, 0] \sums, line ->
        List.map2 sums line Num.add

example : Str
example =
    """
    #####
    .####
    .####
    .####
    .#.#.
    .#...
    .....

    #####
    ##.##
    .#.##
    ...##
    ...#.
    ...#.
    .....

    .....
    #....
    #....
    #...#
    #.#.#
    #.###
    #####

    .....
    .....
    #.#..
    ###..
    ###.#
    ###.#
    #####

    .....
    .....
    .....
    #....
    #.#..
    #.#.#
    #####
    """

expect part1 example == 3
