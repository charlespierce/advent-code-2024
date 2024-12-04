module [solution]

solution = { day: 4, part1, part2 }

import Grid

part1 : Str -> _
part1 = \input ->
    firstChar = 'X'
    restChars = Str.toUtf8 "MAS"

    grid = parse input
    
    Grid.walkWithPoint grid 0 \count, char, point ->
        if char == firstChar then
            count + countMatchesAt grid point restChars
        else
            count

part2 : Str -> _
part2 = \input ->
    firstChar = 'A'
    expectedChars = ('M', 'S')

    grid = parse input

    Grid.walkWithPoint grid 0 \count, char, point ->
        if char == firstChar then
            if isXPattern grid point expectedChars then
                count + 1
            else
                count
        else
            count

parse : Str -> Grid.Grid U8
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map Str.toUtf8

isXPattern : Grid.Grid U8, Grid.Point, (U8, U8) -> Bool
isXPattern = \grid, point, expectedPair ->
    isXPatternInner grid point expectedPair
    |> Result.isOk

isXPatternInner : Grid.Grid U8, Grid.Point, (U8, U8) -> Result {} _
isXPatternInner = \grid, point, expectedPair ->
    upLeft = Grid.get? grid (Grid.shiftPoint point UpLeft)
    upRight = Grid.get? grid (Grid.shiftPoint point UpRight)
    downLeft = Grid.get? grid (Grid.shiftPoint point DownLeft)
    downRight = Grid.get? grid (Grid.shiftPoint point DownRight)

    firstPairMatches = pairMatches expectedPair (upLeft, downRight)
    secondPairMatches = pairMatches expectedPair (upRight, downLeft)

    if firstPairMatches && secondPairMatches then
        Ok {}
    else
        Err NoMatch

pairMatches : (U8, U8), (U8, U8) -> Bool
pairMatches = \a, b ->
    (a.0 == b.0 && a.1 == b.1) || (a.0 == b.1 && a.1 == b.0)

countMatchesAt : Grid.Grid U8, Grid.Point, List U8 -> U64
countMatchesAt = \grid, start, chars ->
    Grid.allDirections
    |> List.keepIf (\direction -> matchWord grid start direction chars)
    |> List.len

matchWord : Grid.Grid U8, Grid.Point, Grid.Direction, List U8 -> Bool
matchWord = \grid, start, direction, chars ->
    matchWordInner grid start direction chars
    |> Result.isOk

matchWordInner : Grid.Grid U8, Grid.Point, Grid.Direction, List U8 -> Result {} _
matchWordInner = \grid, start, direction, chars ->
    when chars is
        [] -> Ok {}
        [next, .. as rest] ->
            nextPoint = Grid.shiftPoint start direction
            value = Grid.get? grid nextPoint

            if value == next then
                matchWordInner grid nextPoint direction rest
            else
                Err NoMatch

example : Str
example =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

expect part1 example == 18

expect part2 example == 9