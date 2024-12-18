module [solution]

solution = { day: 18, part1, part2 }

import Dijkstra
import Grid exposing [Grid]
import Parser exposing [Parser]
import Point exposing [Point, Direction]

part1 : Str -> _
part1 = part1Generator 71 1024

part2 : Str -> _
part2 = part2Generator 71

part1Generator : U64, U64 -> (Str -> Result U64 _)
part1Generator = \extent, corruption ->
    \input ->
        grid =
            Str.trim input
                |> Parser.parseStr? parseBytes
                |> List.takeFirst corruption
                |> corruptBytes (initialize extent)

        Dijkstra.solve
            start
            (neighbors grid)
            (done grid)
        |> Result.map .1

part2Generator : U64 -> (Str -> Result Point _)
part2Generator = \extent ->
    \input ->
        bytes =
            Str.trim input
                |> Parser.parseStr? parseBytes

        checker = \count ->
            grid =
                List.takeFirst bytes count
                |> corruptBytes (initialize extent)

            Dijkstra.solve
                start
                (neighbors grid)
                (done grid)
            |> Result.isErr

        binarySearch 0 (List.len bytes) checker
        |> Result.try \count ->
            List.get bytes (count - 1)

Tile : [Safe, Corrupted]

initialize : U64 -> Grid Tile
initialize = \extent ->
    List.repeat Safe extent
    |> List.repeat extent

start : Point
start = { x: 0, y: 0 }

neighbors : Grid Tile -> (Point -> List (Point, U64))
neighbors = \grid ->
    \point ->
        Point.cardinalDirections
        |> List.keepOks \dir -> checkNeighbor grid point dir
        |> List.map \n -> (n, 1)

done : Grid Tile -> (Point -> Bool)
done = \grid ->
    extent =
        Grid.height grid
        |> Num.toI64
        |> Num.sub 1

    end = { x: extent, y: extent }

    \point -> point == end

checkNeighbor : Grid Tile, Point, Direction -> Result Point _
checkNeighbor = \grid, point, dir ->
    neighborPoint = Point.shift point dir
    tile = Grid.get? grid neighborPoint

    when tile is
        Safe -> Ok neighborPoint
        Corrupted -> Err Invalid

corruptBytes : List Point, Grid Tile -> Grid Tile
corruptBytes = \bytes, grid ->
    List.walk bytes grid \g, p ->
        Grid.set g p Corrupted

parseBytes : Parser (List Point)
parseBytes =
    Parser.separatedList
        parsePoint
        (Parser.tag "\n")

parsePoint : Parser Point
parsePoint =
    Parser.separatedPair
        Parser.signed
        (Parser.tag ",")
        Parser.signed
    |> Parser.map \(x, y) -> { x, y }

binarySearch : U64, U64, (U64 -> Bool) -> Result U64 _
binarySearch = \lower, upper, check ->
    if upper - lower == 1 then
        Ok upper
    else
        midPoint = (upper + lower) // 2

        if check midPoint then
            binarySearch lower midPoint check
        else
            binarySearch midPoint upper check

example : Str
example =
    """
    5,4
    4,2
    4,5
    3,0
    2,1
    6,3
    2,4
    1,5
    0,6
    3,3
    2,6
    5,1
    1,2
    5,5
    2,5
    6,5
    1,4
    0,4
    6,4
    1,1
    6,1
    1,0
    0,5
    1,6
    2,0
    """

expect (part1Generator 7 12) example == Ok 22

expect (part2Generator 7) example == Ok { x: 6, y: 1 }
