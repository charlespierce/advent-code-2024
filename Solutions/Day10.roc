module [solution]

solution = { day: 10, part1, part2 }

import Grid
import Point

part1 : Str -> _
part1 = \input ->
    parse input
    |> findTrailScores

part2 : Str -> _
part2 = \input ->
    parse input
    |> findTrailRatings

Map : Grid.Grid U8

parse : Str -> Map
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map parseLine

parseLine : Str -> List U8
parseLine = \line ->
    Str.toUtf8 line
    |> List.map \digit -> (digit - '0')

findTrailRatings : Map -> U64
findTrailRatings = \map ->
    Grid.walkWithPoint map 0 \count, height, point ->
        if height == 0 then
            findTrailCounts map point height
            |> Num.add count
        else
            count

findTrailCounts : Map, Point.Point, U8 -> U64
findTrailCounts = \map, point, height ->
    if height == 9 then
        1
    else
        nextHeight = height + 1

        Point.cardinalDirections
            |> List.map \dir -> Point.shift point dir
            |> List.keepOks \neighbor ->
                neighborHeight = Grid.get? map neighbor

                if neighborHeight == nextHeight then
                    findTrailCounts map neighbor nextHeight
                    |> Ok
                else
                    Err NotAPath
            |> List.sum

findTrailScores : Map -> U64
findTrailScores = \map ->
    Grid.walkWithPoint map 0 \count, height, point ->
        if height == 0 then
            findTrailEnds map point height
            |> Set.len
            |> Num.add count
        else
            count

findTrailEnds : Map, Point.Point, U8 -> Set Point.Point
findTrailEnds = \map, point, height ->
    if height == 9 then
        Set.single point
    else
        nextHeight = height + 1

        Point.cardinalDirections
            |> List.map \dir -> Point.shift point dir
            |> List.keepOks \neighbor ->
                neighborHeight = Grid.get? map neighbor

                if neighborHeight == nextHeight then
                    findTrailEnds map neighbor nextHeight
                    |> Ok
                else
                    Err NotAPath
            |> List.walk (Set.empty {}) \a, b -> Set.union a b

example : Str
example =
    """
    89010123
    78121874
    87430965
    96549874
    45678903
    32019012
    01329801
    10456732
    """

expect part1 example == 36

expect part2 example == 81
