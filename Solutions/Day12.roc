module [solution]

solution = { day: 12, part1, part2 }

import Grid
import Point

part1 : Str -> _
part1 = \input ->
    parse input
    |> findRegions
    |> List.map \r -> r.area * r.perimeter
    |> List.sum

part2 : Str -> _
part2 = \input ->
    parse input
    |> findRegions
    |> List.map \r -> r.area * r.sides
    |> List.sum

Region : { id : U8, area : U64, perimeter : U64, sides : U64 }

parse : Str -> Grid.Grid U8
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map Str.toUtf8

emptyRegion : U8 -> Region
emptyRegion = \id -> { id, area: 0, perimeter: 0, sides: 0 }

findRegions : Grid.Grid U8 -> List Region
findRegions = \grid ->
    Grid.walkWithPoint grid ([], Set.empty {}) \(regions, visited), id, point ->
        if Set.contains visited point then
            (regions, visited)
        else
            (newRegion, newVisited) = exploreRegion id point grid
            (
                List.append regions newRegion,
                Set.union visited newVisited,
            )
    |> .0

exploreRegion : U8, Point.Point, Grid.Grid U8 -> (Region, Set Point.Point)
exploreRegion = \id, start, grid ->
    exploreRegionInner start (emptyRegion id) (Set.empty {}) grid

exploreRegionInner : Point.Point, Region, Set Point.Point, Grid.Grid U8 -> (Region, Set Point.Point)
exploreRegionInner = \start, region, visited, grid ->
    newVisited = Set.insert visited start
    newCorners = countCorners start region.id grid
    newRegion = { region & area: region.area + 1, sides: region.sides + newCorners }

    Point.cardinalDirections
    |> List.walk (newRegion, newVisited) \(walkRegion, walkVisited), dir ->
        neighbor = Point.shift start dir

        when Grid.get grid neighbor is
            Ok id if id == walkRegion.id ->
                if Set.contains walkVisited neighbor then
                    (walkRegion, walkVisited)
                else
                    exploreRegionInner neighbor walkRegion walkVisited grid

            _ ->
                (
                    { walkRegion & perimeter: walkRegion.perimeter + 1 },
                    walkVisited,
                )

countCorners : Point.Point, U8, Grid.Grid U8 -> U64
countCorners = \point, id, grid ->
    inRegion =
        Point.allDirections
        |> List.keepIf \dir ->
            Ok id == Grid.get grid (Point.shift point dir)
        |> Set.fromList

    [
        (Up, Left, UpLeft),
        (Down, Left, DownLeft),
        (Down, Right, DownRight),
        (Up, Right, UpRight),
    ]
    |> List.keepIf \(dir1, dir2, diagonal) -> isCorner inRegion dir1 dir2 diagonal
    |> List.len

isCorner : Set Point.Direction, Point.Direction, Point.Direction, Point.Direction -> Bool
isCorner = \inRegion, dir1, dir2, diagonal ->
    (
        Set.contains inRegion dir1
        && Set.contains inRegion dir2
        && !(Set.contains inRegion diagonal)
    )
    ||
    (
        !(Set.contains inRegion dir1)
        && !(Set.contains inRegion dir2)
    )

example : Str
example =
    """
    RRRRIICCFF
    RRRRIICCCF
    VVRRRCCFFF
    VVRCCCJFFF
    VVVVCJJCFE
    VVIVCCJJEE
    VVIIICJJEE
    MIIIIIJJEE
    MIIISIJEEE
    MMMISSJEEE
    """

expect part1 example == 1930

expect part2 example == 1206
