module [solution]

solution = { day: 20, part1, part2 }

import Grid exposing [Grid]
import Point exposing [Point]

part1 : Str -> _
part1 = \input ->
    maze = parse input
    route = mapRoute maze

    countCheats maze route
    |> Dict.walk 0 \acc, saved, count ->
        if saved >= 100 then
            acc + count
        else
            acc

part2 : Str -> _
part2 = \input ->
    route = parse input |> mapRoute

    countLongCheats route
    |> Dict.walk 0 \acc, saved, count ->
        if saved >= 100 then
            acc + count
        else
            acc

Tile : [Wall, Track]
Maze : { start : Point, end : Point, grid : Grid Tile }

mapRoute : Maze -> Dict Point U64
mapRoute = \maze ->
    mapRouteInner maze maze.start 0 (Dict.empty {})

mapRouteInner : Maze, Point, U64, Dict Point U64 -> Dict Point U64
mapRouteInner = \maze, current, distance, route ->
    newRoute = Dict.insert route current distance

    if current == maze.end then
        newRoute
    else
        maybeNext =
            Point.cardinalDirections
            |> List.keepOks \dir ->
                pos = Point.shift current dir
                when Grid.get maze.grid pos is
                    Err _ -> Err Blocked
                    Ok Wall -> Err Blocked
                    Ok Track if Dict.contains newRoute pos -> Err Visited
                    Ok Track -> Ok pos
            |> List.first

        when maybeNext is
            Ok next -> mapRouteInner maze next (distance + 1) newRoute
            Err _ -> crash "No path possible"

countCheats : Maze, Dict Point U64 -> Dict U64 U64
countCheats = \maze, route ->
    Grid.walkWithPoint maze.grid (Dict.empty {}) \counts, tile, point ->
        when tile is
            Track -> counts
            Wall -> checkCheats route point counts

checkCheats : Dict Point U64, Point, Dict U64 U64 -> Dict U64 U64
checkCheats = \route, current, counts ->
    up = Point.shift current Up
    down = Point.shift current Down
    left = Point.shift current Left
    right = Point.shift current Right

    when (Dict.get route up, Dict.get route down, Dict.get route left, Dict.get route right) is
        (Ok upCount, Ok downCount, _, _) ->
            saved = Num.absDiff upCount downCount |> Num.subSaturated 2

            Dict.update counts saved \existing ->
                when existing is
                    Ok c -> Ok (c + 1)
                    Err _ -> Ok 1

        (_, _, Ok leftCount, Ok rightCount) ->
            saved = Num.absDiff leftCount rightCount |> Num.subSaturated 2

            Dict.update counts saved \existing ->
                when existing is
                    Ok c -> Ok (c + 1)
                    Err _ -> Ok 1

        _ -> counts

countLongCheats : Dict Point U64 -> Dict U64 U64
countLongCheats = \route ->
    points = Dict.toList route

    List.walkWithIndex points (Dict.empty {}) \counts, (first, firstLen), index ->
        List.dropFirst points (index + 1)
        |> List.walk counts \acc, (second, secondLen) ->
            dist = manhattanDistance first second
            saved =
                Num.absDiff firstLen secondLen
                |> Num.subSaturated dist

            if saved > 0 && dist <= 20 then
                Dict.update acc saved \existing ->
                    when existing is
                        Ok c -> Ok (c + 1)
                        Err _ -> Ok 1
            else
                acc

manhattanDistance : Point, Point -> U64
manhattanDistance = \a, b ->
    xDiff = Num.absDiff a.x b.x
    yDiff = Num.absDiff a.y b.y

    (Num.toU64 xDiff) + (Num.toU64 yDiff)

parse : Str -> Maze
parse = \input ->
    start = { x: 0, y: 0 }
    end = { x: 0, y: 0 }

    Str.trim input
    |> Str.splitOn "\n"
    |> List.walkWithIndex { start, end, grid: [] } parseLine

parseLine : Maze, Str, U64 -> Maze
parseLine = \maze, line, y ->
    base = { start: maze.start, end: maze.end, list: [] }
    single =
        Str.toUtf8 line
        |> List.walkWithIndex base (parseElement y)

    {
        start: single.start,
        end: single.end,
        grid: List.append maze.grid single.list,
    }

PartialMaze : { start : Point, end : Point, list : List Tile }

parseElement : U64 -> (PartialMaze, U8, U64 -> PartialMaze)
parseElement = \y ->
    \partial, char, x ->
        when char is
            '#' -> { partial & list: List.append partial.list Wall }
            '.' -> { partial & list: List.append partial.list Track }
            'S' ->
                start = { x: Num.toI64 x, y: Num.toI64 y }
                { partial & start, list: List.append partial.list Track }

            'E' ->
                end = { x: Num.toI64 x, y: Num.toI64 y }
                { partial & end, list: List.append partial.list Track }

            _ -> crash "Invalid maze"
