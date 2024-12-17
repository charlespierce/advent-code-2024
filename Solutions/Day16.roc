module [solution]

solution = { day: 16, part1, part2 }

import Dijkstra
import Grid
import Point

part1 : Str -> _
part1 = \input ->
    grid = parse input
    startReindeer = start grid

    Dijkstra.solve
        startReindeer
        (neighbors grid)
        (done grid)
    |> Result.map .1

part2 : Str -> _
part2 = \input ->
    grid = parse input
    startReindeer = start grid

    (end, _, dict) =
        Dijkstra.solve?
            startReindeer
            (neighbors grid)
            (done grid)

    collectVisited dict end
    |> Set.len
    |> Ok


collectVisited : Dict Reindeer (List Reindeer, _), Reindeer -> Set Point.Point
collectVisited = \dict, current ->
    single = Set.single current.position

    when Dict.get dict current is
        Err _ -> single
        Ok (list, _) ->
            List.walk list single \set, prev ->
                Set.union set (collectVisited dict prev)

Reindeer : { position : Point.Point, facing : Point.Direction }

# By inspection, in the input and all examples, the start is in the bottom-left corner and the
# end is in the top-right corner
start : Grid.Grid Tile -> Reindeer
start = \grid ->
    startY =
        Grid.height grid
        |> Num.sub 2
        |> Num.toI64

    {
        position: { x: 1, y: startY },
        facing: Right,
    }

endPoint : Grid.Grid Tile -> Point.Point
endPoint = \grid ->
    endX =
        Grid.width grid
        |> Num.sub 2
        |> Num.toI64

    { x: endX, y: 1 }

done : Grid.Grid Tile -> (Reindeer -> Bool)
done = \grid ->
    end = endPoint grid

    \current -> current.position == end

neighbors : Grid.Grid Tile -> (Reindeer -> List (Reindeer, U64))
neighbors = \grid ->
    \current ->
        []
        |> List.appendIfOk (maybeForward grid current)
        |> List.appendIfOk (maybeTurnLeft grid current)
        |> List.appendIfOk (maybeTurnRight grid current)

maybeForward : Grid.Grid Tile, Reindeer -> Result (Reindeer, U64) _
maybeForward = \grid, current ->
    next = Point.shift current.position current.facing

    nextTile = Grid.get? grid next

    when nextTile is
        Open -> Ok ({ current & position: next }, 1)
        Wall -> Err Blocked

maybeTurnLeft : Grid.Grid Tile, Reindeer -> Result (Reindeer, U64) _
maybeTurnLeft = \grid, current ->
    nextFacing = turnLeft current.facing

    maybeForward grid { current & facing: nextFacing }
    |> Result.map \(next, cost) -> (next, cost + 1000)

maybeTurnRight : Grid.Grid Tile, Reindeer -> Result (Reindeer, U64) _
maybeTurnRight = \grid, current ->
    nextFacing = turnRight current.facing

    maybeForward grid { current & facing: nextFacing }
    |> Result.map \(next, cost) -> (next, cost + 1000)

turnLeft : Point.Direction -> Point.Direction
turnLeft = \dir ->
    when dir is
        Up -> Left
        Right -> Up
        Down -> Right
        Left -> Down
        _ -> crash "Invalid direction"

turnRight : Point.Direction -> Point.Direction
turnRight = \dir ->
    when dir is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
        _ -> crash "Invalid direction"

Tile : [Open, Wall]

parse : Str -> Grid.Grid Tile
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map parseLine

parseLine : Str -> List Tile
parseLine = \line ->
    Str.toUtf8 line
    |> List.map \char ->
        when char is
            '.' -> Open
            '#' -> Wall
            'S' -> Open
            'E' -> Open
            _ -> crash "Invalid tile"

example1 =
    """
    ###############
    #.......#....E#
    #.#.###.#.###.#
    #.....#.#...#.#
    #.###.#####.#.#
    #.#.#.......#.#
    #.#.#####.###.#
    #...........#.#
    ###.#.#####.#.#
    #...#.....#.#.#
    #.#.#.###.#.#.#
    #.....#...#.#.#
    #.###.#.#.#.#.#
    #S..#.....#...#
    ###############
    """

expect part1 example1 == Ok 7036

expect part2 example1 == Ok 45

example2 =
    """
    #################
    #...#...#...#..E#
    #.#.#.#.#.#.#.#.#
    #.#.#.#...#...#.#
    #.#.#.#.###.#.#.#
    #...#.#.#.....#.#
    #.#.#.#.#.#####.#
    #.#...#.#.#.....#
    #.#.#####.#.###.#
    #.#.#.......#...#
    #.#.###.#####.###
    #.#.#...#.....#.#
    #.#.#.#####.###.#
    #.#.#.........#.#
    #.#.#.#########.#
    #S#.............#
    #################
    """

expect part1 example2 == Ok 11048

expect part2 example2 == Ok 64