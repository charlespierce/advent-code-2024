module [solution]

solution = { day: 15, part1, part2 }

import Grid
import Point

part1 : Str -> _
part1 = \input ->
    (wh, moves) = parse input

    List.walk moves wh applyMove
    |> allBoxesGps

part2 : Str -> _
part2 = \input ->
    (wh, moves) = parseWide input

    List.walk moves wh applyMoveWide
    |> allBoxesGps

Tile : [Empty, Box, BoxRight, Wall]

Warehouse : { robot : Point.Point, layout : Grid.Grid Tile }

applyMove : Warehouse, Point.Direction -> Warehouse
applyMove = \wh, dir ->
    when applyMoveInner wh.robot wh.layout dir is
        Ok layout ->
            {
                robot: Point.shift wh.robot dir,
                layout,
            }

        Err _ -> wh

applyMoveWide : Warehouse, Point.Direction -> Warehouse
applyMoveWide = \wh, dir ->
    when dir is
        Left | Right -> applyMove wh dir
        Up | Down ->
            when applyMoveWideInner wh.robot wh.layout dir is
                Ok layout ->
                    {
                        robot: Point.shift wh.robot dir,
                        layout,
                    }

                Err _ -> wh

        _ -> crash "Invalid direction"

applyMoveInner : Point.Point, Grid.Grid Tile, Point.Direction -> Result (Grid.Grid Tile) _
applyMoveInner = \start, layout, dir ->
    current = Grid.get? layout start
    nextPos = Point.shift start dir

    when Grid.get? layout nextPos is
        Empty ->
            Grid.set layout nextPos current
            |> Grid.set start Empty
            |> Ok

        Wall -> Err CantMove
        Box | BoxRight ->
            newLayout = applyMoveInner? nextPos layout dir

            Grid.set newLayout nextPos current
            |> Grid.set start Empty
            |> Ok

applyMoveWideInner : Point.Point, Grid.Grid Tile, Point.Direction -> Result (Grid.Grid Tile) _
applyMoveWideInner = \start, layout, dir ->
    current = Grid.get? layout start
    nextPos = Point.shift start dir

    when Grid.get? layout nextPos is
        Empty ->
            Grid.set layout nextPos current
            |> Grid.set start Empty
            |> Ok

        Wall -> Err CantMove
        Box ->
            newLayout = applyMoveWideInner? nextPos layout dir
            otherBox = Point.shift nextPos Right
            newLayout2 = applyMoveWideInner? otherBox newLayout dir

            Grid.set newLayout2 nextPos current
            |> Grid.set start Empty
            |> Ok

        BoxRight ->
            newLayout = applyMoveWideInner? nextPos layout dir
            otherBox = Point.shift nextPos Left
            newLayout2 = applyMoveWideInner? otherBox newLayout dir

            Grid.set newLayout2 nextPos current
            |> Grid.set start Empty
            |> Ok

allBoxesGps : Warehouse -> I64
allBoxesGps = \wh ->
    Grid.walkWithPoint wh.layout 0 \sum, tile, point ->
        when tile is
            Empty -> sum
            Wall -> sum
            Box -> sum + (gps point)
            BoxRight -> sum

gps : Point.Point -> I64
gps = \point ->
    (100 * point.y) + point.x

parse : Str -> (Warehouse, List Point.Direction)
parse = \input ->
    when Str.splitOn input "\n\n" is
        [first, second] ->
            (
                parseWarehouse first,
                parseMoves second,
            )

        _ -> crash "Invalid input"

parseWide : Str -> (Warehouse, List Point.Direction)
parseWide = \input ->
    when Str.splitOn input "\n\n" is
        [first, second] ->
            (
                parseWarehouseWide first,
                parseMoves second,
            )

        _ -> crash "Invalid input"

parseWarehouse : Str -> Warehouse
parseWarehouse = \input ->
    start = {
        robot: { x: 0, y: 0 },
        layout: [],
    }

    Str.splitOn input "\n"
    |> List.walkWithIndex start parseWarehouseLine

parseWarehouseLine : Warehouse, Str, U64 -> Warehouse
parseWarehouseLine = \wh, line, y ->
    start = {
        robot: wh.robot,
        tiles: [],
    }

    { robot, tiles } =
        Str.toUtf8 line
        |> List.walkWithIndex start \state, char, x ->
            when char is
                '#' -> { state & tiles: List.append state.tiles Wall }
                '.' -> { state & tiles: List.append state.tiles Empty }
                'O' -> { state & tiles: List.append state.tiles Box }
                '@' -> { robot: { x: Num.toI64 x, y: Num.toI64 y }, tiles: List.append state.tiles Empty }
                _ -> crash "Invalid char"

    { robot, layout: List.append wh.layout tiles }

parseWarehouseWide : Str -> Warehouse
parseWarehouseWide = \input ->
    start = {
        robot: { x: 0, y: 0 },
        layout: [],
    }

    Str.splitOn input "\n"
    |> List.walkWithIndex start parseWarehouseLineWide

parseWarehouseLineWide : Warehouse, Str, U64 -> Warehouse
parseWarehouseLineWide = \wh, line, y ->
    start = {
        robot: wh.robot,
        tiles: [],
    }

    { robot, tiles } =
        Str.toUtf8 line
        |> List.walkWithIndex start \state, char, x ->
            when char is
                '#' -> { state & tiles: List.concat state.tiles [Wall, Wall] }
                '.' -> { state & tiles: List.concat state.tiles [Empty, Empty] }
                'O' -> { state & tiles: List.concat state.tiles [Box, BoxRight] }
                '@' -> { robot: { x: Num.toI64 (x * 2), y: Num.toI64 y }, tiles: List.concat state.tiles [Empty, Empty] }
                _ -> crash "Invalid char"

    { robot, layout: List.append wh.layout tiles }

parseMoves : Str -> List Point.Direction
parseMoves = \input ->
    Str.toUtf8 input
    |> List.keepOks \char ->
        when char is
            '^' -> Ok Up
            '<' -> Ok Left
            '>' -> Ok Right
            'v' -> Ok Down
            _ -> Err NotValid

example : Str
example =
    """
    ##########
    #..O..O.O#
    #......O.#
    #.OO..O.O#
    #..O@..O.#
    #O#..O...#
    #O..O..O.#
    #.OO.O.OO#
    #....O...#
    ##########

    <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
    vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
    ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
    <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
    ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
    ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
    >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
    <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
    ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
    v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
    """

expect part1 example == 10092

expect part2 example == 9021
