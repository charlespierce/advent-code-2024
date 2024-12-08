module [solution]

solution = { day: 6, part1, part2 }

import Grid

part1 : Str -> _
part1 = \input ->
    (grid, start) = parse input
    visited =
        @Guard { position: start, direction: Up }
        |> determinePath grid (Set.empty {})
        |> Result.withDefault (Set.empty {})
        |> Set.map position

    Set.len visited

part2 : Str -> _
part2 = \input ->
    (grid, start) = parse input
    startGuard = @Guard { position: start, direction: Up }
    visited =
        determinePath startGuard grid (Set.empty {})
        |> Result.withDefault (Set.empty {})
        |> Set.map position

    Set.keepIf visited \obstruction ->
        checkLoopWithNewObstruction obstruction startGuard grid
    |> Set.len

Tile : [Open, Blocked]

parse : Str -> (Grid.Grid Tile, Grid.Point)
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.walkWithIndex ([], { x: 0, y: 0 }) parseLine

parseLine : (Grid.Grid Tile, Grid.Point), Str, U64 -> (Grid.Grid Tile, Grid.Point)
parseLine = \(grid, start), line, y ->
    (gridLine, newStart) =
        Str.toUtf8 line
        |> List.walkWithIndex ([], start) \(gl, s), char, x ->
            when char is
                '.' -> (List.append gl Open, s)
                '^' -> (List.append gl Open, { x: Num.toI64 x, y: Num.toI64 y })
                '#' -> (List.append gl Blocked, s)
                _ -> crash "Unreachable"
    (List.append grid gridLine, newStart)

checkLoopWithNewObstruction : Grid.Point, Guard, Grid.Grid Tile -> Bool
checkLoopWithNewObstruction = \obstruction, guard, grid ->
    newGrid = Grid.set grid obstruction Blocked

    when determinePath guard newGrid (Set.empty {}) is
        Ok _ -> Bool.false
        Err Loop -> Bool.true

determinePath : Guard, Grid.Grid Tile, Set Guard -> Result (Set Guard) [Loop]
determinePath = \guard, grid, visited ->
    if Set.contains visited guard then
        Err Loop
    else
        newVisited = Set.insert visited guard
        when moveGuard guard grid is
            Ok newGuard -> determinePath newGuard grid newVisited
            Err OutOfBounds -> Ok newVisited

moveGuard : Guard, Grid.Grid Tile -> Result Guard [OutOfBounds]
moveGuard = \guard, grid ->
    newPosition = Grid.shiftPoint (position guard) (direction guard)
    when Grid.get? grid newPosition is
        Open -> Ok (withPosition guard newPosition)
        Blocked -> moveGuard (withDirection guard (turnRight (direction guard))) grid

turnRight : Grid.Direction -> Grid.Direction
turnRight = \dir ->
    when dir is
        Up -> Right
        Right -> Down
        Down -> Left
        Left -> Up
        _ -> crash "Unreachable"

Guard := { position : Grid.Point, direction : Grid.Direction } implements [Eq, Hash]

position : Guard -> Grid.Point
position = \@Guard guard -> guard.position

direction : Guard -> Grid.Direction
direction = \@Guard guard -> guard.direction

withDirection : Guard, Grid.Direction -> Guard
withDirection = \@Guard guard, newDirection ->
    @Guard { guard & direction: newDirection }

withPosition : Guard, Grid.Point -> Guard
withPosition = \@Guard guard, newPosition ->
    @Guard { guard & position: newPosition }

example : Str
example =
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

expect part1 example == 41

expect part2 example == 6
