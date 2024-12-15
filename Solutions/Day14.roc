module [solution]

solution = { day: 14, part1, part2 }

import Parser exposing [Parser, parseStr, separatedPair, tag, signed, map, pair]
import Point

part1 : Str -> _
part1 = part1Generator 101 103

part1Generator : I64, I64 -> (Str -> _)
part1Generator = \width, height ->
    \input ->
        robots = parseRobots? input

        start = { width, height, robots }

        List.range { start: At 0, end: Before 100 }
        |> List.walk start \lobby, _ ->
            moveAllRobots lobby
        |> safetyFactor
        |> Ok

part2 : Str -> _
part2 = \input ->
    robots = parseRobots? input

    lobby = { width: 101, height: 103, robots }

    xVarStep = calcMinVariance lobby lobby.width \r -> r.position.x
    yVarStep = calcMinVariance lobby lobby.height \r -> r.position.y

    minStep = findMinStep xVarStep.1 yVarStep.1 lobby

    Ok minStep

calcMinVariance : Lobby, I64, (Robot -> I64) -> (F64, I64)
calcMinVariance = \lobby, count, mapper ->
    start = { lobby, min: Num.infinityF64, step: -1 }

    val =
        List.range { start: At 0, end: Before count }
        |> List.walk start (varianceWalker mapper)

    (val.min, val.step)

WalkState : { lobby : Lobby, min : F64, step : I64 }

varianceWalker : (Robot -> I64) -> (WalkState, I64 -> WalkState)
varianceWalker = \mapper ->
    \state, step ->
        var =
            List.map state.lobby.robots mapper
            |> variance

        lobby = moveAllRobots state.lobby

        if var < state.min then
            { lobby, min: var, step }
        else
            { state & lobby }

variance : List I64 -> F64
variance = \values ->
    count =
        List.len values
        |> Num.toF64
    mean =
        List.sum values
        |> Num.toF64
        |> Num.div count
    List.map values \val ->
        Num.toF64 val
        |> Num.sub mean
        |> Num.pow 2
    |> List.sum
    |> Num.div count

findMinStep : I64, I64, Lobby -> I64
findMinStep = \time, yTarget, lobby ->
    if time % lobby.height == yTarget then
        time
    else
        findMinStep
            (time + lobby.width)
            yTarget
            lobby

Robot : { position : Point.Point, velocity : Point.Point }

Lobby : {
    width : I64,
    height : I64,
    robots : List Robot,
}

Quadrant : [UpLeft, UpRight, DownLeft, DownRight]

moveAllRobots : Lobby -> Lobby
moveAllRobots = \lobby ->
    robots =
        lobby.robots
        |> List.map (moveRobot lobby)

    { lobby & robots }

moveRobot : Lobby -> (Robot -> Robot)
moveRobot = \lobby ->
    \robot ->
        newX =
            robot.position.x
            |> Num.add robot.velocity.x
            |> Num.add lobby.width
            |> Num.rem lobby.width

        newY =
            robot.position.y
            |> Num.add robot.velocity.y
            |> Num.add lobby.height
            |> Num.rem lobby.height

        { robot & position: { x: newX, y: newY } }

safetyFactor : Lobby -> U64
safetyFactor = \lobby ->
    countQuadrants lobby
    |> Dict.walk 1 \product, _, count ->
        product * count

countQuadrants : Lobby -> Dict Quadrant U64
countQuadrants = \lobby ->
    lobby.robots
    |> List.keepOks (determineQuadrant lobby)
    |> List.walk (Dict.empty {}) \dict, quadrant ->
        Dict.update dict quadrant \existing ->
            when existing is
                Ok count -> Ok (count + 1)
                Err Missing -> Ok 1

determineQuadrant : Lobby -> (Robot -> Result Quadrant _)
determineQuadrant = \lobby ->
    midX = lobby.width // 2
    midY = lobby.height // 2

    \robot ->
        x = Num.compare robot.position.x midX
        y = Num.compare robot.position.y midY

        when (x, y) is
            (LT, LT) -> Ok UpLeft
            (LT, GT) -> Ok DownLeft
            (GT, LT) -> Ok UpRight
            (GT, GT) -> Ok DownRight
            _ -> Err OnEdge

parseRobots : Str -> Result (List Robot) _
parseRobots = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        parseStr line parseRobot

parseRobot : Parser Robot
parseRobot =
    pair
        parsePosition
        parseVelocity
    |> map \(position, velocity) -> { position, velocity }

parsePosition : Parser Point.Point
parsePosition =
    pair
        (tag "p=")
        parsePoint
    |> map .1

parseVelocity : Parser Point.Point
parseVelocity =
    pair
        (tag " v=")
        parsePoint
    |> map .1

parsePoint : Parser Point.Point
parsePoint =
    separatedPair
        signed
        (tag ",")
        signed
    |> map \(x, y) -> { x, y }

example : Str
example =
    """
    p=0,4 v=3,-3
    p=6,3 v=-1,-3
    p=10,3 v=-1,2
    p=2,0 v=2,-1
    p=0,0 v=1,3
    p=3,0 v=-2,-2
    p=7,6 v=-1,-3
    p=3,0 v=-1,-2
    p=9,3 v=2,3
    p=7,3 v=-1,2
    p=2,4 v=2,-3
    p=9,5 v=-3,-3
    """

expect (part1Generator 11 7) example == Ok 12
