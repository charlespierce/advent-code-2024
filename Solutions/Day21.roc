module [solution]

solution = { day: 21, part1, part2 }

import Point exposing [Point]

part1 : Str -> _
part1 = \input ->
    parse input
    |> List.map \keys ->
        robotPathLength keys 2
        |> Num.mul (numericToNumber keys)
    |> List.sum

part2 : Str -> _
part2 = \input ->
    parse input
    |> List.map \keys ->
        robotPathLength keys 25
        |> Num.mul (numericToNumber keys)
    |> List.sum

parse : Str -> List (List NumericKey)
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map parseNumeric

parseNumeric : Str -> List NumericKey
parseNumeric = \line ->
    Str.toUtf8 line
    |> List.map \char ->
        when char is
            '0' -> Zero
            '1' -> One
            '2' -> Two
            '3' -> Three
            '4' -> Four
            '5' -> Five
            '6' -> Six
            '7' -> Seven
            '8' -> Eight
            '9' -> Nine
            'A' -> Activate
            _ -> crash "Invalid Key"

numericToNumber : List NumericKey -> U64
numericToNumber = \keys ->
    List.walkUntil keys 0 \num, key ->
        when key is
            Activate -> Break num
            Zero -> Continue (num * 10)
            One -> Continue (num * 10 + 1)
            Two -> Continue (num * 10 + 2)
            Three -> Continue (num * 10 + 3)
            Four -> Continue (num * 10 + 4)
            Five -> Continue (num * 10 + 5)
            Six -> Continue (num * 10 + 6)
            Seven -> Continue (num * 10 + 7)
            Eight -> Continue (num * 10 + 8)
            Nine -> Continue (num * 10 + 9)

robotPathLength : List NumericKey, U64 -> U64
robotPathLength = \keys, iterations ->
    numericToDirectional keys
    |> directionalRobots iterations

Cache : Dict (List DirectionalKey, U64) U64

directionalRobots : List DirectionalKey, U64 -> U64
directionalRobots = \keys, iterations ->
    directionalRobotsInner keys iterations (Dict.empty {})
    |> .0

directionalRobotsInner : List DirectionalKey, U64, Cache -> (U64, Cache)
directionalRobotsInner = \keys, depth, cache ->
    if depth == 0 then
        (List.len keys, cache)
    else
        when Dict.get cache (keys, depth) is
            Ok value -> (value, cache)
            Err KeyNotFound ->
                (total, newCache) =
                    directionalToDirectional keys
                    |> List.splitOn Activate
                    |> List.dropLast 1
                    |> List.walk (0, cache) \(totalLen, innerCache), subList ->
                        (subLen, updatedCache) =
                            List.append subList Activate
                            |> directionalRobotsInner (depth - 1) innerCache

                        (totalLen + subLen, updatedCache)

                (
                    total,
                    Dict.insert newCache (keys, depth) total,
                )

numericToDirectional : List NumericKey -> List DirectionalKey
numericToDirectional = \keys ->
    List.walk keys (Activate, []) \(prev, neededKeys), key ->
        nextPath = shortestNumericPath prev key

        updatedKeys =
            List.concat neededKeys nextPath
            |> List.append Activate

        (key, updatedKeys)
    |> .1

directionalToDirectional : List DirectionalKey -> List DirectionalKey
directionalToDirectional = \keys ->
    List.walk keys (Activate, []) \(prev, neededKeys), key ->
        updatedKeys =
            List.concat neededKeys (shortestDirectionalPath prev key)
            |> List.append Activate

        (key, updatedKeys)
    |> .1

NumericKey : [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Activate]

numericKeypadPosition : NumericKey -> Point
numericKeypadPosition = \digit ->
    when digit is
        Zero -> { x: 1, y: 3 }
        One -> { x: 0, y: 2 }
        Two -> { x: 1, y: 2 }
        Three -> { x: 2, y: 2 }
        Four -> { x: 0, y: 1 }
        Five -> { x: 1, y: 1 }
        Six -> { x: 2, y: 1 }
        Seven -> { x: 0, y: 0 }
        Eight -> { x: 1, y: 0 }
        Nine -> { x: 2, y: 0 }
        Activate -> { x: 2, y: 3 }

DirectionalKey : [Up, Down, Left, Right, Activate]

shortestNumericPath : NumericKey, NumericKey -> List DirectionalKey
shortestNumericPath = \start, end ->
    startPos = numericKeypadPosition start
    endPos = numericKeypadPosition end

    vert = verticalNumericPath startPos endPos
    hori = horizontalNumericPath startPos endPos

    when (cmp startPos.x endPos.x, cmp startPos.y endPos.y) is
        (EQ, EQ) -> []
        (EQ, LT) | (EQ, GT) -> vert
        (LT, EQ) | (GT, EQ) -> hori
        (LT, LT) ->
            if startPos.x == 0 && endPos.y == 3 then
                List.concat hori vert
            else
                List.concat vert hori
        (LT, GT) -> List.concat vert hori
        (GT, LT) -> List.concat hori vert
        (GT, GT) ->
            if startPos.y == 3 && endPos.x == 0 then
                List.concat vert hori
            else
                List.concat hori vert

horizontalNumericPath : Point, Point -> List DirectionalKey
horizontalNumericPath = \start, end ->
    if start.x < end.x then
        List.repeat Right (Num.toU64 (end.x - start.x))
    else
        List.repeat Left (Num.toU64 (start.x - end.x))

verticalNumericPath : Point, Point -> List DirectionalKey
verticalNumericPath = \start, end ->
    if start.y < end.y then
        List.repeat Down (Num.toU64 (end.y - start.y))
    else
        List.repeat Up (Num.toU64 (start.y - end.y))

shortestDirectionalPath : DirectionalKey, DirectionalKey -> List DirectionalKey
shortestDirectionalPath = \start, end ->
    when (start, end) is
        (Up, Up) -> []
        (Up, Down) -> [Down]
        (Up, Left) -> [Down, Left]
        (Up, Right) -> [Down, Right]
        (Up, Activate) -> [Right]
        (Down, Up) -> [Up]
        (Down, Down) -> []
        (Down, Left) -> [Left]
        (Down, Right) -> [Right]
        (Down, Activate) -> [Up, Right]
        (Left, Up) -> [Right, Up]
        (Left, Down) -> [Right]
        (Left, Left) -> []
        (Left, Right) -> [Right, Right]
        (Left, Activate) -> [Right, Right, Up]
        (Right, Up) -> [Left, Up]
        (Right, Down) -> [Left]
        (Right, Left) -> [Left, Left]
        (Right, Right) -> []
        (Right, Activate) -> [Up]
        (Activate, Up) -> [Left]
        (Activate, Down) -> [Left, Down]
        (Activate, Left) -> [Down, Left, Left]
        (Activate, Right) -> [Down]
        (Activate, Activate) -> []

cmp : I64, I64 -> [LT, EQ, GT]
cmp = \a, b ->
    if a < b then
        LT
    else if a > b then
        GT
    else
        EQ

example : Str
example =
    """
    029A
    980A
    179A
    456A
    379A
    """

expect part1 example == 126384
