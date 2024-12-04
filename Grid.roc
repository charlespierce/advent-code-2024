module [
    Direction,
    Point,
    Grid,
    allDirections,
    shiftPoint,
    get,
    walkWithPoint,
]

Direction : [UpLeft, Up, UpRight, Left, Right, DownLeft, Down, DownRight]

allDirections : List Direction
allDirections = [UpLeft, Up, UpRight, Left, Right, DownLeft, Down, DownRight]

Point : { x : I64, y : I64 }

shiftPoint : Point, Direction -> Point
shiftPoint = \{ x, y }, dir ->
    when dir is
        UpLeft -> { x: x - 1, y: y - 1 }
        Up -> { x, y: y - 1 }
        UpRight -> { x: x + 1, y: y - 1 }
        Left -> { x: x - 1, y }
        Right -> { x: x + 1, y }
        DownLeft -> { x: x - 1, y: y + 1 }
        Down -> { x, y: y + 1 }
        DownRight -> { x: x + 1, y: y + 1 }

Grid a : List (List a)

get : Grid a, Point -> Result a [OutOfBounds]
get = \grid, point ->
    x = toU64? point.x
    y = toU64? point.y

    grid
    |> List.get? y
    |> List.get? x
    |> Ok

walkWithPoint : Grid a, state, (state, a, Point -> state) -> state
walkWithPoint = \grid, initial, func ->
    List.walkWithIndex grid initial \rowState, row, y ->
        List.walkWithIndex row rowState \state, elem, x ->
            func state elem { x: Num.toI64 x, y: Num.toI64 y }

toU64 : I64 -> Result U64 [OutOfBounds]
toU64 = \input ->
    if input >= 0 then
        Ok (Num.toU64 input)
    else
        Err OutOfBounds