module [
    Direction,
    Point,
    allDirections,
    shift,
    diff,
    negate,
]

Direction : [UpLeft, Up, UpRight, Left, Right, DownLeft, Down, DownRight]

allDirections : List Direction
allDirections = [UpLeft, Up, UpRight, Left, Right, DownLeft, Down, DownRight]

Point : { x : I64, y : I64 }

shift : Point, Direction -> Point
shift = \{ x, y }, dir ->
    when dir is
        UpLeft -> { x: x - 1, y: y - 1 }
        Up -> { x, y: y - 1 }
        UpRight -> { x: x + 1, y: y - 1 }
        Left -> { x: x - 1, y }
        Right -> { x: x + 1, y }
        DownLeft -> { x: x - 1, y: y + 1 }
        Down -> { x, y: y + 1 }
        DownRight -> { x: x + 1, y: y + 1 }

diff : Point, Point -> Point
diff = \point1, point2 -> {
    x: point1.x - point2.x,
    y: point1.y - point2.y,
}

negate : Point -> Point
negate = \{ x, y } ->
    { x: -x, y: -y }
