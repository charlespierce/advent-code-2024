module [
    Grid,
    get,
    set,
    width,
    height,
    walkWithPoint,
]

import Point exposing [Point]

Grid a : List (List a)

get : Grid a, Point -> Result a [OutOfBounds]
get = \grid, point ->
    x = toU64? point.x
    y = toU64? point.y

    grid
        |> List.get? y
        |> List.get? x
        |> Ok

set : Grid a, Point, a -> Grid a
set = \grid, point, value ->
    when toU64 point.y is
        Err _ -> grid
        Ok y ->
            List.update grid y \row ->
                when toU64 point.x is
                    Err _ -> row
                    Ok x -> List.set row x value

height : Grid a -> U64
height = \grid -> List.len grid

width : Grid a -> U64
width = \grid ->
    List.get grid 0
    |> Result.map List.len
    |> Result.withDefault 0

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
