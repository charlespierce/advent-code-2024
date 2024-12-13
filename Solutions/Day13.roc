module [solution]

solution = { day: 13, part1, part2 }

import Parser exposing [Parser, parseStr, pair, delimited, separatedPair, map, tag, number, any, triple]
import Point

part1 : Str -> _
part1 = \input ->
    parse? input
        |> List.keepOks solve
        |> List.map tokenCost
        |> List.sum
        |> Ok

part2 : Str -> _
part2 = \input ->
    parse? input
        |> List.map shiftTarget
        |> List.keepOks solve
        |> List.map tokenCost
        |> List.sum
        |> Ok

Game : {
    a : Point.Point,
    b : Point.Point,
    target : Point.Point,
}

Solution : {
    a : I64,
    b : I64,
}

parse : Str -> Result (List Game) _
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n\n"
    |> List.mapTry \gameStr ->
        parseStr gameStr parseGame

parseGame : Parser Game
parseGame =
    triple
        parseButton
        parseButton
        parseTarget
    |> map \(a, b, target) -> { a, b, target }

parseButton : Parser Point.Point
parseButton =
    delimited
        parseButtonOpening
        parseButtonPoint
        (tag "\n")

parseButtonOpening : Parser _
parseButtonOpening =
    pair
        (tag "Button ")
        any
    |> pair (tag ": X+")

parseButtonPoint : Parser Point.Point
parseButtonPoint =
    separatedPair
        number
        (tag ", Y+")
        number
    |> map \(x, y) -> { x: Num.toI64 x, y: Num.toI64 y }

parseTarget : Parser Point.Point
parseTarget =
    pair
        parseTargetOpening
        parseTargetPoint
    |> map .1

parseTargetOpening : Parser _
parseTargetOpening = tag "Prize: X="

parseTargetPoint : Parser Point.Point
parseTargetPoint =
    separatedPair
        number
        (tag ", Y=")
        number
    |> map \(x, y) -> { x: Num.toI64 x, y: Num.toI64 y }

shiftTarget : Game -> Game
shiftTarget = \game ->
    { game &
        target: {
            x: game.target.x + 10_000_000_000_000,
            y: game.target.y + 10_000_000_000_000,
        },
    }

solve : Game -> Result Solution [NoSolution]
solve = \game ->
    denom = solutionDenominator game

    aNum = game.target.x * game.b.y - (game.target.y * game.b.x)
    bNum = game.target.y * game.a.x - (game.target.x * game.a.y)

    if aNum % denom == 0 && bNum % denom == 0 then
        Ok {
            a: aNum // denom,
            b: bNum // denom,
        }
    else
        Err NoSolution

solutionDenominator : Game -> I64
solutionDenominator = \{ a, b } ->
    a.x * b.y - (a.y * b.x)

tokenCost : Solution -> U64
tokenCost = \{ a, b } ->
    Num.toU64 b + ((Num.toU64 a) * 3)

example : Str
example =
    """
    Button A: X+94, Y+34
    Button B: X+22, Y+67
    Prize: X=8400, Y=5400

    Button A: X+26, Y+66
    Button B: X+67, Y+21
    Prize: X=12748, Y=12176

    Button A: X+17, Y+86
    Button B: X+84, Y+37
    Prize: X=7870, Y=6450

    Button A: X+69, Y+23
    Button B: X+27, Y+71
    Prize: X=18641, Y=10279
    """

expect part1 example == Ok 480

expect part2 example == Ok 875318608908
