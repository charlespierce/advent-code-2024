module [solution]

solution = { day: 8, part1, part2 }

import Point

part1 : Str -> _
part1 = \input ->
    field = parse input

    findAntinodes field
    |> Set.len

part2 : Str -> _
part2 = \input ->
    field = parse input

    findAntinodesWithResonance field
    |> Set.len

Field : {
    width : I64,
    height : I64,
    antennas : Antennas,
}

Antennas : Dict U8 (List Point.Point)

parse : Str -> Field
parse = \input ->
    lines =
        Str.trim input
        |> Str.splitOn "\n"

    height =
        List.len lines
        |> Num.toI64

    width =
        List.get lines 0
        |> Result.withDefault ""
        |> Str.toUtf8
        |> List.len
        |> Num.toI64

    antennas = List.walkWithIndex lines (Dict.empty {}) parseLine

    { width, height, antennas }

parseLine : Antennas, Str, U64 -> Antennas
parseLine = \antennas, line, y ->
    Str.toUtf8 line
    |> List.walkWithIndex antennas \antennas2, char, x ->
        when char is
            '.' -> antennas2
            a -> addAntenna antennas2 a (Point.fromIndex { x, y })

findAntinodes : Field -> Set Point.Point
findAntinodes = \field ->
    possiblities = Dict.walk field.antennas (Set.empty {}) antinodesForFrequency

    Set.keepIf possiblities \antinode -> isValid antinode field

findAntinodesWithResonance : Field -> Set Point.Point
findAntinodesWithResonance = \field ->
    Dict.walk field.antennas (Set.empty {}) (antinodesWithResonance field)

antinodesForFrequency : Set Point.Point, U8, List Point.Point -> Set Point.Point
antinodesForFrequency = \antinodes, _frequency, antennas ->
    pairwise antennas
    |> List.joinMap calculateAntinodes
    |> Set.fromList
    |> Set.union antinodes

antinodesWithResonance : Field -> (Set Point.Point, U8, List Point.Point -> Set Point.Point)
antinodesWithResonance = \field ->
    \antinodes, _frequency, antennas ->
        pairwise antennas
        |> List.joinMap (calculateAntinodesWithResonance field)
        |> Set.fromList
        |> Set.union antinodes

calculateAntinodes : (Point.Point, Point.Point) -> List Point.Point
calculateAntinodes = \(a, b) ->
    diff = Point.diff a b
    first = Point.sum a diff
    second = Point.sum b (Point.negate diff)

    [first, second]

calculateAntinodesWithResonance : Field -> ((Point.Point, Point.Point) -> List Point.Point)
calculateAntinodesWithResonance = \field ->
    \(a, b) ->
        step = slope a b

        resonanceInner [a] field a step
        |> resonanceInner field a (Point.negate step)

resonanceInner : List Point.Point, Field, Point.Point, Point.Point -> List Point.Point
resonanceInner = \values, field, start, step ->
    next = Point.sum start step
    if isValid next field then
        List.append values next
        |> resonanceInner field next step
    else
        values

addAntenna : Antennas, U8, Point.Point -> Antennas
addAntenna = \antennas, char, point ->
    Dict.update antennas char \maybeList ->
        when maybeList is
            Ok list -> Ok (List.append list point)
            Err Missing -> Ok [point]

isValid : Point.Point, Field -> Bool
isValid = \point, field ->
    point.x >= 0 && point.x < field.width && point.y >= 0 && point.y < field.height

pairwise : List a -> List (a, a)
pairwise = \list ->
    List.mapWithIndex list \item, index -> (item, index)
    |> List.joinMap \(item, index) ->
        List.dropFirst list (index + 1)
        |> List.map \item2 -> (item, item2)

slope : Point.Point, Point.Point -> Point.Point
slope = \point1, point2 ->
    { x: diffX, y: diffY } = Point.diff point1 point2
    divisor = gcd diffX diffY

    {
        x: diffX // divisor,
        y: diffY // divisor,
    }

gcd : I64, I64 -> I64
gcd = \a, b ->
    absA = Num.abs a
    absB = Num.abs b

    gcdInner absA absB

gcdInner : I64, I64 -> I64
gcdInner = \a, b ->
    if b == 0 then
        a
    else
        gcdInner b (a % b)

example : Str
example =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """

expect part1 example == 14

expect part2 example == 34
