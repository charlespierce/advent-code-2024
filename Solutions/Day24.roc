module [solution]

solution = { day: 24, part1, part2 }

import Parser exposing [Parser]

part1 : Str -> _
part1 = \input ->
    parse? input
        |> findBitsValue "z"
        |> Ok

part2 : Str -> _
part2 = \input ->
    # Used with manual manipulation to determine the appropriate swaps
    wires = parse? input

    x = findBitsValue wires "x"
    y = findBitsValue wires "y"
    z = findBitsValue wires "z"

    Ok (numToBits (x + y), numToBits z)

numToBits : U64 -> Str
numToBits = \num ->
    if num == 0 then
        ""
    else if Num.bitwiseAnd num 1 == 1 then
        Num.shiftRightZfBy num 1
        |> numToBits
        |> Str.concat "1"
    else
        Num.shiftRightZfBy num 1
        |> numToBits
        |> Str.concat "0"

findBitsValue : Wires, Str -> U64
findBitsValue = \wires, prefix ->
    Dict.keys wires
    |> List.keepIf \key -> Str.startsWith key prefix
    |> List.map \key -> Str.dropPrefix key prefix
    |> List.keepOks Str.toU8
    |> List.sortDesc
    |> List.map \key -> "$(prefix)$(padZero key)"
    |> List.map \key -> execute key wires
    |> List.walk 0 \sum, bit ->
        if bit then
            sum * 2 + 1
        else
            sum * 2

padZero : U8 -> Str
padZero = \num ->
    raw = Num.toStr num
    if num < 10 then
        "0$(raw)"
    else
        raw

Operation : [Literal Bool, And Str Str, Or Str Str, Xor Str Str]
Wires : Dict Str Operation

parse : Str -> Result Wires _
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.dropIf Str.isEmpty
    |> List.mapTry \line ->
        Parser.parseStr line parseOperation
    |> Result.map Dict.fromList

parseOperation : Parser (Str, Operation)
parseOperation =
    Parser.oneOf [parseOr, parseAnd, parseXor, parseLiteral]

parseOr : Parser (Str, Operation)
parseOr = parseBinary "OR" Or

parseAnd : Parser (Str, Operation)
parseAnd = parseBinary "AND" And

parseXor : Parser (Str, Operation)
parseXor = parseBinary "XOR" Xor

parseBinary : Str, (Str, Str -> Operation) -> Parser (Str, Operation)
parseBinary = \opName, opMapper ->
    opTag = " $(opName) "

    Parser.separatedPair
        (Parser.separatedPair parseIdentifier (Parser.tag opTag) parseIdentifier)
        (Parser.tag " -> ")
        parseIdentifier
    |> Parser.map \((lhs, rhs), end) -> (end, opMapper lhs rhs)

parseLiteral : Parser (Str, Operation)
parseLiteral =
    Parser.separatedPair
        parseIdentifier
        (Parser.tag ": ")
        parseBool
    |> Parser.map \(id, val) -> (id, Literal val)

parseIdentifier : Parser Str
parseIdentifier =
    Parser.many1 Parser.alphaNum
    |> Parser.map \list ->
        when Str.fromUtf8 list is
            Ok val -> val
            Err _ -> crash "Invalid id"

parseBool : Parser Bool
parseBool =
    Parser.digit
    |> Parser.map \val ->
        when val is
            0 -> Bool.false
            1 -> Bool.true
            _ -> crash "Invalid literal"

execute : Str, Wires -> Bool
execute = \id, wires ->
    when Dict.get wires id is
        Ok (Literal value) -> value
        Ok (And lhs rhs) ->
            left = execute lhs wires
            right = execute rhs wires
            left && right

        Ok (Or lhs rhs) ->
            left = execute lhs wires
            right = execute rhs wires
            left || right

        Ok (Xor lhs rhs) ->
            left = execute lhs wires
            right = execute rhs wires
            xor left right

        Err _ -> crash "Invalid id"

xor : Bool, Bool -> Bool
xor = \a, b ->
    if a == b then
        Bool.false
    else
        Bool.true

example : Str
example =
    """
    x00: 1
    x01: 0
    x02: 1
    x03: 1
    x04: 0
    y00: 1
    y01: 1
    y02: 1
    y03: 1
    y04: 1

    ntg XOR fgs -> mjb
    y02 OR x01 -> tnw
    kwq OR kpj -> z05
    x00 OR x03 -> fst
    tgd XOR rvg -> z01
    vdt OR tnw -> bfw
    bfw AND frj -> z10
    ffh OR nrd -> bqk
    y00 AND y03 -> djm
    y03 OR y00 -> psh
    bqk OR frj -> z08
    tnw OR fst -> frj
    gnj AND tgd -> z11
    bfw XOR mjb -> z00
    x03 OR x00 -> vdt
    gnj AND wpb -> z02
    x04 AND y00 -> kjc
    djm OR pbm -> qhw
    nrd AND vdt -> hwm
    kjc AND fst -> rvg
    y04 OR y02 -> fgs
    y01 AND x02 -> pbm
    ntg OR kjc -> kwq
    psh XOR fgs -> tgd
    qhw XOR tgd -> z09
    pbm OR djm -> kpj
    x03 XOR y03 -> ffh
    x00 XOR y04 -> ntg
    bfw OR bqk -> z06
    nrd XOR fgs -> wpb
    frj XOR qhw -> z04
    bqk OR frj -> z07
    y03 OR x01 -> nrd
    hwm AND bqk -> z03
    tgd XOR rvg -> z12
    tnw OR pbm -> gnj
    """

expect
    got = part1 example

    got == Ok 2024
