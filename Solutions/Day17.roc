module [solution]

solution = { day: 17, part1, part2 }

import Parser exposing [Parser]

part1 : Str -> _
part1 = \input ->
    Str.trim input
        |> Parser.parseStr? parseComputer
        |> execute
        |> formatOutput
        |> Ok

part2 : Str -> _
part2 = \input ->
    Str.splitOn input "\n"
        |> List.get 4
        |> Result.try? \line -> Parser.parseStr? line parseProgramLine
        |> findInitialValues
        |> List.min

Opcode : [Adv U8, Bxl U8, Bst U8, Jnz U8, Bxc, Out U8, Bdv U8, Cdv U8]

Computer : {
    registerA : U64,
    registerB : U64,
    registerC : U64,
    program : List Opcode,
    output : List U64,
    instructionPointer : U64,
}

formatOutput : Computer -> Str
formatOutput = \comp ->
    comp.output
    |> List.map Num.toStr
    |> List.intersperse ","
    |> List.walk "" Str.concat

execute : Computer -> Computer
execute = \comp ->
    when List.get comp.program comp.instructionPointer is
        Err OutOfBounds -> comp
        Ok opcode ->
            apply opcode comp
            |> execute

apply : Opcode, Computer -> Computer
apply = \opcode, comp ->
    instructionPointer = comp.instructionPointer + 1
    when opcode is
        Adv op ->
            registerA =
                comboValue op comp
                |> rightShift comp.registerA

            { comp & registerA, instructionPointer }

        Bxl op ->
            registerB = Num.bitwiseXor comp.registerB (Num.toU64 op)

            { comp & registerB, instructionPointer }

        Bst op ->
            registerB =
                comboValue op comp
                |> Num.rem 8

            { comp & registerB, instructionPointer }

        Jnz op ->
            if comp.registerA == 0 then
                { comp & instructionPointer }
            else
                ip =
                    Num.toU64 op
                    |> Num.divTrunc 2

                { comp & instructionPointer: ip }

        Bxc ->
            registerB = Num.bitwiseXor comp.registerB comp.registerC

            { comp & registerB, instructionPointer }

        Out op ->
            value =
                comboValue op comp
                |> Num.rem 8

            output = List.append comp.output value

            { comp & output, instructionPointer }

        Bdv op ->
            registerB =
                comboValue op comp
                |> rightShift comp.registerA

            { comp & registerB, instructionPointer }

        Cdv op ->
            registerC =
                comboValue op comp
                |> rightShift comp.registerA

            { comp & registerC, instructionPointer }

rightShift : U64, U64 -> U64
rightShift = \distance, value ->
    if distance > 64 then
        0
    else
        Num.shiftRightZfBy value (Num.toU8 distance)

comboValue : U8, Computer -> U64
comboValue = \raw, comp ->
    when raw is
        0 | 1 | 2 | 3 -> Num.toU64 raw
        4 -> comp.registerA
        5 -> comp.registerB
        6 -> comp.registerC
        _ -> crash "Invalid combo operand"

parseComputer : Parser Computer
parseComputer = \input ->
    (registerA, rest1) = parseRegister? input
    (registerB, rest2) = parseRegister? rest1
    (registerC, rest3) = parseRegister? rest2

    (_, rest4) = (Parser.tag "\n")? rest3

    (program, rest5) = parseOpcodes? rest4

    Ok (
        {
            registerA,
            registerB,
            registerC,
            program,
            output: [],
            instructionPointer: 0,
        },
        rest5,
    )

parseRegister : Parser U64
parseRegister =
    Parser.delimited
        (Parser.triple (Parser.tag "Register ") (Parser.any) (Parser.tag ": "))
        Parser.number
        (Parser.tag "\n")

parseOpcodes : Parser (List Opcode)
parseOpcodes =
    Parser.pair (Parser.tag "Program: ") (Parser.separatedList parseOpcode (Parser.tag ","))
    |> Parser.map .1

parseOpcode : Parser Opcode
parseOpcode =
    Parser.separatedPair Parser.digit (Parser.tag ",") Parser.digit
    |> Parser.map \(code, operand64) ->
        operand = Num.toU8 operand64
        when code is
            0 -> Adv operand
            1 -> Bxl operand
            2 -> Bst operand
            3 -> Jnz operand
            4 -> Bxc
            5 -> Out operand
            6 -> Bdv operand
            7 -> Cdv operand
            _ -> crash "Unreachable"

parseProgramLine : Parser (List U8)
parseProgramLine =
    Parser.pair (Parser.tag "Program: ") parseRawProgram
    |> Parser.map .1

parseRawProgram : Parser (List U8)
parseRawProgram =
    Parser.digit
    |> Parser.map Num.toU8
    |> Parser.separatedList (Parser.tag ",")

findInitialValues : List U8 -> List U64
findInitialValues = \program ->
    findInitialValuesInner program [0]

findInitialValuesInner : List U8, List U64 -> List U64
findInitialValuesInner = \program, registers ->
    when program is
        [] -> registers
        [.. as rest, b] ->
            newRegisters =
                List.joinMap registers \a -> findNextRegisters a b

            findInitialValuesInner rest newRegisters

findNextRegisters : U64, U8 -> List U64
findNextRegisters = \a, b ->
    List.range { start: At 0, end: At 7 }
    |> List.keepOks \c -> checkPossibleCValue a b c

checkPossibleCValue : U64, U8, U8 -> Result U64 [Invalid]
checkPossibleCValue = \a, b, c ->
    bottomBits =
        Num.bitwiseXor b c
        |> Num.bitwiseXor 4
        |> Num.toU64

    trialA =
        Num.shiftLeftBy a 3
        |> Num.bitwiseOr bottomBits

    offset =
        Num.bitwiseXor b c
        |> Num.bitwiseXor 5

    offsetBits =
        Num.shiftRightZfBy trialA offset
        |> Num.rem 8
        |> Num.toU8

    if offsetBits == c then
        Ok trialA
    else
        Err Invalid

example : Str
example =
    """
    Register A: 729
    Register B: 0
    Register C: 0

    Program: 0,1,5,4,3,0
    """

expect part1 example == Ok "4,6,3,5,6,3,5,2,1,0"

expect
    input =
        """
        Register A: 0
        Register B: 0
        Register C: 0

        Program: 2,4,1,1,7,5,1,5,4,0,5,5,0,3,3,0
        """

    registerA =
        part2 input
        |> Result.withDefault 0

    checkInput =
        """
        Register A: $(Num.toStr registerA)
        Register B: 0
        Register C: 0

        Program: 2,4,1,1,7,5,1,5,4,0,5,5,0,3,3,0
        """

    check = part1 checkInput

    check == Ok "2,4,1,1,7,5,1,5,4,0,5,5,0,3,3,0"
