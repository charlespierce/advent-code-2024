module [solution]

solution = { day: 9, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parseDisk input
    |> compact
    |> calculateChecksum

part2 : Str -> _
part2 = \input ->
    parseFilesystem input
    |> defrag
    |> calculateFsChecksum

Block : [Empty, Filled U64]

Disk : List Block

Entry : [Space U8, File U64 U8]

Filesystem : List Entry

parseDisk : Str -> Disk
parseDisk = \input ->
    Str.trim input
    |> Str.toUtf8
    |> List.mapWithIndex \val, index -> (val, index)
    |> List.joinMap parseBlocks

parseBlocks : (U8, U64) -> List Block
parseBlocks = \(char, index) ->
    len = Num.toU64 (char - '0')
    block =
        if index % 2 == 0 then
            Filled (index // 2)
        else
            Empty

    List.repeat block len

parseFilesystem : Str -> Filesystem
parseFilesystem = \input ->
    Str.trim input
    |> Str.toUtf8
    |> List.mapWithIndex parseEntry

parseEntry : U8, U64 -> Entry
parseEntry = \char, index ->
    len = char - '0'
    if index % 2 == 0 then
        File (index // 2) len
    else
        Space len

compact : Disk -> Disk
compact = \disk ->
    upper = (List.len disk) - 1
    compactInner disk 0 upper

compactInner : Disk, U64, U64 -> Disk
compactInner = \disk, lower, upper ->
    if upper <= lower then
        disk
    else
        lowerBlock = List.get disk lower
        upperBlock = List.get disk upper

        when (lowerBlock, upperBlock) is
            (Ok Empty, Ok (Filled id)) ->
                swapped =
                    disk
                    |> List.set lower (Filled id)
                    |> List.set upper (Empty)

                compactInner swapped (lower + 1) (upper - 1)

            (Ok Empty, Ok Empty) -> compactInner disk lower (upper - 1)
            (Ok (Filled _), Ok (Filled _)) -> compactInner disk (lower + 1) upper
            (Ok (Filled _), Ok Empty) -> compactInner disk (lower + 1) (upper - 1)
            _ -> crash "Unreachable"

defrag : Filesystem -> Filesystem
defrag = \fs ->
    upper = (List.len fs) - 1
    defragInner fs upper

defragInner : Filesystem, U64 -> Filesystem
defragInner = \fs, upper ->
    if upper == 0 then
        fs
    else
        upperValue = List.get fs upper |> unwrap

        when upperValue is
            Space _ -> defragInner fs (upper - 1)
            File id length ->
                when findSpace fs length upper is
                    Err NotFound -> defragInner fs (upper - 1)
                    Ok (index, spaceLength) ->
                        newEntries = determineNewEntries length spaceLength id
                        fs
                        |> List.set upper (Space length)
                        |> replaceWith index newEntries
                        |> defragInner (upper + (List.len newEntries) - 2)

findSpace : Filesystem, U8, U64 -> Result (U64, U8) [NotFound]
findSpace = \fs, length, maxIndex ->
    List.walkWithIndexUntil fs (Err NotFound) \state, entry, index ->
        if index >= maxIndex then
            Break (Err NotFound)
        else
            when entry is
                Space len if len >= length -> Break (Ok (index, len))
                _ -> Continue state

determineNewEntries : U8, U8, U64 -> Filesystem
determineNewEntries = \neededLength, spaceLength, id ->
    if spaceLength == neededLength then
        [File id neededLength]
    else if spaceLength > neededLength then
        [File id neededLength, Space (spaceLength - neededLength)]
    else
        []

unwrap : Result a _ -> a
unwrap = \res ->
    when res is
        Ok val -> val
        Err _ -> crash "Unwrap"

replaceWith : List a, U64, List a -> List a
replaceWith = \list, index, newItems ->
    { before, others } = List.splitAt list index

    before
    |> List.concat newItems
    |> List.concat (List.dropFirst others 1)

calculateChecksum : Disk -> U64
calculateChecksum = \disk ->
    disk
    |> List.mapWithIndex \block, index ->
        when block is
            Empty -> 0
            Filled id -> id * index
    |> List.sum

calculateFsChecksum : Filesystem -> U64
calculateFsChecksum = \fs ->
    fs
    |> List.joinMap \entry ->
        when entry is
            Space len -> List.repeat Empty (Num.toU64 len)
            File id len -> List.repeat (Filled id) (Num.toU64 len)
    |> calculateChecksum

example : Str
example = "2333133121414131402"

expect part1 example == 1928

expect part2 example == 2858
