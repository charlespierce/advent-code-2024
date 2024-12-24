module [solution]

solution = { day: 23, part1, part2 }

part1 : Str -> _
part1 = \input ->
    parse input
    |> findTriplets
    |> Set.keepIf hasTNode
    |> Set.len

part2 : Str -> _
part2 = \input ->
    parse input
    |> findAllCliques
    |> List.walk (0, Set.empty {}) \(count, max), clique ->
        if Set.len clique > count then
            (Set.len clique, clique)
        else
            (count, max)
    |> .1
    |> Set.toList
    |> List.sortWith cmpStr
    |> List.intersperse ","
    |> List.joinMap Str.toUtf8
    |> Str.fromUtf8

Network : Dict Str (Set Str)

parse : Str -> Network
parse = \input ->
    Str.trim input
    |> Str.splitOn "\n"
    |> List.map \line -> Str.splitOn line "-"
    |> List.walk (Dict.empty {}) \network, connections ->
        when connections is
            [first, second] ->
                Dict.update network first (insertConnection second)
                |> Dict.update second (insertConnection first)

            _ -> crash "Invalid input"

insertConnection : Str -> (Result (Set Str) _ -> Result (Set Str) _)
insertConnection = \value ->
    \existing ->
        when existing is
            Ok set -> Ok (Set.insert set value)
            Err _ -> Ok (Set.single value)

hasTNode : Set Str -> Bool
hasTNode = \set ->
    Set.walkUntil set Bool.false \_, node ->
        if Str.startsWith node "t" then
            Break Bool.true
        else
            Continue Bool.false

findTriplets : Network -> Set (Set Str)
findTriplets = \network ->
    Dict.walk network (Set.empty {}) (secondLegTriplets network)

secondLegTriplets : Network -> (Set (Set Str), Str, Set Str -> Set (Set Str))
secondLegTriplets = \network ->
    \found, node, neighbors ->
        Set.walk neighbors found (thirdLegCheck network node)

thirdLegCheck : Network, Str -> (Set (Set Str), Str -> Set (Set Str))
thirdLegCheck = \network, first ->
    \found, second ->
        Dict.get network second
        |> Result.withDefault (Set.empty {})
        |> Set.walk found \f, third ->
            triad = hasConnection network third first

            if triad then
                Set.insert f (Set.fromList [first, second, third])
            else
                f

hasConnection : Network, Str, Str -> Bool
hasConnection = \network, first, second ->
    Dict.get network first
    |> Result.map \s -> Set.contains s second
    |> Result.withDefault Bool.false

findAllCliques : Network -> List (Set Str)
findAllCliques = \network ->
    p =
        Dict.keys network
        |> Set.fromList

    findAllCliquesInner
        (Set.empty {})
        p
        (Set.empty {})
        []
        network

findAllCliquesInner : Set Str, Set Str, Set Str, List (Set Str), Network -> List (Set Str)
findAllCliquesInner = \r, p, x, all, network ->
    if Set.isEmpty p then
        if Set.isEmpty x then
            List.append all r
        else
            all
    else
        Set.walk p (p, x, all) \(p1, x1, acc), v ->
            neighbors =
                Dict.get network v
                |> Result.withDefault (Set.empty {})

            updatedAcc =
                findAllCliquesInner
                    (Set.union r (Set.single v))
                    (Set.intersection p1 neighbors)
                    (Set.intersection x1 neighbors)
                    acc
                    network

            updatedP = Set.remove p1 v
            updatedX = Set.insert x1 v

            (updatedP, updatedX, updatedAcc)
        |> .2

cmpStr : Str, Str -> [LT, EQ, GT]
cmpStr = \a, b ->
    cmpList (Str.toUtf8 a) (Str.toUtf8 b)

cmpList : List U8, List U8 -> [LT, EQ, GT]
cmpList = \a, b ->
    zip a b
    |> List.walkUntil (cmpLen a b) \lenDiff, (l, r) ->
        if l < r then
            Break LT
        else if l > r then
            Break GT
        else
            Continue lenDiff

cmpLen : List U8, List U8 -> [LT, EQ, GT]
cmpLen = \a, b ->
    aLen = List.len a
    bLen = List.len b

    if aLen < bLen then
        LT
    else if aLen > bLen then
        GT
    else
        EQ

zip : List a, List b -> List (a, b)
zip = \left, right ->
    List.map2 left right \l, r -> (l, r)

example : Str
example =
    """
    kh-tc
    qp-kh
    de-cg
    ka-co
    yn-aq
    qp-ub
    cg-tb
    vc-aq
    tb-ka
    wh-tc
    yn-cg
    kh-ub
    ta-co
    de-co
    tc-td
    tb-wq
    wh-td
    ta-ka
    td-qp
    aq-cg
    wq-ub
    ub-vc
    de-ta
    wq-aq
    wq-vc
    wh-yn
    ka-de
    kh-ta
    co-tc
    wh-qp
    tb-vc
    td-yn
    """

expect part1 example == 7
expect part2 example == Ok "co,de,ka,ta"
