module [
    Heap,
    empty,
    emptyInt,
    fromList,
    fromListInt,
    push,
    pop,
]

Heap a := {
    data : List a,
    comparator : Comparator a,
}

Comparator a : a, a -> [GT, LT, EQ]

empty : Comparator a -> Heap a
empty = \comparator -> @Heap {
        data: [],
        comparator,
    }

emptyInt : {} -> Heap (Int _)
emptyInt = \{} -> @Heap {
        data: [],
        comparator: intComparator,
    }

fromList : List a, Comparator a -> Heap a
fromList = \list, comparator ->
    start = empty comparator
    List.walk list start push

fromListInt : List (Int a) -> Heap (Int a)
fromListInt = \list -> fromList list intComparator

push : Heap a, a -> Heap a
push = \@Heap heap, item ->
    data = List.append heap.data item
    bubbleUp (@Heap { heap & data })

pop : Heap a -> Result (a, Heap a) [HeapWasEmpty]
pop = \@Heap heap ->
    when heap.data is
        [] -> Err HeapWasEmpty
        [single] -> Ok (single, empty heap.comparator)
        [first, last] -> Ok (first, @Heap { heap & data: [last] })
        [first, .. as middle, last] ->
            data = List.prepend middle last
            newHeap = bubbleDown (@Heap { heap & data })
            Ok (first, newHeap)

bubbleUp : Heap a -> Heap a
bubbleUp = \@Heap heap ->
    startIndex =
        List.len heap.data
        |> Num.sub 1
    bubbleUpInner (@Heap heap) startIndex

bubbleUpInner : Heap a, U64 -> Heap a
bubbleUpInner = \@Heap heap, index ->
    when parentIndex index is
        Ok parent ->
            curVal = getUnchecked heap.data index
            parentVal = getUnchecked heap.data parent

            when heap.comparator parentVal curVal is
                GT ->
                    newData = List.swap heap.data parent index
                    bubbleUpInner
                        (@Heap { heap & data: newData })
                        parent

                LT | EQ -> @Heap heap

        Err AtRoot ->
            @Heap heap

bubbleDown : Heap a -> Heap a
bubbleDown = \heap -> bubbleDownInner heap 0

bubbleDownInner : Heap a, U64 -> Heap a
bubbleDownInner = \@Heap heap, index ->
    (leftChildIndex, rightChildIndex) = childIndexes index

    leftChild = List.get heap.data leftChildIndex
    rightChild = List.get heap.data rightChildIndex
    cur = getUnchecked heap.data index

    when (leftChild, rightChild) is
        (Ok left, Ok right) ->
            curWithLeft = heap.comparator cur left
            curWithRight = heap.comparator cur right
            leftWithRight = heap.comparator left right

            when (curWithLeft, curWithRight, leftWithRight) is
                (GT, GT, GT) | (LT, GT, _) | (EQ, GT, _) ->
                    newData = List.swap heap.data index rightChildIndex
                    bubbleDownInner
                        (@Heap { heap & data: newData })
                        rightChildIndex

                (GT, GT, _) | (GT, LT, _) | (GT, EQ, _) ->
                    newData = List.swap heap.data index leftChildIndex
                    bubbleDownInner
                        (@Heap { heap & data: newData })
                        leftChildIndex

                _ -> @Heap heap

        (Ok left, Err _) ->
            when heap.comparator cur left is
                GT ->
                    newData = List.swap heap.data index leftChildIndex
                    bubbleDownInner
                        (@Heap { heap & data: newData })
                        leftChildIndex

                LT | EQ -> @Heap heap

        _ -> @Heap heap

getUnchecked : List a, U64 -> a
getUnchecked = \list, index ->
    when List.get list index is
        Ok val -> val
        Err _ -> crash "Unexpected index - this shouldn't happen"

intComparator : Int _, Int _ -> [GT, LT, EQ]
intComparator = \a, b ->
    if a < b then
        LT
    else if a > b then
        GT
    else
        EQ

parentIndex : U64 -> Result U64 [AtRoot]
parentIndex = \cur ->
    if cur == 0 then
        Err AtRoot
    else
        Ok ((cur - 1) // 2)

childIndexes : U64 -> (U64, U64)
childIndexes = \cur -> (cur * 2 + 1, cur * 2 + 2)

testCollect : Heap U64 -> List U64
testCollect = \heap ->
    testCollectInner heap []

testCollectInner : Heap U64, List U64 -> List U64
testCollectInner = \heap, list ->
    when pop heap is
        Ok (value, newHeap) -> testCollectInner newHeap (List.append list value)
        Err HeapWasEmpty -> list

testHeap : Heap U64
testHeap =
    emptyInt {}
    |> push 2
    |> push 36
    |> push 19
    |> push 7
    |> push 17
    |> push 1
    |> push 100
    |> push 3
    |> push 25

expect testCollect testHeap == [1, 2, 3, 7, 17, 19, 25, 36, 100]
