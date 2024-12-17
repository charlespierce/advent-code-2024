module [
    solve,
]

import Heap exposing [Heap]

State a : {
    unvisited: Heap (UnvisitedNode a),
    history : Dict a (List a, U64),
    visited : Set a,
    neighbors : a -> List (a, U64),
    done: a -> Bool,
}

solve : a, (a -> List (a, U64)), (a -> Bool) -> Result (a, U64, Dict a (List a, U64)) [NoSolution] where a implements Hash & Eq
solve = \start, neighbors, done ->
    unvisited =
        Heap.empty unvisitedComparator
        |> Heap.push { value: start, cost: 0 }

    history =
        Dict.empty {}
        |> Dict.insert start ([], 0)

    state = {
        unvisited,
        history,
        visited: Set.empty {},
        neighbors,
        done,
    }

    solveInner state

solveInner : State a -> Result (a, U64, Dict a (List a, U64)) [NoSolution] where a implements Hash & Eq
solveInner = \state ->
    when Heap.pop state.unvisited is
        Err HeapWasEmpty -> Err NoSolution
        Ok ({ value, cost }, _) if state.done value -> Ok (value, cost, state.history)
        Ok ({ value, cost }, heap) ->
            if Set.contains state.visited value then
                solveInner { state & unvisited: heap }
            else
                visited = Set.insert state.visited value

                (unvisited, history) =
                    state.neighbors value
                    |> List.walk (heap, state.history) \(uv, hist), (neighbor, neighborCost) ->
                        newCost = cost + neighborCost
                
                        when Dict.get hist neighbor is
                            Ok (_, oldCost) if oldCost < newCost -> (uv, hist)
                            Ok (prev, oldCost) if oldCost == newCost ->
                                (
                                    uv,
                                    Dict.insert hist neighbor (List.append prev value, oldCost)
                                )
                            Ok _ ->
                                (
                                    Heap.push uv { value: neighbor, cost: newCost },
                                    Dict.insert hist neighbor ([value], newCost),
                                )
                            Err _ ->
                                (
                                    Heap.push uv { value: neighbor, cost: newCost },
                                    Dict.insert hist neighbor ([value], newCost),
                                )

                solveInner { state & unvisited, history, visited }

UnvisitedNode a : {
    value : a,
    cost : U64,
}

unvisitedComparator : UnvisitedNode a, UnvisitedNode a -> [GT, LT, EQ]
unvisitedComparator = \a, b ->
    if a.cost < b.cost then
        LT
    else if a.cost > b.cost then
        GT
    else
        EQ
