module BreadthFirstSearch

open SparseVector
open MatrixMultiplication

let fPlus a b =
    match a, b with
    | Option.None, Option.None -> Option.None
    | _ -> Some()

let fMulti a b =
    match a, b with
    | Option.None, _
    | _, Option.None -> Option.None
    | _ -> Some()

let fPlusMask a b =
    match a, b with
    | Some (), Option.None -> Some()
    | _ -> Option.None

let fPlusVisited number =
    let f a b =
        match a, b with
        | Option.None, Some () -> Some number
        | Some x, Option.None -> Some x
        | _ -> Option.None

    f

let BFS startVertexList graphMatrix =

    let rec inner (front: Vector<unit>) visited iterationNumber =
        if front.IsEmpty then
            visited
        else
            let newFront = multiplication fPlus fMulti front graphMatrix

            let front = vectorAddition fPlusMask newFront visited

            let visited = vectorAddition (fPlusVisited iterationNumber) visited front

            inner front visited (iterationNumber + 1u)

    let front = Vector(startVertexList, graphMatrix.Length1, ())
    let visited = Vector(startVertexList, graphMatrix.Length1, 0u)

    inner front visited 1u
