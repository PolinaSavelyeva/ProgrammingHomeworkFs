module BreadthFirstSearch

open Graph
open SparseVector
open MatrixAndVectorOperations

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
    | Some _, Option.None -> Some()
    | _ -> Option.None

let fPlusVisited number =
    let f a b =
        match a, b with
        | Option.None, Some _ -> Some number
        | Some x, Option.None -> Some x
        | _ -> Option.None

    f

let BFS multiLevel addMultiLevel addLevel startVertexList (graph: Graph<'Value>) =

    let rec inner (front: Vector<unit>) visited iterationNumber =
        if front.IsEmpty then
            visited
        else
            let newFront =
                multiplication multiLevel addMultiLevel fPlus fMulti front graph.AdjacencyMatrix

            let front = vectorAddition addLevel fPlusMask newFront visited

            let visited = vectorAddition addLevel (fPlusVisited iterationNumber) visited front

            inner front visited (iterationNumber + 1u)

    let front = Vector(startVertexList, graph.VerticesCount, ())
    let visited = Vector(startVertexList, graph.VerticesCount, 0u)

    inner front visited 1u
