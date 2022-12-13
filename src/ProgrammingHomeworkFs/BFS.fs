module BreadthFirstSearch

open SparseMatrix
open SparseVector
open MatrixMultiplication

let fPlus (a: Option<bool>) (b: Option<bool>) : Option<bool> =
    match a, b with
    | Option.None, Option.None -> Option.None
    | _ -> Some true

let fMulti (a: Option<bool>) (b: Option<'value>) : Option<bool> =
    match a, b with
    | Option.None, _
    | _, Option.None -> Option.None
    | _ -> Some true

let fPlusMask (a: Option<bool>) (b: Option<int>) =
    match a, b with
    | Some true, Option.None -> Some true
    | _ -> Option.None

let fPlusVisited number =
    let f (a: Option<int>) (b: Option<bool>) =
        match a, b with
        | Option.None, Some true -> Some number
        | Some x, Option.None -> Some x
        | _ -> Option.None

    f

let BFS (startVertexList: list<int>) (graphMatrix: Matrix<'value>) =

    let rec innerBFS (front: Vector<bool>) (visited: Vector<int>) (iterationNumber: int) =
        if front.IsEmpty then
            visited
        else
            let newFront = multiplication fPlus fMulti front graphMatrix

            let front = vectorAddition fPlusMask newFront visited

            let visited = vectorAddition (fPlusVisited iterationNumber) visited front

            innerBFS front visited (iterationNumber + 1)

    let front =
        Vector(
            Array.init graphMatrix.Length1 (fun n ->
                if List.contains n startVertexList then
                    Some true
                else
                    Option.None)
        )

    let visited =
        Vector(
            Array.init graphMatrix.Length1 (fun n ->
                if List.contains n startVertexList then
                    Some 0
                else
                    Option.None)
        )

    innerBFS front visited 1
