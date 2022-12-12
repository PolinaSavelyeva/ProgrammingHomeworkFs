module BreadthFirstSearch

open System
open SparseMatrix
open SparseVector
open MatrixMultiplication

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

let frontFormation (vertexList: list<int>) (length: int) =

    let partition (list: list<int>) (length: int) =
        let rec f list left right =
            match list with
            | [] -> left, right
            | hd :: tl ->
                if hd < length / 2 then
                    f tl (hd :: left) right
                else
                    f tl left ((hd - length / 2) :: right)

        f list [] []

    let rec binTreeFormation (list: list<int>) (length: int) =
        if length = 1 then
            if List.length list = 0 then
                BinTree.None
            else
                BinTree.Leaf(true)
        else
            let left, right = partition list length

            let left' = binTreeFormation left (length / 2)
            let right' = binTreeFormation right (length / 2)

            if left' = BinTree.None && right' = BinTree.None then
                BinTree.None
            else
                BinTree.Node(left', right')

    if length = 0 then
        Vector(BinTree.None, length, length)
    else
        let squareLength = int (2.0 ** ceil (Math.Log(length, 2)))
        Vector(binTreeFormation vertexList squareLength, length, squareLength)

let visitedFormation (vector: Vector<'value>) (number: int) =
    let rec f (tree: BinTree<'value>) =
        match tree with
        | Leaf _ -> Leaf number
        | BinTree.None -> BinTree.None
        | Node (left, right) -> Node(f left, f right)

    Vector(f vector.Storage, vector.Length, vector.SquareLength)

let matrixFormation (tripleList: (int * int * 'value option) list) (rows: int) (columns: int) (length: int) =

    let partition (list: (int * int * 'value option) list) (length: int) =
        let rec f list one two three four =
            match list with
            | [] -> one, two, three, four
            | hd :: tl ->
                if first hd < length / 2 then
                    if second hd < length / 2 then
                        f tl (hd :: one) two three four
                    else
                        f tl one ((first hd, second hd - length / 2, third hd) :: two) three four
                elif second hd < length / 2 then
                    f tl one two ((first hd - length / 2, second hd, third hd) :: three) four
                else
                    f tl one two three ((first hd - length / 2, second hd - length / 2, third hd) :: four)

        f list [] [] [] []

    let rec quadTreeFormation (list: (int * int * 'value option) list) (length: int) =
        if length = 1 then
            if List.length list = 0 then
                QuadTree.None
            else
                QuadTree.Leaf(third list[0])
        else
            let one, two, three, four = partition list length

            let one' = quadTreeFormation one (length / 2)
            let two' = quadTreeFormation two (length / 2)
            let three' = quadTreeFormation three (length / 2)
            let four' = quadTreeFormation four (length / 2)

            if one' = QuadTree.None && two' = QuadTree.None && three' = QuadTree.None && four' = QuadTree.None then
                QuadTree.None
            else
                QuadTree.Node(one', two', three', four')

    if length = 0 then
        Matrix(QuadTree.None, length, length, length)
    else
        let squareLength =
            int (2.0 ** ceil (max (Math.Log(rows, 2)) (Math.Log(columns, 2))))

        Matrix(quadTreeFormation tripleList squareLength, rows, columns, squareLength)

let reverseVisited (vector: Vector<'value>) =
    let rec f (tree: BinTree<'value>) =
        match tree with
        | BinTree.None -> BinTree.Leaf true
        | BinTree.Leaf _ -> BinTree.None
        | BinTree.Node (left, right) ->
            let left' = f left
            let right' = f right

            if left' = BinTree.None && right' = BinTree.None then
                BinTree.None
            else
                BinTree.Node(f left, f right)

    Vector(f vector.Storage, vector.Length, vector.SquareLength)

let fPlus (a: Option<bool>) (b: Option<bool>) : Option<bool> =
    match a, b with
    | Option.None, Option.None -> Option.None
    | _ -> Some true

let fMulti (a: Option<bool>) (b: Option<'value>) : Option<bool> =
    match a, b with
    | Option.None, _
    | _, Option.None -> Option.None
    | _ -> Some true

let fPlusMask (a: Option<bool>) (b: Option<bool>) =
    match a, b with
    | Some true, Some true -> Some true
    | _ -> Option.None

let fPlusVisited (a: Option<int>) (b: Option<int>) =
    match a, b with
    | Option.None, Some x
    | Some x, Option.None -> Some x
    | _ -> Option.None

let BFS (startVertexList: list<int>) (graphMatrix: Matrix<'value>) =

    let rec innerBFS (front: Vector<bool>) (visited: Vector<int>) (iterationNumber: int) =
        if front.Storage = BinTree.None then
            visited
        else
            let newFront = multiplication fPlus fMulti front graphMatrix
            let reverseVisited = reverseVisited visited

            let front = vectorAddition fPlusMask newFront reverseVisited

            let visited =
                vectorAddition fPlusVisited visited (visitedFormation front iterationNumber)

            innerBFS front visited (iterationNumber + 1)

    let front = frontFormation startVertexList graphMatrix.Length1
    let visited = visitedFormation front 0
    innerBFS front visited 1
