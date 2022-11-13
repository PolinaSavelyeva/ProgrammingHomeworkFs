module SparseMatrix

open System

type QuadTree<'value> =
    | None
    | Leaf of 'value
    | Node of QuadTree<'value> * QuadTree<'value> * QuadTree<'value> * QuadTree<'value>

let toSquare (arr: 'value option[,]) : int =
    let length1 = Array2D.length1 arr
    let length2 = Array2D.length2 arr
    let log1 = Math.Log(length1, 2)
    let log2 = Math.Log(length2, 2)

    if ceil log1 = log1 && ceil log2 = log2 && log1 = log2 then
        length1
    else
        int (2.0 ** ceil (max log1 log2))

type squareArray<'value> =
    struct
        val Memory: 'value option[,]
        val Head1: int
        val Head2: int
        val Length: int

        new(memory, head1, head2, length) =
            { Memory = memory
              Head1 = head1
              Head2 = head2
              Length = length }
    end

let toQuadTree (arr: 'value option[,]) : QuadTree<'value> =
    let fromOptionToQTree optionValue : QuadTree<'value> =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some (x) -> QuadTree.Leaf(x)

    let rec qTreeFormation (arr: squareArray<'value>) : QuadTree<'value> =
        let memory = arr.Memory
        let hd1 = arr.Head1
        let hd2 = arr.Head2
        let length = arr.Length
        let realLength1 = Array2D.length1 memory
        let realLength2 = Array2D.length2 memory

        if hd1 >= realLength1 || hd2 >= realLength2 then
            QuadTree.None
        elif length = 1 then
            fromOptionToQTree memory[hd1, hd2]
        else
            let first = qTreeFormation (squareArray (memory, hd1, hd2, length / 2))

            let second =
                qTreeFormation (squareArray (memory, hd1, hd2 + length / 2, length / 2))

            let third = qTreeFormation (squareArray (memory, hd1 + length / 2, hd2, length / 2))

            let fourth =
                qTreeFormation (squareArray (memory, hd1 + length / 2, hd2 + length / 2, length / 2))

            if first = QuadTree.None && second = QuadTree.None && third = QuadTree.None && fourth = QuadTree.None then
                QuadTree.None
            else
                Node(first, second, third, fourth)

    qTreeFormation (squareArray (arr, 0, 0, toSquare arr))

type Matrix<'value, 'value0 when 'value: equality>(arr: 'value option[,]) =

    let length1 = Array2D.length1 arr
    let length2 = Array2D.length2 arr
    let storage = toQuadTree arr
    let squareLength = toSquare arr

    member this.Length1 = length1
    member this.Length2 = length2
    member this.Storage = storage
    member this.SquareLength = squareLength

let takeElementOfMatrix (i: int) (j: int) (matrix: Matrix<'value, 'value0>) =

    let rec whichElement i j size tree =
        match tree with
        | QuadTree.Leaf x -> Some(x)
        | QuadTree.None -> Option.None
        | QuadTree.Node (a, b, c, d) ->
            let n = size / 2

            if i < n && j < n then whichElement i j n a
            elif i < n && j >= n then whichElement i (j - n) n b
            elif i >= n && j < n then whichElement (i - n) j n c
            else whichElement (i - n) (j - n) n d

    if i < matrix.Length1 && j < matrix.Length2 then
        whichElement i j matrix.SquareLength matrix.Storage
    else
        failwith "Index out of the range.  Error in -takeElementOfMatrix- function. "
