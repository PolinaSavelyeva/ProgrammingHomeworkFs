module SparseMatrix

open System

type QuadTree<'value> =
    | None
    | Leaf of 'value
    | Node of QuadTree<'value> * QuadTree<'value> * QuadTree<'value> * QuadTree<'value>

let toSquare arr =
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
        val HeadRow: int
        val HeadColumn: int
        val Length: int

        new(memory, headRow, headColumn, length) =
            { Memory = memory
              HeadRow = headRow
              HeadColumn = headColumn
              Length = length }
    end

let toQuadTree arr =
    let fromOptionToQTree optionValue =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some x -> QuadTree.Leaf(x)

    let rec qTreeFormation (arr: squareArray<'value>) =
        let memory = arr.Memory
        let hd1 = arr.HeadRow
        let hd2 = arr.HeadColumn
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

type Matrix<'value when 'value: equality> =
    struct

        val Storage: QuadTree<'value>
        val Length1: int
        val Length2: int
        val SquareLength: int

        new(storage, length1, length2, squareLength) =
            { Storage = storage
              Length1 = length1
              Length2 = length2
              SquareLength = squareLength }

        new(arr) =
            { Storage = toQuadTree arr
              Length1 = Array2D.length1 arr
              Length2 = Array2D.length2 arr
              SquareLength = toSquare arr }
    end

let takeElementOfMatrix i j (matrix: Matrix<'value>) =

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
