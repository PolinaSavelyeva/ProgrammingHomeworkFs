module SparseMatrix

open System

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

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

let toSquareValue (length1: int) (length2: int) =
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


let toQuadTreeFromCOO (tripleList: list<int * int * Option<'value>>) (rows: int) (columns: int) (length: int) =

    let partition (list: list<int * int * Option<'value>>) (length: int) =
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

    let rec quadTreeFormation (list: list<int * int * Option<'value>>) (length: int) =
        if length = 1 then
            if List.length list = 0 then
                QuadTree.None
            else
                match third list[0] with
                | Option.None -> QuadTree.None
                | Some x -> QuadTree.Leaf x
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
        QuadTree.None
    else
        let squareLength = toSquareValue rows columns
        quadTreeFormation tripleList squareLength

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

        new(arr: 'value option[,]) =
            { Storage = toQuadTree arr
              Length1 = Array2D.length1 arr
              Length2 = Array2D.length2 arr
              SquareLength = toSquare arr }

        new(tripleList, rows, columns, length) =
            { Storage = toQuadTreeFromCOO tripleList rows columns length
              Length1 = rows
              Length2 = columns
              SquareLength = toSquareValue rows columns }

        member this.Item
            with get (i, j) =
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

                takeElementOfMatrix i j this

        member this.IsEmpty = this.Storage = QuadTree.None
    end
