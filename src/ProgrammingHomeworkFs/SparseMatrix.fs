module SparseMatrix

open System
open Converters

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

type QuadTree<'Value> =
    | None
    | Leaf of 'Value
    | Node of QuadTree<'Value> * QuadTree<'Value> * QuadTree<'Value> * QuadTree<'Value>

let toSquareValue length1 length2 =
    let log1 = Math.Log(toDouble length1, 2)
    let log2 = Math.Log(toDouble length2, 2)

    if ceil log1 = log1 && ceil log2 = log2 && log1 = log2 then
        length1
    else
        uint (2.0 ** ceil (max log1 log2))

type SquareArray<'Value> =

    val Memory: 'Value option[,]
    val HeadRow: uint
    val HeadColumn: uint
    val Length: uint

    new(memory, headRow, headColumn, length) =
        { Memory = memory
          HeadRow = headRow
          HeadColumn = headColumn
          Length = length }

let toQuadTree arr =
    let fromOptionToQTree optionValue =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some x -> QuadTree.Leaf x

    let rec qTreeFormation (arr: SquareArray<'Value>) =
        let memory = arr.Memory
        let hd1 = arr.HeadRow
        let hd2 = arr.HeadColumn
        let length = arr.Length
        let realLength1 = Array2D.length1 memory
        let realLength2 = Array2D.length2 memory

        if hd1 >= uint realLength1 || hd2 >= uint realLength2 then
            QuadTree.None
        elif length = 1u then
            fromOptionToQTree memory[toInt hd1, toInt hd2]
        else
            let first = qTreeFormation (SquareArray(memory, hd1, hd2, length / 2u))

            let second =
                qTreeFormation (SquareArray(memory, hd1, hd2 + length / 2u, length / 2u))

            let third =
                qTreeFormation (SquareArray(memory, hd1 + length / 2u, hd2, length / 2u))

            let fourth =
                qTreeFormation (SquareArray(memory, hd1 + length / 2u, hd2 + length / 2u, length / 2u))

            if first = QuadTree.None && second = QuadTree.None && third = QuadTree.None && fourth = QuadTree.None then
                QuadTree.None
            else
                Node(first, second, third, fourth)

    qTreeFormation (SquareArray(arr, 0u, 0u, toSquareValue <| uint (Array2D.length1 arr) <| uint (Array2D.length2 arr)))

type COOMatrix<'Value> =
    struct
        val Data: list<uint * uint * Option<'Value>>
        val SquareLength: uint

        new(tripleList, rows, columns) =
            { Data = tripleList
              SquareLength = toSquareValue rows columns }

        new(tripleList, squareLength) =
            { Data = tripleList
              SquareLength = squareLength }
    end

let toQuadTreeFromCOO (tripleList: list<uint * uint * Option<'Value>>) rows columns =

    let maxRowIndex = rows - 1u
    let maxColumnIndex = columns - 1u

    let partition (matrix: COOMatrix<'Value>) =
        //TODO delete
        if matrix.SquareLength = 0u then
            matrix, matrix, matrix, matrix
        else
            let halfSquareLength = matrix.SquareLength / 2u

            let rec inner list one two three four =
                match list with
                | [] -> one, two, three, four
                | (i, j, value) :: tl ->

                    if i < halfSquareLength && j < halfSquareLength then
                        inner tl ((i, j, value) :: one) two three four
                    elif i < halfSquareLength && j >= halfSquareLength then
                        inner tl one ((i, j - halfSquareLength, value) :: two) three four
                    elif i >= halfSquareLength && j < halfSquareLength then
                        inner tl one two ((i - halfSquareLength, j, value) :: three) four
                    else
                        inner tl one two three ((i - halfSquareLength, j - halfSquareLength, value) :: four)

            let one, two, three, four = inner matrix.Data [] [] [] []

            COOMatrix(one, halfSquareLength), COOMatrix(two, halfSquareLength), COOMatrix(three, halfSquareLength), COOMatrix(four, halfSquareLength)

    let rec quadTreeFormation (matrix: COOMatrix<'Value>) =

        if
            matrix.SquareLength = 1u
            && matrix.Data.Length = 1
            && first matrix.Data.Head <= maxRowIndex
            && second matrix.Data.Head <= maxColumnIndex
        then
            QuadTree.Leaf (third matrix.Data.Head).Value
        elif matrix.Data.Length < 1 then
            QuadTree.None
        else
            let one, two, three, four = partition matrix

            let one' = quadTreeFormation one
            let two' = quadTreeFormation two
            let three' = quadTreeFormation three
            let four' = quadTreeFormation four

            match one', two', three', four' with
            | QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None -> QuadTree.None
            | _ -> QuadTree.Node(one', two', three', four')

    if tripleList.Length = 0 then
        QuadTree.None
    else
        quadTreeFormation <| COOMatrix(tripleList, rows, columns)

type Matrix<'Value when 'Value: equality> =

    val Storage: QuadTree<'Value>
    val Length1: uint
    val Length2: uint
    val SquareLength: uint

    new(storage, length1, length2) =
        { Storage = storage
          Length1 = length1
          Length2 = length2
          SquareLength = toSquareValue length1 length2 }

    new(tripleList, rows, columns) = Matrix(toQuadTreeFromCOO tripleList rows columns, rows, columns)

    new(arr: 'Value option[,]) = Matrix(toQuadTree arr, Array2D.length1 arr |> uint, Array2D.length2 arr |> uint)

    member this.Item
        with get (i, j) =
            let takeElementOfMatrix i j (matrix: Matrix<'Value>) =
                let rec whichElement i j size tree =
                    match tree with
                    | QuadTree.Leaf x -> Some(x)
                    | QuadTree.None -> Option.None
                    | QuadTree.Node (a, b, c, d) ->
                        let n = size / 2u

                        if i < n && j < n then whichElement i j n a
                        elif i < n && j >= n then whichElement i (j - n) n b
                        elif i >= n && j < n then whichElement (i - n) j n c
                        else whichElement (i - n) (j - n) n d

                if i < matrix.Length1 && j < matrix.Length2 then
                    whichElement i j matrix.SquareLength matrix.Storage
                else
                    failwith "Index out of the range.  Error in -takeElementOfMatrix- function"

            takeElementOfMatrix i j this

    member this.IsEmpty = this.Storage = QuadTree.None
