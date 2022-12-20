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

let toSquare arr =
    let length1 = Array2D.length1 arr
    let length2 = Array2D.length2 arr
    let log1 = Math.Log(length1, 2)
    let log2 = Math.Log(length2, 2)

    if ceil log1 = log1 && ceil log2 = log2 && log1 = log2 then
        uint length1
    else
        uint (2.0 ** ceil (max log1 log2))

let toSquareValue length1 length2 =
    let log1 = Math.Log(toDouble length1, 2)
    let log2 = Math.Log(toDouble length2, 2)

    if ceil log1 = log1 && ceil log2 = log2 && log1 = log2 then
        length1
    else
        uint (2.0 ** ceil (max log1 log2))

type SquareArray<'Value> =
    struct
        val Memory: 'Value option[,]
        val HeadRow: uint
        val HeadColumn: uint
        val Length: uint

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

    qTreeFormation (SquareArray(arr, 0u, 0u, toSquare arr))


let toQuadTreeFromCOO (tripleList: list<uint * uint * Option<'Value>>) rows columns =

    let partition list length =
        let rec f list one two three four =
            match list with
            | [] -> one, two, three, four
            | hd :: tl ->
                if first hd < length / 2u then
                    if second hd < length / 2u then
                        f tl (hd :: one) two three four
                    else
                        f tl one ((first hd, second hd - length / 2u, third hd) :: two) three four
                elif second hd < length / 2u then
                    f tl one two ((first hd - length / 2u, second hd, third hd) :: three) four
                else
                    f tl one two three ((first hd - length / 2u, second hd - length / 2u, third hd) :: four)

        f list [] [] [] []

    let rec quadTreeFormation list length =
        if length = 1u then
            if List.isEmpty list then
                QuadTree.None
            else
                match third list[0] with
                | Option.None -> QuadTree.None
                | Some x -> QuadTree.Leaf x
        else
            let one, two, three, four = partition list length

            let one' = quadTreeFormation one (length / 2u)
            let two' = quadTreeFormation two (length / 2u)
            let three' = quadTreeFormation three (length / 2u)
            let four' = quadTreeFormation four (length / 2u)

            if one' = QuadTree.None && two' = QuadTree.None && three' = QuadTree.None && four' = QuadTree.None then
                QuadTree.None
            else
                QuadTree.Node(one', two', three', four')

    if tripleList.Length = 0 then
        QuadTree.None
    else
        let squareLength = toSquareValue rows columns
        quadTreeFormation tripleList squareLength

type Matrix<'Value when 'Value: equality> =
    struct

        val Storage: QuadTree<'Value>
        val Length1: uint
        val Length2: uint
        val SquareLength: uint

        new(storage, length1, length2, squareLength) =
            { Storage = storage
              Length1 = length1
              Length2 = length2
              SquareLength = squareLength }

        new(arr: 'Value option[,]) =
            { Storage = toQuadTree arr
              Length1 = Array2D.length1 arr |> uint
              Length2 = Array2D.length2 arr |> uint
              SquareLength = toSquare arr }

        new(tripleList, rows, columns) =
            { Storage = toQuadTreeFromCOO tripleList rows columns
              Length1 = rows
              Length2 = columns
              SquareLength = toSquareValue rows columns }

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
    end
