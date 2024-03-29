module SparseVector

open System
open Converters

type BinTree<'Value> =
    | None
    | Leaf of 'Value
    | Node of BinTree<'Value> * BinTree<'Value>

let toSquare (arr: 'Value option[]) =
    let length = arr.Length
    let log = Math.Log(length, 2)

    if (ceil log) = log then
        uint length
    else
        uint (2.0 ** ceil log)

let toSquareValue (length: uint) =
    let log = Math.Log(toDouble length, 2)
    if (ceil log) = log then length else uint (2.0 ** ceil log)

type SquareArray<'Value> =
    struct
        val Memory: array<Option<'Value>>
        val Head: uint
        val Length: uint

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Length = length }
    end

let toBinTree arr =

    let fromOptionToBinTree optionValue =
        match optionValue with
        | Option.None -> BinTree.None
        | Some x -> BinTree.Leaf(x)

    let rec binTreeFormation (squareArr: SquareArray<'Value>) =
        let hd = squareArr.Head
        let length = squareArr.Length
        let memory = squareArr.Memory
        let realLength = memory.Length

        if hd >= uint realLength then
            BinTree.None
        elif length = 1u then
            fromOptionToBinTree memory[toInt hd]
        else
            let left = binTreeFormation (SquareArray(memory, hd, length / 2u))
            let right = binTreeFormation (SquareArray(memory, hd + length / 2u, length / 2u))

            if left = BinTree.None && right = BinTree.None then
                BinTree.None
            else
                Node(left, right)

    binTreeFormation (SquareArray(arr, 0u, toSquare arr))

let toBinTreeFromCOO list realLength weight =

    let partition list length =
        let rec f list left right =
            match list with
            | [] -> left, right
            | hd :: tl ->
                if hd < length / 2u then
                    f tl (hd :: left) right
                else
                    f tl left ((hd - length / 2u) :: right)

        f list [] []

    let rec binTreeFormation list length =
        if length = 1u then
            if List.isEmpty list then
                BinTree.None
            else
                BinTree.Leaf(weight)
        else
            let left, right = partition list length

            let left' = binTreeFormation left (length / 2u)
            let right' = binTreeFormation right (length / 2u)

            if left' = BinTree.None && right' = BinTree.None then
                BinTree.None
            else
                BinTree.Node(left', right')

    if realLength = 0u then
        BinTree.None
    else
        let squareLength = uint (2.0 ** ceil (Math.Log(toDouble realLength, 2)))
        binTreeFormation list squareLength

type Vector<'Value when 'Value: equality> =

    val Storage: BinTree<'Value>
    val Length: uint
    val SquareLength: uint

    new(storage, length) =
        { Storage = storage
          Length = length
          SquareLength = toSquareValue length }

    new(arr) =
        let storage = toBinTree arr
        let length = uint arr.Length
        Vector(storage, length)

    new(list, realLength, weight) =
        let storage = toBinTreeFromCOO list realLength weight
        Vector(storage, realLength)

    member this.Item
        with get i =
            let takeElementOfVector i (vector: Vector<'Value>) =
                let rec whichElement i size tree =
                    match tree with
                    | BinTree.Leaf x -> Some(x)
                    | BinTree.None -> Option.None
                    | BinTree.Node (left, right) ->
                        let n = size / 2u

                        if i < n then
                            whichElement i n left
                        else
                            whichElement (i - n) n right

                if i < vector.Length then
                    whichElement i vector.SquareLength vector.Storage
                else
                    failwith "Index out of the range. Error in -takeElementOfVector- function"

            takeElementOfVector i this

    member this.IsEmpty = this.Storage = BinTree.None
