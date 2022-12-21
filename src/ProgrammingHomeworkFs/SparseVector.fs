module SparseVector

open System
open Converters

type BinTree<'value> =
    | None
    | Leaf of 'value
    | Node of BinTree<'value> * BinTree<'value>

let toSquare (arr: 'value option[]) =
    let length = arr.Length
    let log = Math.Log(length, 2)

    if (ceil log) = log then
        uint length
    else
        uint (2.0 ** ceil log)

let toSquareValue (length: uint) =
    let log = Math.Log(toDouble length, 2)
    if (ceil log) = log then length else uint (2.0 ** ceil log)

type squareArray<'value> =
    struct
        val Memory: array<Option<'value>>
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

    let rec binTreeFormation (squareArr: squareArray<'value>) =
        let hd = squareArr.Head
        let length = squareArr.Length
        let memory = squareArr.Memory
        let realLength = memory.Length

        if hd >= uint realLength then
            BinTree.None
        elif length = 1u then
            fromOptionToBinTree memory[toInt hd]
        else
            let left = binTreeFormation (squareArray (memory, hd, length / 2u))
            let right = binTreeFormation (squareArray (memory, hd + length / 2u, length / 2u))

            if left = BinTree.None && right = BinTree.None then
                BinTree.None
            else
                Node(left, right)

    binTreeFormation (squareArray (arr, 0u, toSquare arr))

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
            if List.length list = 0 then
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

type Vector<'value when 'value: equality> =
    struct

        val Storage: BinTree<'value>
        val Length: uint
        val SquareLength: uint

        new(storage, length, squareLength) =
            { Storage = storage
              Length = length
              SquareLength = squareLength }

        new(arr) =
            { Storage = toBinTree arr
              Length = uint arr.Length
              SquareLength = toSquare arr }

        new(list, realLength, weight) =
            { Storage = toBinTreeFromCOO list realLength weight
              Length = realLength
              SquareLength = toSquareValue realLength }

        member this.Item
            with get i =
                let takeElementOfVector i (vector: Vector<'value>) =
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
                        failwith "Index out of the range. Error in -takeElementOfVector- function. "

                takeElementOfVector i this

        member this.IsEmpty = this.Storage = BinTree.None
    end

let vectorAddition (plusOperation: 'value1 option -> 'value2 option -> 'value3 option) (vector1: Vector<'value1>) (vector2: Vector<'value2>) : Vector<'value3> =

    let f x y =
        let z = plusOperation x y

        match z with
        | Option.None -> BinTree.None
        | Some z -> BinTree.Leaf z

    let rec treesAddition tree1 tree2 =
        match tree1, tree2 with
        | BinTree.Leaf x, BinTree.Leaf y -> f (Some x) (Some y)

        | BinTree.None, x ->
            match x with
            | Leaf a -> f Option.None (Some a)
            | BinTree.None -> BinTree.None
            | BinTree.Node (a, b) -> treesAddition (BinTree.Node(BinTree.None, BinTree.None)) (BinTree.Node(a, b))

        | x, BinTree.None ->
            match x with
            | Leaf a -> f (Some a) Option.None
            | BinTree.None -> BinTree.None
            | BinTree.Node (a, b) -> treesAddition (BinTree.Node(a, b)) (BinTree.Node(BinTree.None, BinTree.None))

        | BinTree.Node (x, y), BinTree.Node (z, w) ->
            let left = treesAddition x z
            let right = treesAddition y w

            if left = BinTree.None && right = BinTree.None then
                BinTree.None
            else
                BinTree.Node(left, right)
        | BinTree.Node (x, y), BinTree.Leaf z -> treesAddition <| BinTree.Node(x, y) <| BinTree.Node(BinTree.Leaf z, BinTree.Leaf z)
        | BinTree.Leaf z, BinTree.Node (x, y) -> treesAddition <| BinTree.Node(BinTree.Leaf z, BinTree.Leaf z) <| BinTree.Node(x, y)

    if vector1.Length = vector2.Length then
        Vector(treesAddition vector1.Storage vector2.Storage, vector1.Length, vector1.SquareLength)
    else
        failwith $"Cannot add vector. Expected vector1.Length : %A{vector1.Length} = vector2.Length : %A{vector2.Length}. "
