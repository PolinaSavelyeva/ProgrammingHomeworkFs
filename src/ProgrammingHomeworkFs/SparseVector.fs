module SparseVector

open System

type BinTree<'value> =
    | None
    | Leaf of 'value
    | Node of BinTree<'value> * BinTree<'value>

let toSquare (arr: 'value option[]) =
    let length = arr.Length
    let log = Math.Log(length, 2)
    if (ceil log) = log then length else int (2.0 ** ceil log)

type squareArray<'value> =
    struct
        val Memory: 'value option[]
        val Head: int
        val Length: int

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

        if hd >= realLength then
            BinTree.None
        elif length = 1 then
            fromOptionToBinTree memory[hd]
        else
            let left = binTreeFormation (squareArray (memory, hd, length / 2))
            let right = binTreeFormation (squareArray (memory, hd + length / 2, length / 2))

            if left = BinTree.None && right = BinTree.None then
                BinTree.None
            else
                Node(left, right)

    binTreeFormation (squareArray (arr, 0, toSquare arr))

type Vector<'value when 'value: equality> =
    struct

        val Storage: BinTree<'value>
        val Length: int
        val SquareLength: int

        new(storage, length, squareLength) =
            { Storage = storage
              Length = length
              SquareLength = squareLength }

        new(arr: 'value option[]) =
            { Storage = toBinTree arr
              Length = arr.Length
              SquareLength = toSquare arr }

        member this.Item
            with get i =
                let takeElementOfVector i (vector: Vector<'value>) =
                    let rec whichElement i size tree =
                        match tree with
                        | BinTree.Leaf x -> Some(x)
                        | BinTree.None -> Option.None
                        | BinTree.Node (left, right) ->
                            let n = size / 2

                            if i < n then
                                whichElement i n left
                            else
                                whichElement (i - n) n right

                    if i < vector.Length then
                        whichElement i vector.SquareLength vector.Storage
                    else
                        failwith "Index out of the range. Error in -takeElementOfVector- function. "

                takeElementOfVector i this
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
        failwith "Cannot add two vector. Expected vector1.Length = vector2.Length. "
