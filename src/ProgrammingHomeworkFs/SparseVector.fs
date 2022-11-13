module SparseVector

open System

type BinTree<'value> =
    | None
    | Leaf of 'value
    | Node of BinTree<'value> * BinTree<'value>

let toSquare (arr: array<Option<'value>>) : int =
    let length = arr.Length
    let log = Math.Log(length, 2)
    if (ceil log) = log then
        length
    else
        int (2.0 ** ceil(log))

type squareArray<'value> =
    struct
        val Memory: array<Option<'value>>
        val Head: int
        val Length: int

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Length = length }
    end

let toBinTree (arr: array<Option<'value>>) : BinTree<'value> =

    let fromOptionToBinTree optionValue : BinTree<'value> =
        match optionValue with
        | Option.None -> BinTree.None
        | Some x -> BinTree.Leaf(x)

    let rec binTreeFormation (squareArr: squareArray<'value>) : BinTree<'value> =
        let hd = squareArr.Head
        let length = squareArr.Length
        let memory = squareArr.Memory
        let realLength = memory.Length
        if hd >= realLength then
            BinTree.None
        elif length = 1 then
            fromOptionToBinTree memory[hd]
        else
            let left = binTreeFormation (squareArray(memory, hd, length/2))
            let right = binTreeFormation (squareArray(memory, hd + length/2, length/2))
            if left = BinTree.None && right = BinTree.None then
                BinTree.None
            else
                Node(left, right)

    binTreeFormation (squareArray(arr, 0, toSquare arr))

type Vector<'value, 'value0 when 'value: equality> (arr: array<Option<'value>>) =
    let length = arr.Length
    let storage = toBinTree arr
    let squareLength = toSquare arr

    member this.Length = length
    member this.Storage = storage
    member this.SquareLength = squareLength


let takeElementOfVector (i: int) (vector: Vector<'value, 'value0>) : Option<'value> =

    let rec whichElement (i : int) (size : int) (tree : BinTree<'value>) : Option<'value> =
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
