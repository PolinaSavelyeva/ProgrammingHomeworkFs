module MatrixMultiplication

open SparseVector
open SparseMatrix
open Converters
open System

let multiplication plusOperation (multiOperation: Option<'Value1> -> Option<'Value2> -> Option<'Value3>) (vector: Vector<'Value1>) (matrix: Matrix<'Value2>) =

    let rec multiTrees binTree quadTree =
        match binTree, quadTree with
        | BinTree.Leaf x, QuadTree.Leaf y ->
            let z = multiOperation (Some x) (Some y)

            match z with
            | Option.None -> BinTree.None
            | Some z -> BinTree.Leaf z
        | BinTree.None, _
        | _, QuadTree.None -> BinTree.None
        | BinTree.Node (left, right), QuadTree.Node (first, second, third, fourth) ->

            let first =
                (vectorAddition
                 <| plusOperation
                 <| Vector(multiTrees left first, vector.Length, vector.SquareLength)
                 <| Vector(multiTrees right third, vector.Length, vector.SquareLength))
                    .Storage

            let second =
                (vectorAddition
                 <| plusOperation
                 <| Vector(multiTrees left second, vector.Length, vector.SquareLength)
                 <| Vector(multiTrees right fourth, vector.Length, vector.SquareLength))
                    .Storage

            if first = BinTree.None && second = BinTree.None then
                BinTree.None
            else
                BinTree.Node(first, second)
        | BinTree.Leaf x, QuadTree.Node (first, second, third, fourth) ->
            multiTrees
            <| BinTree.Node(BinTree.Leaf x, BinTree.Leaf x)
            <| QuadTree.Node(first, second, third, fourth)
        | BinTree.Node (left, right), QuadTree.Leaf x ->
            multiTrees
            <| BinTree.Node(left, right)
            <| QuadTree.Node(QuadTree.Leaf x, QuadTree.Leaf x, QuadTree.Leaf x, QuadTree.Leaf x)

    let rec binTreeCutter (tree: BinTree<'Value>) expectedSize currentSize =
        match tree with
        | BinTree.Node (first, _) when expectedSize <> currentSize -> binTreeCutter first expectedSize (currentSize / 2u)
        | _ -> tree

    let rec binTreeGrower (tree: BinTree<'Value>) expectedSize currentSize =
        if expectedSize <> currentSize then
            binTreeGrower (BinTree.Node(tree, BinTree.None)) expectedSize (currentSize * 2u)
        else
            tree

    if vector.Length = matrix.Length1 then
        let growTree =
            if vector.SquareLength <> matrix.SquareLength then
                multiTrees (binTreeGrower vector.Storage matrix.SquareLength vector.SquareLength) matrix.Storage
            else
                multiTrees vector.Storage matrix.Storage

        let cutTree =
            if matrix.Length1 > matrix.Length2 then
                let expectedLength = uint (2.0 ** ceil (Math.Log(toDouble matrix.Length2, 2)))
                binTreeCutter growTree expectedLength matrix.SquareLength
            else
                growTree

        Vector(cutTree, vector.Length, vector.SquareLength)
    else
        failwith
            $"Multiplication operation is not defined.\n
         Expected %A{vector.Length} = %A{matrix.Length1}.\n
         Error in -multiplication- function"
