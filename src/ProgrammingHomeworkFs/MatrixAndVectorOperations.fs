module MatrixAndVectorOperations

open SparseVector
open SparseMatrix
open Converters
open System

let vectorAddition level (plusOperation: 'Value1 option -> 'Value2 option -> 'Value3 option) (vector1: Vector<'Value1>) (vector2: Vector<'Value2>) : Vector<'Value3> =

    let f x y =
        let z = plusOperation x y

        match z with
        | Option.None -> BinTree.None
        | Some z -> BinTree.Leaf z

    let rec treesAddition parallelLevel tree1 tree2 =
        match tree1, tree2 with
        | BinTree.Leaf x, BinTree.Leaf y -> f (Some x) (Some y)

        | BinTree.None, x ->
            match x with
            | BinTree.Leaf a -> f Option.None (Some a)
            | BinTree.None -> BinTree.None
            | BinTree.Node (a, b) -> treesAddition parallelLevel (BinTree.Node(BinTree.None, BinTree.None)) (BinTree.Node(a, b))

        | x, BinTree.None ->
            match x with
            | BinTree.Leaf a -> f (Some a) Option.None
            | BinTree.None -> BinTree.None
            | BinTree.Node (a, b) -> treesAddition parallelLevel (BinTree.Node(a, b)) (BinTree.Node(BinTree.None, BinTree.None))

        | BinTree.Node (x, y), BinTree.Node (z, w) ->
            if parallelLevel = 0u then
                let left = treesAddition 0u x z
                let right = treesAddition 0u y w

                if left = BinTree.None && right = BinTree.None then
                    BinTree.None
                else
                    BinTree.Node(left, right)
            else
                let tasks =
                    [| async { return treesAddition (parallelLevel - 1u) x z }; async { return treesAddition (parallelLevel - 1u) y w } |]

                let results = tasks |> Async.Parallel |> Async.RunSynchronously

                if results[0] = BinTree.None && results[1] = BinTree.None then
                    BinTree.None
                else
                    BinTree.Node(results[0], results[1])

        | BinTree.Node (x, y), BinTree.Leaf z -> treesAddition parallelLevel (BinTree.Node(x, y)) (BinTree.Node(BinTree.Leaf z, BinTree.Leaf z))
        | BinTree.Leaf z, BinTree.Node (x, y) -> treesAddition parallelLevel (BinTree.Node(BinTree.Leaf z, BinTree.Leaf z)) (BinTree.Node(x, y))

    if vector1.Length = vector2.Length then
        Vector(treesAddition level vector1.Storage vector2.Storage, vector1.Length)
    else
        failwith $"Expected vector1.Length : %A{vector1.Length} = vector2.Length : %A{vector2.Length}"

let multiplication multiLevel addLevel plusOperation (multiOperation: Option<'Value1> -> Option<'Value2> -> Option<'Value3>) (vector: Vector<'Value1>) (matrix: Matrix<'Value2>) =

    let rec multiTrees parallelLevel binTree quadTree =
        match binTree, quadTree with
        | BinTree.Leaf x, QuadTree.Leaf y ->
            let z = multiOperation (Some x) (Some y)

            match z with
            | Option.None -> BinTree.None
            | Some z -> BinTree.Leaf z
        | BinTree.None, _
        | _, QuadTree.None -> BinTree.None
        | BinTree.Node (left, right), QuadTree.Node (first, second, third, fourth) ->
            if parallelLevel = 0u then
                let first =
                    (vectorAddition addLevel
                     <| plusOperation
                     <| Vector(multiTrees 0u left first, vector.Length)
                     <| Vector(multiTrees 0u right third, vector.Length))
                        .Storage

                let second =
                    (vectorAddition addLevel
                     <| plusOperation
                     <| Vector(multiTrees 0u left second, vector.Length)
                     <| Vector(multiTrees 0u right fourth, vector.Length))
                        .Storage

                if first = BinTree.None && second = BinTree.None then
                    BinTree.None
                else
                    BinTree.Node(first, second)
            else
                let multiTasks =
                    [| async { return Vector(multiTrees (parallelLevel - 1u) left first, vector.Length) }
                       async { return Vector(multiTrees (parallelLevel - 1u) right third, vector.Length) }
                       async { return Vector(multiTrees (parallelLevel - 1u) left second, vector.Length) }
                       async { return Vector(multiTrees (parallelLevel - 1u) right fourth, vector.Length) } |]

                let multiResults = multiTasks |> Async.Parallel |> Async.RunSynchronously

                let leftTree1 = multiResults[0]
                let leftTree2 = multiResults[1]
                let rightTree1 = multiResults[2]
                let rightTree2 = multiResults[3]

                let addTasks =
                    [| async { return (vectorAddition addLevel plusOperation leftTree1 leftTree2).Storage }; async { return (vectorAddition addLevel plusOperation rightTree1 rightTree2).Storage } |]

                let addResults = addTasks |> Async.Parallel |> Async.RunSynchronously

                if addResults[0] = BinTree.None && addResults[1] = BinTree.None then
                    BinTree.None
                else
                    BinTree.Node(addResults[0], addResults[1])

        | BinTree.Leaf x, QuadTree.Node (first, second, third, fourth) ->
            multiTrees parallelLevel
            <| BinTree.Node(BinTree.Leaf x, BinTree.Leaf x)
            <| QuadTree.Node(first, second, third, fourth)
        | BinTree.Node (left, right), QuadTree.Leaf x ->
            multiTrees parallelLevel
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
                multiTrees multiLevel (binTreeGrower vector.Storage matrix.SquareLength vector.SquareLength) matrix.Storage
            else
                multiTrees multiLevel vector.Storage matrix.Storage

        let cutTree =
            if matrix.Length1 > matrix.Length2 then
                let expectedLength = uint (2.0 ** ceil (Math.Log(toDouble matrix.Length2, 2)))
                binTreeCutter growTree expectedLength matrix.SquareLength
            else
                growTree

        Vector(cutTree, vector.Length)
    else
        failwith
            $"Multiplication operation is not defined.\n
         Expected %A{vector.Length} = %A{matrix.Length1}.\n
         Error in -multiplication- function"
