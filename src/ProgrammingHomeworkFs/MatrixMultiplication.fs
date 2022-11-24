module MatrixMultiplication

open SparseVector
open SparseMatrix

let multiplication plusOperation (multiOperation: 'value1 -> 'value2 -> 'value3) (vector: Vector<'value1>) (matrix: Matrix<'value2>) =

    let rec fPlus a b =
        match a, b with
        | BinTree.Leaf x, BinTree.Leaf y -> BinTree.Leaf(plusOperation x y)
        | BinTree.None, x
        | x, BinTree.None -> x
        | BinTree.Node (x, y), BinTree.Node (z, w) -> BinTree.Node(fPlus x z, fPlus y w)
        | BinTree.Node (x, y), BinTree.Leaf z
        | BinTree.Leaf z, BinTree.Node (x, y) -> fPlus <| BinTree.Node(x, y) <| BinTree.Node(BinTree.Leaf(z), BinTree.Leaf(z))

    let rec multiTrees binTree quadTree =
        match binTree, quadTree with
        | BinTree.Leaf a, QuadTree.Leaf b -> BinTree.Leaf(multiOperation a b)
        | BinTree.None, _
        | _, QuadTree.None -> BinTree.None
        | BinTree.Node (left, right), QuadTree.Node (first, second, third, fourth) ->
            // if
            //     (first = QuadTree.None && second = QuadTree.None && third = QuadTree.None && fourth = QuadTree.None)
            //     && (left = BinTree.None && right = BinTree.None)
            // then
            //     BinTree.None
            // else
            BinTree.Node(fPlus (multiTrees left first) (multiTrees right third), fPlus (multiTrees left second) (multiTrees right fourth))
        | BinTree.Leaf a, QuadTree.Node (first, second, third, fourth) ->
            multiTrees
            <| BinTree.Node(BinTree.Leaf(a), BinTree.Leaf(a))
            <| QuadTree.Node(first, second, third, fourth)
        | BinTree.Node (left, right), QuadTree.Leaf b ->
            multiTrees
            <| BinTree.Node(left, right)
            <| QuadTree.Node(QuadTree.Leaf(b), QuadTree.Leaf(b), QuadTree.Leaf(b), QuadTree.Leaf(b))

    let rec binTreeCutter (tree: BinTree<'value>) expectedSize currentSize =
        if expectedSize <> currentSize then
            match tree with
            | BinTree.Node(first, _) -> binTreeCutter first expectedSize (currentSize / 2)
            | _ -> tree
        else
            tree
        // match tree with
        // | BinTree.Node (first, _) when expectedSize <> currentSize -> binTreeCutter first expectedSize (currentSize / 2)
        // | tree when expectedSize = currentSize -> tree
        // | _ -> failwith "Expected expectedSize or currentSize to be a power of two. "

    let rec binTreeGrower (tree: BinTree<'value>) expectedSize currentSize =
        if expectedSize <> currentSize then
            binTreeGrower (BinTree.Node(tree, BinTree.None)) expectedSize (currentSize * 2)
        else
            tree

    if vector.Length = matrix.Length1 then
       // Здесь нужно добавить, потому что вектор меньше,чем результирующие столбцы.
       if vector.SquareLength < matrix.SquareLength then
            let growTree =
                multiTrees (binTreeGrower vector.Storage matrix.SquareLength vector.SquareLength) matrix.Storage

            Vector(growTree, vector.Length, vector.SquareLength)

        // Здесь вообще ничего можно не делать.
        elif vector.SquareLength = matrix.SquareLength then
            // let cutTree =
                // binTreeCutter (multiTrees vector.Storage matrix.Storage) matrix.SquareLength vector.SquareLength

            Vector(multiTrees vector.Storage matrix.Storage, vector.Length, vector.SquareLength)

        // А здесь нужно отрезать. Но ты, кажется, режешь не ту разницу.
        // Нужно резать разницу между степенью двойки вектора и степенью двойки столбцов,
        // а у тебя получается, что длина вектора = количеству строк матрицы, она же и максимальна (твой toSquare возвращает max, если я правильно понял), поэтому ничего не режется.
        else
            let cutTree =
                binTreeCutter (multiTrees vector.Storage matrix.Storage) matrix.SquareLength vector.SquareLength

            Vector(cutTree, vector.Length, vector.SquareLength)
    else
        failwith
            "Multiplication operation is not defined.\n
         Expected vector.Length = matrix.Length1.\n
         Error in -multiplication- function. "
