module BreadthFirstSearch

open System
open SparseMatrix

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x


let toQuadTreeFromCOO (tripleList: (int * 'value option * int) list) (length1: int) (length2: int) =

    let toQuads (tripleList: (int * 'value option * int) list) (size: int) =
        let fQuad1 triple =
            (first triple) < size && (third triple) < size

        let fQuad2 triple =
            (first triple) < size && (third triple) >= size

        let fQuad3 triple =
            (first triple) >= size && (third triple) < size

        let fQuad4 triple =
            (first triple) >= size && (third triple) >= size

        let firstQuadList = List.filter fQuad1 tripleList

        let secondQuadList = List.filter fQuad2 tripleList
        let secondQuadList' = List.map (fun (x, y, z) -> (x, y, z - size)) secondQuadList

        let thirdQuadList = List.filter fQuad3 tripleList
        let thirdQuadList' = List.map (fun (x, y, z) -> (x - size, y, z)) thirdQuadList

        let forthQuadList = List.filter fQuad4 tripleList

        let forthQuadList' =
            List.map (fun (x, y, z) -> (x - size, y, z - size)) forthQuadList

        firstQuadList, secondQuadList', thirdQuadList', forthQuadList'

    let leafFormation a =
        match a with
        | [] -> QuadTree.None
        | [ _, Option.None, _ ] -> QuadTree.None
        | [ _, Some x, _ ] -> QuadTree.Leaf x
        | _ ->
            failwith
                "Expected a.Length = 1.\n
                          Error in -fromCOOToQuadTree- function. "

    let rec nodeOrganization (quad: (int * 'value option * int) list) size =
        if size = 1 then
            leafFormation quad
        else
            let one, two, three, four = toQuads quad (size / 2)

            let one' = nodeOrganization one (size / 2)
            let two' = nodeOrganization two (size / 2)
            let three' = nodeOrganization three (size / 2)
            let four' = nodeOrganization four (size / 2)

            if one' = QuadTree.None && two' = QuadTree.None && three' = QuadTree.None && four' = QuadTree.None then
                QuadTree.None
            else
                QuadTree.Node(one', two', three', four')


    if tripleList.Length = 1 then
        leafFormation tripleList
    elif tripleList.Length = 0 then
        QuadTree.None
    else
        let squareLength =
            int (2.0 ** ceil (max (Math.Log(length1, 2)) (Math.Log(length2, 2))))

        let one, two, three, four = toQuads tripleList (squareLength / 2)
        QuadTree.Node(nodeOrganization one (squareLength / 2), nodeOrganization two (squareLength / 2), nodeOrganization three (squareLength / 2), nodeOrganization four (squareLength / 2))




// 2. Реализовать алгоритм обхода в ширину, используя матрично-векторные операции.
// Не забыть про то, что алгоритм обхода в ширину —- это алгоритм обхода графа,
// а не просто набор операций над матрицами и векторами.

// 3. Добавить тесты как на отдельные операции, так и на обход в ширину целиком.

// [(4, 3, 3)
//  (1, 2, 3)
//  (1, 2, 4)
//  (5, 5 ,7)
//  (9, 9, 9)]

//0 1 2 3 4 5 6 7 8| 9 10 11 .. 16
//1     * *       |
//2
//3
//4       *
//5             * |
//6
//7
//8
//-------------------
//9               | *
(* Node
  (
   Node(Node (None, Leaf 2, None, None),
        Node (None, Leaf 2, None, None),
        Node (None, None, None, Leaf 3),
        Node (None, None, None, Leaf 5)),

   Node (None, None, None, None),

   Node (None, None, None, None),

   Node (None,
         None,
         None,
         Node (None, None, None, Leaf 9)))
   )



   Node
  (
  Node
     (Node (Leaf 2, None, None, None),
      Node (Leaf 2, None, None, None),
      Node (Leaf 3, None, None, None),
      Node (Leaf 5, None, None, None)),

   Node (None, None, None, None),

   ode (None, None, None, None),

   Node (None, None, None, Node (Leaf 9, None, None, None)))
*)
