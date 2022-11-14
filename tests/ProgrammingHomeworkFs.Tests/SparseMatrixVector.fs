module SparseMatrixVector

open Expecto

module SparseVectorTests =
    open SparseVector

    [<Tests>]
    let tests =
        testList
            "SparseVector tests"
            [ testCase "toSquare random array"
              <| fun _ ->
                  let arr =
                      toSquare [| Some(1); Some(2); Some(3); Some(4); Some(5); Some(6); Some(7); Some(8); Some(9) |]

                  Expect.equal arr 16 "toSquare expected : 16"
              testCase "toSquare one element array"
              <| fun _ ->
                  let arr = toSquare [| Some(1) |]
                  Expect.equal arr 1 "toSquare expected : 1"
              testCase "toSquare empty array"
              <| fun _ ->
                  let arr = toSquare [||]
                  Expect.equal arr 0 "toSquare expected : 0"
              testProperty "toSquare property test array"
              <| fun (arr: int option[]) ->
                  Expect.isLessThanOrEqual
                  <| arr.Length
                  <| toSquare arr
                  <| "toSquare expected less than or equal array length result"
              testCase "toBinTree random array 1"
              <| fun _ ->
                  let tree = toBinTree [| Some(1); Some(1); Option.None; Option.None |]
                  Expect.equal tree (Node(Node(Leaf(1), Leaf(1)), BinTree.None)) "toBinTree expected : (Node(Node(Leaf(1), Leaf(1)), BinTree.None))"
              testCase "toBinTree string array"
              <| fun _ ->
                  let tree = toBinTree [| Some("qq"); Some("qd") |]
                  Expect.equal tree (Node(Leaf("qq"), Leaf("qd"))) "toBinTree expected : (Node(Node(Leaf(1), Leaf(1)), BinTree.None))"
              testCase "toBinTree None array"
              <| fun _ ->
                  let tree = toBinTree [| Option.None |]
                  Expect.equal tree BinTree.None "toBinTree expected : BinTree.None"
              testCase "toBinTree random array 2"
              <| fun _ ->
                  let tree =
                      toBinTree [| Some(1); Some(1); Option.None; Option.None; Some(1); Some(1); Option.None; Option.None; Some(1); Some(1); Option.None; Option.None |]

                  Expect.equal
                      tree
                      (Node(Node(Node(Node(Leaf 1, Leaf 1), None), Node(Node(Leaf 1, Leaf 1), None)), Node(Node(Node(Leaf 1, Leaf 1), None), None)))
                      "toBinTree expected : (Node (Node (Node (Node (Leaf 1, Leaf 1), None), Node (Node (Leaf 1, Leaf 1), None)), Node (Node (Node (Leaf 1, Leaf 1), None), None)))"
              testProperty "takeElementOfVector property test"
              <| fun (arr: int option[]) (i: uint) ->
                  let arr' = Array.append arr [| Some(1) |]
                  let i' = int i % arr'.Length

                  Expect.equal
                  <| arr'[int i']
                  <| takeElementOfVector (int i') (Vector arr')
                  <| "takeElementOfVector expected same result as Array.get"
              testCase "takeElementOfVector empty array"
              <| fun _ ->
                  let actualResult =
                      Expect.throws (fun _ -> takeElementOfVector 152 (Vector [||]) |> ignore) "Index out of the range. Error in -takeElementOfVector- function."

                  actualResult ]

module SparseMatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "SparseMatrix tests"
            [ testCase "toSquare random array2D 1"
              <| fun _ ->
                  let arr =
                      toSquare (array2D [ [ Some(1); Some(2); Some(3) ]; [ Some(4); Some(5); Some(6) ]; [ Some(7); Some(8); Some(9) ] ])

                  Expect.equal arr 4 "toSquare expected : 4"
              testCase "toSquare random array2D 2"
              <| fun _ ->
                  let arr =
                      toSquare (
                          array2D
                              [ [ Some(1); Some(2); Some(3) ]
                                [ Some(4); Some(5); Some(6) ]
                                [ Some(7); Some(8); Some(9) ]
                                [ Some(1); Some(2); Some(3) ]
                                [ Some(1); Some(2); Some(3) ]
                                [ Some(1); Some(2); Some(3) ] ]
                      )

                  Expect.equal arr 8 "toSquare expected : 8"
              testCase "toSquare one element array2D"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1) ] ])
                  Expect.equal arr 1 "toSquare expected : 1"
              testCase "toSquare random array2D 3"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1) ]; [ Some(1) ] ])
                  Expect.equal arr 2 "toSquare expected : 2"
              testCase "toSquare random array2D 4"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1); Some(0) ] ])
                  Expect.equal arr 2 "toSquare expected : 2"
              testProperty "toSquare property test array2D"
              <| fun (arr: int option[,]) ->
                  Expect.isLessThanOrEqual
                  <| max (Array2D.length1 arr) (Array2D.length2 arr)
                  <| toSquare arr
                  <| "toSquare expected less than or equal array length result"
              testCase "toQuadTree random array2D 1"
              <| fun _ ->
                  let tree = toQuadTree (array2D [ [ Some(1) ]; [ Some(1) ] ])
                  Expect.equal tree (Node(Leaf 1, None, Leaf 1, None)) "toQuadTree expected : (Node (Leaf 1, None, Leaf 1, None)) "
              testCase "toQuadTree empty array"
              <| fun _ ->
                  let tree = toQuadTree (array2D [ [] ])
                  Expect.equal tree QuadTree.None "toQuadTree expected : QuadTree.None"
              testCase "toQuadTree random array2D 2"
              <| fun _ ->
                  let tree =
                      toQuadTree (array2D [ [ Some(1); Some(1) ]; [ Option.None; Option.None ]; [ Some(1); Some(1) ] ])

                  Expect.equal
                      tree
                      (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))
                      "toQuadTree expected : (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))"
              testProperty "takeElementOfMatrix property test"
              <| fun (arr: int option[,]) (i: uint) (j: uint) ->
                  let arr' =
                      if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
                          (array2D [ [ Some(1); Some(2) ]; [ Some(2); Some(2) ] ])
                      else
                          arr

                  let i' = int i % (Array2D.length1 arr')
                  let j' = int j % (Array2D.length2 arr')

                  Expect.equal
                  <| arr'[int i', int j']
                  <| takeElementOfMatrix (int i') (int j') (Matrix arr')
                  <| "Unexpected result"
              testCase "takeElementOfMatrix empty array"
              <| fun _ ->
                  let actualResult =
                      Expect.throws (fun _ -> takeElementOfMatrix 152 11 (Matrix(array2D [ []; [] ])) |> ignore) "Index out of the range. Error in -takeElementOfVector- function."

                  actualResult ]

module MatrixMultiplicationTests =

    open MatrixMultiplication
    open SparseVector
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "MatrixMultiplication tests"
            [ testCase "MatrixMultiplication random vector and matrix"
              <| fun _ ->
                  let vec = Vector([| Some(0); Some(1) |])
                  let mat = Matrix(array2D [ [ Some(1); Some(1) ]; [ Some(1); Some(1) ] ])
                  let res = multiplication (+) (*) vec mat
                  Expect.equal res.Storage (BinTree.Node(BinTree.Leaf(1), BinTree.Leaf(1))) "MatrixMultiplication expected : Node (Leaf 1, Leaf 1)"
              testCase "MatrixMultiplication empty vector and matrix"
              <| fun _ ->
                  let vec = Vector([||])
                  let mat = Matrix(array2D [])
                  let res = multiplication (+) (*) vec mat
                  Expect.equal res.Storage BinTree.None "MatrixMultiplication expected : BinTree.None" ]
