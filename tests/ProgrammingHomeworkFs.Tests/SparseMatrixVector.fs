module SparseMatrixVector

open Expecto

module SparseVectorTests =
    open SparseVector
    [<Tests>]
    let tests =
        testList
            "SparseVector tests"
            [ testCase "16"
              <| fun _ ->
                  let arr = toSquare [|Some(1);Some(2);Some(3);Some(4);Some(5);Some(6);Some(7);Some(8);Some(9)|]
                  Expect.equal arr 16 "16"
              testCase "1"
              <| fun _ ->
                  let arr = toSquare [|Some(1)|]
                  Expect.equal arr 1 "1"
              testCase "0"
              <| fun _ ->
                  let arr = toSquare [||]
                  Expect.equal arr 0 "0"
              testProperty "prop"
              <|  fun (arr : int option[]) ->
                  Expect.isLessThanOrEqual <|  arr.Length  <| toSquare arr<| "Unexpected result"
              testCase "toBinTree"
              <| fun _ ->
                  let tree = toBinTree [|Some(1);Some(1);Option.None; Option.None|]
                  Expect.equal tree (Node(Node(Leaf(1), Leaf(1)), BinTree.None)) "16"
              testCase "toBinTree 2"
              <| fun _ ->
                  let tree = toBinTree [|Some("qq");Some("qd")|]
                  Expect.equal tree (Node(Leaf("qq"), Leaf("qd"))) "16"
              testCase "toBinTree 3"
              <| fun _ ->
                  let tree = toBinTree [|Option.None|]
                  Expect.equal tree (BinTree.None) "16"
              testCase "toBinTree 4"
              <| fun _ ->
                  let tree = toBinTree [|Some(1);Some(1);Option.None; Option.None;Some(1);Some(1);Option.None; Option.None;Some(1);Some(1);Option.None; Option.None|]
                  Expect.equal tree (Node (Node (Node (Node (Leaf 1, Leaf 1), None), Node (Node (Leaf 1, Leaf 1), None)), Node (Node (Node (Leaf 1, Leaf 1), None), None))) "f"
              testProperty "take prop"
              <|  fun (arr : int option[]) (i : uint) ->
                   let arr' = Array.append arr [|Some(1)|]
                   let i' = int i % arr'.Length
                   Expect.equal <|  arr'[int i']  <| takeElementOfVector (int i') (Vector arr')<| "Unexpected result"
              testCase "null arr"
              <| fun _ ->
                  let actualResult =
                      Expect.throws
                          (fun _ -> takeElementOfVector 152 (Vector [||]) |> ignore)
                          "Index out of the range. Error in -takeElementOfVector- function."
                  actualResult
            ]
module SparseMatrixTests =
    open SparseMatrix
    [<Tests>]
    let tests =
        testList
            "SparseMatrix tests"
            [ testCase "16"
              <| fun _ ->
                  let arr = toSquare (array2D [ [Some(1);Some(2);Some(3)]; [Some(4);Some(5);Some(6)]; [Some(7);Some(8);Some(9)] ])
                  Expect.equal arr 4 "4"
              testCase "116"
              <| fun _ ->
                  let arr = toSquare (array2D [ [Some(1);Some(2);Some(3)]; [Some(4);Some(5);Some(6)]; [Some(7);Some(8);Some(9)]; [Some(1);Some(2);Some(3)];[Some(1);Some(2);Some(3)];[Some(1);Some(2);Some(3)]; ])
                  Expect.equal arr 8 "8"
              testCase "1"
              <| fun _ ->
                  let arr = toSquare (array2D [[Some(1)]])
                  Expect.equal arr 1 "1"
              testCase "11"
              <| fun _ ->
                  let arr = toSquare (array2D [[Some(1)]; [Some(1)]])
                  Expect.equal arr 2 "1"
              testCase "111"
              <| fun _ ->
                  let arr = toSquare (array2D [[Some(1); Some(0)]])
                  Expect.equal arr 2 "1"
              testProperty "prop"
              <|  fun (arr : int option[,]) ->
                  Expect.isLessThanOrEqual <|  max (Array2D.length1 arr) (Array2D.length2 arr) <| toSquare arr <| "Unexpected result"
            ]
