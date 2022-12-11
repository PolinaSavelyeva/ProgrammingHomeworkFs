module BreadthFirstSearchTests

open Expecto


module VectorTests =
    open SparseVector
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "toBinTree tests"
            [ testCase "toBinTreeFromCOO random list"
              <| fun _ ->
                  let length = 5
                  let list = [ 2; 4 ]

                  let actualResult = (toBinTreeFromCOO list length).Storage

                  let expectedResult =
                      (Node(Node(None, Node(Leaf true, None)), Node(Node(Leaf true, None), None)))

                  Expect.equal actualResult expectedResult "Unexpected result. "

              testCase "toBinTreeFromCOO empty list"
              <| fun _ ->
                  let length = 5
                  let list = []

                  let actualResult = (toBinTreeFromCOO list length).Storage
                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult "Unexpected result. "

              testProperty "toBinTreeFromCOO property list"
              <| fun (list: list<int>) (length: int) ->

                  let length' = abs length + 1
                  let list' = List.distinct (list |> List.map (fun n -> (abs n) % length'))

                  let simpleFormation list length =
                      let newArray = Array.create length Option.None

                      for i in 0 .. List.length list - 1 do
                          newArray[list[i]] <- Some true

                      newArray

                  let actualResult = (toBinTreeFromCOO list' length').Storage
                  let expectedResult = toBinTree (simpleFormation list' length')

                  Expect.equal actualResult expectedResult "Unexpected result. "

              ]

module MatrixTests =
    open SparseMatrix
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "toQuadTree tests"
            [ testCase "toQuadTreeFromCOO random list"
              <| fun _ ->
                  let rows = 4
                  let columns = 4
                  let length = 5

                  let list =
                      [ (0, 0, Some 9); (1, 3, Some 10); (1, 1, Some 0); (2, 2, Some -9); (3, 3, Some 10) ]

                  let actualResult = (toQuadTreeFromCOO list rows columns length).Storage

                  let expectedResult =
                      Node(Node(Leaf(Some 9), None, None, Leaf(Some 0)), Node(None, None, None, Leaf(Some 10)), None, Node(Leaf(Some -9), None, None, Leaf(Some 10)))

                  Expect.equal actualResult expectedResult "Unexpected result. "


              testProperty "toQuadTreeFromCOO property list"
              <| fun (list: (int * int) list) (length: int) ->

                  let length' = abs length + 1

                  let list' =
                      List.distinct (list |> List.map (fun (x, y) -> ((abs x) % length', (abs y) % length')))
                      |> List.map (fun (x, y) -> (x, y, Some 100))

                  let simpleFormation list length =
                      let new2DArray = Array2D.create length length Option.None

                      for i in 0 .. List.length list - 1 do
                          new2DArray[first list[i], second list[i]] <- Some(third list[i])

                      new2DArray

                  let actualResult = (toQuadTreeFromCOO list' length' length' length').Storage
                  let expectedResult = toQuadTree (simpleFormation list' length')

                  Expect.equal actualResult expectedResult "Unexpected result. "

              ]
