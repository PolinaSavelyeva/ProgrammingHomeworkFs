module BreadthFirstSearchTests

open Expecto


module VectorTests =
    open SparseVector
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "Vector operations tests"
            [ testCase "frontFormation random list test"
              <| fun _ ->
                  let length = 5
                  let list = [ 2; 4 ]

                  let actualResult = (frontFormation list length).Storage

                  let expectedResult =
                      Node(Node(None, Node(Leaf true, None)), Node(Node(Leaf true, None), None))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testCase "frontFormation empty list test"
              <| fun _ ->

                  let length = 5
                  let list = []

                  let actualResult = (frontFormation list length).Storage
                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "frontFormation property test"
              <| fun (list: list<int>) (length: int) ->

                  let length' = abs length + 1
                  let list' = List.distinct (list |> List.map (fun n -> (abs n) % length'))

                  let naiveFormation list length =
                      let newArray = Array.create length Option.None

                      for i in 0 .. List.length list - 1 do
                          newArray[list[i]] <- Some true

                      newArray

                  let actualResult = (frontFormation list' length').Storage
                  let expectedResult = toBinTree (naiveFormation list' length')

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "visitedFormation property test"
              <| fun (list: list<int>) (length: int) ->

                  let length' = abs length + 1
                  let list' = List.distinct (list |> List.map (fun n -> (abs n) % length'))

                  let naiveFormation list length =
                      let newArray = Array.create length Option.None

                      for i in 0 .. List.length list - 1 do
                          newArray[list[i]] <- Some 0

                      newArray

                  let actualResult = (visitedFormation (frontFormation list' length') 0).Storage
                  let expectedResult = toBinTree (naiveFormation list' length')

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              ]

module MatrixTests =
    open SparseMatrix
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "Matrix operations tests"
            [ testCase "matrixFormation random list test"
              <| fun _ ->

                  let rows = 4
                  let columns = 4
                  let length = 5

                  let list =
                      [ (0, 0, Some 9); (1, 3, Some 10); (1, 1, Some 0); (2, 2, Some -9); (3, 3, Some 10) ]

                  let actualResult = (matrixFormation list rows columns length).Storage

                  let expectedResult =
                      Node(Node(Leaf(Some 9), None, None, Leaf(Some 0)), Node(None, None, None, Leaf(Some 10)), None, Node(Leaf(Some -9), None, None, Leaf(Some 10)))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "


              testProperty "matrixFormation property test"
              <| fun (list: (int * int) list) (length: int) ->

                  let length' = abs length + 1

                  let list' =
                      List.distinct (list |> List.map (fun (x, y) -> ((abs x) % length', (abs y) % length')))
                      |> List.map (fun (x, y) -> (x, y, Some 100))

                  let naiveFormation list length =
                      let new2DArray = Array2D.create length length Option.None

                      for i in 0 .. List.length list - 1 do
                          new2DArray[first list[i], second list[i]] <- Some(third list[i])

                      new2DArray

                  let actualResult = (matrixFormation list' length' length' length').Storage
                  let expectedResult = naiveFormation list' length' |> toQuadTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. " ]

module BFSTests =
    open SparseVector
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "BFS tests"
            [ testCase "BFS random test"
              <| fun _ ->

                  let length = 16
                  let tripleList = [ (1, 3, Some 3); (1, 2, Some 2); (3, 5, Some 5); (1, 4, Some 9) ]
                  let gMatrix = matrixFormation tripleList length length length
                  let startVertexList = [ 1 ]

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult =
                      [| Option.None
                         Some 0
                         Some 1
                         Some 1
                         Some 1
                         Some 2
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None
                         Option.None |]
                      |> toBinTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "BFS property test"
              <| fun (tripleList: (int * int) list) (vertexList: list<int>) (length: int) ->

                  let length' = abs length + 1

                  let tripleList' =
                      List.distinct (tripleList |> List.map (fun (x, _) -> ((abs x) % length', (abs x) % length')))
                      |> List.map (fun (x, _) -> (x, x, Some 100))

                  let vertexList' =
                      List.distinct (vertexList |> List.map (fun n -> (abs n) % length'))

                  let actualResult =
                      (matrixFormation tripleList' length' length' length' |> BFS vertexList').Storage

                  let expectedResult =
                      (visitedFormation (frontFormation vertexList' length') 0).Storage

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. " ]
