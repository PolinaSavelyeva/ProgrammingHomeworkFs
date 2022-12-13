module BreadthFirstSearchTests

open Expecto
open SparseVector

module MatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "Matrix operations tests"
            [ testCase "toQuadTreeFromCOO random list test"
              <| fun _ ->

                  let rows = 4
                  let columns = 4
                  let length = 5

                  let list =
                      [ (0, 0, Some 9); (1, 3, Some 10); (1, 1, Some 0); (2, 2, Some -9); (3, 3, Some 10) ]

                  let actualResult = toQuadTreeFromCOO list rows columns length

                  let expectedResult =
                      Node(Node(Leaf 9, None, None, Leaf 0), Node(None, None, None, Leaf 10), None, Node(Leaf -9, None, None, Leaf 10))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "


              testProperty "toQuadTreeFromCOO property test"
              <| fun (list: (int * int) list) (length: int) ->

                  let length' = abs length + 1

                  let list' =
                      List.distinct (list |> List.map (fun (x, y) -> ((abs x) % length', (abs y) % length')))
                      |> List.map (fun (x, y) -> (x, y, Some 100))

                  let naiveFormation list length =
                      let new2DArray = Array2D.create length length Option.None

                      for i in 0 .. List.length list - 1 do
                          new2DArray[first list[i], second list[i]] <- third list[i]

                      new2DArray

                  let actualResult = toQuadTreeFromCOO list' length' length' length'
                  let expectedResult = naiveFormation list' length' |> toQuadTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. " ]

module BFSTests =
    open SparseMatrix
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "BFS tests"
            [ testCase "BFS random test"
              <| fun _ ->

                  let length = 16
                  let tripleList = [ (1, 3, Some 3); (1, 2, Some 2); (3, 5, Some 5); (1, 4, Some 9) ]
                  let gMatrix = Matrix(tripleList, length, length, length)
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

              testCase "BFS empty list test"
              <| fun _ ->

                  let length = 14
                  let tripleList = [ (1, 1, Some 3); (11, 2, Some 2); (0, 5, Some 5); (0, 4, Some 9) ]
                  let gMatrix = Matrix(tripleList, length, length, length)
                  let startVertexList = []

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testCase "BFS all vertexes list test"
              <| fun _ ->

                  let length = 4
                  let tripleList = [ (0, 1, Some 3); (1, 2, Some 2); (0, 0, Some 5); (1, 1, Some 9) ]
                  let gMatrix = Matrix(tripleList, length, length, length)
                  let startVertexList = [ 0; 1; 2; 3 ]

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult =
                      BinTree.Node(BinTree.Node(BinTree.Leaf 0, BinTree.Leaf 0), BinTree.Node(BinTree.Leaf 0, BinTree.Leaf 0))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testCase "BFS empty graph test"
              <| fun _ ->

                  let length = 0
                  let tripleList = []
                  let gMatrix = Matrix(tripleList, length, length, length)
                  let startVertexList = []

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "BFS property test graph with only loops"
              <| fun (tripleList: (int * int) list) (vertexList: list<int>) (length: int) ->

                  let length' = abs length + 1

                  let tripleList' =
                      List.distinct (tripleList |> List.map (fun (x, _) -> ((abs x) % length', (abs x) % length')))
                      |> List.map (fun (x, _) -> (x, x, Some 100))

                  let vertexList' =
                      List.distinct (vertexList |> List.map (fun n -> (abs n) % length'))

                  let graphMatrix = Matrix(tripleList', length', length', length')

                  let actualResult = (graphMatrix |> BFS vertexList').Storage

                  let expectedResult =
                      Array.init graphMatrix.Length1 (fun n -> if List.contains n vertexList' then Some 0 else Option.None)
                      |> toBinTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "BFS property test naive bfs"
              <| fun (tripleList: (int * int) list) (vertexList: list<int>) (length: int) ->

                  let length' = abs length + 1

                  let tripleList' =
                      List.distinct (tripleList |> List.map (fun (x, _) -> ((abs x) % length', (abs x) % length')))
                      |> List.map (fun (x, _) -> (x, x, Some 100))

                  let vertexList' =
                      List.distinct (vertexList |> List.map (fun n -> (abs n) % length'))

                  let graphMatrix = Matrix(tripleList', length', length', length')

                  let naiveBFS (vertexList: list<int>) (graphMatrix: Matrix<'value>) =

                      let addToQueue queue vertex iterationNumber =
                          let rec inner list counter =
                              if counter < 0 then
                                  list
                              else if graphMatrix[vertex, counter] = Option.None then
                                  inner list (counter - 1)
                              else
                                  inner (list @ [ counter, iterationNumber ]) (counter - 1)

                          inner queue (graphMatrix.Length2 - 1)

                      let rec inner queue result visited =
                          match queue with
                          | [] -> result
                          | vertex, iter as hd :: tl ->
                              if List.contains vertex visited then
                                  inner tl result visited
                              else
                                  let visited = vertex :: visited
                                  let newQ = addToQueue tl vertex (iter + 1)
                                  inner newQ (result @ [ hd ]) visited

                      let queue = vertexList |> List.map (fun x -> (x, 0))
                      if queue.IsEmpty then [] else inner queue [] []

                  let toArray (list: (int * int) list) length =
                      let ans = Array.create length Option.None

                      for i in 0 .. list.Length - 1 do
                          ans[fst list[i]] <- Some(snd list[i])

                      ans

                  let actualResult = (graphMatrix |> BFS vertexList').Storage

                  let expectedResult = toArray (naiveBFS vertexList' graphMatrix) length' |> toBinTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. " ]
