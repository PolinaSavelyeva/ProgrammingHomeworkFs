module BreadthFirstSearchTests

open SparseVector
open Converters
open System.Collections.Generic
open Expecto

module MatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "Matrix operations tests"
            [ testCase "toQuadTreeFromCOO random list test"
              <| fun _ ->

                  let rows = 4u
                  let columns = 4u

                  let list =
                      [ (0u, 0u, Some 9); (1u, 3u, Some 10); (1u, 1u, Some 0); (2u, 2u, Some -9); (3u, 3u, Some 10) ]

                  let actualResult = toQuadTreeFromCOO list rows columns

                  let expectedResult =
                      Node(Node(Leaf 9, None, None, Leaf 0), Node(None, None, None, Leaf 10), None, Node(Leaf -9, None, None, Leaf 10))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "


              testProperty "toQuadTreeFromCOO property test without empty lists"
              <| fun (tupleList: list<uint * uint>) ->

                  let tripleList =
                      List.append tupleList [ (0u, 0u) ] |> List.distinct |> List.map (fun (x, y) -> (x, y, Some 100))

                  let triple = List.maxBy (fun z -> first z + second z) tripleList
                  let length = first triple + second triple + 1u

                  let naiveFormation list length =
                      let new2DArray = Array2D.create (toInt length) (toInt length) Option.None

                      for i in 0 .. List.length list - 1 do
                          new2DArray[first list[i] |> toInt, second list[i] |> toInt] <- third list[i]

                      new2DArray

                  let actualResult = toQuadTreeFromCOO tripleList length length
                  let expectedResult = naiveFormation tripleList length |> toQuadTree

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

                  let length = 16u

                  let tripleList =
                      [ (1u, 3u, Some 3); (1u, 2u, Some 2); (3u, 5u, Some 5); (1u, 4u, Some 9) ]

                  let gMatrix = Matrix(tripleList, length, length)
                  let startVertexList = [ 1u ]

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult =
                      [| Option.None
                         Some 0u
                         Some 1u
                         Some 1u
                         Some 1u
                         Some 2u
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

                  let length = 14u

                  let tripleList =
                      [ (1u, 1u, Some 3); (11u, 2u, Some 2); (0u, 5u, Some 5); (0u, 4u, Some 9) ]

                  let startVertexList = []

                  let gMatrix = Matrix(tripleList, length, length)

                  let actualResult = (BFS startVertexList gMatrix).Storage
                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testCase "BFS all vertexes list test"
              <| fun _ ->

                  let length = 4u

                  let tripleList =
                      [ (0u, 1u, Some 3); (1u, 2u, Some 2); (0u, 0u, Some 5); (1u, 1u, Some 9) ]

                  let startVertexList = [ 0u; 1u; 2u; 3u ]

                  let gMatrix = Matrix(tripleList, length, length)

                  let actualResult = (BFS startVertexList gMatrix).Storage

                  let expectedResult =
                      BinTree.Node(BinTree.Node(BinTree.Leaf 0u, BinTree.Leaf 0u), BinTree.Node(BinTree.Leaf 0u, BinTree.Leaf 0u))

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testCase "BFS empty graph test"
              <| fun _ ->

                  let length = 0u

                  let tripleList = []
                  let startVertexList = []

                  let gMatrix = Matrix(tripleList, length, length)

                  let actualResult = (BFS startVertexList gMatrix).Storage
                  let expectedResult = BinTree.None

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "BFS property test graph with only loops without empty lists"
              <| fun (tupleList: list<uint * uint>) (vertexList: list<uint>) ->

                  let newVertexList = List.append vertexList [ 0u ] |> List.distinct

                  let tripleList =
                      List.append tupleList [ (0u, 0u) ] |> List.distinct |> List.map (fun (x, _) -> (x, x, Some 100))

                  let triple = List.maxBy (fun z -> first z + second z) tripleList
                  let length = max (first triple + second triple) (List.max newVertexList) + 1u

                  let graphMatrix = Matrix(tripleList, length, length)

                  let actualResult = (graphMatrix |> BFS newVertexList).Storage

                  let expectedResult =
                      Array.init (toInt length) (fun n ->
                          if List.contains (uint n) newVertexList then
                              Some 0u
                          else
                              Option.None)
                      |> toBinTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. "

              testProperty "BFS property test naive bfs without empty lists"
              <| fun (tupleList: list<uint * uint>) (vertexList: list<uint>) ->

                  let newVertexList = List.append vertexList [ 0u ] |> List.distinct

                  let tripleList =
                      List.append tupleList [ (0u, 0u) ] |> List.distinct |> List.map (fun (x, _) -> (x, x, Some 100))

                  let triple = List.maxBy (fun z -> first z + second z) tripleList
                  let length = max (first triple + second triple) (List.max newVertexList) + 1u

                  let graphMatrix = Matrix(tripleList, length, length)

                  let naiveBFS (vertexList: list<uint>) (graphMatrix: Matrix<'value>) =

                      let rec answerFormation (queue: Queue<uint>) (ans: array<Option<uint>>) (current: uint) =
                          if queue.Count = 0 then
                              ans
                          else
                              let vertex = queue.Dequeue()

                              for j in 0 .. toInt graphMatrix.Length2 - 1 do
                                  if graphMatrix[vertex, uint j] <> Option.None then
                                      if ans[j] = Option.None then
                                          queue.Enqueue(uint j)
                                          ans[j] <- Some current

                              answerFormation queue ans (current + 1u)

                      let queue = Queue(vertexList)

                      let ans =
                          Array.init (toInt graphMatrix.Length1) (fun n ->
                              if List.contains (uint n) vertexList then
                                  Some 0u
                              else
                                  Option.None)

                      answerFormation queue ans 1u

                  let actualResult = (graphMatrix |> BFS newVertexList).Storage
                  let expectedResult = naiveBFS newVertexList graphMatrix |> toBinTree

                  Expect.equal actualResult expectedResult $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}. " ]
