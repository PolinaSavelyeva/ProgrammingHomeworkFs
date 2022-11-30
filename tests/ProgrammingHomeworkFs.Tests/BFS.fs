module BreadthFirstSearchTests

open Expecto
open BreadthFirstSearch
open SparseMatrix
open SparseVector

module TreeTests =
    [<Tests>]
    let tests =
        testList
            "Fronts tests"
            [ testList
                  "fromCOOToQuadTree tests"
                  [ testCase "fromCOOToQuadTree random list 1"
                    <| fun _ ->
                        let size = 16

                        let list =
                            [ (4, Some 3, 3); (1, Some 2, 3); (1, Some 2, 4); (5, Some 5, 7); (9, Some 9, 9) ]

                        let actualResult = toQuadTreeFromCOO list size size

                        let expectedResult =
                            QuadTree.Node(
                                QuadTree.Node(
                                    QuadTree.Node(QuadTree.None, QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf 2), QuadTree.None, QuadTree.None),
                                    QuadTree.Node(QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.Leaf 2, QuadTree.None), QuadTree.None, QuadTree.None, QuadTree.None),
                                    QuadTree.Node(QuadTree.None, QuadTree.Node(QuadTree.None, QuadTree.Leaf 3, QuadTree.None, QuadTree.None), QuadTree.None, QuadTree.None),
                                    QuadTree.Node(QuadTree.None, QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf 5), QuadTree.None, QuadTree.None)
                                ),
                                QuadTree.None,
                                QuadTree.None,
                                QuadTree.Node(
                                    QuadTree.Node(QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf 9), QuadTree.None, QuadTree.None, QuadTree.None),
                                    QuadTree.None,
                                    QuadTree.None,
                                    QuadTree.None
                                )
                            )

                        Expect.equal actualResult expectedResult "Unexpected result. "
                    testCase "fromCOOToQuadTree random list 2"
                    <| fun _ ->
                        let size = 4

                        let list =
                            [ (1, Some 3, 3); (1, Some 2, 2); (2, Some 2, 2); (3, Some 5, 1); (1, Some 9, 1) ]

                        let actualResult = toQuadTreeFromCOO list size size

                        let expectedResult =
                            QuadTree.Node(
                                QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf 9),
                                QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.Leaf 2, QuadTree.Leaf 3),
                                QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf 5),
                                QuadTree.Node(QuadTree.Leaf 2, QuadTree.None, QuadTree.None, QuadTree.None)
                            )

                        Expect.equal actualResult expectedResult "Unexpected result. "
                    testCase "fromCOOToQuadTree empty list"
                    <| fun _ ->
                        let size = 10000
                        let list = []
                        let actualResult = toQuadTreeFromCOO list size size
                        let expectedResult = QuadTree.None

                        Expect.equal actualResult expectedResult "Unexpected result. "

                    testCase "fromCOOToQuadTree one element list"
                    <| fun _ ->
                        let size = 1
                        let lst = [ (0, Some 1, 0) ]
                        let actualResult = toQuadTreeFromCOO lst size size
                        let expectedResult = QuadTree.Leaf 1

                        Expect.equal actualResult expectedResult "Unexpected result. "

                    testCase "BFS random"
                    <| fun _ ->
                        let size = 16
                        let list = [ (1, Some 3, 3); (1, Some 2, 2); (3, Some 5, 5); (1, Some 9, 4) ]
                        let gMatrix = Matrix(toQuadTreeFromCOO list size size, size, size, size)
                        let start = [ 1 ]
                        let actualResult = BFS start gMatrix

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


                        Expect.equal actualResult.Storage (toBinTree expectedResult) "Unexpected result. " ] ]
