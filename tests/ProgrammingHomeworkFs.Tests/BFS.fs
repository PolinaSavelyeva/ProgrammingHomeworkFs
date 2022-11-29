module BreadthFirstSearchTests

open Expecto
open BreadthFirstSearch
open SparseMatrix

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
                                    QuadTree.Node(None, QuadTree.Node(None, None, None, Leaf 2), None, None),
                                    QuadTree.Node(QuadTree.Node(None, None, Leaf 2, None), None, None, None),
                                    QuadTree.Node(None, QuadTree.Node(None, Leaf 3, None, None), None, None),
                                    QuadTree.Node(None, QuadTree.Node(None, None, None, Leaf 5), None, None)
                                ),
                                None,
                                None,
                                QuadTree.Node(QuadTree.Node(QuadTree.Node(None, None, None, Leaf 9), None, None, None), None, None, None)
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
                                QuadTree.Node(None, None, None, Leaf 9),
                                QuadTree.Node(None, None, Leaf 2, Leaf 3),
                                QuadTree.Node(None, None, None, Leaf 5),
                                QuadTree.Node(Leaf 2, None, None, None)
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

                        Expect.equal actualResult expectedResult "Unexpected result. " ] ]
