module BreadthFirstSearchTests

open Expecto

module TreeTests =
    open SparseVector
    open BreadthFirstSearch

    [<Tests>]
    let tests =
        testList
            "toBinTree tests"
            [   testCase "toBinTree random list"
                <| fun _ ->
                    let length = 5
                    let list = [2;4]

                    let actualResult = (toBinTreeFromCOO list length).Storage
                    let expectedResult = (Node (Node (None, Node (Leaf true, None)), Node (Node (Leaf true, None), None)))

                    Expect.equal actualResult expectedResult "Unexpected result. "

                testCase "toBinTree empty list"
                <| fun _ ->
                    let length = 5
                    let list = []

                    let actualResult = (toBinTreeFromCOO list length).Storage
                    let expectedResult = BinTree.None

                    Expect.equal actualResult expectedResult "Unexpected result. "

                testProperty "toBinTree property list"
                <| fun (list : list<int>) (length : int) ->

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

