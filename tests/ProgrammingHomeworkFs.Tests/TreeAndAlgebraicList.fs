namespace TreeAndAlgebraicList

open AlgebraicList
open Tree
open Expecto

module TreeTests =
    [<Tests>]
    let tests =
        testList
            "Tree and AlgebraicList tests"
            [ testList
                  "uniqueValues tests"
                  [ testCase "uniqueValues simple test -- random int tree"
                    <| fun _ ->
                        let actualResult =
                            uniqueValues (Node(1, [ Node(1, [ Node(5, [ Node(0, []) ]); Node(8, []) ]) ]))

                        Expect.equal actualResult 4 "Node(1, [Node(1,[Node(5, [Leaf(0)]); Leaf(8)])]) has 4 unique values "

                    testCase "uniqueValues simple test -- random string tree"
                    <| fun _ ->
                        let actualResult =
                            uniqueValues (Node("abc", [ Node("90", [ Node("abc", [ Node("90", []) ]) ]) ]))

                        Expect.equal actualResult 2 "Node('abc', [Node('90',[Node('abc', [Leaf('90')])])]) has 2 unique values "

                    testCase "uniqueValues simple test -- one Node tree"
                    <| fun _ ->
                        let actualResult = uniqueValues (Node(1, [ Node(5, []) ]))

                        Expect.equal actualResult 2 "Node(1,[Leaf(5)]) has 2 unique values "

                    testCase "uniqueValues simple test -- one Leaf tree"
                    <| fun _ ->
                        let actualResult = uniqueValues (Node(9000, []))

                        Expect.equal actualResult 1 "Node(Leaf(9000)) has 1 unique value "

                    testProperty "uniqueValues property test -- 'int tree property by number of elements "
                    <| fun (tree: Tree<int>) -> Expect.isLessThanOrEqual <| uniqueValues tree <| len (toList tree) <| "Unexpected result"

                    testProperty "uniqueValues property test -- 'string tree property by number of elements "
                    <| fun (tree: Tree<string>) -> Expect.isLessThanOrEqual <| uniqueValues tree <| len (toList tree) <| "Unexpected result"

                    testProperty "uniqueValues property test -- 'int tree property by non-zero number of elements "
                    <| fun (tree: Tree<int>) -> Expect.notEqual <| uniqueValues tree <| 0 <| "Unexpected result"

                    testCase "toList simple test -- random string tree"
                    <| fun _ ->
                        let actualResult =
                            quickSort (toList (Node("abc", [ Node("90", [ Node("abc", [ Node("90", []) ]) ]) ])))

                        Expect.equal
                            actualResult
                            (Construct("90", Construct("90", Construct("abc", Construct("abc", Empty)))))
                            "Expression has same result as Construct('90',Construct('90', Construct('abc', Construct('abc', Empty)))) "

                    testCase "toList simple test -- random float tree"
                    <| fun _ ->
                        let actualResult =
                            quickSort (toList (Node(0.12, [ Node(999, [ Node(19.9, [ Node(0, []) ]) ]) ])))

                        Expect.equal
                            actualResult
                            (Construct(0, Construct(0.12, Construct(19.9, Construct(999, Empty)))))
                            "Expression has same result as Construct(0,Construct(0.12, Construct(19.9, Construct(999, Empty)))) "

                    testCase "toList simple test -- one Node tree"
                    <| fun _ ->
                        let actualResult = toList (Node(-5, [ Node(-5, []) ]))

                        Expect.equal actualResult (Construct(-5, Construct(-5, Empty))) "Expression has same result as Construct(-5, Construct(-5, Empty)) "

                    testCase "toList simple test -- one Leaf tree"
                    <| fun _ ->
                        let actualResult = toList (Node(0, []))

                        Expect.equal actualResult (Construct(0, Empty)) "Expression has same result as Construct(0, Empty) "

                    testProperty "toList property test -- 'int tree property by number of elements "
                    <| fun (tree: Tree<int>) -> Expect.isGreaterThanOrEqual <| len (toList tree) <| uniqueValues tree <| "Unexpected result"

                    testProperty "toList property test -- 'string tree property by number of elements "
                    <| fun (tree: Tree<string>) -> Expect.isGreaterThanOrEqual <| len (toList tree) <| uniqueValues tree <| "Unexpected result" ] ]
