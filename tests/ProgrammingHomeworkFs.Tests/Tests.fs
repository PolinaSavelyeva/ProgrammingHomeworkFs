namespace Tests

open Expecto

module AlgebraicListTests =
    open AlgebraicList

    [<Tests>]
    let tests =
        testList
            "Types and functions tests"
            [ testList
                  "bubbleSort tests"
                  [ testCase "bubbleSort test -- random List"
                    <| fun _ ->
                        let actualResult =
                            bubbleSort (Construct(48, Construct(0, Construct(0, Construct(734, Construct(-880, Construct(9, Empty)))))))

                        Expect.equal
                            actualResult
                            (Construct(-880, Construct(0, Construct(0, Construct(9, Construct(48, Construct(734, Empty)))))))
                            "bubbleSort (Construct(48, Construct(0, Construct(0, Construct(734, Construct(-880, Construct(9, Empty))))))) is (Construct(-880, Construct(0, Construct(0, Construct(9, Construct(48, Construct(734, Empty))))))) "

                    testCase "bubbleSort test -- same values"
                    <| fun _ ->
                        let actualResult = bubbleSort (Construct(0, Construct(0, Construct(0, Empty))))

                        Expect.equal
                            actualResult
                            (Construct(0, Construct(0, Construct(0, Empty))))
                            "bubbleSort (Construct(0, Construct(0, Construct(0, Empty)))) is (Construct(0, Construct(0, Construct(0, Empty)))) "

                    testCase "bubbleSort test -- List<string>"
                    <| fun _ ->
                        let actualResult =
                            bubbleSort (Construct("ab", Construct("abc", Construct("cdd", Empty))))

                        Expect.equal
                            actualResult
                            (Construct("ab", Construct("abc", Construct("cdd", Empty))))
                            "bubbleSort (Construct('ab', Construct('abc', Construct('cdd', Empty)))) is (Construct('ab', Construct('abc', Construct('cdd', Empty))))"

                    testCase "bubbleSort test -- List<float>"
                    <| fun _ ->
                        let actualResult =
                            bubbleSort (Construct(4.1, Construct(3.5, Construct(2.9, Empty))))

                        Expect.equal
                            actualResult
                            (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))
                            "bubbleSort (Construct(4.1, Construct( 3.5, Construct(2.9, Empty)))) is (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))"

                    testCase "bubbleSort test -- List<string> 2"
                    <| fun _ ->
                        let actualResult =
                            bubbleSort (Construct("4.1", Construct("3.5", Construct("2.9", Empty))))

                        Expect.equal
                            actualResult
                            (Construct("2.9", Construct("3.5", Construct("4.1", Empty))))
                            "bubbleSort (Construct('4.1', Construct('3.5', Construct('2.9', Empty)))) is  (Construct ('2.9', Construct ('3.5', Construct ('4.1', Empty))))"

                    testCase "bubbleSort test -- Empty"
                    <| fun _ ->
                        let actualResult = bubbleSort Empty
                        Expect.equal actualResult Empty "bubbleSort Empty is Empty"

                    testCase "bubbleSort test -- one value"
                    <| fun _ ->
                        let actualResult = bubbleSort (Construct(19.87, Empty))
                        Expect.equal actualResult (Construct(19.87, Empty)) "bubbleSort (Construct(19.87, Empty)) is (Construct(19.87, Empty))" ]
              testList
                  "quickSort tests"
                  [ testProperty "quickSort test -- List<int>"
                    <| fun (lst: List<int>) -> Expect.equal <| bubbleSort lst <| quickSort lst <| "Unexpected result"

                    testProperty "quickSort test -- List<string>"
                    <| fun (lst: List<string>) -> Expect.equal <| bubbleSort lst <| quickSort lst <| "Unexpected result" ]
              testList
                  "concat tests"
                  [ testCase "concat test -- List<string>"
                    <| fun _ ->
                        let actualResult = concat (Construct("ABCE", Empty)) (Construct("FG", Empty))

                        Expect.equal
                            actualResult
                            (Construct("ABCE", Construct("FG", Empty)))
                            "concat (Construct('ABCE', Empty)) and (Construct('FG', Empty)) is  (Construct('ABCE', Construct('FG', Empty))) "

                    testCase "concat test -- Empty"
                    <| fun _ ->
                        let actualResult = concat Empty Empty
                        Expect.equal actualResult Empty "concat Empty and Empty is Empty"

                    testCase "concat test -- Empty and non-Empty"
                    <| fun _ ->
                        let actualResult = concat Empty (Construct(1, Empty))
                        Expect.equal actualResult (Construct(1, Empty)) "concat Empty and (Construct(1, Empty)) is (Construct(1, Empty))"

                    testCase "concat test -- non-Empty and Empty"
                    <| fun _ ->
                        let actualResult = concat (Construct(1, Empty)) Empty
                        Expect.equal actualResult (Construct(1, Empty)) "concat (Construct(1, Empty)) and Empty is (Construct(1, Empty))"

                    testCase "concat test -- same Lists"
                    <| fun _ ->
                        let actualResult =
                            concat (Construct(1, Construct(2, Construct(3, Empty)))) (Construct(1, Construct(2, Construct(3, Empty))))

                        Expect.equal
                            actualResult
                            (Construct(1, Construct(2, Construct(3, Construct(1, Construct(2, Construct(3, Empty)))))))
                            "concat (Construct(1, Construct(2, Construct(3, Empty)))) and (Construct(1, Construct(2, Construct(3, Empty)))) is (Construct(1, Construct(2, Construct(3, Construct(1, Construct(2, Construct(3, Empty)))))))" ] ]

module OopListTests =
    open OopList

    [<Tests>]
    let tests =
        testList
            "Types and functions tests"
            [ testList
                  "bubbleSort MyOOPList tests"
                  [ testCase "bubbleSort test -- random int MyOOPList"
                    <| fun _ ->
                        let actualResult =
                            let lst = List(1, List(3, EmptyList()))
                            algebraicListOfIList (bubbleSort lst)

                        Expect.equal
                            actualResult
                            (AlgebraicList.Construct(1, AlgebraicList.Construct(3, AlgebraicList.Empty)))
                            "bubbleSort List(1, List(3, EmptyList())) as same as (AlgebraicList.Construct(1, AlgebraicList.Construct(3, Empty)))"

                    testCase "bubbleSort test -- random string MyOOPList"
                    <| fun _ ->
                        let actualResult =
                            let lst = List("18", List("1", EmptyList()))
                            algebraicListOfIList (bubbleSort lst)

                        Expect.equal
                            actualResult
                            (AlgebraicList.Construct("1", AlgebraicList.Construct("18", AlgebraicList.Empty)))
                            "bubbleSort List('18', List('1', EmptyList())) as same as (AlgebraicList.Construct('1', AlgebraicList.Construct('18', Empty))))" ]
              testList
                  "quickOOPSort MyOOPList tests"
                  [ testCase "quickOOPSort test -- random int MyOOPList"
                    <| fun _ ->
                        let actualResult =
                            let lst = List(1, List(3, EmptyList()))
                            algebraicListOfIList (quickSort lst)

                        Expect.equal
                            actualResult
                            (AlgebraicList.Construct(1, AlgebraicList.Construct(3, AlgebraicList.Empty)))
                            "quickOOPSort List(1, List(3, EmptyList())) as same as (AlgebraicList.Construct(1, AlgebraicList.Construct(3, Empty)))"

                    testCase "quickOOPSort test -- random string MyOOPList"
                    <| fun _ ->
                        let actualResult =
                            let lst = List("18", List("1", EmptyList()))
                            algebraicListOfIList (quickSort lst)

                        Expect.equal
                            actualResult
                            (AlgebraicList.Construct("1", AlgebraicList.Construct("18", AlgebraicList.Empty)))
                            "quickOOPSort List('18', List('1', EmptyList())) as same as (AlgebraicList.Construct('1', AlgebraicList.Construct('18', Empty))))" ]
              testList
                  "quickOOPSort MyOOPList property tests"
                  [ testProperty "quickSort test -- MyList<int>"
                    <| fun (lst: list<int>) -> Expect.equal <| List.sort lst <| listOfIList (bubbleSort (iListOfList lst)) <| "Unexpected result"

                    testProperty "quickSort test -- MyList<string>"
                    <| fun (lst: list<string>) -> Expect.equal <| List.sort lst <| listOfIList (bubbleSort (iListOfList lst)) <| "Unexpected result" ]
              testList
                  "concatOOP MyOOPList tests"
                  [ testCase "quickOOPSort test -- same string MyOOPLists"
                    <| fun _ ->
                        let actualResult =
                            let lst1 = List("18", List("1", EmptyList()))
                            let lst2 = List("18", List("1", EmptyList()))
                            algebraicListOfIList (concat lst1 lst2)

                        Expect.equal
                            actualResult
                            (AlgebraicList.Construct("18", AlgebraicList.Construct("1", AlgebraicList.Construct("18", AlgebraicList.Construct("1", AlgebraicList.Empty)))))
                            "concatOOP List('18', List('1', EmptyList())) and List('18', List('1', EmptyList())) as same as (AlgebraicList.Construct('18', AlgebraicList.Construct('1', AlgebraicList.Construct('18', AlgebraicList.Construct('1', Empty)))))"

                    testCase "quickOOPSort test -- same EmptyLists"
                    <| fun _ ->
                        let actualResult =
                            let lst1 = EmptyList()
                            let lst2 = EmptyList()
                            algebraicListOfIList (concat lst1 lst2)

                        Expect.equal actualResult AlgebraicList.Empty "concatOOP EmptyList() and EmptyList() as same as AlgebraicList.Empty" ] ]
