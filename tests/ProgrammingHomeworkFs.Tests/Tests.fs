namespace Tests

open OopType
open AlgebraicType
open Expecto

module SayTests =
    [<Tests>]
    let tests =
        testList "Types and functions tests" [
            testList "bubbleSort MyList tests" [
                testCase "bubbleSort test -- random MyList"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct(48, Construct(0, Construct(0, Construct(734, Construct(-880, Construct(9, Empty)))))))
                    Expect.equal actualResult (Construct(-880, Construct(0, Construct(0, Construct(9, Construct(48, Construct(734, Empty)))))))
                        "bubbleSort (Construct(48, Construct(0, Construct(0, Construct(734, Construct(-880, Construct(9, Empty))))))) is (Construct(-880, Construct(0, Construct(0, Construct(9, Construct(48, Construct(734, Empty))))))) "

                testCase "bubbleSort test -- same values"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct(0, Construct(0, Construct(0, Empty))))
                    Expect.equal actualResult (Construct(0, Construct(0, Construct(0, Empty))))
                        "bubbleSort (Construct(0, Construct(0, Construct(0, Empty)))) is (Construct(0, Construct(0, Construct(0, Empty)))) "

                testCase "bubbleSort test -- MyList<string>"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct("ab", Construct("abc", Construct("cdd", Empty))))
                    Expect.equal actualResult (Construct("ab", Construct("abc", Construct("cdd", Empty))))
                        "bubbleSort (Construct('ab', Construct('abc', Construct('cdd', Empty)))) is (Construct('ab', Construct('abc', Construct('cdd', Empty))))"

                testCase "bubbleSort test -- MyList<float>"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct(4.1, Construct(3.5, Construct(2.9, Empty))))
                    Expect.equal actualResult (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))
                        "bubbleSort (Construct(4.1, Construct( 3.5, Construct(2.9, Empty)))) is (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))"

                testCase "bubbleSort test -- MyList<string> 2"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct("4.1", Construct("3.5", Construct("2.9", Empty))))
                    Expect.equal actualResult (Construct("2.9", Construct("3.5", Construct("4.1", Empty))))
                        "bubbleSort (Construct('4.1', Construct('3.5', Construct('2.9', Empty)))) is  (Construct ('2.9', Construct ('3.5', Construct ('4.1', Empty))))"

                testCase "bubbleSort test -- Empty"
                <| fun _ ->
                    let actualResult =
                        bubbleSort Empty
                    Expect.equal actualResult Empty
                        "bubbleSort Empty is Empty"

                testCase "bubbleSort test -- one value"
                <| fun _ ->
                    let actualResult =
                        bubbleSort (Construct(19.87, Empty))
                    Expect.equal actualResult (Construct(19.87, Empty))
                        "bubbleSort (Construct(19.87, Empty)) is (Construct(19.87, Empty))"
            ]
            testList "quickSort MyList tests" [
                testProperty "quickSort test -- MyList<int>"
                <| fun (lst: MyList<int>) ->
                    Expect.equal <| bubbleSort lst <| quickSort lst <| "Unexpected result"

                testProperty "quickSort test -- MyList<string>"
                <| fun (lst: MyList<string>) ->
                    Expect.equal <| bubbleSort lst <| quickSort lst <| "Unexpected result"
            ]
            testList "concat MyList tests" [
                testCase "concat test -- MyList<string>"
                <| fun _ ->
                    let actualResult =
                        concat (Construct("ABCE", Empty)) (Construct("FG", Empty))
                    Expect.equal actualResult (Construct("ABCE", Construct("FG", Empty)))
                        "concat (Construct('ABCE', Empty)) and (Construct('FG', Empty)) is  (Construct('ABCE', Construct('FG', Empty))) "

                testCase "concat test -- Empty"
                <| fun _ ->
                    let actualResult =
                        concat Empty Empty
                    Expect.equal actualResult Empty
                        "concat Empty and Empty is Empty"

                testCase "concat test -- Empty and non-Empty"
                <| fun _ ->
                    let actualResult =
                        concat Empty (Construct(1, Empty))
                    Expect.equal actualResult (Construct(1, Empty))
                        "concat Empty and (Construct(1, Empty)) is (Construct(1, Empty))"

                testCase "concat test -- non-Empty and Empty"
                <| fun _ ->
                    let actualResult =
                        concat (Construct(1, Empty)) Empty
                    Expect.equal actualResult (Construct(1, Empty))
                        "concat (Construct(1, Empty)) and Empty is (Construct(1, Empty))"

                testCase "concat test -- same MyLists"
                <| fun _ ->
                    let actualResult =
                        concat (Construct(1, Construct(2, Construct(3, Empty)))) (Construct(1, Construct(2, Construct(3, Empty))))
                    Expect.equal actualResult (Construct(1, Construct(2, Construct(3, Construct(1, Construct(2, Construct(3, Empty)))))))
                        "concat (Construct(1, Construct(2, Construct(3, Empty)))) and (Construct(1, Construct(2, Construct(3, Empty)))) is (Construct(1, Construct(2, Construct(3, Construct(1, Construct(2, Construct(3, Empty)))))))"
            ]
            testList "bubbleOOPSort MyOOPList tests" [
                testCase "bubbleOOPSort test -- random int MyOOPList"
                <| fun _ ->
                    let actualResult =
                        let lst = MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList()))
                        fromMyOOPListToMyList (bubbleOOPSort lst)
                    Expect.equal actualResult (Construct(1, Construct(3, Empty)))
                        "bubbleOOPSort MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList())) as same as (Construct(1, Construct(3, Empty)))"

                testCase "bubbleOOPSort test -- random string MyOOPList"
                <| fun _ ->
                    let actualResult =
                        let lst = MyOOPNonEmptyList("18", MyOOPNonEmptyList("1", MyOOPEmptyList()))
                        fromMyOOPListToMyList (bubbleOOPSort lst)
                    Expect.equal actualResult (Construct("1", Construct("18", Empty)))
                        "bubbleOOPSort MyOOPNonEmptyList('18', MyOOPNonEmptyList('1', MyOOPEmptyList())) as same as (Construct('1', Construct('18', Empty))))"
            ]
            testList "quickOOPSort MyOOPList tests" [
                testCase "quickOOPSort test -- random int MyOOPList"
                 <| fun _ ->
                    let actualResult =
                        let lst = MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList()))
                        fromMyOOPListToMyList (quickOOPSort lst)
                    Expect.equal actualResult (Construct(1, Construct(3, Empty)))
                        "quickOOPSort MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList())) as same as (Construct(1, Construct(3, Empty)))"

                testCase "quickOOPSort test -- random string MyOOPList"
                <| fun _ ->
                    let actualResult =
                        let lst = MyOOPNonEmptyList("18", MyOOPNonEmptyList("1", MyOOPEmptyList()))
                        fromMyOOPListToMyList (quickOOPSort lst)
                    Expect.equal actualResult (Construct("1", Construct("18", Empty)))
                        "quickOOPSort MyOOPNonEmptyList('18', MyOOPNonEmptyList('1', MyOOPEmptyList())) as same as (Construct('1', Construct('18', Empty))))"
            ]
            testList "quickOOPSort MyOOPList property tests" [
                testProperty "quickSort test -- MyList<int>"
                <| fun (lst : list<int>) ->
                    Expect.equal<| List.sort lst <|  fromMyOOPListToList (bubbleOOPSort (fromListToMyOOPList lst)) <| "Unexpected result"

                testProperty "quickSort test -- MyList<string>"
                <| fun (lst : list<string>) ->
                    Expect.equal<| List.sort lst <|  fromMyOOPListToList (bubbleOOPSort (fromListToMyOOPList lst)) <| "Unexpected result"
            ]
            testList "concatOOP MyOOPList tests" [
                testCase "quickOOPSort test -- same string MyOOPLists"
                <| fun _ ->
                     let actualResult =
                        let lst1 = MyOOPNonEmptyList("18", MyOOPNonEmptyList("1", MyOOPEmptyList()))
                        let lst2 = MyOOPNonEmptyList("18", MyOOPNonEmptyList("1", MyOOPEmptyList()))
                        fromMyOOPListToMyList (concatOOP lst1 lst2)
                     Expect.equal actualResult (Construct("18", Construct("1", Construct("18", Construct("1", Empty)))))
                        "concatOOP MyOOPNonEmptyList('18', MyOOPNonEmptyList('1', MyOOPEmptyList())) and MyOOPNonEmptyList('18', MyOOPNonEmptyList('1', MyOOPEmptyList())) as same as (Construct('18', Construct('1', Construct('18', Construct('1', Empty)))))"

                testCase "quickOOPSort test -- same MyOOPEmptyLists"
                <| fun _ ->
                     let actualResult =
                        let lst1 = MyOOPEmptyList()
                        let lst2 = MyOOPEmptyList()
                        fromMyOOPListToMyList (concatOOP lst1 lst2)
                     Expect.equal actualResult Empty
                         "concatOOP MyOOPEmptyList() and MyOOPEmptyList() as same as Empty"
            ]
        ]
