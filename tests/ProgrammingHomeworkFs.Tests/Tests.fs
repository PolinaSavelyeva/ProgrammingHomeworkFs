namespace ProgrammingHomeworkFs.Tests

open Expecto
open FsCheck
open ProgrammingHomeworkFs

module SayTests =
    [<Tests>]
    let tests =
        testList "Types and functions tests" [
            testList "bubbleSort MyList tests" [
                testCase "Tests 1.0"
                <| fun _ ->
                    let ActualResult =
                        bubbleSort (
                            Construct(
                                48,
                                Construct(
                                    0,
                                    Construct(
                                        0,
                                        Construct(734, Construct(-880, Construct(9, Empty)))
                                    )
                                )
                            )
                        )

                    Expect.equal
                        ActualResult
                        (Construct(
                            -880,
                            Construct(
                                0,
                                Construct(0, Construct(9, Construct(48, Construct(734, Empty))))
                            )
                        ))
                        "bubbleSort (Construct(48, Construct(0, Construct(0, Construct(734, Construct(-880, Construct(9, Empty))))))) is (Construct(-880, Construct(0, Construct(0, Construct(9, Construct(48, Construct(734, Empty))))))) "
                testCase "Tests 1.1"
                <| fun _ ->
                    let ActualResult = bubbleSort (Construct(0, Construct(0, Construct(0, Empty))))

                    Expect.equal
                        ActualResult
                        (Construct(0, Construct(0, Construct(0, Empty))))
                        "bubbleSort (Construct(0, Construct(0, Construct(0, Empty)))) is (Construct(0, Construct(0, Construct(0, Empty)))) "
                testCase "Tests 1.2"
                <| fun _ ->
                    let ActualResult =
                        bubbleSort (Construct("ab", Construct("abc", Construct("cdd", Empty))))

                    Expect.equal
                        ActualResult
                        (Construct("ab", Construct("abc", Construct("cdd", Empty))))
                        "bubbleSort (Construct('ab', Construct('abc', Construct('cdd', Empty)))) is (Construct('ab', Construct('abc', Construct('cdd', Empty))))"
                testCase "Tests 1.3"
                <| fun _ ->
                    let ActualResult =
                        bubbleSort (Construct(4.1, Construct(3.5, Construct(2.9, Empty))))

                    Expect.equal
                        ActualResult
                        (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))
                        "bubbleSort (Construct(4.1, Construct( 3.5, Construct(2.9, Empty)))) is (Construct(2.9, Construct(3.5, Construct(4.1, Empty))))"
                testCase "Tests 1.4"
                <| fun _ ->
                    let ActualResult =
                        bubbleSort (Construct("4.1", Construct("3.5", Construct("2.9", Empty))))

                    Expect.equal
                        ActualResult
                        (Construct("2.9", Construct("3.5", Construct("4.1", Empty))))
                        "bubbleSort (Construct('4.1', Construct('3.5', Construct('2.9', Empty)))) is  (Construct ('2.9', Construct ('3.5', Construct ('4.1', Empty))))"
                testCase "Tests 1.5"
                <| fun _ ->
                    let ActualResult = bubbleSort Empty
                    Expect.equal ActualResult Empty "bubbleSort Empty is Empty"
                testCase "Tests 1.6"
                <| fun _ ->
                    let ActualResult = bubbleSort (Construct(19.87, Empty))

                    Expect.equal
                        ActualResult
                        (Construct(19.87, Empty))
                        "bubbleSort (Construct(19.87, Empty)) is (Construct(19.87, Empty))"
            ]
            testList "quickSort MyList tests" [
                testProperty "Tests 2.0"
                <| fun (lst: MyList<int>) -> (bubbleSort lst) = (quickSort lst)
                testProperty "Tests 2.1"
                <| fun (lst: MyList<string>) -> (bubbleSort lst) = (quickSort lst)
            ]
            testList "concat MyList tests" [
                testCase "Tests 3.0"
                <| fun _ ->
                    let ActualResult = concat (Construct("ABCE", Empty)) (Construct("FG", Empty))

                    Expect.equal
                        ActualResult
                        (Construct("ABCE", Construct("FG", Empty)))
                        "concat (Construct('ABCE', Empty)) and (Construct('FG', Empty)) is  (Construct('ABCE', Construct('FG', Empty))) "
                testCase "Tests 3.1"
                <| fun _ ->
                    let ActualResult = concat Empty Empty
                    Expect.equal ActualResult Empty "concat Empty and Empty is Empty"
                testCase "Tests 3.2"
                <| fun _ ->
                    let ActualResult = concat Empty (Construct(1, Empty))

                    Expect.equal
                        ActualResult
                        (Construct(1, Empty))
                        "concat Empty and (Construct(1, Empty)) is (Construct(1, Empty))"
                testCase "Tests 3.3"
                <| fun _ ->
                    let ActualResult = concat (Construct(1, Empty)) Empty

                    Expect.equal
                        ActualResult
                        (Construct(1, Empty))
                        "concat (Construct(1, Empty)) and Empty is (Construct(1, Empty))"
                testCase "Tests 3.4"
                <| fun _ ->
                    let ActualResult =
                        concat
                            (Construct(1, Construct(2, Construct(3, Empty))))
                            (Construct(1, Construct(2, Construct(3, Empty))))

                    Expect.equal
                        ActualResult
                        (Construct(
                            1,
                            Construct(
                                2,
                                Construct(3, Construct(1, Construct(2, Construct(3, Empty))))
                            )
                        ))
                        "concat (Construct(1, Construct(2, Construct(3, Empty)))) and (Construct(1, Construct(2, Construct(3, Empty)))) is (Construct(1, Construct(2, Construct(3, Construct(1, Construct(2, Construct(3, Empty)))))))"
            ]
        ]
