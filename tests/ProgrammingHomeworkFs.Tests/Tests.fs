namespace ProgrammingHomeworkFs.Tests

open Expecto
open ProgrammingHomeworkFs

module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Tests 1.0"
            <| fun _ ->
                let actualResult = expo 7.0 12
                Expect.equal actualResult 13841287201.0 "Exponentiation 7 x 12 = 13841287201"
            testCase "Tests 1.1"
            <| fun _ ->
                let actualResult = expo 0.0 2000
                Expect.equal actualResult 0 "Exponentiation 0 x 2000 = 0"
            testCase "Tests 1.2"
            <| fun _ ->
                let actualResult = expo -200.0 3
                Expect.equal actualResult -8000000 "Exponentiation -200 x 3 = -8000000"
            testCase "Tests 1.3"
            <| fun _ ->
                let actualResult = expo 2.0 -1
                Expect.equal actualResult 0.5 "Exponentiation 2 x -1 = 0.5"
            testCase "Tests 1.4"
            <| fun _ ->
                let actualResult =
                    Expect.throws
                        (fun _ ->
                            expo 0.0 0
                            |> ignore
                        )
                        "Don't try to raise zero to the power of zero. Expected both non-zero basement and non-zero exponent. \n Error when calling - expo - function."

                actualResult
            testCase "Tests 2.0"
            <| fun _ ->
                let actualResult = fastExpo 7.0 12
                Expect.equal actualResult 13841287201.0 "Fast exponentiation 7 x 12 = 13841287201"
            testCase "Tests 2.1"
            <| fun _ ->
                let actualResult = fastExpo 0.0 12344
                Expect.equal actualResult 0 "Fast exponentiation 0 x 12344 = 0"
            testCase "Tests 2.2"
            <| fun _ ->
                let actualResult =
                    Expect.throws
                        (fun _ ->
                            fastExpo 0.0 0
                            |> ignore
                        )
                        "Don't try to raise zero to the power of zero. Expected both non-zero basement and non-zero exponent. \n Error when calling - fastExpo - function"

                actualResult
            testCase "Tests 2.3"
            <| fun _ ->
                let actualResult = fastExpo -200.0 3
                Expect.equal actualResult -8000000 "Fast exponentiation -200 x 3 = -8000000"
            testCase "Tests 3.0"
            <| fun _ ->
                let actualResult =
                    arrays [|
                        -100
                        20
                        186
                        918
                        0
                        -192
                        11
                    |]

                Expect.equal
                    actualResult
                    1110
                    "Maximum - minimum of [|-100; 20; 186; 918; 0 ; -192; 11|] = 1110"
            testCase "Tests 3.1"
            <| fun _ ->
                let actualResult =
                    arrays [|
                        -8
                        -8
                        -8
                        -8
                    |]

                Expect.equal actualResult 0 "Maximum - minimum of [|-8; -8; -8; -8|] = 0"
            testCase "Tests 3.2"
            <| fun _ ->
                let actualResult =
                    arrays [|
                        102932
                        12
                        -10
                        2331141
                        -15
                        -19
                        1991
                        900
                        -1000
                    |]

                Expect.equal
                    actualResult
                    2332141
                    "Maximum - minimum of [|102932; 12; -10; 2331141; -15; -19; 1991; 900; -1000|] = 2332141"
            testCase "Tests 3.3"
            <| fun _ ->
                let actualResult = arrays [| 1 |]
                Expect.equal actualResult 0 "Maximum - minimum of [|1|] = 0"
            testCase "Tests 3.4"
            <| fun _ ->
                let actualResult =
                    Expect.throws
                        (fun _ ->
                            arrays [||]
                            |> ignore
                        )
                        "There is no max and min elements in empty array. Expected at least 1 element in array. \n Error when calling - arrays - function"

                actualResult
            testCase "Tests 3.5"
            <| fun _ ->
                let actualResult =
                    arrays [|
                        1.02839
                        12.393902
                        16.18
                        -1928.32
                        1892
                        19930.9
                        0
                    |]

                Expect.equal
                    actualResult
                    21859.22
                    "Maximum - minimum of [|1.02839; 12.393902; 16.18; -1928.32; 1892 ; 19930.9; 0|] = 21859.22"
            testCase "Tests 3.6"
            <| fun _ ->
                let actualResult =
                    arrays [|
                        2.2
                        2.2
                        2.2
                        2.2
                    |]

                Expect.equal actualResult 0 "Maximum - minimum of [|2.2; 2.2; 2.2; 2.2|] = 0"
            testCase "Tests 4.0"
            <| fun _ ->
                let actualResult = odds 3 12

                Expect.equal
                    actualResult
                    [|
                        5
                        7
                        9
                        11
                    |]
                    "Odds between 3 and 12: [|5;7;9;11|]"
            testCase "Tests 4.1"
            <| fun _ ->
                let actualResult = odds 5 7
                Expect.equal actualResult [||] "There is no odds between 5 and 7: [||]"
            testCase "Tests 4.2"
            <| fun _ ->
                let actualResult = odds -100 -110

                Expect.equal
                    actualResult
                    [|
                        -109
                        -107
                        -105
                        -103
                        -101
                    |]
                    "Odds between -100 and -110: [|-109; -107; -105; -103; -101|]"
        ]
