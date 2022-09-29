namespace ProgrammingHomeworkFs.Tests

open Expecto
open ProgrammingHomeworkFs

module SayTests =
    [<Tests>]
    let tests =
        testList "samples" [
            testCase "Tests 1.0"
            <| fun _ ->
                let ActualResult = expo 7.0 12
                Expect.equal ActualResult 13841287201.0 "Exponentiation 7 x 12 = 13841287201"
            testCase "Tests 1.1"
            <| fun _ ->
                let ActualResult = expo 0.0 2000
                Expect.equal ActualResult 0 "Exponentiation 0 x 2000 = 0"
            testCase "Tests 1.2"
            <| fun _ ->
                let ActualResult = expo -200.0 3
                Expect.equal ActualResult -8000000 "Exponentiation -200 x 3 = -8000000"
            testCase "Tests 1.3"
            <| fun _ ->
                let ActualResult = expo 2 -1
                Expect.equal ActualResult 0.5 "Exponentiation 2 x -1 = 0.5"
            testCase "Tests 1.4"
            <| fun _ ->
                let ActualResult = expo 0 0
                Expect.equal ActualResult -1 "Exponentiation 0 x 0 is not defined"
            testCase "Tests 2.0"
            <| fun _ ->
                let ActualResult = expo 7.0 12
                Expect.equal ActualResult 13841287201.0 "Fast exponentiation 7 x 12 = 13841287201"
            testCase "Tests 2.1"
            <| fun _ ->
                let ActualResult = fast_expo 0.0 12344
                Expect.equal ActualResult 0 "Fast exponentiation 0 x 12344 = 0"
            testCase "Tests 2.2"
            <| fun _ ->
                let ActualResult = fast_expo 0.0 0
                Expect.equal ActualResult -1 "Fast exponentiation 0 x 0 is not defined"
            testCase "Tests 2.3"
            <| fun _ ->
                let ActualResult = expo -200.0 3
                Expect.equal ActualResult -8000000 "Fast exponentiation -200 x 3 = -8000000"
            testCase "Tests 3.0"
            <| fun _ ->
                let ActualResult =
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
                    ActualResult
                    1110
                    "Maximum - minimum of [|-100; 20; 186; 918; 0 ; -192; 11|] = 1110"
            testCase "Tests 3.1"
            <| fun _ ->
                let ActualResult =
                    arrays [|
                        -8
                        -8
                        -8
                        -8
                    |]

                Expect.equal ActualResult 0 "Maximum - minimum of [|-8; -8; -8; -8|] = 0"
            testCase "Tests 3.2"
            <| fun _ ->
                let ActualResult =
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
                    ActualResult
                    2332141
                    "Maximum - minimum of [|102932; 12; -10; 2331141; -15; -19; 1991; 900; -1000|] = 2332141"
            testCase "Tests 4.0"
            <| fun _ ->
                let ActualResult = odds 3 12

                Expect.equal
                    ActualResult
                    [|
                        5
                        7
                        9
                        11
                    |]
                    "Odds between 3 and 12: [|5;7;9;11|]"
            testCase "Tests 4.1"
            <| fun _ ->
                let ActualResult = odds 5 7
                Expect.equal ActualResult [||] "There is no odds between 5 and 7: [||]"
            testCase "Tests 4.2"
            <| fun _ ->
                let ActualResult = odds -100 -110

                Expect.equal
                    ActualResult
                    [|
                        -109
                        -107
                        -105
                        -103
                        -101
                    |]
                    "Odds between -100 and -110: [|-109; -107; -105; -103; -101|]"
        ]
