module MtxReaderTests

open Expecto
open Microsoft.FSharp.Core
open MtxReader
open SparseMatrix
open Converters

module SayTests =
    [<Tests>]
    let tests =
        testList
            "MtxReader function tests"
            [ testCase "3x3 size real symmetric mtx test"
              <| fun _ ->
                  let path = __SOURCE_DIRECTORY__ + "/Matrices/1138_bus.mtx"
                  let file = MtxFile(path)
                  let actualResult = toSparseMatrix real file

                  let expectedResult =
                      Matrix(
                          [ (0u, 0u, Some 1.1)
                            (1u, 1u, Some 2.2)
                            (1u, 0u, Some 1.2)
                            (0u, 1u, Some 1.2)
                            (2u, 0u, Some 1.3)
                            (0u, 2u, Some 1.3)
                            (2u, 1u, Some 2.3)
                            (1u, 2u, Some 2.3)
                            (2u, 2u, Some 3.3) ],
                          3u,
                          3u
                      )

                  Expect.equal actualResult.Storage expectedResult.Storage $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}"

              testCase "4x4 size pattern symmetric mtx test"
              <| fun _ ->
                  let path = __SOURCE_DIRECTORY__ + "/Matrices/dwt_59.mtx"
                  let file = MtxFile(path)
                  let actualResult = toSparseMatrix pattern file

                  let expectedResult =
                      Matrix(
                          [ (0u, 0u, Some())
                            (1u, 1u, Some())
                            (2u, 2u, Some())
                            (3u, 3u, Some())
                            (1u, 0u, Some())
                            (0u, 1u, Some())
                            (2u, 0u, Some())
                            (0u, 2u, Some())
                            (3u, 0u, Some())
                            (0u, 3u, Some())
                            (2u, 1u, Some())
                            (1u, 2u, Some())
                            (3u, 1u, Some())
                            (1u, 3u, Some())
                            (3u, 2u, Some())
                            (2u, 3u, Some()) ],
                          4u,
                          4u
                      )

                  Expect.equal actualResult.Storage expectedResult.Storage $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}"

              testCase "empty pattern symmetric mtx test"
              <| fun _ ->
                  let path = __SOURCE_DIRECTORY__ + "/Matrices/bcspwr08.mtx"
                  let file = MtxFile(path)
                  let actualResult = toSparseMatrix pattern file
                  let expectedResult = Matrix([], 0u, 0u)

                  Expect.equal actualResult.Storage expectedResult.Storage $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}"

              testCase "3x3 size integer symmetric only diagonal mtx test"
              <| fun _ ->
                  let path = __SOURCE_DIRECTORY__ + "/Matrices/bcsstm25.mtx"
                  let file = MtxFile(path)
                  let actualResult = toSparseMatrix integer file

                  let expectedResult =
                      Matrix([ (0u, 0u, Some 1); (1u, 1u, Some 2); (2u, 2u, Some 3) ], 3u, 3u)

                  Expect.equal actualResult.Storage expectedResult.Storage $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}"

              testCase "3x3 size integer general mtx test"
              <| fun _ ->
                  let path = __SOURCE_DIRECTORY__ + "/Matrices/dwt_50.mtx"
                  let file = MtxFile(path)
                  let actualResult = toSparseMatrix integer file

                  let expectedResult =
                      Matrix([ (0u, 0u, Some 1); (0u, 1u, Some 2); (0u, 2u, Some 3) ], 3u, 3u)

                  Expect.equal actualResult.Storage expectedResult.Storage $"Unexpected: %A{actualResult}.\n Expected: %A{expectedResult}" ]
