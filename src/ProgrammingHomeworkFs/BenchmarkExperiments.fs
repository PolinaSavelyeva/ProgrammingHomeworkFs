module BenchmarkExperiments

open BenchmarkDotNet.Attributes
open MatrixAndVectorOperations

let random = System.Random()

let IntPlusOperation opt1 opt2 =
    match opt1, opt2 with
    | Option.Some a, Option.Some b -> Option.Some(a + b)
    | Option.Some a, Option.None
    | Option.None, Option.Some a -> Option.Some(a)
    | Option.None, Option.None -> Option.None

let IntMultiOperation opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some(a * b)
    | Option.None, _
    | _, Option.None -> Option.None

type AdditionBenchmark() =

    let mutable vector1 = SparseVector.Vector([||])
    let mutable vector2 = SparseVector.Vector([||])

    [<Params(1_000_000u, 2_500_000u)>]
    member val Length = 0u with get, set

    [<Params(1u, 2u, 3u, 4u)>]
    member val ParallelLevel = 0u with get, set

    // Greater level equals more matrix's sparsity
    // Level 1 equals dense matrix
    [<Params(1, 5, 10)>]
    member val DensityLevel = 1 with get, set

    [<GlobalSetup>]
    member this.SetUpVectors() =
        vector1 <-
            SparseVector.Vector(
                Array.init (Converters.toInt this.Length) (fun n ->
                    if n % this.DensityLevel <> 0 then
                        Option.None
                    else
                        Some(random.Next()))
            )

        vector2 <-
            SparseVector.Vector(
                Array.init (Converters.toInt this.Length) (fun n ->
                    if n % this.DensityLevel <> 0 then
                        Option.None
                    else
                        Some(random.Next()))
            )

    [<Benchmark(Baseline = true)>]
    member this.BaselineAddition() =
        vectorAddition <| 0u <| IntPlusOperation <| vector1 <| vector2

    [<Benchmark>]
    member this.ParallelAddition() =
        vectorAddition <| this.ParallelLevel <| IntPlusOperation <| vector1 <| vector1

type MultiplicationBenchmark() =

    let mutable vector = SparseVector.Vector([||])
    let mutable matrix = SparseMatrix.Matrix(array2D [||])

    [<Params(2500u)>]
    member val Length1 = 0u with get, set

    [<Params(2500u)>]
    member val Length2 = 0u with get, set

    [<Params(1u, 2u)>]
    member val MultiParallelLevel = 0u with get, set

    [<Params(0u, 1u)>]
    member val AddParallelLevel = 0u with get, set

    [<Params(1, 10)>]
    member val DensityLevel = 1 with get, set

    [<GlobalSetup>]
    member this.SetUpVectorAndMatrix() =
        vector <-
            SparseVector.Vector(
                Array.init (Converters.toInt this.Length1) (fun n ->
                    if n % this.DensityLevel <> 0 then
                        Option.None
                    else
                        Some(random.Next()))
            )

        matrix <-
            SparseMatrix.Matrix(
                Array2D.init (Converters.toInt this.Length1) (Converters.toInt this.Length2) (fun n _ ->
                    if n % this.DensityLevel <> 0 then
                        Option.None
                    else
                        Some(random.Next()))
            )

    [<Benchmark(Baseline = true)>]
    member this.BaselineMultiplication() =
        multiplication <| 0u <| 0u <| IntPlusOperation <| IntMultiOperation <| vector <| matrix

    [<Benchmark>]
    member this.ParallelMultiplication() =
        multiplication
        <| this.MultiParallelLevel
        <| this.AddParallelLevel
        <| IntPlusOperation
        <| IntMultiOperation
        <| vector
        <| matrix
