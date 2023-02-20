module BenchmarkExperiments

open BenchmarkDotNet.Attributes
open MatrixAndVectorOperations

let random = System.Random()

let fPlusInt opt1 opt2 =
    match opt1, opt2 with
    | Option.Some a, Option.Some b -> Option.Some(a + b)
    | Option.Some a, Option.None
    | Option.None, Option.Some a -> Option.Some(a)
    | Option.None, Option.None -> Option.None

let fMultiInt opt1 opt2 =
    match opt1, opt2 with
    | Some a, Some b -> Some(a * b)
    | Option.None, _
    | _, Option.None -> Option.None

type AdditionBenchmark() =

    let mutable vector1 = SparseVector.Vector([||])
    let mutable vector2 = SparseVector.Vector([||])

    [<Params(2_500_000u, 3_500_000u)>]
    member val Length = 0u with get, set

    [<Params(1u, 2u, 3u, 4u)>]
    member val ParallelLevel = 0u with get, set

    // Greater level equals more matrix's sparsity
    // Level 1 equals dense matrix
    [<Params(1u, 5u, 10u)>]
    member val DensityLevel = 0u with get, set

    [<GlobalSetup>]
    member this.SetUpVectors() =

        let initializer n =
            if n % Converters.toInt this.DensityLevel <> 0 then
                Option.None
            else
                Some(random.Next())

        let intLength = Converters.toInt this.Length

        vector1 <- SparseVector.Vector(Array.init intLength initializer)
        vector2 <- SparseVector.Vector(Array.init intLength initializer)

    [<Benchmark(Baseline = true)>]
    member this.BaselineAddition() =
        vectorAddition 0u fPlusInt vector1 vector2

    [<Benchmark>]
    member this.ParallelAddition() =
        vectorAddition this.ParallelLevel fPlusInt vector1 vector1

type MultiplicationBenchmark() =

    let mutable vector = SparseVector.Vector([||])
    let mutable matrix = SparseMatrix.Matrix(array2D [||])

    [<Params(1500u, 3000u)>]
    member val Length1 = 0u with get, set

    [<Params(2500u)>]
    member val Length2 = 0u with get, set

    [<Params(1u, 2u, 3u)>]
    member val MultiParallelLevel = 0u with get, set

    [<Params(0u, 1u)>]
    member val AddParallelLevel = 0u with get, set

    [<Params(1u, 10u)>]
    member val DensityLevel = 0u with get, set

    [<GlobalSetup>]
    member this.SetUpVectorAndMatrix() =

        let arrayInitializer n =
            if n % Converters.toInt this.DensityLevel <> 0 then
                Option.None
            else
                Some(random.Next())

        let array2DInitializer n _ =
            if n % Converters.toInt this.DensityLevel <> 0 then
                Option.None
            else
                Some(random.Next())

        let intLength1 = Converters.toInt this.Length1
        let intLength2 = Converters.toInt this.Length2

        vector <- SparseVector.Vector(Array.init intLength1 arrayInitializer)
        matrix <- SparseMatrix.Matrix(Array2D.init intLength1 intLength2 array2DInitializer)

    [<Benchmark(Baseline = true)>]
    member this.BaselineMultiplication() =
        multiplication 0u 0u fPlusInt fMultiInt vector matrix

    [<Benchmark>]
    member this.ParallelMultiplication() =
        multiplication this.MultiParallelLevel this.AddParallelLevel fPlusInt fMultiInt vector matrix
