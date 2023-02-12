module BenchmarkExperiments

open BenchmarkDotNet.Attributes

type AdditionBenchmark() =

    let mutable vector1 = SparseVector.Vector([||])
    let mutable vector2 = SparseVector.Vector([||])

    let plusOperation opt1 opt2 =
        match opt1, opt2 with
        | Option.Some a, Option.Some b -> Option.Some(a + b)
        | Option.Some a, Option.None
        | Option.None, Option.Some a -> Option.Some(a)
        | Option.None, Option.None -> Option.None

    let random = System.Random()

    [<Params(10_000u, 100_000u, 1_000_000u)>]
    member val Length = 0u with get, set

    [<Params(2u, 4u)>]
    member val Level = 0u with get, set

    [<GlobalSetup>]
    member this.SetUpVectorAndMatrix() =
        vector1 <- SparseVector.Vector(Array.init (Converters.toInt this.Length) (fun _ -> Some(random.Next())))
        vector2 <- SparseVector.Vector(Array.init (Converters.toInt this.Length) (fun _ -> Some(random.Next())))

    [<Benchmark(Baseline = true)>]
    member this.BaselineAddition() =
        SparseVector.vectorAddition <| plusOperation <| vector1 <| vector2

    [<Benchmark>]
    member this.ParallelAddition() =
        ParallelMatrixAndVectorOperations.vectorAddition
        <| this.Level
        <| plusOperation
        <| vector1
        <| vector1

type MultiplicationBenchmark() =

    let mutable vector = SparseVector.Vector([||])
    let mutable matrix = SparseMatrix.Matrix(array2D [||])

    let plusOperation opt1 opt2 =
        match opt1, opt2 with
        | Option.Some a, Option.Some b -> Option.Some(a + b)
        | Option.Some a, Option.None
        | Option.None, Option.Some a -> Option.Some(a)
        | Option.None, Option.None -> Option.None

    let multiOperation opt1 opt2 =
        match opt1, opt2 with
        | Some a, Some b -> Some(a * b)
        | Option.None, _
        | _, Option.None -> Option.None

    let random = System.Random()

    [<Params(1000u, 2000u, 3000u)>]
    member val Length = 0u with get, set

    [<Params(2u, 4u)>]
    member val Level = 0u with get, set

    [<GlobalSetup>]
    member this.SetUpVectorAndMatrix() =
        vector <- SparseVector.Vector(Array.init (Converters.toInt this.Length) (fun _ -> Some(random.Next())))
        matrix <- SparseMatrix.Matrix(Array2D.init (Converters.toInt this.Length) (Converters.toInt this.Length) (fun _ _ -> Some(random.Next())))

    [<Benchmark(Baseline = true)>]
    member this.BaselineMultiplication() =
        MatrixMultiplication.multiplication <| plusOperation <| multiOperation <| vector <| matrix

    [<Benchmark>]
    member this.ParallelMultiplication() =
        ParallelMatrixAndVectorOperations.multiplication
        <| this.Level
        <| plusOperation
        <| multiOperation
        <| vector
        <| matrix
