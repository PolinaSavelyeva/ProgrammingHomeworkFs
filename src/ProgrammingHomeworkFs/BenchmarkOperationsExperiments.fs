module BenchmarkOperationsExperiments

open BenchmarkDotNet.Attributes
open BreadthFirstSearch

let graph =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/coAuthorsCiteseer.mtx", Converters.pattern)

type BFSBenchmark() =

    [<Params(0u, 1u, 2u, 3u, 4u, 5u)>]
    member val MultiParallelLevel = 0u with get, set

    [<Benchmark>]
    member this.ParallelMultiplication() =
        BFS this.MultiParallelLevel 0u 0u [ 1u ] graph
