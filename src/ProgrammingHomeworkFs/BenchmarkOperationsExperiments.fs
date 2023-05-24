module BenchmarkOperationsExperiments

open BenchmarkDotNet.Attributes
open BreadthFirstSearch

let graph112 =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/112.mtx", Converters.pattern)

let graph512 =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/512.mtx", Converters.pattern)

let graph1000 =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/1000.mtx", Converters.pattern)

let graph5000 =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/5000.mtx", Converters.pattern)

let graph10000 =
    Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/10000.mtx", Converters.pattern)

type BFSBenchmark() =

    [<Params(0u, 1u, 2u, 3u, 4u)>]
    member val MultiParallelLevel = 0u with get, set

    [<Params(0u, 1u, 2u)>]
    member val AddMultiParallelLevel = 0u with get, set

    [<Params(0u, 1u, 2u, 3u, 4u)>]
    member val AddParallelLevel = 0u with get, set

    [<Benchmark>]
    member this.BFS112() =
        BFS this.MultiParallelLevel this.AddMultiParallelLevel this.AddParallelLevel (List.init 1 uint) graph112

    member this.BFS512() =
        BFS this.MultiParallelLevel this.AddMultiParallelLevel this.AddParallelLevel (List.init 1 uint) graph512

    member this.BFS1000() =
        BFS this.MultiParallelLevel this.AddMultiParallelLevel this.AddParallelLevel (List.init 1 uint) graph1000

    member this.BFS5000() =
        BFS this.MultiParallelLevel this.AddMultiParallelLevel this.AddParallelLevel (List.init 1 uint) graph5000

    member this.BFS10000() =
        BFS this.MultiParallelLevel this.AddMultiParallelLevel this.AddParallelLevel (List.init 1 uint) graph10000
