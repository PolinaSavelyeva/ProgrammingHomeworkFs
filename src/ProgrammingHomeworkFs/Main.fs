namespace ProgrammingHomeworkFs

open BenchmarkDotNet.Running
open BenchmarkOperationsExperiments

module Main =
    [<EntryPoint>]
    let main _ =
        //let summaryBFS = BenchmarkRunner.Run<BFSBenchmark>()
        let graph =
            Graph.Graph(__SOURCE_DIRECTORY__ + "/Graphs/coAuthorsCiteseer.mtx", Converters.pattern)

        printf $"%A{graph.VerticesCount}"
        0
