namespace ProgrammingHomeworkFs

open BenchmarkDotNet.Running
open BenchmarkOperationsExperiments
open SparseVector

module Main =
    [<EntryPoint>]
    let main _ =
        let summaryResult = BenchmarkRunner.Run<BFSBenchmark>()
        0
