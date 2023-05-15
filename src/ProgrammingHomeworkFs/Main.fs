namespace ProgrammingHomeworkFs

open BenchmarkDotNet.Running
open BenchmarkOperationsExperiments

module Main =
    [<EntryPoint>]
    let main _ =
        let summaryBFS = BenchmarkRunner.Run<BFSBenchmark>()

        0
