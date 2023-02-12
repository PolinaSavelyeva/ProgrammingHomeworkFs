namespace ProgrammingHomeworkFs

open BenchmarkDotNet.Running
open BenchmarkExperiments

module Main =
    [<EntryPoint>]
    let main _ =
        let summaryAddition = BenchmarkRunner.Run<AdditionBenchmark>()
        let summaryMulti = BenchmarkRunner.Run<MultiplicationBenchmark>()
        0
