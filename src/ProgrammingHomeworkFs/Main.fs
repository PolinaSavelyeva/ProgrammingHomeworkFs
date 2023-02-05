namespace ProgrammingHomeworkFs

open MtxReader

module Main =

    [<EntryPoint>]
    let main _ =
        let array =
            readLines "/Users/polinas/Documents/ProgrammingHomeworkFs/src/ProgrammingHomeworkFs/Matrices/494_bus.mtx"

        printf $"%A{(mtxToGraphMatrix array).Storage}"
        0
