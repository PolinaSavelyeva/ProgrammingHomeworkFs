namespace ProgrammingHomeworkFs
open SparseVector
module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let i = 0
        let arr = [|Some 0; Some -1|]
        let arr' = Array.append arr [|Some(1)|]
        let i' = int i % arr'.Length
        let res = takeElementOfVector i' (Vector arr')
        printf $" %A{res}"
        0
