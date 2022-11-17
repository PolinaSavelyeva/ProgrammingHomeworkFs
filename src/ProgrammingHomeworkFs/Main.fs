namespace ProgrammingHomeworkFs

open SparseMatrix
open SparseVector
open MatrixMultiplication
module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let arrSome = Vector([|Option.None; Some 1; Option.None; Some 2|])

        let tableSome = Matrix(array2D
                            [[Some 7; Option.None; Option.None; Option.None; Option.None]
                             [Some 7; Option.None; Option.None; Option.None; Some 4]
                             [Option.None; Option.None; Option.None; Option.None; Option.None]
                             [Option.None; Some 8; Some 1; Some 9; Option.None]])
        let res = multiplication (+) (*) arrSome tableSome
        printf $"%A{res.Storage}"
        0
