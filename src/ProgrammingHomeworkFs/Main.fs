namespace ProgrammingHomeworkFs

open SparseMatrix
open SparseVector

module Main =

    [<EntryPoint>]
    let main _ =
        let arr = [|Some 1|]
        let arr2d = array2D([[Some 0; Some 1]])

        let vec = arr |> Vector
        let mtx = arr2d |> Matrix

        let res = MatrixMultiplication.multiplication (+) (*) vec mtx

        printfn $"%A{res.Storage}"

        0
