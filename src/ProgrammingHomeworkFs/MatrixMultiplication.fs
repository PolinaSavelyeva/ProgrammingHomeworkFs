module MatrixMultiplication

open SparseVector
open SparseMatrix

(*let multiplication fPlus fMultiply (vector: Vector<'value, 'obj>) (matrix: Matrix<'value, 'obj>) =
    if vector.SquareLength = matrix.SquareLength then
        let m = vector.SquareLength
        let v = Array.create m Option.None

        for i in 0 .. m - 1 do
            for k in 0 .. m - 1 do
                if v[i] = Option.None then
                    v[i] <- fMultiply (takeElementOfMatrix k i matrix) (takeElementOfVector k vector)
                else
                    v[i] <- fPlus v[i] (fMultiply (takeElementOfMatrix k i matrix) (takeElementOfVector k vector))
        Vector(v)
    else
        failwith $"Cannot multiply matrix and vector because of incompatible dimensions. \n
                  Expected : {vector.BLength} dimension. But given : {matrix.QLength}. \n
                  Error in -multiplication- function."

let fPlusOptionNumbers a b =
    match a, b with
    | Some(x), Option.None -> Some(x)
    | Option.None, Some(x) -> Some(x)
    | Some(x), Some(y) -> Some(x + y)
    | Option.None, Option.None -> Option.None

let fMultiplyOptionNumbers a b =
    match a, b with
    | _, Option.None -> Option.None
    | Option.None, _ -> Option.None
    | Some(x), Some(y) -> Some(x * y)
*)
