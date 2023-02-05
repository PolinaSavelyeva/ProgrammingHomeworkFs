module MtxReader

open SparseMatrix

let readLines pathToFile =
    let lines = System.IO.File.ReadAllLines pathToFile
    lines

let mtxToGraphMatrix (array : array<string>) =

    let firstLine =
        if Array.tryHead array = Option.None then
            failwith "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in firstLine value -toGraphMatrix- function"
        else
            array[0].Split()

    let weightType, graphType =
        if firstLine.Length <> 5 then
            failwith "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in weightType, graphType values -toGraphMatrix- function"
        else
            firstLine[3], firstLine[4]

    let rec commentsCutter (array : array<string>) =
        if array.Length = 0 then
            failwith "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in -commentsCutter- function"
        else
            if array[0][0] <> '%' then
                array
            else
                commentsCutter array[1..]

    let matrixFormation (array : array<string>) =

        let firstLine = array[0].Split()
        let rows = uint firstLine[0]
        let columns = uint firstLine[1]

        let rec listFormation (array : array<string>)  =
            if array.Length = 0 then
                []
            else
                let currentLine = array[0].Split()
                (uint currentLine[0], uint currentLine[1], Some (float currentLine[2])) :: (listFormation array[1..])

        Matrix(listFormation array[1..], rows, columns)

    commentsCutter array |> matrixFormation




