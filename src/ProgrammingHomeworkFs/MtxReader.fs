module MtxReader

open SparseMatrix

let readLines pathToFile =
    let lines = System.IO.File.ReadAllLines pathToFile
    lines

let symmetryIntMtxToGraph (array: array<string>) =

    let rec commentsCutter (array: array<string>) =
        if array.Length = 0 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in -symmetryIntMtxToGraph- function"
        else if array[0][0] <> '%' then
            array
        else
            commentsCutter array[1..]

    let matrixFormation (array: array<string>) =

        let firstLine = array[ 0 ].Split()
        let rows = uint firstLine[0]
        let columns = uint firstLine[1]

        let rec listFormation (array: array<string>) =
            if array.Length = 0 then
                []
            else
                let currentLine = array[ 0 ].Split()

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some(System.Int32.Parse(currentLine[2])))
                :: (listFormation array[1..])

        Matrix(listFormation array[1..], rows, columns)

    commentsCutter array |> matrixFormation

let symmetryDoubleMtxToGraph (array: array<string>) =

    let rec commentsCutter (array: array<string>) =
        if array.Length = 0 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in -symmetryDoubleMtxToGraph- function"
        else if array[0][0] <> '%' then
            array
        else
            commentsCutter array[1..]

    let matrixFormation (array: array<string>) =

        let firstLine = array[ 0 ].Split()
        let rows = uint firstLine[0]
        let columns = uint firstLine[1]

        let rec listFormation (array: array<string>) =
            if array.Length = 0 then
                []
            else
                let currentLine = array[ 0 ].Split()

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some(System.Double.Parse(currentLine[2])))
                :: (listFormation array[1..])

        Matrix(listFormation array[1..], rows, columns)

    commentsCutter array |> matrixFormation

let symmetryPatternMtxToGraph (array: array<string>) =

    let rec commentsCutter (array: array<string>) =
        if array.Length = 0 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                      Error in -symmetryPatternMtxToGraph- function"
        else if array[0][0] <> '%' then
            array
        else
            commentsCutter array[1..]

    let matrixFormation (array: array<string>) =

        let firstLine = array[ 0 ].Split()
        let rows = uint firstLine[0]
        let columns = uint firstLine[1]

        let rec listFormation (array: array<string>) =
            if array.Length = 0 then
                []
            else
                let currentLine = array[ 0 ].Split()

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some()) :: (listFormation array[1..])

        Matrix(listFormation array[1..], rows, columns)

    commentsCutter array |> matrixFormation
