module MtxReader

open SparseMatrix

type MtxFile(pathToFile: string) =

    let rec commentsCutter (array: array<string>) =
        if array.Length = 0 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                 Error in -commentsCutter- function"
        else if array[0][0] <> '%' then
            array
        else
            commentsCutter array[1..]

    let array = System.IO.File.ReadAllLines pathToFile

    let zeroArrayLine =
        if Array.tryHead array = Option.None then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                 Error in -zeroArrayLine- value"
        else
            array[ 0 ].Split()

    let weight, symmetry =
        if zeroArrayLine.Length <> 5 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                 Error in -weight, symmetry- value"
        else
            zeroArrayLine[ 3 ].ToLower(), zeroArrayLine[ 4 ].ToLower() = "symmetric"

    let cutArray = commentsCutter array

    let zeroCutArrayLine =
        if Array.tryHead cutArray = Option.None then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                 Error in -zeroCutArrayLine- value"
        else
            cutArray[ 0 ].Split()

    let rows, columns =
        if zeroCutArrayLine.Length <> 3 then
            failwith
                "Incorrect file was given. Expected MatrixMarket format file.\n
                 Error in -rows, columns- value"
        else
            uint zeroCutArrayLine[0], uint zeroCutArrayLine[1]

    member this.Array = array
    member this.Weight = weight
    member this.IsMtxSymmetric = symmetry
    member this.CutArray = cutArray
    member this.Rows = rows
    member this.Columns = columns

let symmetryIntMtxToGraph (array: array<string>) rows columns =

    let rec listOfIntFormation (array: array<string>) =
        if array.Length = 0 then
            []
        else
            let currentLine = array[ 0 ].Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u
            let weight = Some(int currentLine[2])

            if vertexOne = vertexTwo then
                (vertexOne, vertexTwo, weight) :: (listOfIntFormation array[1..])
            else
                (vertexOne, vertexTwo, weight) :: (vertexTwo, vertexOne, weight) :: (listOfIntFormation array[1..])

    Matrix(listOfIntFormation array[1..], rows, columns)

let symmetryDoubleMtxToGraph (array: array<string>) rows columns =

    let rec listOfDoubleFormation (array: array<string>) =
        if array.Length = 0 then
            []
        else
            let currentLine = array[ 0 ].Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u
            let weight = Some(float currentLine[2])

            if vertexOne = vertexTwo then
                (vertexOne, vertexTwo, weight) :: (listOfDoubleFormation array[1..])
            else
                (vertexOne, vertexTwo, weight)
                :: (vertexTwo, vertexOne, weight) :: (listOfDoubleFormation array[1..])

    Matrix(listOfDoubleFormation array[1..], rows, columns)

let symmetryPatternMtxToGraph (array: array<string>) rows columns =

    let rec listOfUnitFormation (array: array<string>) =
        if array.Length = 0 then
            []
        else
            let currentLine = array[ 0 ].Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u

            if vertexOne = vertexTwo then
                (vertexOne, vertexTwo, Some()) :: (listOfUnitFormation array[1..])
            else
                (vertexOne, vertexTwo, Some()) :: (vertexTwo, vertexOne, Some()) :: (listOfUnitFormation array[1..])

    Matrix(listOfUnitFormation array[1..], rows, columns)
