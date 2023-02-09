module MtxReader

open SparseMatrix

type MtxFile (pathToFile : string) =

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
    let zeroArrayLine = array[ 0 ].Split()
    let weight = zeroArrayLine[3].ToLower()
    let symmetry = zeroArrayLine[4].ToLower() = "symmetric"
    let cutArray = commentsCutter array
    let zeroCutArrayLine = cutArray[ 0 ].Split()
    let rows = uint zeroCutArrayLine[0]
    let columns = uint zeroCutArrayLine[1]
    
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

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some(System.Int32.Parse(currentLine[2])))
                :: (listOfIntFormation array[1..])

        Matrix(listOfIntFormation array, rows, columns)

let symmetryDoubleMtxToGraph (array: array<string>) rows columns =

        let rec listOfDoubleFormation (array: array<string>) =
            if array.Length = 0 then
                []
            else
                let currentLine = array[ 0 ].Split()

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some(System.Double.Parse(currentLine[2])))
                :: (listOfDoubleFormation array[1..])

        Matrix(listOfDoubleFormation array, rows, columns)

let symmetryPatternMtxToGraph (array: array<string>) rows columns =

        let rec listOfUnitFormation (array: array<string>) =
            if array.Length = 0 then
                []
            else
                let currentLine = array[ 0 ].Split()

                (uint currentLine[0] - 1u, uint currentLine[1] - 1u, Some())
                :: (listOfUnitFormation array[1..])

        Matrix(listOfUnitFormation array, rows, columns)
