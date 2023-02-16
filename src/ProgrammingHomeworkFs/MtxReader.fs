module MtxReader

open SparseMatrix

type MtxFile(pathToFile: string) =

    let data = seq { yield! System.IO.File.ReadLines pathToFile }
    let noCommentsData = Seq.skipWhile (fun (n: string) -> n[0] = '%') data
    let firstDataLine = (Seq.head data).Split()
    let afterCommentsLine = (Seq.head noCommentsData).Split()
    let weightType = firstDataLine[ 3 ].ToLower()
    let isSymmetric = firstDataLine[ 4 ].ToLower() = "symmetric"
    let rows = uint afterCommentsLine[0]
    let columns = uint afterCommentsLine[1]

    member this.Data = data
    member this.NoCommentsData = noCommentsData
    member this.WeightType = weightType
    member this.IsSymmetric = isSymmetric
    member this.Rows = rows
    member this.Columns = columns

let toSparseMatrix converter (file: MtxFile) =

    let rec symmetricListFormation (sequence: seq<string>) =
        if Seq.length sequence = 0 then
            []
        else
            let currentLine = (Seq.head sequence).Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u
            let weight = converter currentLine

            if vertexOne = vertexTwo then
                (vertexOne, vertexTwo, weight) :: (symmetricListFormation <| Seq.removeAt 0 sequence)
            else
                (vertexOne, vertexTwo, weight)
                :: (vertexTwo, vertexOne, weight) :: (symmetricListFormation <| Seq.removeAt 0 sequence)

    let rec generalListFormation (sequence: seq<string>) =
        if Seq.length sequence = 0 then
            []
        else
            let currentLine = (Seq.head sequence).Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u
            let weight = converter currentLine

            (vertexOne, vertexTwo, weight) :: (generalListFormation <| Seq.removeAt 0 sequence)

    if file.IsSymmetric then
        Matrix(symmetricListFormation <| Seq.removeAt 0 file.NoCommentsData, file.Rows, file.Columns)
    else
        Matrix(generalListFormation <| Seq.removeAt 0 file.NoCommentsData, file.Rows, file.Columns)
