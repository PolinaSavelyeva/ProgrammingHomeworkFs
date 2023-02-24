module MtxReader

open SparseMatrix

type MtxFile(pathToFile: string) =

    let data = System.IO.File.ReadLines pathToFile
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

    let rec listOfSeq (sequence: seq<string>) =
        if Seq.isEmpty sequence then
            []
        else
            let currentLine = (Seq.head sequence).Split()
            let vertexOne = uint currentLine[0] - 1u
            let vertexTwo = uint currentLine[1] - 1u
            let weight = converter currentLine

            if vertexOne = vertexTwo then
                (vertexOne, vertexTwo, weight) :: (listOfSeq <| Seq.removeAt 0 sequence)
            else
                (vertexOne, vertexTwo, weight)
                :: (vertexTwo, vertexOne, weight) :: (listOfSeq <| Seq.removeAt 0 sequence)

    let mapping (line: string) =
        let splitLine = line.Split()
        (uint splitLine[0] - 1u, uint splitLine[1] - 1u, converter splitLine)

    let source = Seq.removeAt 0 file.NoCommentsData

    if file.IsSymmetric then
        Matrix(listOfSeq source, file.Rows, file.Columns)
    else
        Matrix(Seq.map mapping source |> Seq.toList, file.Rows, file.Columns)
