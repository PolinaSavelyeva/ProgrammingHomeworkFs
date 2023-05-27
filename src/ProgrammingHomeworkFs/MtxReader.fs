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

    let source = Seq.removeAt 0 file.NoCommentsData

    let tripleMapping triple =
        let i, j, v = triple
        (j, i, v)

    let lineMapping (line: string) =
        let splitLine = line.Split()
        (uint splitLine[0] - 1u, uint splitLine[1] - 1u, converter splitLine)

    let tripleSeq = Seq.map lineMapping source

    if file.IsSymmetric then
        Matrix(
            tripleSeq
            |> Seq.filter (fun (i, j, _) -> i <> j)
            |> Seq.map tripleMapping
            |> Seq.append tripleSeq
            |> Seq.toList,
            file.Rows,
            file.Columns
        )
    else
        Matrix(tripleSeq |> Seq.toList, file.Rows, file.Columns)
