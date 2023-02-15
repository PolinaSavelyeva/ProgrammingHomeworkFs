module MtxReader

open SparseMatrix

(*type MtxFile(pathToFile: string) =

    let allLinesSeq = seq { yield! System.IO.File.ReadLines pathToFile }
    let dataLinesSeq = Seq.skipWhile (fun (n : string) -> n[0] = '%') allLinesSeq
    let firstSplitLine = (Seq.head allLinesSeq).Split()
    let firstSplitDataLine = (Seq.head dataLinesSeq).Split()
    let weightType = firstSplitLine[ 3 ].ToLower()
    let isSymmetric = firstSplitLine[ 4 ].ToLower() = "symmetric"
    let rows = uint firstSplitDataLine[0]
    let columns = uint firstSplitDataLine[1]

    let fromMtxToSparseMatrix converter =
        let rec tripleListFormation (sequence: seq<string>) =
                if Seq.length sequence = 0 then
                    []
                else
                    let currentLine = (Seq.head sequence).Split()
                    let vertexOne = uint currentLine[0] - 1u
                    let vertexTwo = uint currentLine[1] - 1u

                    if vertexOne = vertexTwo then
                        (vertexOne, vertexTwo, converter currentLine[2]) :: (tripleListFormation <| Seq.removeAt 0 sequence)
                    else
                        (vertexOne, vertexTwo, converter currentLine[2]) :: (vertexTwo, vertexOne, converter currentLine[2]) :: (tripleListFormation <| Seq.removeAt 0 sequence)

        Matrix(tripleListFormation <| Seq.removeAt 0 dataLinesSeq, rows, columns)

    member this.ReadLines = allLinesSeq
    member this.ReadDataLines = dataLinesSeq
    member this.WeightType = weightType
    member this.IsSymmetric = isSymmetric
    member this.Rows = rows
    member this.Columns = columns
    member this.ToMatrix = matrix*)

