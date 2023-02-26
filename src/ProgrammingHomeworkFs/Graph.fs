module Graph

open SparseMatrix
open MtxReader

type Graph<'Value when 'Value: equality> =

    val AdjacencyMatrix: Matrix<'Value>
    val VerticesCount: uint

    new(adjacencyMatrix) =
        { AdjacencyMatrix = adjacencyMatrix
          VerticesCount =
            if adjacencyMatrix.Length1 <> adjacencyMatrix.Length2 then
                failwith
                    $"Incorrect matrix type was given.\n
                               Expected length1 : %A{adjacencyMatrix.Length1} = length2 : %A{adjacencyMatrix.Length2}."
            else
                adjacencyMatrix.Length1 }

    new(tripleList, verticesCount) =
        { AdjacencyMatrix = Matrix(tripleList, verticesCount, verticesCount)
          VerticesCount = verticesCount }

    new(mtxFile: MtxFile, converter) =
        { AdjacencyMatrix = toSparseMatrix converter mtxFile
          VerticesCount =
            if mtxFile.Rows <> mtxFile.Columns then
                failwith
                    $"Incorrect MatrixMarket file was given.\n
                    Expected rows : %A{mtxFile.Rows} = columns : %A{mtxFile.Columns}."
            else
                mtxFile.Rows }

    member this.GetEdge(u, v) = this.AdjacencyMatrix[u, v]
