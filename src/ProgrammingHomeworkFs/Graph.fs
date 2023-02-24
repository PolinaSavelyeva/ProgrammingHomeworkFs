module Graph

open SparseMatrix
open MtxReader

type Graph<'Value when 'Value: equality> =

    val AdjacencyMatrix: Matrix<'Value>
    val VerticesCount: uint

    new(adjacencyMatrix, verticesCount) =
        { AdjacencyMatrix = adjacencyMatrix
          VerticesCount = verticesCount }

    new(tripleList, verticesCount) =
        { AdjacencyMatrix = Matrix(tripleList, verticesCount, verticesCount)
          VerticesCount = verticesCount }

    new(mtxFile: MtxFile, converter) =
        { AdjacencyMatrix = toSparseMatrix converter mtxFile
          VerticesCount = mtxFile.Rows }
