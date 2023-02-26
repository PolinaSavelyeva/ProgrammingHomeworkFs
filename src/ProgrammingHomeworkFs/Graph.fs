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
        let adjacencyMatrix = toSparseMatrix converter mtxFile

        { AdjacencyMatrix = adjacencyMatrix
          VerticesCount = Graph(adjacencyMatrix).VerticesCount }

    member this.GetEdge(u, v) = this.AdjacencyMatrix[u, v]