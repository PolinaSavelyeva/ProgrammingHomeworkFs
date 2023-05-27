module Graph

open SparseMatrix
open MtxReader

type Graph<'Value when 'Value: equality> =

    val AdjacencyMatrix: Matrix<'Value>

    new(adjacencyMatrix) = { AdjacencyMatrix = adjacencyMatrix }

    new(tripleList: list<uint * uint * Option<'Value>>, verticesCount) = { AdjacencyMatrix = Matrix(tripleList, verticesCount, verticesCount) }

    new(mtxPath: string, converter) = { AdjacencyMatrix = toSparseMatrix converter (MtxFile mtxPath) }

    member this.VerticesCount =
        let length1 = this.AdjacencyMatrix.Length1
        let length2 = this.AdjacencyMatrix.Length2

        if length1 <> length2 then
            failwith
                $"Incorrect matrix type was given.\n
                               Expected length1 : %A{length1} = length2 : %A{length2}."
        else
            length1

    member this.GetEdge(u, v) = this.AdjacencyMatrix[u, v]
