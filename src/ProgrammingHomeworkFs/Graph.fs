module Graph

open SparseMatrix
open MtxReader

type Graph<'Value when 'Value: equality> =

    val AdjacencyMatrix: Matrix<'Value>
    val squareTypeFailWith: unit

    new(adjacencyMatrix: Matrix<'Value>) =
        { squareTypeFailWith =
            if adjacencyMatrix.Length1 <> adjacencyMatrix.Length2 then
                failwith
                    $"Incorrect matrix type was given.\n
                               Expected length1 : %A{adjacencyMatrix.Length1} = length2 : %A{adjacencyMatrix.Length2}."
          AdjacencyMatrix = adjacencyMatrix }

    new(tripleList, verticesCount) =
        { squareTypeFailWith = ()
          AdjacencyMatrix = Matrix(tripleList, verticesCount, verticesCount) }

    new(mtxFile: MtxFile, converter) =
        { squareTypeFailWith = () // Matrix from MatrixMarket file supposed to be square
          AdjacencyMatrix = toSparseMatrix converter mtxFile }

    member this.VerticesCount = this.AdjacencyMatrix.Length1
    member this.GetEdge(u, v) = this.AdjacencyMatrix[u, v]
