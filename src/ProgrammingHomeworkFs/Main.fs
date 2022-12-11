namespace ProgrammingHomeworkFs

open BreadthFirstSearch
open SparseMatrix

module Main =

    [<EntryPoint>]
    let main _ =
        let listTuples = [ (0, 0, Some 4); (2, 1, Some 1); (3, 7, Some 9) ]
        let size = 10
        let res = (toQuadTreeFromCOO listTuples size size size).Storage

        printf $" %A{res}"
        (*Node
  (
  Node (Leaf (Some 9), None, None, Leaf (Some 0)),
   Node (None, None, None, Leaf (Some 10)),
   None,
   Node (Leaf (Some -9), None, None, Leaf (Some 10)))*)

        0
