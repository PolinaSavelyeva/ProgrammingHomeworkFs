module Tree

open AlgebraicList
open System.Collections.Generic

type Tree<'Value> = Node of vl: 'Value * bdy: list<Tree<'Value>>

let rec toType f acc tree =
    match tree with
    | Node (vl, []) -> f acc vl
    | Node (vl, bdy) -> List.fold (toType f) (f acc vl) bdy

let uniqueValues tree =
    let acc = HashSet<'Value>()

    let f (acc: HashSet<'Value>) a =
        acc.Add a |> ignore
        acc

    (toType f acc tree).Count

let toList tree =
    let acc = Empty

    let f acc a = Construct(a, acc)

    toType f acc tree
