module Tree

open AlgebraicList
open System.Collections.Generic

type Tree<'value> = Node of vl: 'value * bdy: list<Tree<'value>>

let rec toType f acc (tree: Tree<'value>) =
    match tree with
    | Node (vl, []) -> f acc vl
    | Node (vl, bdy) -> List.fold (toType f) (f acc vl) bdy

let uniqueValues (tree: Tree<'value>) : int =
    let acc = HashSet<'value>()

    let f (acc: HashSet<'value>) a =
        acc.Add a |> ignore
        acc

    (toType f acc tree).Count

let toList (tree: Tree<'value>) : AlgebraicList.List<'value> =
    let acc = Empty

    let f acc a = Construct(a, acc)

    toType f acc tree
