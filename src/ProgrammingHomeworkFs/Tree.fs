module Tree

open AlgebraicList
open System.Collections.Generic

type Tree<'value> = Node of vl: 'value * bdy: list<Tree<'value>> // value body

let rec toType f g (tree: Tree<'value>) =
    match tree with
    | Node (hd, []) -> f hd
    | Node (hd, tl) ->
        match tl with
        | [ x ] ->
            match x with
            | Node (hd1, []) -> g (f hd) (f hd1)
            | Node (hd1, tl1) -> g (f hd) (toType f g (Node(hd1, tl1)))
        | hd1 :: tl1 -> g (toType f g hd1) (toType f g (Node(hd, tl1)))
        | _ -> failwith " System error.\Expected first branch of match : Node (hd, []). \Error in -uniqueValues- function. "

let uniqueValues (tree: Tree<'value>) : int =
    let f a =
        let set = HashSet<'value>()
        set.Add a |> ignore
        set

    let g (a: HashSet<'value>) (b: HashSet<'value>) =
        a.UnionWith(b)
        a

    (toType f g tree).Count

let toList (tree: Tree<'value>) : AlgebraicList.List<'value> =
    let f a = Construct(a, Empty)
    let g a b = concat a b
    toType f g tree
