module Tree

type IList<'value> =
    | Cons of hd : 'value * tl : IList<'value>
    | Empty

type Tree<'value> =
    | Node of hd : 'value * tl : list<Tree<'value>>
    | Leaf of 'value

let uniqueValues (tree : Tree<'value>) : int =
    let rec toSet (tree : Tree<'value>) : Set<'value> =
     match tree with
     | Leaf(hd) -> Set.empty.Add(hd)
     | Node (hd, tl) ->
         match tl with
         | [x] ->
             match x with
             | Node(hd1, tl1) -> Set.empty.Add(hd) + toSet (Node(hd1, tl1))
             | Leaf(hd) -> Set.empty.Add(hd)
         | hd1 :: tl1 -> Set.empty.Add(hd) + toSet hd1 + toSet (Node (hd, tl1))
         | _ -> failwith "Non correct type"

    Set.count (toSet tree)

let tree1 = Node(1, [Node(1, [Leaf(1)]); Leaf(1); Leaf(1)])
printf $"%A{uniqueValues tree1}"
