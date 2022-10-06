module AbstractTrees

(*type BinTree1<'value> =
    | FullNode of value : 'value * left : BinTree1<'value> * right : BinTree1<'value> // поддерево
    | NodeWithLeft of value : 'value * left : BinTree1<'value>
    | Leaf of value : 'value //лист не имеет поддеревьев

[<RequireQualifiedAccess>] // заставялет писать вызов полностью
type BinTree2<'value> =
    | Node of value : 'value * left : BinTree2<'value> * right : Option<BinTree2<'value>> // правой ветки может и не быть
    | Leaf of value : 'value //лист не имеет поддеревьев

let tree = Node (3, Leaf(1), Some(Node(4, Leaf(2), None))) //Some -> None

type BinTree3<'value> =
    | Node of value : 'value * left : Option<BinTree3<'value>> * right : Option<BinTree3<'value>> // правой ветки может и не быть
*)
type BinTree4<'value> =
    | FullNode of value : 'value * left : BinTree4<'value> * right : BinTree4<'value> // поддерево
    | NodeWithLeft of value : 'value * left : BinTree4<'value>
    | NodeWithRight of value : 'value * right : BinTree4<'value>
    | Leaf of value : 'value //лист не имеет поддеревьев

let rec minInTree (tree : BinTree4<'value>) : 'value =
    match tree with
    | Leaf x -> x
    | NodeWithLeft (x, leftTree) -> min x (minInTree leftTree)
    | NodeWithRight (x, rightTree) -> min x (minInTree rightTree)
    | FullNode (x, leftTree, rightTree) ->
        let leftX = minInTree leftTree
        let rightX = minInTree rightTree
        min (min x leftX) rightX

let rec summaTree (tree : BinTree4<'value>) =
    match tree with
    | Leaf x -> x
    | NodeWithLeft (x, leftTree) -> x + summaTree leftTree
    | NodeWithRight (x, rightTree) -> x + summaTree rightTree
    | FullNode (x, leftTree, rightTree) ->
        let leftX = summaTree leftTree
        let rightX = summaTree rightTree
        x + rightX + leftX

let tree = FullNode (12, Leaf(12), NodeWithLeft(-1, Leaf(-1)))

printf $"%A{summaTree tree}"

