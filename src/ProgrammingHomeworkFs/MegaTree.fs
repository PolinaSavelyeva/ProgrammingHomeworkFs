module Tree
open AbstractTrees
// 1. Описать алгебраический тип, задающий дерево,
// в котором внутренний узел может иметь произвольное количество сыновей.
// Листья и внутренние узлы хранят некоторые значени произвольного (одинакового для всех) типа.

type Tree<'value> =
    | Node of hd : 'value * bdy : list<Tree<'value>>
    | Leaf of 'value
    //| Leaf of 'value

let tree = Node(1, [Node(2, [Leaf(2)]); Leaf(2); Leaf(2)])
//   1
//2  2  2
//2

//2. Реализовать функцию, которая по дереву (тип, описанный выше),
//находит количество различных элементов, хранящихся в узлах. Использовать рекурсию,
//не использовать мутабельные переменные.

let rec counter (tree : Tree<'value>) =
    match tree with
    | Leaf hd -> 1
    | Node (hd, body) -> hd <> summaTree leftTree
    | NodeWithRight (x, rightTree) -> x + summaTree rightTree
    | FullNode (x, leftTree, rightTree) ->
        let leftX = summaTree leftTree
        let rightX = summaTree rightTree
        x + rightX + leftX

let rec countElTree (x : 'value) (tree : Tree<'value>) : int =
    match tree with
    | Leaf a ->
        if a <> x then
            1
        else 0
    | Node(a, b) ->
        match b with
        | :? Tree<'value>
        if a <> x then
            1 + countElTree b
        else a == b then
            countElTree b

let rec f (tree : Tree<'value>) : int =
