module Tree
open AbstractTrees
// 1. Описать алгебраический тип, задающий дерево,
// в котором внутренний узел может иметь произвольное количество сыновей.
// Листья и внутренние узлы хранят некоторые значени произвольного (одинакового для всех) типа.

type Tree<'value> =
    | Node of ('value * Tree<'value> list)
    | Leaf of 'value

let probTree = Node(12, [Node(2, [Leaf(9); Leaf(21)]); Leaf(5)])
//        12
//      2   5
//     9 21

//2. Реализовать функцию, которая по дереву (тип, описанный выше),
//находит количество различных элементов, хранящихся в узлах. Использовать рекурсию,
//не использовать мутабельные переменные.

let rec countElTree (tree : Tree<'value>) : int =
    match tree with
    | Leaf _-> 1
    | Node (_, tl) -> 1 +  countElList tl

let rec countElList (lst : list<'value>) : int =
    match lst with
    | [x] -> 1
    | [_;x] ->
        1 + countElTree x
