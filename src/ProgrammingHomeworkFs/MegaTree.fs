module Tree
open AbstractTrees
// 1. Описать алгебраический тип, задающий дерево,
// в котором внутренний узел может иметь произвольное количество сыновей.
// Листья и внутренние узлы хранят некоторые значени произвольного (одинакового для всех) типа.

// Функция создает тип параметра

(*type BinTree<'value> =
    | Node of value : 'value * minNode : myOption<BinTree<'value>>

and myOption<'a> =
    | FullNodeExist of myOption<'a>
    | NodeExist of 'a // Ветка со значением и без потомка
    | None

let tree = Node (1, None)
let tree2 = Node(1, NodeExist(Node(1, None)))*)
type MyNodeList<'value> =
    | Cons of head: 'value * tail: MyNodeList<'value>
    | Empty

type MyTree<'value> =
    | Branch of 'value * MyNodeList<'value>
    | Leaf of 'value

(*let howManyNodes (a : 'value) =
     if a > 0 then
         for
         Some (BinTree<'value> * predNode)
     else
         None*)
// Декартово произведение значения * есть/нет ветка 1 * есть/нет ветка 2 * есть/нет ветка 3 * ....
// число сыновей или None
// если число сыновей то создается столько бинтри, сколько сыновей указано
// Если None, то
