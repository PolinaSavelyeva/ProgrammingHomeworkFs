module Tree

type MyList<'value> =
    | Cons of hd : 'value * tl : MyList<'value>
    | Empty

let rec toList (lst: MyList<'value>) =
     match lst with
     | Empty -> []
     | Cons(hd, tl) -> hd :: toList tl

// Тип дерево с произвольным количеством узлов
type Tree<'value> =
    | Node of hd : 'value * tl : list<Tree<'value>>
    | Leaf of 'value

let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
     match lst1 with
     | Empty -> lst2
     | Cons(hd, tl) -> Cons(hd, concat tl lst2)

/// Функция uniqueValues выводит количество "уникальных" элементов в дереве, используя множества
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

let tree1 = Node(1, [Node(2, [Leaf(3)]); Leaf(4); Leaf(5)])
//printf $"%A{uniqueValues tree1}"

//Реализовать функцию, которая по дереву (тип, описанный выше),
//строит список, содержащий все значения из всех узлов. Для сипска использовать тип MyList из
//предыдущей задачи. Рекурсия —- хорошо, мутабельные переменные —- плохо.

let rec toMyList (tree : Tree<'value>) : MyList<'value> =
 match tree with
 | Leaf(hd) -> Cons(hd, Empty) //Set.empty.Add(hd)
 | Node (hd, tl) ->
     match tl with
     | [] -> Empty
     | [x] ->
         match x with
         | Node(hd1, tl1) -> Cons(hd, Cons(hd1, toMyList (Node(hd1, tl1))))//Set.empty.Add(hd) + toMyList (Node(hd1, tl1))
         | Leaf(hd) -> Cons(hd, Empty) //Set.empty.Add(hd)
     | hd1 :: tl1 -> Cons(hd, concat (toMyList hd1) (toMyList (Node (hd, tl1)))) //Set.empty.Add(hd) + toMyList hd1 + toMyList (Node (hd, tl1))
printf $"%A{toMyList tree1}"
