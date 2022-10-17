module Tree

open AlgebraicList

// Тип Tree - дерево с произвольным количеством узлов
type Tree<'value> = Node of hd: 'value * tl: list<Tree<'value>>

/// Функция uniqueValues возвращает количество "уникальных" элементов в дереве с использованием множеств
let uniqueValues (tree: Tree<'value>) : int =
    let rec setOfValues (tree: Tree<'value>) : Set<'value> =
        match tree with
        | Node (hd, []) -> Set.empty.Add hd
        | Node (hd, tl) ->
            match tl with
            | [ x ] ->
                match x with
                | Node (hd1, []) -> Set.empty.Add hd + Set.empty.Add hd1
                | Node (hd1, tl1) -> Set.empty.Add hd + setOfValues (Node(hd1, tl1))
            | hd1 :: tl1 -> setOfValues hd1 + setOfValues (Node(hd, tl1))
            | _ -> failwith " System error.\Expected first branch of match : Node (hd, []). \Error in -uniqueValues- function. "

    Set.count (setOfValues tree)

/// Функция toList преобразует тип Tree в ALgebraicList.List
let rec toList (tree: Tree<'value>) : AlgebraicList.List<'value> =
    match tree with
    | Node (hd, []) -> Construct(hd, Empty)
    | Node (hd, tl) ->
        match tl with
        | [ x ] ->
            match x with
            | Node (hd1, []) -> Construct(hd, Construct(hd1, Empty))
            | Node (hd1, tl1) -> Construct(hd, toList (Node(hd1, tl1)))
        | hd1 :: tl1 -> concat (toList (Node(hd, tl1))) (toList hd1)
        | _ -> failwith " System error.\Expected first branch of match : Node (hd, []). \Error in -toList- function. "
