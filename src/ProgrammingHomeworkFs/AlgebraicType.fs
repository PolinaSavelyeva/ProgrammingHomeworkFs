module AlgebraicType
open Abstraction

/// Функция lenMyList возвращает длину MyList
let rec lenMyList (lst: MyList<'value>) : int =
    match lst with
    | Empty -> 0
    | Construct (_, Empty) -> 1
    | Construct (_, Construct (hd, tl)) -> lenMyList (Construct(hd, tl)) + 1

/// Функция oneLine выполняет "попарную" сортировку за 1 цикл, с нулевого до последнего элемента
let rec oneLine (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd1, Construct (hd2, tl)) -> Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

let bubbleSort (lst: MyList<'value>) : MyList<'value> =
    let mutable sortedLine: MyList<'value> = lst

    for i in 0 .. (lenMyList lst - 2) do
        sortedLine <- oneLine sortedLine

    sortedLine

/// Функция concat конкатинирует два листа типа MyList
let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

/// Функция leftLst возвращает MyList "левого" под-листа, состоящего из элементов lst <= опорного
let rec leftLst (x: 'value) (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> if hd <= x then Construct(hd, Empty) else Empty
    | Construct (hd, tl) ->
        if hd <= x then
            Construct(hd, leftLst x tl)
        else
            leftLst x tl

/// Функция rightLst возвращает MyList "правого" под-листа, состоящего из элементов lst > опорного
let rec rightLst (x: 'value) (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> if hd > x then Construct(hd, Empty) else Empty
    | Construct (hd, tl) ->
        if hd > x then
            Construct(hd, rightLst x tl)
        else
            rightLst x tl

let rec quickSort (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) -> concat (concat (quickSort (leftLst hd tl)) (Construct(hd, Empty))) (quickSort (rightLst hd tl))
