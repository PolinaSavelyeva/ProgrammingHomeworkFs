module AlgebraicList

// Алгебраический тип AlgebraicList.List - перемножаем декартово
type List<'value> =
    | Construct of head: 'value * tail: List<'value>
    | Empty

/// Функция len возвращает длину AlgebraicList.List
let rec len (lst: List<'value>) : int =
    match lst with
    | Empty -> 0
    | Construct (_, tl) -> len tl + 1

let bubbleSort (lst: List<'value>) : List<'value> =

    /// Функция oneLine выполняет "попарную" сортировку за 1 цикл, с нулевого до последнего элемента
    let rec oneLine (lst: List<'value>) : List<'value> =
        match lst with
        | Empty -> Empty
        | Construct (_, Empty) -> lst
        | Construct (hd1, Construct (hd2, tl)) -> Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

    let mutable sortedLine: List<'value> = lst

    for i in 1 .. len lst do
        sortedLine <- oneLine sortedLine

    sortedLine

/// Функция concat конкатинирует два листа типа AlgebraicList.List
let rec concat (lst1: List<'value>) (lst2: List<'value>) : List<'value> =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

/// Функция splitList совмещает в себе работу rightLiist и leftList
let rec splitList (x: 'value) (lst: List<'value>) =
    match lst with
    | Empty -> Empty, Empty
    | Construct (hd, tl) ->
        let sorted = splitList x tl

        if hd <= x then
            Construct(hd, fst sorted), snd sorted
        else
            fst sorted, Construct(hd, snd sorted)

let rec quickSort (lst: List<'value>) : List<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) ->
        let sorted = splitList hd tl
        concat (quickSort (fst sorted)) (Construct(hd, quickSort (snd sorted)))
