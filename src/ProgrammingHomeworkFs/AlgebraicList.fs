module AlgebraicList

// Алгебраический тип MyList- перемножаем декартово
type List<'value> =
    | Construct of head: 'value * tail: List<'value>
    | Empty

/// Функция lenMyList возвращает длину MyList
let rec len (lst: List<'value>) : int =
    match lst with
    | Empty -> 0
    | Construct (_, tl) -> len tl + 1

/// Функция oneLine выполняет "попарную" сортировку за 1 цикл, с нулевого до последнего элемента
let rec oneLine (lst: List<'value>) : List<'value> =
    match lst with
    | Empty -> Empty
    | Construct (_, Empty) -> lst
    | Construct (hd1, Construct (hd2, tl)) -> Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

let bubbleSort (lst: List<'value>) : List<'value> =
    let mutable sortedLine: List<'value> = lst

    for i in 1 .. len lst do
        sortedLine <- oneLine sortedLine

    sortedLine

/// Функция concat конкатинирует два листа типа MyList
let rec concat (lst1: List<'value>) (lst2: List<'value>) : List<'value> =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

/// Функция newLst совмещает в себе работу rightLst и leftLst
let rec splitLst (x: 'value) (lst: List<'value>) =
    match lst with
    | Empty -> Empty, Empty
    | Construct (hd, tl) ->
        let sorted = splitLst x tl

        if hd <= x then
            Construct(hd, fst sorted), snd sorted
        else
            fst sorted, Construct(hd, snd sorted)

let rec quickSort (lst: List<'value>) : List<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) ->
        let sorted = splitLst hd tl
        concat (quickSort (fst sorted)) (Construct(hd, quickSort (snd sorted)))
