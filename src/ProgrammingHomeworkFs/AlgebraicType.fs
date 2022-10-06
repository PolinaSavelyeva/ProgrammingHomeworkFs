module AlgebraicType

// Алгебраический тип MyList- перемножаем декартово
type MyList<'value> =
    | Construct of head: 'value * tail: MyList<'value>
    | Empty

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

/// Функция newLst совмещает в себе работу rightLst и leftLst
let rec newLst f (x: 'value) (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> if f hd x  then Construct(hd, Empty) else Empty
    | Construct (hd, tl) ->
        if f hd x then
            Construct(hd, newLst f x tl)
        else
            newLst f x tl

let rec quickSort (lst: MyList<'value>) : MyList<'value> =
    let fMin x y = x <= y
    let fMax x y = x > y
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) -> concat ( concat (quickSort (newLst fMin hd tl)) (Construct(hd, Empty))) (quickSort (newLst fMax hd tl))


