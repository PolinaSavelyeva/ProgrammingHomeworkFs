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
    | Construct (hd1, Construct (hd2, tl)) ->
        Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

let bubbleSort (lst: MyList<'value>) : MyList<'value> =
    let mutable sortedLine: MyList<'value> = lst

    for i in 1 .. lenMyList lst do
        sortedLine <- oneLine sortedLine

    sortedLine

/// Функция concat конкатинирует два листа типа MyList
let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

let lft (a, _) = a
let rgt (_, a) = a

/// Функция newLst совмещает в себе работу rightLst и leftLst
let rec newLst (x: 'value) (lst: MyList<'value>) =
    match lst with
    | Empty -> Empty, Empty
    | Construct (hd, tl) ->
        if hd <= x then
            Construct(hd, lft (newLst x tl)), rgt (newLst x tl)
        else
            lft (newLst x tl), Construct(hd,  rgt (newLst x tl))

let rec quickSort (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) ->
        concat (quickSort (lft (newLst hd tl))) (Construct(hd, quickSort (rgt (newLst hd tl))))
