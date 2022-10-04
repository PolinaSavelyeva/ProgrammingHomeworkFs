module ProgrammingHomeworkFs

//Объявление нового типа t5 (списка), являющегося декартовым произведением числа * строки * булевого значения * числа
type t5 = int * string * bool * int

// Создание переменной f типа t5
let f: t5 = (1, "3", true, 3)

// Создание функции g, принимающей значение типа t5 и возвращающей список : (2,"1",false,5)
let g (x: t5) =
    match x with // Разбиение на соответствия с помощью match with
    | _, _, _, _ -> (2, "1", false, 5)

//------------------------------------------------------АЛГЕБРАИЧЕСКИЙ-ТИП--------------------------
// Алгебраический тип MyList- перемножаем декартово
type MyList<'value> =
    | Construct of head: 'value * tail: MyList<'value>
    | Empty

// Рекурсивная функция map, применяющая функцию f ко всем элементам lst типа MyList
let rec map f lst =
    match lst with
    | Empty -> Empty
    | Construct (hd, tl) -> Construct(f hd, map f tl)

// Функция gо, вызывающая функцию map, которая к каждому элементу прибавляет +1
let go () =
    map ((+) 1) (Construct(1, Construct(3, Empty)))

// Функция _gо, вызывающая функцию map, которая к каждому элементу прибавляет -1
let _go () =
    map ((-) 1) (Construct(1, Construct(3, Empty)))

//------------------------------------------------------------ООП-ТИП-------------------------------
// Объявление нового типа IList
type IList<'value> =
    interface
    end

// Объявление нового типа MyOOPNonEmptyList
//[<AllowNullLiteral>]
type MyOOPNonEmptyList<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

// Объявление нового типа MyOOPEmptyList
type MyOOPEmptyList<'value>() =
    interface IList<'value>

// Объявление нового типа IActor
type IActor<'inType, 'outType> =
    abstract Do: 'inType -> 'outType

// Создание типа PlusOneActor на основе IActor, прибавляющий к значению +1
type PlusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x + 1

// Создание типа MinusOneActor на основе IActor, прибавляющий к значению -1
type MinusOneActor() =
    interface IActor<int, int> with
        member this.Do x = x - 1

// Рекурсивная функция oopMap1, аналогичная функции map
let rec oopMap (f: IActor<'value, 'result>) (lst: IList<'value>) =
    if
        lst :? MyOOPEmptyList<'value> // :?  оператор проверки типа (true/false)
    then
        MyOOPEmptyList() :> IList<'result> // :> оператор преобразует тип в тип, находящийся на более высоком уровне иерархии
    elif lst :? MyOOPNonEmptyList<'value> then
        let lst1 = lst :?> MyOOPNonEmptyList<'value> // :?> оператор преобразует тип в тип, находящийся на более низком уровне иерархии
        MyOOPNonEmptyList(f.Do lst1.Head, oopMap f lst1.Tail)
    else
        failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -oopMap- function."

// Рекурсивная функция oopMap2, аналогичная функции oopMap1
let rec oopMap2 (f: IActor<'value, 'result>) (lst: IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'result> // преобразование необходимо для устранения конфликта между разными типами, получаемыми из функции
    | :? MyOOPNonEmptyList<'value> as lst -> // разные lst
        MyOOPNonEmptyList(f.Do lst.Head, oopMap f lst.Tail)
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\n Error in -oopMap2- function."

// Функция go2, вызывающая функцию oopMap, которая к каждому элементу прибавляет +1
let go2 () =
    let lst = MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList()))

    oopMap (PlusOneActor()) lst

// Функция _go2, вызывающая функцию oopMap, которая к каждому элементу прибавляет -1
let _go2 () =
    let lst = MyOOPNonEmptyList(1, MyOOPNonEmptyList(3, MyOOPEmptyList()))

    oopMap (MinusOneActor()) lst

// Рекурсивная функция fromMyListToMyOOPList, преобразовывающая MyList -> MyOOPList
let rec fromMyListToMyOOPList lst =
    match lst with
    | Empty -> MyOOPEmptyList<'value>() :> IList<'value>
    | Construct (hd, tl) -> MyOOPNonEmptyList<'value>(hd, fromMyListToMyOOPList tl)

//------------------------------------------------------№1----АЛГЕБРАИЧЕСКИЙ-ТИП--------------------
/// Функция lenMyList возвращает длину MyList
let rec lenMyList (lst: MyList<'value>) : int =
    match lst with
    | Empty -> 0
    | Construct (_, Empty) -> 1
    | Construct (_, Construct (hd, tl)) -> lenMyList (Construct(hd, tl)) + 1

/// Функция oneLine выполняет "попарную" сортировку за 1 цикл, с нулевого до последнего элементов
let rec oneLine (lst: MyList<'value>) : MyList<'value> =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd1, Construct (hd2, tl)) -> Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

let bubbleSort (lst: MyList<'value>) : MyList<'value> =
    let mutable sortedLine: MyList<'value> = lst

    for i in
        0 .. (lenMyList lst - 2) do
        sortedLine <- oneLine sortedLine

    sortedLine

//------------------------------------------------------№3----АЛГЕБРАИЧЕСКИЙ-ТИП--------------------
let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) : MyList<'value> =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

//------------------------------------------------------№2----АЛГЕБРАИЧЕСКИЙ-ТИП--------------------
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

/// Функция rightLst возвращает MyList "правого под-листа, состоящего из элементов lst > опорного
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

//------------------------------------------------------№1----ООП-ТИП-------------------------------
let rec lenMyOOPList (lst: IList<'value>) : int =
    match lst with
    | :? MyOOPEmptyList<'value> -> 0
    | :? MyOOPNonEmptyList<'value> as lst -> lenMyOOPList lst.Tail + 1
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\n Error in -lenMyOOPList- function."

// Функция takeOOPHead возвращает this.Head
let takeOOPHead (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith "Incorrect type was given. Expected MyOOPNoneEmptyList<'value> type.\n Error in -takeOOPHead- function."

// Функция takeOOPHead возвращает this.Tail
let takeOOPTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            MyOOPEmptyList()
        else
            lst.Tail
    | _ -> failwith "Incorrect type was given, this.Tale member was not found. The type MyOOPNonEmptyList<'value> which has this.Tale member was expected.\n Error in -takeOOPTail- function."

let rec oneOOPLine (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            lst
        elif lst.Head > takeOOPHead lst.Tail then
            MyOOPNonEmptyList( takeOOPHead lst.Tail, oneOOPLine (MyOOPNonEmptyList(lst.Head, takeOOPTail lst.Tail)))
        else
            MyOOPNonEmptyList(lst.Head, oneOOPLine lst.Tail)
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\n Error in -oneOOPLine- function."

let bubbleOOPSort (lst: IList<'value>) : IList<'value> =
    let mutable sortedLine: IList<'value> = lst

    for i in 0 .. (lenMyOOPList lst - 2) do
        sortedLine <- oneOOPLine sortedLine

    sortedLine

//------------------------------------------------------№3----ООП-ТИП-------------------------------
let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) : IList<'value> =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 ->
        MyOOPNonEmptyList(lst1.Head, concatOOP lst1.Tail lst2) :> IList<'value>
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -concatOOP- function."

//------------------------------------------------------№2----ООП-ТИП--------------------
let rec leftOOPLst (x: 'value) (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if lst.Head <= x then
                lst
            else
                MyOOPEmptyList() :> IList<'value>
        elif lst.Head <= x then
            MyOOPNonEmptyList(lst.Head, leftOOPLst x (MyOOPNonEmptyList(x, lst.Tail)))
        else
            leftOOPLst x lst.Tail
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -leftOOPLst- function."

let rec rightOOPLst (x: 'value) (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if lst.Head > x then
                lst
            else
                MyOOPEmptyList() :> IList<'value>
        elif lst.Head > x then
            MyOOPNonEmptyList(lst.Head, rightOOPLst x (MyOOPNonEmptyList(x, lst.Tail)))
        else
            rightOOPLst x lst.Tail
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -rightOOPLst- function."

let rec quickOOPSort (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            lst
        else
            concatOOP (concatOOP (quickOOPSort (leftOOPLst lst.Head lst.Tail)) (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) (quickOOPSort (rightOOPLst lst.Head lst.Tail))
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -quickOOPSort- function."

let rec fromMyOOPListToMyList (lst: IList<'value>) : MyList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Construct(lst.Head, fromMyOOPListToMyList lst.Tail)
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -fromMyOOPListToMyList- function."
