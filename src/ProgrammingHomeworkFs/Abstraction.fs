module Abstraction

//------------------------------------------------------ВВЕДЕНИЕ------------------------------------
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

/// Рекурсивная функция fromMyListToMyOOPList, преобразовывающая MyList -> MyOOPList
let rec fromMyListToMyOOPList lst =
    match lst with
    | Empty -> MyOOPEmptyList<'value>() :> IList<'value>
    | Construct (hd, tl) -> MyOOPNonEmptyList<'value>(hd, fromMyListToMyOOPList tl)
