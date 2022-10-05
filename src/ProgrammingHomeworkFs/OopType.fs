module OopType
open Abstraction

///  Функция lenMyOOPList возвращает длину IList
let rec lenMyOOPList (lst: IList<'value>) : int =
    match lst with
    | :? MyOOPEmptyList<'value> -> 0
    | :? MyOOPNonEmptyList<'value> as lst -> lenMyOOPList lst.Tail + 1
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\n Error in -lenMyOOPList- function."

/// Функция takeOOPHead возвращает this.Head
let takeOOPHead (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith "Incorrect type was given. Expected MyOOPNoneEmptyList<'value> type.\n Error in -takeOOPHead- function."

/// Функция takeOOPTail возвращает this.Tail
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

let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) : IList<'value> =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 ->
        MyOOPNonEmptyList(lst1.Head, concatOOP lst1.Tail lst2) :> IList<'value>
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -concatOOP- function."

let rec newOOPLst f (x: 'value) (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if f lst.Head x then
                lst
            else
                MyOOPEmptyList() :> IList<'value>
        elif f lst.Head x then
            MyOOPNonEmptyList(lst.Head, newOOPLst f x (MyOOPNonEmptyList(x, lst.Tail)))
        else
            newOOPLst f x lst.Tail
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -leftOOPLst- function."

let rec quickOOPSort (lst: IList<'value>) : IList<'value> =
    let fMin x y = x <= y
    let fMax x y = x > y
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            lst
        else
            concatOOP (concatOOP (quickOOPSort (newOOPLst fMin lst.Head lst.Tail)) (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) (quickOOPSort (newOOPLst fMax lst.Head lst.Tail))
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -quickOOPSort- function."

/// Рекурсивная функция fromMyOOPListToMyList, преобразовывающая MyOOPList -> MyList
let rec fromMyOOPListToMyList (lst: IList<'value>) : MyList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Construct(lst.Head, fromMyOOPListToMyList lst.Tail)
    | _ -> failwith "Incorrect type was given. Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types. \n Error in -fromMyOOPListToMyList- function."

