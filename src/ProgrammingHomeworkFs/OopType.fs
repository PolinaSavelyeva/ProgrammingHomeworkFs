module OopType
open AlgebraicType

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

/// Рекурсивная функция fromMyOOPListToMyList, преобразовывающая MyOOPList -> MyList
let rec fromMyOOPListToMyList (lst: IList<'value>) : MyList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Construct(lst.Head, fromMyOOPListToMyList lst.Tail)
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -fromMyOOPListToMyList- function."

/// Рекурсивная функция fromMyListToMyOOPList, преобразовывающая MyList -> MyOOPList
let rec fromMyListToMyOOPList lst =
    match lst with
    | Empty -> MyOOPEmptyList<'value>() :> IList<'value>
    | Construct (hd, tl) -> MyOOPNonEmptyList<'value>(hd, fromMyListToMyOOPList tl)

///  Функция lenMyOOPList возвращает длину IList
let rec lenMyOOPList (lst: IList<'value>) : int =
    match lst with
    | :? MyOOPEmptyList<'value> -> 0
    | :? MyOOPNonEmptyList<'value> as lst -> lenMyOOPList lst.Tail + 1
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -lenMyOOPList- function."

/// Функция takeOOPHead возвращает this.Head
let takeOOPHead (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPNoneEmptyList<'value> type.\
                      \n Error in -takeOOPHead- function."

/// Функция takeOOPTail возвращает this.Tail
let takeOOPTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            MyOOPEmptyList()
        else
            lst.Tail
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}, this.Tale member was not found.\
                      The type MyOOPNonEmptyList<'value> which has this.Tale member was expected.\
                      \n Error in -takeOOPTail- function."

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
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -oneOOPLine- function."

let bubbleOOPSort (lst: IList<'value>) : IList<'value> =
    let mutable sortedLine: IList<'value> = lst

    for i in 1 .. lenMyOOPList lst do
        sortedLine <- oneOOPLine sortedLine

    sortedLine

let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) : IList<'value> =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 ->
        MyOOPNonEmptyList(lst1.Head, concatOOP lst1.Tail lst2) :> IList<'value>
    | _ -> failwith $"Incorrect types was given : {lst1.GetType()}, or {lst2.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -concatOOP- function."

let rec newOOPLst (x: 'value) (lst: IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> -> (MyOOPEmptyList() :> IList<'value>, MyOOPEmptyList() :> IList<'value>)
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Head <= x then
            MyOOPNonEmptyList(lst.Head, lft (newOOPLst x lst.Tail)), rgt (newOOPLst x lst.Tail)
        else
            lft (newOOPLst x lst.Tail), MyOOPNonEmptyList(lst.Head, rgt (newOOPLst x lst.Tail))
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -leftOOPLst- function."

let rec quickOOPSort (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            lst
        else
            concatOOP (quickOOPSort (lft (newOOPLst lst.Head lst.Tail))) (MyOOPNonEmptyList(lst.Head, quickOOPSort (rgt (newOOPLst lst.Head lst.Tail))))
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -quickOOPSort- function."

let rec fromListToMyOOPList lst : IList<'value> =
    match lst with
    | [] -> MyOOPEmptyList() :> IList<'value>
    | hd :: tl -> MyOOPNonEmptyList(hd, fromListToMyOOPList(tl))

let rec fromMyOOPListToList (lst : IList<'value>) : list<'value> =
    match lst with
    | :? MyOOPEmptyList<'value> -> []
    | :? MyOOPNonEmptyList<'value> as lst ->  lst.Head :: fromMyOOPListToList lst.Tail
    | _ -> failwith $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -fromMyOOPListToList- function."
