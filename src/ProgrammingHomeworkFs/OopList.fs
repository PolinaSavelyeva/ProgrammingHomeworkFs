module OopList

// Объявление нового типа IList
type IList<'value> =
    interface
    end

// Объявление нового типа OopList.List
//[<AllowNullLiteral>]
type List<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

// Объявление нового типа OopList.EmptyList
type EmptyList<'value>() =
    interface IList<'value>

/// Рекурсивная функция algebraicListOfIList, преобразовывающая OopList.List -> AlgebraicList.List
let rec algebraicListOfIList (lst: IList<'value>) : AlgebraicList.List<'value> =
    match lst with
    | :? EmptyList<'value> -> AlgebraicList.Empty
    | :? List<'value> as lst -> AlgebraicList.Construct(lst.Head, algebraicListOfIList lst.Tail)
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -fromMyOOPListToMyList- function."

/// Рекурсивная функция iListOfAlgebraicList, преобразовывающая AlgebraicList.List -> OopList.List
let rec iListOfAlgebraicList (lst: AlgebraicList.List<'value>) : IList<'value> =
    match lst with
    | AlgebraicList.Empty -> EmptyList<'value>() :> IList<'value>
    | AlgebraicList.Construct (hd, tl) -> List<'value>(hd, iListOfAlgebraicList tl)

///  Функция len возвращает длину List
let rec len (lst: IList<'value>) : int =
    match lst with
    | :? EmptyList<'value> -> 0
    | :? List<'value> as lst -> len lst.Tail + 1
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -lenMyOOPList- function."

/// Функция takeHead возвращает this.Head
let takeHead (lst: IList<'value>) : 'value =
    match lst with
    | :? List<'value> as lst -> lst.Head
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPNoneEmptyList<'value> type.\
                      \n Error in -takeOOPHead- function."

/// Функция takeTail возвращает this.Tail
let takeTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? List<'value> as lst -> lst.Tail
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}, this.Tale member was not found.\
                      The type MyOOPNonEmptyList<'value> which has this.Tale member was expected.\
                      \n Error in -takeOOPTail- function."

let bubbleSort (lst: IList<'value>) : IList<'value> =

    let rec oneLine (lst: IList<'value>) : IList<'value> =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? List<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                lst
            elif lst.Head > takeHead lst.Tail then
                List(takeHead lst.Tail, oneLine (List(lst.Head, takeTail lst.Tail)))
            else
                List(lst.Head, oneLine lst.Tail)
        | _ ->
            failwith
                $"Incorrect type was given : {lst.GetType()}.\
                          Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                          \n Error in -oneOOPLine- function."

    let mutable sortedLine: IList<'value> = lst

    for i in 1 .. len lst do
        sortedLine <- oneLine sortedLine

    sortedLine

let rec concat (lst1: IList<'value>) (lst2: IList<'value>) : IList<'value> =
    match lst1 with
    | :? EmptyList<'value> -> lst2
    | :? List<'value> as lst1 -> List(lst1.Head, concat lst1.Tail lst2) :> IList<'value>
    | _ ->
        failwith
            $"Incorrect types was given : {lst1.GetType()}, or {lst2.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -concatOOP- function."

let rec splitLst (x: 'value) (lst: IList<'value>) =
    match lst with
    | :? EmptyList<'value> -> (EmptyList() :> IList<'value>, EmptyList() :> IList<'value>)
    | :? List<'value> as lst ->
        let newParts = splitLst x lst.Tail

        if lst.Head <= x then
            List(lst.Head, fst newParts), snd newParts
        else
            fst newParts, List(lst.Head, snd newParts)
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -leftOOPLst- function."

let rec quickSort (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'value>
    | :? List<'value> as lst ->
        let newParts = splitLst lst.Head lst.Tail

        if lst.Tail :? EmptyList<'value> then
            lst
        else
            concat (quickSort (fst newParts)) (List(lst.Head, quickSort (snd newParts)))
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -quickOOPSort- function."

let rec iListOfList lst : IList<'value> =
    match lst with
    | [] -> EmptyList() :> IList<'value>
    | hd :: tl -> List(hd, iListOfList tl)

let rec listOfIList (lst: IList<'value>) =
    match lst with
    | :? EmptyList<'value> -> []
    | :? List<'value> as lst -> lst.Head :: listOfIList lst.Tail
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\
                      Expected MyOOPEmptyList<'value> or MyOOPNonEmptyList<'value> types.\
                      \n Error in -fromMyOOPListToList- function."
