module OopList

type IList<'Value> =
    interface
    end

type List<'Value>(head: 'Value, tail: IList<'Value>) =
    interface IList<'Value>
    member this.Head = head
    member this.Tail = tail

type EmptyList<'Value>() =
    interface IList<'Value>

let rec algebraicListOfIList (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> AlgebraicList.Empty
    | :? List<'Value> as lst -> AlgebraicList.Construct(lst.Head, algebraicListOfIList lst.Tail)
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -fromMyOOPListToMyList- function"

let rec iListOfAlgebraicList lst =
    match lst with
    | AlgebraicList.Empty -> EmptyList<'Value>() :> IList<'Value>
    | AlgebraicList.Construct (hd, tl) -> List<'Value>(hd, iListOfAlgebraicList tl)

let rec len (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> 0
    | :? List<'Value> as lst -> len lst.Tail + 1
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -lenMyOOPList- function"

let takeHead (lst: IList<'Value>) =
    match lst with
    | :? List<'Value> as lst -> lst.Head
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
             Expected MyOOPNoneEmptyList<'Value> type.\n
             Error in -takeOOPHead- function"

let takeTail (lst: IList<'Value>) =
    match lst with
    | :? List<'Value> as lst -> lst.Tail
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}, this.Tale member was not found.\n
            The type MyOOPNonEmptyList<'Value> which has this.Tale member was expected.\n
            Error in -takeOOPTail- function"

let bubbleSort lst =

    let rec oneLine (lst: IList<'Value>) =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> then
                lst
            elif lst.Head > takeHead lst.Tail then
                List(takeHead lst.Tail, oneLine (List(lst.Head, takeTail lst.Tail)))
            else
                List(lst.Head, oneLine lst.Tail)
        | _ ->
            failwith
                $"Incorrect type was given : {lst.GetType()}.\n
                Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
                Error in -oneOOPLine- function"

    let mutable sortedLine: IList<'Value> = lst

    for i in 1 .. len lst do
        sortedLine <- oneLine sortedLine

    sortedLine

let rec concat (lst1: IList<'Value>) (lst2: IList<'Value>) =
    match lst1 with
    | :? EmptyList<'Value> -> lst2
    | :? List<'Value> as lst1 -> List(lst1.Head, concat lst1.Tail lst2) :> IList<'Value>
    | _ ->
        failwith
            $"Incorrect types was given : {lst1.GetType()}, or {lst2.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -concatOOP- function"

let rec splitLst x (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> (EmptyList() :> IList<'Value>, EmptyList() :> IList<'Value>)
    | :? List<'Value> as lst ->
        let newParts = splitLst x lst.Tail

        if lst.Head <= x then
            List(lst.Head, fst newParts), snd newParts
        else
            fst newParts, List(lst.Head, snd newParts)
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -leftOOPLst- function"

let rec quickSort (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
    | :? List<'Value> as lst ->
        let newParts = splitLst lst.Head lst.Tail

        if lst.Tail :? EmptyList<'Value> then
            lst
        else
            concat (quickSort (fst newParts)) (List(lst.Head, quickSort (snd newParts)))
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -quickOOPSort- function"

let rec iListOfList lst =
    match lst with
    | [] -> EmptyList() :> IList<'Value>
    | hd :: tl -> List(hd, iListOfList tl)

let rec listOfIList (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> []
    | :? List<'Value> as lst -> lst.Head :: listOfIList lst.Tail
    | _ ->
        failwith
            $"Incorrect type was given : {lst.GetType()}.\n
            Expected MyOOPEmptyList<'Value> or MyOOPNonEmptyList<'Value> types.\n
            Error in -fromMyOOPListToList- function"
