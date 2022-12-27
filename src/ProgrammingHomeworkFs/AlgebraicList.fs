module AlgebraicList

type List<'Value> =
    | Construct of head: 'Value * tail: List<'Value>
    | Empty

let rec len lst =
    match lst with
    | Empty -> 0
    | Construct (_, tl) -> len tl + 1

let bubbleSort lst =

    let rec oneLine lst =
        match lst with
        | Empty -> Empty
        | Construct (_, Empty) -> lst
        | Construct (hd1, Construct (hd2, tl)) -> Construct(min hd1 hd2, oneLine (Construct(max hd1 hd2, tl)))

    let mutable sortedLine: List<'Value> = lst

    for i in 1 .. len lst do
        sortedLine <- oneLine sortedLine

    sortedLine

let rec concat lst1 lst2 =
    match lst1 with
    | Empty -> lst2
    | Construct (hd, tl) -> Construct(hd, concat tl lst2)

let rec splitList x lst =
    match lst with
    | Empty -> Empty, Empty
    | Construct (hd, tl) ->
        let sorted = splitList x tl

        if hd <= x then
            Construct(hd, fst sorted), snd sorted
        else
            fst sorted, Construct(hd, snd sorted)

let rec quickSort lst =
    match lst with
    | Empty -> Empty
    | Construct (hd, Empty) -> Construct(hd, Empty)
    | Construct (hd, tl) ->
        let sorted = splitList hd tl
        concat (quickSort (fst sorted)) (Construct(hd, quickSort (snd sorted)))
