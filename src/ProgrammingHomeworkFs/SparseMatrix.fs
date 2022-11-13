module SparseMatrix

open System

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

type QuadTree<'value> =
    | None
    | Leaf of 'value
    | Node of QuadTree<'value> * QuadTree<'value> * QuadTree<'value> * QuadTree<'value>

let toSquare (arr: 'value[,]) : int =
    let length1 = Array2D.length1 arr
    let length2 = Array2D.length2 arr
    let log1 = Math.Log(length1, 2)
    let log2 = Math.Log(length2, 2)
    if ceil log1 = log1 && ceil log2 = log2 && log1 = log2 then
        length1
    else
        int (2.0 ** ceil (max log1 log2))

(*let toDoubleMatrix (arr: array<array<Option<'value>>>) : array<array<Option<'value>>> =
    let string = arr.Length
    let column = arr[0].Length

    if
        (ceil (Math.Log(string, 2)) = Math.Log(string, 2))
        && (ceil (Math.Log(column, 2)) = Math.Log(column, 2))
        && (Math.Log(string, 2) = Math.Log(column, 2))
    then
        arr
    else
        let wide = int (2.0 ** ceil (Math.Log(max string column, 2)))
        let stringingWide = wide - string
        let columnWide = wide - column

        let mutable ans = Array.copy arr

        if stringingWide <> 0 then
            let arrOfNone = Array.create wide Option.None

            for i in 0 .. stringingWide - 1 do
                ans <- Array.append ans [| arrOfNone |]

        if columnWide <> 0 then
            for i in 0 .. string - 1 do
                ans[i] <- Array.append ans[i] (Array.create columnWide Option.None)

        ans

let toQuads (arr: array<array<Option<'value>>>) =
    let n = arr.Length / 2

    let ans1 = Array.create n [||]
    let ans2 = Array.create n [||]
    let ans3 = Array.create n [||]
    let ans4 = Array.create n [||]

    for i in 0 .. n - 1 do
        ans1[i] <- Array.sub arr[i] 0 n
        ans2[i] <- Array.sub arr[i] n n
        ans3[i] <- Array.sub arr[i + n] 0 n
        ans4[i] <- Array.sub arr[i + n] n n

    [| ans1; ans2; ans3; ans4 |]

let toQuadTree arr : QuadTree<'value, 'value0> =

    let fromOptionToQTree optionValue : QuadTree<'value, 'value0> =
        match optionValue with
        | Option.None -> QuadTree.None
        | Some x -> QuadTree.Leaf(x)

    let rec qTreeFormation (arr: array<array<Option<'value>>>) : QuadTree<'value, 'value0> =
        match arr with
        | [| [| x |] |] -> fromOptionToQTree x
        | _ ->
            let arrOfQuads = toQuads arr
            let quad1 = qTreeFormation arrOfQuads[0]
            let quad2 = qTreeFormation arrOfQuads[1]
            let quad3 = qTreeFormation arrOfQuads[2]
            let quad4 = qTreeFormation arrOfQuads[3]

            if quad1 = quad2 && quad2 = quad3 && quad3 = quad4 && quad4 = quad1 then
                quad1
            else
                QuadTree.Node(quad1, quad2, quad3, quad4)

    qTreeFormation (toDoubleMatrix arr)

type Matrix<'value, 'value0 when 'value: equality>(matrix: array<array<Option<'value>>>) =
    let length1 = matrix.Length
    let length2 = matrix[0].Length
    let storage = toQuadTree matrix
    let qLength = int (2.0 ** ceil (Math.Log(max length1 length2, 2)))

    member this.QLength = qLength
    member this.Length1 = length1
    member this.Length2 = length2
    member this.Storage = storage

let takeElementOfMatrix (i: int) (j: int) (matrix: Matrix<'value, 'value0>) =
    let mutable n = matrix.QLength

    let whichQuad (i: int) (j: int) =
        n <- n / 2

        if i < n && j < n then i, j, 0 // 1 quad
        elif i < n && j >= n then i, (j - n), 1 // 2 quad
        elif i >= n && j < n then (i - n), j, 2 // 3 quad
        else (i - n), (j - n), 3 // 4 quad

    let rec whichElement i j tree =
        match tree with
        | QuadTree.Leaf x -> Some(x)
        | QuadTree.None -> Option.None
        | QuadTree.Node (a, b, c, d) ->
            let num = whichQuad i j
            let x = third num

            if x = 0 then whichElement (first num) (second num) a
            elif x = 1 then whichElement (first num) (second num) b
            elif x = 2 then whichElement (first num) (second num) c
            else whichElement (first num) (second num) d

    if i < n && j < n then
        whichElement i j matrix.Storage
    else
        failwith "Index out of the range.  Error in -takeElementOfMatrix- function. "
*)
