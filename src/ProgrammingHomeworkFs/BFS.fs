module BreadthFirstSearch

open System
open SparseMatrix
open SparseVector
open MatrixMultiplication

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

let toBinTreeFromCOO (vertexList: list<int>) (length : int) =

    let partition (list : list<int>) (length : int)=
        let rec f list left right =
            match list with
            | []  -> left, right
            | hd :: tl ->
                if hd < length/2 then
                    f tl (hd :: left) right
                else
                    f tl left ((hd - length/2) :: right)
        f list [] []

    let rec binTreeFormation (list : list<int>) (length : int) =
       if length = 1 then
           if List.length list = 0 then
               BinTree.None
           else
               BinTree.Leaf(true)
       else
           let left, right = partition list length

           let left' = binTreeFormation left (length/2)
           let right' = binTreeFormation right (length/2)

           if left' = BinTree.None && right' = BinTree.None then
               BinTree.None
           else
               BinTree.Node(left', right')

    if length = 0 then
        Vector(BinTree.None, length, length)
    else
        let squareLength = int (2.0 ** ceil (Math.Log(length, 2)))
        Vector(binTreeFormation vertexList squareLength, length, squareLength)

let toQuadTreeFromCOO (tripleList : (int * int * 'value option) list) (rows: int) (columns: int) (length : int) =

    let partition (list : (int * int * 'value option) list) (length : int) =
        let rec f list one two three four =
            match list with
            | []  -> one, two, three, four
            | hd :: tl ->
                if first hd < length/2 then
                    if second hd < length/2 then
                        f tl (hd :: one) two three four
                    else
                        f tl one ((first hd, second hd - length/2, third hd) :: two) three four
                else
                    if second hd < length/2 then
                        f tl one two ((first hd - length/2, second hd, third hd):: three) four
                    else
                        f tl one two three ((first hd - length/2, second hd - length/2, third hd) :: four)
        f list [] [] [] []

    let rec quadTreeFormation (list : (int * int * 'value option) list) (length : int) =
       if length = 1 then
           if List.length list = 0 then
               QuadTree.None
           else
               QuadTree.Leaf(third list[0])
       else
           let one, two, three, four = partition list length

           let one' = quadTreeFormation one (length/2)
           let two' = quadTreeFormation two (length/2)
           let three' = quadTreeFormation three (length/2)
           let four' = quadTreeFormation four (length/2)

           if one' = QuadTree.None && two' = QuadTree.None && three' = QuadTree.None && four' = QuadTree.None then
               QuadTree.None
           else
               QuadTree.Node(one', two', three', four')

    if length = 0 then
        Matrix(QuadTree.None, length, length, length)
    else
        let squareLength = int (2.0 ** ceil (max (Math.Log(rows, 2)) (Math.Log(columns, 2))))
        Matrix(quadTreeFormation tripleList squareLength, rows, columns, squareLength)

(*let toQuadTree (tripleList : (int * int * 'value option) list) (rows: int) (columns: int) (length : int) =

    let rec quadTreeFormation (list : (int * int *'value option) list) (length : int) =
       if length = 1 then
           if List.length list = 0 then
               QuadTree.None
           else
               QuadTree.Leaf(third list[0])
       else
           let firstSecond, thirdFourth = list |> List.partition (fun n -> first n < length/2)
           let one, two = firstSecond |> List.partition (fun n -> second n < length/2)
           let three, four = thirdFourth |> List.partition (fun n -> second n < length/2)

           let left', right' = binTreeFormation left (length/2), binTreeFormation (right |> List.map (fun n -> n - length/2)) (length/2)

           if left' = BinTree.None && right' = BinTree.None then
               BinTree.None
           else
               BinTree.Node( left', right')

    let squareLength =
        if (ceil (Math.Log(length, 2))) = Math.Log(length, 2) then
            length
        else
            int (2.0 ** ceil (Math.Log(length, 2)))

    binTreeFormation vertexList squareLength*)


(*let toQuadTreeFromCOO (tripleList: (int * 'value option * int) list) (rows: int) (columns: int) (length : int)=

    let toQuads (tripleList: (int * 'value option * int) list) (size: int) =
        let fQuad1 triple =
            (first triple) < size && (third triple) < size

        let fQuad2 triple =
            (first triple) < size && (third triple) >= size

        let fQuad3 triple =
            (first triple) >= size && (third triple) < size

        let fQuad4 triple =
            (first triple) >= size && (third triple) >= size

        let firstQuadList = List.filter fQuad1 tripleList

        let secondQuadList = List.filter fQuad2 tripleList
        let secondQuadList' = List.map (fun (x, y, z) -> (x, y, z - size)) secondQuadList

        let thirdQuadList = List.filter fQuad3 tripleList
        let thirdQuadList' = List.map (fun (x, y, z) -> (x - size, y, z)) thirdQuadList

        let forthQuadList = List.filter fQuad4 tripleList

        let forthQuadList' =
            List.map (fun (x, y, z) -> (x - size, y, z - size)) forthQuadList

        firstQuadList, secondQuadList', thirdQuadList', forthQuadList'

    let leafFormation a =
        match a with
        | [] -> QuadTree.None
        | [ _, Option.None, _ ] -> QuadTree.None
        | [ _, Some x, _ ] -> QuadTree.Leaf x
        | _ ->
            failwith
                "Expected a.Length = 1.\n
                          Error in -fromCOOToQuadTree- function. "

    let rec nodeOrganization (quad: (int * 'value option * int) list) size =
        if size = 1 then
            leafFormation quad
        else
            let one, two, three, four = toQuads quad (size / 2)

            let one' = nodeOrganization one (size / 2)
            let two' = nodeOrganization two (size / 2)
            let three' = nodeOrganization three (size / 2)
            let four' = nodeOrganization four (size / 2)

            if one' = QuadTree.None && two' = QuadTree.None && three' = QuadTree.None && four' = QuadTree.None then
                QuadTree.None
            else
                QuadTree.Node(one', two', three', four')


    if tripleList.Length = 1 then
        leafFormation tripleList
    elif tripleList.Length = 0 then
        QuadTree.None
    else
        let squareLength =
            int (2.0 ** ceil (max (Math.Log(length1, 2)) (Math.Log(length2, 2))))

        let one, two, three, four = toQuads tripleList (squareLength / 2)
        QuadTree.Node(nodeOrganization one (squareLength / 2), nodeOrganization two (squareLength / 2), nodeOrganization three (squareLength / 2), nodeOrganization four (squareLength / 2))

let toBinTreeFromCOO (tupleList: (int * 'value option) list) (length: int) =

    let toSides (tupleList: (int * 'value option) list) (size: int) =
        let left tuple = fst tuple < size

        let right tuple = fst tuple >= size

        let leftList = List.filter left tupleList

        let rightList = List.filter right tupleList
        let rightList' = List.map (fun (x, y) -> (x - size, y)) rightList

        leftList, rightList'


    let leafFormation a =
        match a with
        | [] -> BinTree.None
        | [ _, Option.None ] -> BinTree.None
        | [ _, Some x ] -> BinTree.Leaf x
        | _ ->
            failwith
                "Expected a.Length = 1.\n
                          Error in -toBinTreeFromCOO- function. "

    let rec sideOrganization (side: (int * 'value option) list) size =
        if size = 1 then
            leafFormation side
        else
            let one, two = toSides side (size / 2)

            let one' = sideOrganization one (size / 2)
            let two' = sideOrganization two (size / 2)

            if one' = BinTree.None && two' = BinTree.None then
                BinTree.None
            else
                BinTree.Node(one', two')


    if tupleList.Length = 1 then
        leafFormation tupleList
    elif tupleList.Length = 0 then
        BinTree.None
    else
        let squareLength = int (2.0 ** ceil (Math.Log(length, 2)))

        let one, two = toSides tupleList (squareLength / 2)
        BinTree.Node(sideOrganization one (squareLength / 2), sideOrganization two (squareLength / 2))

let BFS (start: list<int>) (gMatrix: Matrix<'value>) =
    //TODO fPlus fMulti

    let rec toTriples (list: list<int>) weight =
        match list with
        | [] -> []
        | [ x ] -> [ (x, weight) ]
        | hd :: tl -> (hd, weight) :: toTriples tl weight

    let fPlus (a: Option<bool>) (b: Option<bool>) : Option<bool> = // bool + value = bool
        match a, b with
        | Some true, _ -> Some true
        | Option.None, Option.None -> Option.None //?
        | Option.None, _ -> Some true
        | _ -> failwith "Lol1"

    let fMulti (a: Option<bool>) (b: Option<'value>) : Option<bool> =
        match a, b with
        | Some true, Some _ -> Some true
        | Option.None, _
        | _, Option.None -> Option.None //?
        | _ -> failwith "Lol2"

    let rec innerBFS (visited: Vector<int>) (front: Vector<bool>) (current: int) =
        let front' = multiplication fPlus fMulti front gMatrix

        if front'.Storage = BinTree.None then
            visited
        else
            let rec reverse tree =
                match tree with
                | BinTree.None -> BinTree.Leaf true
                | BinTree.Leaf _ -> BinTree.None
                | BinTree.Node (left, right) ->
                    let left' = reverse left
                    let right' = reverse right

                    if left' = BinTree.None && right' = BinTree.None then
                        BinTree.None
                    else
                        BinTree.Node(reverse left, reverse right)

            let reverseVisited =
                Vector(reverse visited.Storage, visited.Length, visited.SquareLength) //[| N 2 1 0 N 1 N |]

            let ans = Array.create visited.Length Option.None

            for i in 0 .. visited.Length - 1 do
                if front'[i] = Some true && reverseVisited[i] = Some true then
                    ans[i] <- Some current
                elif front'[i] = Option.None && reverseVisited[i] = Some true then
                    ans[i] <- visited[i]
                elif front'[i] = Some true && reverseVisited[i] = Option.None then
                    ans[i] <- visited[i]
                else
                    ans[i] <- Option.None

            let visited' = Vector(toBinTree ans, visited.Length, visited.SquareLength)

            innerBFS visited' front' (current + 1)

    let front =
        Vector(toBinTreeFromCOO (toTriples start (Some true)) gMatrix.Length1, gMatrix.SquareLength, gMatrix.SquareLength)

    let visited =
        Vector(toBinTreeFromCOO (toTriples start (Some 0)) gMatrix.Length1, gMatrix.SquareLength, gMatrix.SquareLength)

    innerBFS visited front 1*)
