module SparseMatrixVectorTests

open Expecto
open Converters

module SparseVectorTests =
    open SparseVector

    [<Tests>]
    let tests =
        testList
            "SparseVector tests"
            [ testCase "toSquare random array"
              <| fun _ ->
                  let arr =
                      toSquare [| Some(1); Some(2); Some(3); Some(4); Some(5); Some(6); Some(7); Some(8); Some(9) |]

                  Expect.equal arr 16u "toSquare expected : 16"
              testCase "toSquare one element array"
              <| fun _ ->
                  let arr = toSquare [| Some(1) |]
                  Expect.equal arr 1u "toSquare expected : 1"
              testCase "toSquare empty array"
              <| fun _ ->
                  let arr = toSquare [||]
                  Expect.equal arr 0u "toSquare expected : 0"
              testProperty "toSquare property test array"
              <| fun (arr: array<Option<int>>) ->
                  Expect.isLessThanOrEqual
                  <| arr.Length
                  <| toInt (toSquare arr)
                  <| "toSquare expected less than or equal array length result"
              testCase "toBinTree random array 1"
              <| fun _ ->
                  let tree = toBinTree [| Some(1); Some(1); Option.None; Option.None |]
                  Expect.equal tree (Node(Node(Leaf(1), Leaf(1)), BinTree.None)) "toBinTree expected : (Node(Node(Leaf(1), Leaf(1)), BinTree.None))"
              testCase "toBinTree string array"
              <| fun _ ->
                  let tree = toBinTree [| Some("qq"); Some("qd") |]
                  Expect.equal tree (Node(Leaf("qq"), Leaf("qd"))) "toBinTree expected : (Node(Node(Leaf(1), Leaf(1)), BinTree.None))"
              testCase "toBinTree None array"
              <| fun _ ->
                  let tree = toBinTree [| Option.None |]
                  Expect.equal tree BinTree.None "toBinTree expected : BinTree.None"
              testCase "toBinTree random array 2"
              <| fun _ ->
                  let tree =
                      toBinTree [| Some(1); Some(1); Option.None; Option.None; Some(1); Some(1); Option.None; Option.None; Some(1); Some(1); Option.None; Option.None |]

                  Expect.equal
                      tree
                      (Node(Node(Node(Node(Leaf 1, Leaf 1), None), Node(Node(Leaf 1, Leaf 1), None)), Node(Node(Node(Leaf 1, Leaf 1), None), None)))
                      "toBinTree expected : (Node (Node (Node (Node (Leaf 1, Leaf 1), None), Node (Node (Leaf 1, Leaf 1), None)), Node (Node (Node (Leaf 1, Leaf 1), None), None)))"
              testProperty "takeElementOfVector property test"
              <| fun (arr: array<Option<int>>) (i: uint) ->
                  let arr' = Array.append arr [| Some(1) |]
                  let i' = toInt i % arr'.Length

                  Expect.equal
                  <| arr'[i']
                  <| Vector(arr')[uint i']
                  <| "takeElementOfVector expected same result as Array.get"
              testCase "takeElementOfVector empty array"
              <| fun _ ->
                  let actualResult =
                      Expect.throws (fun _ -> Vector([||])[152u] |> ignore) "Index out of the range. Error in -takeElementOfVector- function."

                  actualResult
              testProperty "vectorAddition property test"
              <| fun (x: uint) ->

                  let length1 = x + 1u |> toInt

                  let rnd = System.Random()
                  let arr1 = Array.init length1 (fun _ -> rnd.Next(100))

                  let arr1Some =
                      arr1 |> Array.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                  let arr2 = Array.init length1 (fun _ -> rnd.Next(100))

                  let arr2Some =
                      arr2 |> Array.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                  let vector1 = Vector(arr1Some)
                  let vector2 = Vector(arr2Some)

                  let fPlus opt1 opt2 =
                      match opt1, opt2 with
                      | Option.Some a, Option.Some b -> Option.Some(a + b)
                      | Option.Some a, Option.None
                      | Option.None, Option.Some a -> Option.Some(a)
                      | Option.None, Option.None -> Option.None

                  let naiveAddition (arr1: array<Option<int>>) (arr2: array<Option<int>>) =

                      let length = arr1.Length
                      let mutable result = Array.zeroCreate length

                      for i in 0 .. length - 1 do
                          result[i] <- fPlus arr1[i] arr2[i]

                      result

                  let expectedResult = Vector(naiveAddition arr1Some arr2Some)
                  let actualResult = vectorAddition fPlus vector1 vector2

                  Expect.equal actualResult.Storage expectedResult.Storage "Undefined result. " ]

module SparseMatrixTests =
    open SparseMatrix

    [<Tests>]
    let tests =
        testList
            "SparseMatrix tests"
            [ testCase "toSquare random array2D 1"
              <| fun _ ->
                  let arr =
                      toSquare (array2D [ [ Some(1); Some(2); Some(3) ]; [ Some(4); Some(5); Some(6) ]; [ Some(7); Some(8); Some(9) ] ])

                  Expect.equal arr 4u "toSquare expected : 4"
              testCase "toSquare random array2D 2"
              <| fun _ ->
                  let arr =
                      toSquare (
                          array2D
                              [ [ Some(1); Some(2); Some(3) ]
                                [ Some(4); Some(5); Some(6) ]
                                [ Some(7); Some(8); Some(9) ]
                                [ Some(1); Some(2); Some(3) ]
                                [ Some(1); Some(2); Some(3) ]
                                [ Some(1); Some(2); Some(3) ] ]
                      )

                  Expect.equal arr 8u "toSquare expected : 8"
              testCase "toSquare one element array2D"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1) ] ])
                  Expect.equal arr 1u "toSquare expected : 1"
              testCase "toSquare random array2D 3"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1) ]; [ Some(1) ] ])
                  Expect.equal arr 2u "toSquare expected : 2"
              testCase "toSquare random array2D 4"
              <| fun _ ->
                  let arr = toSquare (array2D [ [ Some(1); Some(0) ] ])
                  Expect.equal arr 2u "toSquare expected : 2"
              testProperty "toSquare property test array2D"
              <| fun (arr: int option[,]) ->
                  Expect.isLessThanOrEqual
                  <| max (Array2D.length1 arr) (Array2D.length2 arr)
                  <| toInt (toSquare arr)
                  <| "toSquare expected less than or equal array length result"
              testCase "toQuadTree random array2D 1"
              <| fun _ ->
                  let tree = toQuadTree (array2D [ [ Some(1) ]; [ Some(1) ] ])
                  Expect.equal tree (Node(Leaf 1, None, Leaf 1, None)) "toQuadTree expected : (Node (Leaf 1, None, Leaf 1, None)) "
              testCase "toQuadTree empty array"
              <| fun _ ->
                  let tree = toQuadTree (array2D [ [] ])
                  Expect.equal tree QuadTree.None "toQuadTree expected : QuadTree.None"
              testCase "toQuadTree random array2D 2"
              <| fun _ ->
                  let tree =
                      toQuadTree (array2D [ [ Some(1); Some(1) ]; [ Option.None; Option.None ]; [ Some(1); Some(1) ] ])

                  Expect.equal
                      tree
                      (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))
                      "toQuadTree expected : (Node(Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None, Node(Leaf(1), Leaf(1), QuadTree.None, QuadTree.None), QuadTree.None))"
              testProperty "takeElementOfMatrix property test"
              <| fun (arr: int option[,]) (i: uint) (j: uint) ->
                  let arr' =
                      if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
                          (array2D [ [ Some(1); Some(2) ]; [ Some(2); Some(2) ] ])
                      else
                          arr

                  let i' = i % uint (Array2D.length1 arr')
                  let j' = j % uint (Array2D.length2 arr')

                  Expect.equal <| arr'[int i', int j'] <| Matrix(arr')[i', j'] <| "Unexpected result"
              testCase "takeElementOfMatrix empty array"
              <| fun _ ->
                  let actualResult =
                      Expect.throws (fun _ -> Matrix(array2D [ []; [] ])[152u, 11u] |> ignore) "Index out of the range. Error in -takeElementOfVector- function."

                  actualResult ]

module MatrixMultiplicationTests =

    open MatrixMultiplication
    open SparseVector
    open SparseMatrix

    let fPlusInt a b =
        match a, b with
        | Some x, Some y -> Some(x + y)
        | Option.None, Some x
        | Some x, Option.None -> Some x
        | Option.None, Option.None -> Option.None

    let fMultiInt a b =
        match a, b with
        | Some x, Some y -> Some(x * y)
        | Option.None, _
        | _, Option.None -> Option.None

    [<Tests>]
    let tests =
        testList
            "MatrixMultiplication tests"
            [ testCase "MatrixMultiplication random vector and matrix"
              <| fun _ ->
                  let vec = Vector([| Some(0); Some(1) |])
                  let mat = Matrix(array2D [ [ Some(1); Some(1) ]; [ Some(1); Some(1) ] ])
                  let res = multiplication fPlusInt fMultiInt vec mat

                  Expect.equal res.Storage (BinTree.Node(BinTree.Leaf(1), BinTree.Leaf(1))) "MatrixMultiplication expected : Node (Leaf 1, Leaf 1)"
              testCase "MatrixMultiplication empty vector and matrix"
              <| fun _ ->
                  let vec = Vector([||])
                  let mat = Matrix(array2D [])
                  let res = multiplication fPlusInt fMultiInt vec mat
                  Expect.equal res.Storage BinTree.None "MatrixMultiplication expected : BinTree.None"
              testCase "MatrixMultiplication None association vector and matrix"
              <| fun _ ->
                  let vec = Vector([| Some(1); Some(1); Some(1) |])

                  let mat =
                      Matrix(array2D [ [ Option.None; Option.None; Option.None ]; [ Option.None; Option.None; Option.None ]; [ Option.None; Option.None; Option.None ] ])

                  let res = multiplication fPlusInt fMultiInt vec mat
                  Expect.equal res.Storage BinTree.None "MatrixMultiplication expected : BinTree.None"
              testCase "MatrixMultiplication string vector and matrix"
              <| fun _ ->
                  let fPlusString (a: string option) (b: string option) =
                      match a, b with
                      | Some x, Some y -> Some(String.concat x [ y ])
                      | Some x, Option.None
                      | Option.None, Some x -> Some x
                      | Option.None, Option.None -> Option.None

                  let fMultiString (a: string option) (b: string option) =
                      match a, b with
                      | Some x, Some y -> Some(String.replicate x.Length y)
                      | Option.None, _
                      | _, Option.None -> Option.None

                  let vec = Vector([| Some("a"); Some("ab"); Some("abc") |])

                  let mat =
                      Matrix(array2D [ [ Option.None; Some("abcd"); Option.None ]; [ Option.None; Option.None; Some("aa") ]; [ Some("abcsd"); Some("d"); Option.None ] ])

                  let res = multiplication fPlusString fMultiString vec mat

                  Expect.equal
                      res.Storage
                      (BinTree.Node(BinTree.Node(BinTree.Leaf("abcsdabcsdabcsd"), BinTree.Leaf("ddd")), BinTree.Node(BinTree.Leaf("aaaa"), BinTree.None)))
                      "MatrixMultiplication expected : Node (Node (Leaf 'abcsdabcsdabcsd', Leaf 'ddd'), Node (Leaf 'aaaa', None)) "
              testProperty "MatrixMultiplication property test"
              <| fun (x: uint) (y: uint) ->

                  let length1 = x + 1u |> toInt
                  let length2 = y + 1u |> toInt

                  let rnd = System.Random()
                  let arr = Array.init length1 (fun _ -> rnd.Next(100))
                  let arrSome = arr |> Array.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                  let arr2d = Array2D.init length1 length2 (fun _ _ -> rnd.Next(100))

                  let arr2dSome =
                      arr2d |> Array2D.map (fun n -> if n % 2 = 0 then Some n else Option.None)

                  let vector = Vector(arrSome)
                  let matrix = Matrix(arr2dSome)

                  let naiveMulti (arr: int option[]) (arr2d: int option[,]) =
                      let fPlus opt1 opt2 =
                          match opt1, opt2 with
                          | Option.Some a, Option.Some b -> Option.Some(a + b)
                          | Option.Some a, Option.None
                          | Option.None, Option.Some a -> Option.Some(a)
                          | Option.None, Option.None -> Option.None

                      let fMulti opt1 opt2 =
                          match opt1, opt2 with
                          | Option.Some a, Option.Some b -> Option.Some(a * b)
                          | _, Option.None
                          | Option.None, _ -> Option.None


                      let rows = arr.Length
                      let columns = Array2D.length2 arr2d
                      let mutable result = Array.zeroCreate columns

                      for j in 0 .. columns - 1 do
                          for i in 0 .. rows - 1 do
                              result[j] <- fPlus result[j] (fMulti arr[i] arr2d[i, j])

                      result

                  let rec isNoneReduce tree =
                      match tree with
                      | BinTree.Node (BinTree.None, BinTree.None) -> false
                      | BinTree.Node (x, y) -> isNoneReduce x && isNoneReduce y
                      | BinTree.Leaf _ -> true
                      | BinTree.None -> true

                  let expectedResult = Vector(naiveMulti arrSome arr2dSome)
                  let actualResult = multiplication fPlusInt fMultiInt vector matrix
                  let actualResult' = isNoneReduce actualResult.Storage

                  Expect.equal
                      actualResult.Storage
                      expectedResult.Storage
                      $"\n Array : %A{arrSome}\n
                        \n Array2d : %A{arr2dSome}\n
                        \n Array tree : %A{vector.Storage}\n
                        \n Matrix tree : %A{matrix.Storage}\n. "

                  Expect.equal actualResult.Length matrix.Length1 "Expected actualResult.Length = matrix.Length1. "

                  Expect.equal actualResult' true "Tree is not reduced. " ]
