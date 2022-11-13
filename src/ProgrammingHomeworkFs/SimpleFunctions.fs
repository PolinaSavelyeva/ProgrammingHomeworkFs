module SimpleFunctions

let expo (basement: float) (exponent: int) : float =
    if basement = 0.0 && exponent = 0 then
        failwith "Don't try to raise zero to the power of zero. Expected both non-zero basement and non-zero exponent. \n Error when calling - expo - function."
    elif basement = 0.0 && exponent < 0 then
        failwith "Don't try to raise zero to the negative power. Expected zero basement and positive <> 0 exponent. \n Error when calling - expo - function."
    elif basement = 0.0 then
        0.0
    else
        let mutable ans = 1.0
        let mutable counter = abs exponent

        let product = if exponent > 0 then basement else 1.0 / basement

        while counter > 0 do
            ans <- ans * product
            counter <- counter - 1

        ans

let rec fastExpo (basement: float) (exponent: int) : float =
    if basement = 0.0 && exponent = 0 then
        failwith "Don't try to raise zero to the power of zero. Expected both non-zero basement and non-zero exponent. \n Error when calling - fastExpo - function"
    elif basement = 0.0 && exponent < 0 then
        failwith "Don't try to raise zero to the negative power. Expected zero basement and positive <> 0 exponent. \n Error when calling - fastExpo - function."
    elif exponent = 1 then
        basement
    elif exponent = 0 then
        1.0
    else
        let body = fastExpo basement (exponent / 2)

        if exponent % 2 = 0 then
            body * body
        else
            body * body * basement

let inline arrays (arr: 'value array) =
    if arr.Length > 0 then
        let mutable maximum = arr[0]
        let mutable minimum = arr[0]

        for i in 1 .. arr.Length - 1 do
            if maximum < arr[i] then
                maximum <- arr[i]
            elif minimum > arr[i] then
                minimum <- arr[i]

        maximum - minimum
    else
        failwith "There is no max and min elements in empty array. Expected at least 1 element in array. \n Error when calling - arrays - function"

let odds (x: int) (y: int) =
    let minim = if x < y then x else y
    let maxim = if x > y then x else y

    let odds_array =
        [| for i in minim + minim % 2 + 1 .. 2 .. maxim - maxim % 2 do
               yield i |]

    odds_array
