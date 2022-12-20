module SimpleFunctions

let expo basement exponent =
    if basement = 0.0 && exponent = 0 then
        failwith
            $"Expected non-zero basement and non-zero exponent, but given basement : %A{basement} and exponent : %A{exponent}. \n
                 Error in -expo- function. "
    elif basement = 0.0 && exponent < 0 then
        failwith
            $"Expected zero basement and non-negative exponent, but given basement : %A{basement} and exponent : %A{exponent}. \n
                 Error in -expo- function. "
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

let rec fastExpo basement exponent =
    if basement = 0.0 && exponent = 0 then
        failwith
            "Expected both non-zero basement and non-zero exponent.\n
                  Error in -fastExpo- function. "
    elif basement = 0.0 && exponent < 0 then
        failwith
            "Expected zero basement and non-negative exponent.\n
                  Error in -fastExpo- function. "
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

let inline maxMinOfArray (arr: array<'Value>) =
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
        failwith
            "Expected at least 1 element in array.\n
                  Error in -maxMinOfArray- function. "

let arrayOfOdds x y =
    let minimum = if x < y then x else y
    let maximum = if x > y then x else y

    let oddsArray =
        [| for i in minimum + minimum % 2 + 1 .. 2 .. maximum - maximum % 2 do
               yield i |]

    oddsArray
