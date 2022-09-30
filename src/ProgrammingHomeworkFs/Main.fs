module ProgrammingHomeworkFs

// Number 1
let expo (basement: float) (exponent: int) : float =
    if
        basement = 0.0
        && exponent = 0
    then
        failwith "Undefined expo"
    elif basement = 0.0 then
        0.0
    else
        let mutable ans = 1.0
        let mutable counter = abs exponent // counter for while-loop

        let product = // product will be multiplied by itself
            if exponent > 0 then
                basement
            else
                1.0
                / basement

        while counter > 0 do
            ans <-
                ans
                * product

            counter <-
                counter
                - 1

        ans

// Number 2
let rec fastExpo (basement: float) (exponent: int) : float = // exponent >= 0
    if
        basement = 0.0
        && exponent = 0
    then
        failwith "Undefined fastExpo"
    elif exponent = 1 then
        basement
    elif exponent = 0 then
        1.0
    else
        let body =
            fastExpo
                basement
                (exponent
                 / 2) // save "previous" value of rec in body

        if exponent % 2 = 0 then
            body
            * body
        else
            body
            * body
            * basement

// Number 3
let arrays (arr: int array) =
    if arr.Length > 0 then
        let mutable maximum = arr[0]
        let mutable minimum = arr[0]

        for i in
            1 .. arr.Length
                 - 1 do
            if maximum < arr[i] then
                maximum <- arr[i]
            elif minimum > arr[i] then
                minimum <- arr[i]

        maximum
        - minimum
    else
        failwith "Undefined min/max array"

// Number 4
let odds (x: int) (y: int) =
    let odds_array =
        if x < y then
            [|
                for i in
                    x
                    + x % 2
                    + 1 .. y - y % 2 do
                    if abs i % 2 = 1 then
                        yield i
            |]
        else
            [|
                for i in
                    y
                    + y % 2
                    + 1 .. x - x % 2 do
                    if abs i % 2 = 1 then
                        yield i
            |]

    odds_array

module Main =

    [<EntryPoint>]
    let main (argv: string array) = 0
