module Converters

open System

let toInt (unsignedInt: uint) =
    try
        Convert.ToInt32(unsignedInt)
    with :? OverflowException ->
        failwith $"%A{unsignedInt} is outside the range of the Int32 type."

let toDouble (unsignedDouble: uint) =
    try
        Convert.ToDouble(unsignedDouble)
    with :? OverflowException ->
        failwith $"%A{unsignedDouble} is outside the range of the Double type."

let real (arr: array<string>) = Some(float arr[2])

let integer (arr: array<string>) = Some(int arr[2])

let pattern _ = Some()
