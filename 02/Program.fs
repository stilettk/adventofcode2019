// Learn more about F# at http://fsharp.org

open System

type Operation = Add = 1 | Multiply = 2 | End = 99

let setListElement index newValue list = 
    list |> List.mapi (fun i value -> if i = index then newValue else value)

let readLine list lineIndex = 
    let skip = lineIndex * 4
    let take = min 4 (List.length list - skip)
    list |> List.skip skip |> List.take take

let execute opCode a b = 
    match opCode with
    | 1 -> a + b
    | 2 -> a * b
    | _ -> 0

let rec intCode lineIndex program = 
    let instruction = readLine program lineIndex
    match instruction with
    | [opCode; i1; i2; i3] when opCode <> 99 -> program |> setListElement i3 (execute opCode program.[i1] program.[i2]) |> intCode (lineIndex + 1)
    | [99; _; _; _;]
    | _ -> program

[<EntryPoint>]
let main argv =
    let result = Input.program |> setListElement 1 12 |> setListElement 2 2 |> intCode 0
    printf "%A" result.[0]
    0 // return an integer exit code
