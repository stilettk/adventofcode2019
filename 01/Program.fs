// Learn more about F# at http://fsharp.org

open System
// 15838248
let atLeastZero x = if x < 0 then 0 else x

let rec getFuel mass = 
    let fuel = mass / 3 - 2 |> atLeastZero
    fuel + if fuel > 0 then getFuel fuel else 0

[<EntryPoint>]
let main argv =
    let result = Input.modules |> List.map getFuel |> List.sum
    Console.WriteLine result
    0 // return an integer exit code
