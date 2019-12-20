module task01.Program

let rec getFuel mass =
    let fuel = mass / 3 - 2 |> max 0
    fuel + if fuel > 0 then getFuel fuel else 0

[<EntryPoint>]
let main argv =
    let result = Input.modules |> List.sumBy getFuel
    printfn "%A" result
    0 // return an integer exit code
