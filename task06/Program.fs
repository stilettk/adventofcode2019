module Program

type Object = string
type Orbits = Map<Object, Object list>

let createOrbitsMap (input: string seq): Orbits =
    input
    |> Seq.map (fun x -> x.Split(")"))
    |> Seq.map (fun x -> x.[0], x.[1])
    |> Seq.groupBy fst
    |> Seq.map (fun (key, value) -> (key, Seq.map snd value |> Seq.toList))
    |> Map.ofSeq

let getTotalOrbitsCount (orbitsMap: Orbits) =
    let rec getOrbitsCount count object = 
        let directCount =
            match orbitsMap.TryFind object with
            | None -> 0
            | Some satellites -> satellites |> List.sumBy (getOrbitsCount (count + 1))
        count + directCount
    getOrbitsCount 0 "COM"

[<EntryPoint>]
let main argv =
    let map = createOrbitsMap Input.input
    printfn "result1 = %A" (getTotalOrbitsCount map)
    0 // return an integer exit code
