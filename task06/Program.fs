module Program

type Object = Object of string
type Satellites = Map<Object, Object list>
type Orbits = Map<Object, Object>

type Connection = | Orbit | Satellite
type ConnectionMap = Map<Object, (Object * Connection) list>

let createMap (input: string seq): ConnectionMap =
    input
    |> Seq.map (fun x -> x.Split(")") |> Array.map Object)
    |> Seq.collect (fun x -> [ x.[1], (x.[0], Orbit); x.[0], (x.[1], Satellite) ])
    |> Seq.distinct
    |> Seq.groupBy fst
    |> Seq.map (fun (key, value) -> (key, Seq.map snd value |> Seq.toList))
    |> Map.ofSeq

let getTotalOrbitsCount (map: ConnectionMap) =
    let rec getOrbitsCount count object =
        let getSatellites = List.choose (fun x -> if snd x = Satellite then Some <| fst x else None)
        let directCount =
            match map.TryFind object with
            | None -> 0
            | Some connections -> connections |> getSatellites |> Seq.sumBy (getOrbitsCount <| count + 1)
        count + directCount
    getOrbitsCount 0 (Object "COM")

let findMinimumDistance map source destination =
    let rec bfs visited queue =
        match queue with
        | [] -> None
        | (current, distance) :: _ when current = destination -> Some <| distance - 2
        | (current, distance) :: nextQueue ->
            let visited = Set.add current visited
            let neighbors = Map.tryFind current map
            let queue =
                match neighbors with
                | None -> nextQueue
                | Some n ->
                    let chooseNext next =
                        match fst next with
                        | n when not <| Set.contains n visited -> Some(n, distance + 1)
                        | _ -> None
                    nextQueue @ List.choose chooseNext n
            bfs visited queue
    bfs Set.empty [ source, 0 ]

[<EntryPoint>]
let main argv =
    let orbitsMap = createMap Input.input
    printfn "result1 = %A" <| getTotalOrbitsCount orbitsMap
    printfn "result2 = %A" <| findMinimumDistance orbitsMap (Object "YOU") (Object "SAN")
    0 // return an integer exit code
