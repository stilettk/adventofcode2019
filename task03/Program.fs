open Input;

type Coordinate = { x: int; y: int }

let move start vector = { x = start.x + fst vector; y = start.y + snd vector }

type PathSegment = { Start: Coordinate; End: Coordinate }

let getPathSegments (path: Vector seq) =
    let folder start vector =
        let finish = move start vector
        { Start = start; End = finish }, finish
    path |> Seq.mapFold folder { x = 0; y = 0 } |> fst |> Seq.toList

let getIntersections segments1 segments2 =
    let overlaps s11 s12 s21 s22 =
        min s11 s12 <= max s21 s22 && min s21 s22 <= max s11 s12

    let intersects (seg1: PathSegment, seg2: PathSegment) =
        overlaps seg1.Start.x seg1.End.x seg2.Start.x seg2.End.x &&
        overlaps seg1.Start.y seg1.End.y seg2.Start.y seg2.End.y

    let getIntersection segments =
        if intersects segments then
            match segments with
            | { Start = { x = x1 }; End = { x = x2 } }, { Start = { y = y1 }; End = { y = y2 } }
            | { Start = { y = y1 }; End = { y = y2 } }, { Start = { x = x1 }; End = { x = x2 } }
                when x1 = x2 && y1 = y2 -> Some { x = x1; y = y1 }
            | _ -> None
        else None

    Seq.allPairs segments1 segments2 |> Seq.filter intersects |> Seq.map getIntersection |> Seq.choose id

let getPathLengthToIntersection intersection segments =
    let containsPoint point seg =
        let between x x1 x2 = min x1 x2 <= x && x <= max x1 x2
        between point.x seg.Start.x seg.End.x && between point.y seg.Start.y seg.End.y
    let getDistance p1 p2 = abs (p2.x - p1.x) + abs (p2.y - p1.y)
    let rec accDistance distanceSoFar segments =
        match segments with
        | [] -> distanceSoFar
        | seg :: _ when containsPoint intersection seg ->
            distanceSoFar + (getDistance seg.Start intersection)
        | seg :: rest -> accDistance (distanceSoFar + getDistance seg.Start seg.End) rest
    accDistance 0 segments

[<EntryPoint>]
let main argv =
    let pathSegments = input |> Array.map getPathSegments |> fun x -> x.[0], x.[1]
    let intersections = pathSegments ||> getIntersections |> Seq.except [ { x = 0; y = 0 } ] |> Seq.toList

    let minDistanceToIntersection =
        intersections |> Seq.map (fun p -> abs p.x + abs p.y) |> Seq.min
    printfn "result1: %A" minDistanceToIntersection

    let getTotalPathLengthToIntersection intersection =
        let getPathLength = getPathLengthToIntersection intersection
        pathSegments ||> fun s1 s2 -> (getPathLength s1) + (getPathLength s2)
    let minPathToIntersection = intersections |> Seq.map getTotalPathLengthToIntersection |> Seq.min
    printfn "result2: %A" minPathToIntersection

    0
