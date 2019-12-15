// Learn more about F# at http://fsharp.org

open System;
open Input;

type Coordinate = { x: int; y: int }

let move start vector = { x = start.x + fst vector; y = start.y + snd vector }

type PathSegment = { Start: Coordinate; End: Coordinate }

let getPathSegments (path: Vector seq) =
    let folder start vector =
        let finish = move start vector
        { Start = start; End = finish }, finish
    path |> Seq.mapFold folder { x = 0; y = 0 } |> fst

let getIntersections (segments1, segments2) =
    let overlaps s11 s12 s21 s22 =
        let s1 = [ s11; s12 ] |> List.sort
        let s2 = [ s21; s22 ] |> List.sort
        s1.[0] <= s2.[1] && s2.[0] <= s1.[1]
    let intersects (seg1: PathSegment, seg2: PathSegment) =
        overlaps seg1.Start.x seg1.End.x seg2.Start.x seg2.End.x &&
        overlaps seg1.Start.y seg1.End.y seg2.Start.y seg2.End.y
    let getIntersection segments =
        if intersects segments then
            match segments with
            | { Start = { x = x1 }; End = { x = x2 } }, { Start = { y = y1 }; End = { y = y2 } }
                when x1 = x2 && y1 = y2
                 -> Some { x = x1; y = y1 }
            | { Start = { y = y1 }; End = { y = y2 } }, { Start = { x = x1 }; End = { x = x2 } }
                when x1 = x2 && y1 = y2
                 -> Some { x = x1; y = y1 }
            | _ -> None
        else None
    Seq.allPairs segments1 segments2 |> Seq.filter intersects |> Seq.map getIntersection |> Seq.choose id


[<EntryPoint>]
let main argv =
    printfn "%A" input
    let result =
        input
        |> fun x -> getPathSegments (fst x), getPathSegments (snd x)
        |> getIntersections
        |> Seq.except [ { x = 0; y = 0 } ]
        |> Seq.map (fun p -> Math.Abs p.x + Math.Abs p.y)
        |> Seq.min
    printfn "%A" result
    0
