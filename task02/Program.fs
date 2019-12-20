module task02.Program

type Operation = | Add = 1 | Multiply = 2 | End = 99

let setListElement index newValue list =
    list |> List.mapi (fun i value -> if i = index then newValue else value)

let readLine list lineIndex =
    let skip = lineIndex * 4
    let take = min 4 (List.length list - skip)
    list |> List.skip skip |> List.take take

let execute opCode a b =
    match opCode with
    | Operation.Add -> a + b
    | Operation.Multiply -> a * b
    | _ -> 0

let intCode program =
    let rec processLine lineIndex program =
        let instruction = readLine program lineIndex
        match instruction with
        | [ opCode; i1; i2; i3 ] when enum opCode <> Operation.End ->
            let result = execute (enum opCode) program.[i1] program.[i2]
            program |> setListElement i3 result |> processLine (lineIndex + 1)
        | _ -> program
    processLine 0 program

let initProgram noun verb program = program |> setListElement 1 noun |> setListElement 2 verb

let findInput result program =
    let rec loop noun verb =
        let checkResult = program |> initProgram noun verb |> intCode |> List.item 0
        match checkResult with
        | x when x = result -> Some noun, Some verb
        | _ when noun < 99 -> loop (noun + 1) verb
        | _ when verb < 99 -> loop 0 (verb + 1)
        | _ -> None, None
    loop 1 1

[<EntryPoint>]
let main argv =
    let result1 = Input.program |> initProgram 12 2 |> intCode
    printfn "result1=%A" result1.[0]

    let noun, verb = Input.program |> findInput 19690720
    match noun, verb with
    | Some x, Some y -> printfn "result2=%A" (100 * x + y)
    | _ -> printfn "result2 not found"
    0 // return an integer exit code
