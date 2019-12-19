module Program

type Operation =
    | Add
    | Multiply
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | Equals
    | End
let getOperation value =
    match value with
    | 1 -> Add
    | 2 -> Multiply
    | 3 -> Input
    | 4 -> Output
    | 5 -> JumpIfTrue
    | 6 -> JumpIfFalse
    | 7 -> LessThan
    | 8 -> Equals
    | 99 | _ -> End

type ParameterMode = | Position | Immediate
let getParameterMode value =
    match value with
    | 1 -> Immediate
    | 0 | _ -> Position

let setElem index newValue list =
    list |> List.mapi (fun i value -> if i = index then newValue else value)

let intCode input program =
    let rec exec input index (program: int list) output =
        let exec = exec input
        let operation = program.[index] |> sprintf "%05i"
        let opCode: Operation = operation.Substring(3) |> int |> getOperation

        let getValue instructionIndex =
            let value = program.[index + instructionIndex]
            let parameterMode: ParameterMode = operation.Substring(3 - instructionIndex, 1) |> (int >> getParameterMode)
            match parameterMode with
            | Immediate -> value
            | Position -> program.[value]

        match opCode with
        | Add -> (program |> setElem (program.[index + 3]) (getValue 1 + getValue 2), output) ||> exec (index + 4)
        | Multiply -> (program |> setElem (program.[index + 3]) (getValue 1 * getValue 2), output) ||> exec (index + 4)
        | Input -> (program |> setElem (program.[index + 1]) input, output) ||> exec (index + 2)
        | Output -> (program, (output @ [ getValue 1 ])) ||> exec (index + 2)
        | JumpIfTrue ->
            let pointer = if (getValue 1) <> 0 then getValue 2 else index + 3
            (program, output) ||> exec pointer
        | JumpIfFalse ->
            let pointer = if (getValue 1) = 0 then getValue 2 else index + 3
            (program, output) ||> exec pointer
        | LessThan ->
            let value = if getValue 1 < getValue 2 then 1 else 0
            (program |> setElem program.[index + 3] value, output) ||> exec (index + 4)
        | Equals ->
            let value = if getValue 1 = getValue 2 then 1 else 0
            (program |> setElem program.[index + 3] value, output) ||> exec (index + 4)
        | End -> program, output
    snd (exec input 0 program [])

[<EntryPoint>]
let main argv =
    let result1 = Input.program |> Array.toList |> intCode 1
    printfn "result1=%A" result1
    
    Input.program |> Array.toList |> intCode 5 |> printfn "result2=%A"
    0 // return an integer exit code
