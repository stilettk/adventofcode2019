module Program

type Operation = | Add | Multiply | Input | Output | End
let getOperation value =
    match value with
    | 1 -> Add
    | 2 -> Multiply
    | 3 -> Input
    | 4 -> Output
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
        | End -> program, output
    snd (exec input 0 program [])

[<EntryPoint>]
let main argv =
    let result1 = Input.program |> Array.toList |> intCode 1
    printfn "result1=%A" result1
    0 // return an integer exit code
