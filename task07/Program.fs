module task06.Program
open task05

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
    let rec exec input (program: int list) output index =
        let operation = program.[index] |> sprintf "%05i"
        let opCode: Operation = operation.Substring(3) |> int |> getOperation

        let getValue instructionIndex =
            let value = program.[index + instructionIndex]
            let parameterMode: ParameterMode = operation.Substring(3 - instructionIndex, 1) |> (int >> getParameterMode)
            match parameterMode with
            | Immediate -> value
            | Position -> program.[value]

        match opCode with
        | Add ->
            let program = program |> setElem program.[index + 3] (getValue 1 + getValue 2)
            exec input program output (index + 4)
        | Multiply ->
            let program = program |> setElem program.[index + 3] (getValue 1 * getValue 2)
            exec input program output (index + 4)
        | Input ->
            let input, program =
                match input with
                | i :: rest -> rest, program |> setElem program.[index + 1] i
                | [] -> input, program
            exec input program output (index + 2)
        | Output -> exec input program (output @ [ getValue 1 ]) (index + 2)
        | JumpIfTrue ->
            let pointer = if (getValue 1) <> 0 then getValue 2 else index + 3
            exec input program output pointer
        | JumpIfFalse ->
            let pointer = if (getValue 1) = 0 then getValue 2 else index + 3
            exec input program output pointer
        | LessThan ->
            let value = if getValue 1 < getValue 2 then 1 else 0
            let program = program |> setElem program.[index + 3] value
            exec input program output (index + 4)
        | Equals ->
            let value = if getValue 1 = getValue 2 then 1 else 0
            let program = program |> setElem program.[index + 3] value
            exec input program output (index + 4)
        | End -> program, output
    snd (exec input program [] 0)

let getThrusterSignal program phaseSettings =
    let processAmplifier signal phaseSetting = intCode [ phaseSetting; signal ] program |> List.item 0
    phaseSettings |> List.fold processAmplifier 0

let getMaxThrusterSignal program phaseSettings =    
    let rec insertions x = function
      | []             -> [[x]]
      | y :: ys as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))
    let rec permutations = function
      | []      -> seq [ [] ]
      | x :: xs -> Seq.collect (insertions x) (permutations xs)
      
    permutations phaseSettings |> Seq.map (getThrusterSignal program) |> Seq.max

[<EntryPoint>]
let main argv =
    let result1 = getMaxThrusterSignal (List.ofArray Input.input) [4;3;2;1;0]
    printfn "result1=%A" result1
    0 // return an integer exit code
