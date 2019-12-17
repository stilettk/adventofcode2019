open System.Text.RegularExpressions

let isValidPassword1 password = Regex.IsMatch(password, "^0*1*2*3*4*5*6*7*8*9*$") && Regex.IsMatch(password, "(\d)\1+")

let isValidPassword2 (password: string) =
    let rec isValid pass minGroup currGroup =
        match pass with
        | x :: y :: _ when x > y -> false
        | x :: y :: rest when x = y -> isValid (y :: rest) minGroup (currGroup + 1)
        | _ :: rest ->
            let minGroup =
                if currGroup > 1 && currGroup < minGroup then currGroup
                else minGroup
            isValid rest minGroup 1
        | [] -> minGroup = 2
    isValid (password.ToCharArray() |> Array.toList) password.Length 1

let countValidPasswords isValidFunc min max =
    seq { min .. max }
    |> Seq.map string
    |> Seq.filter isValidFunc
    |> Seq.length

[<EntryPoint>]
let main argv =
    let input = 134564, 585159
    input ||> countValidPasswords isValidPassword1 |> printfn "result1 = %A"
    input ||> countValidPasswords isValidPassword2 |> printfn "result2 = %A"
    0 // return an integer exit code
