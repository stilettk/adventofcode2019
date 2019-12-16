open System.Text.RegularExpressions

let isValidPassword password =
    Regex.IsMatch(password, "^0*1*2*3*4*5*6*7*8*9*$") && Regex.IsMatch(password, "(.)\1+")
    
let countValidPasswords min max =
    seq {min..max} |> Seq.map string |> Seq.filter isValidPassword |> Seq.length

[<EntryPoint>]
let main argv =
    let input = 134564, 585159
    input ||> countValidPasswords |> printfn "result = %A" 
    0 // return an integer exit code
