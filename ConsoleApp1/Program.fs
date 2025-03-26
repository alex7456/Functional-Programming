let getFunction (isSumDigits: bool) =
    if isSumDigits then
        
        let sumDigits n =
            let rec loop acc num =
                match num with
                | 0 -> acc
                | _ -> loop (acc + num % 10) (num / 10)
            loop 0 n
        sumDigits
    else
        let rec fibonacci n =
            match n with
            | 0 -> 0
            | 1 -> 1
            | _ -> fibonacci(n-1) + fibonacci(n-2)
        fibonacci


let sumdigitsfunc = getFunction true
let sumres = sumdigitsfunc 123
printfn "%d" sumres
let fibonaccifunc = getFunction false 
let fibres = fibonaccifunc 10
printfn "%d" fibres