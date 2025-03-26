let conditionalDigitTraversal number operation initial condition =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            let newAcc = 
                match condition digit with
                | true -> operation acc digit
                | false -> acc
            loop (n / 10) newAcc
    loop number initial

let testConditionalDigitTraversal () =
    let number = 123456789
    
    let sumEven = conditionalDigitTraversal number (+) 0 (fun x -> x % 2 = 0)
    printfn "Сумма четных цифр числа %d: %d" number sumEven
    
    let productGreaterThan5 = conditionalDigitTraversal number (*) 1 (fun x -> x > 5)
    printfn "Произведение цифр числа %d больше 5: %d" number productGreaterThan5
    
    let maxOdd = conditionalDigitTraversal number max System.Int32.MinValue (fun x -> x % 2 <> 0)
    printfn "Максимальная нечетная цифра числа %d: %d" number maxOdd


testConditionalDigitTraversal()