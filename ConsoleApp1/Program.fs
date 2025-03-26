let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let traverseCoprimesWithCond number operation initial cond =
    let rec loop current acc =
        match current <= 1 with
        | true -> acc
        | false ->
            let isCoprime = gcd current number = 1
            let meetsCond = cond current
            let newAcc = 
                match isCoprime && meetsCond with
                | true -> operation acc current
                | false -> acc
            loop (current - 1) newAcc
    loop number initial

let sumEvenCoprimes n =
    traverseCoprimesWithCond n (+) 0 (fun x -> x % 2 = 0)

let testCondCoprimes () =
    printfn "Сумма чётных взаимно простых с 10: %d" (sumEvenCoprimes 10)

testCondCoprimes()