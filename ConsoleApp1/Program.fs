let rec gcd a b = 
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let traverseCoprimes number operation initial =
    let rec loop current acc =
        match current <= 1 with
        | true -> acc
        | false ->
            let isCoprime = gcd current number = 1
            let newAcc = 
                match isCoprime with
                | true -> operation acc current
                | false -> acc
            loop (current - 1) newAcc
    loop number initial

let eulerPhi n = 
    traverseCoprimes n (fun acc _ -> acc + 1) 0

let testCoprimes () =
    printfn "Сумма взаимно простых с 10: %d" (traverseCoprimes 10 (+) 0)
    printfn "φ(10) = %d" (eulerPhi 10)

testCoprimes()
