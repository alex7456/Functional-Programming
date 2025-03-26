﻿let rec gcd a b = 
    match b with 
    | 0 -> a 
    | _ -> gcd b (a % b)

let smallestDivisor n =
    let rec loop i =
        match i with
        | _ when i * i > n -> n
        | _ when n % i = 0 -> i
        | _ -> loop (i + 1)
    loop 2

let countEvenNonCoprimes n =
    let rec loop current count =
        match current with
        | 0 -> count
        | _ ->
            let digit = current % 10
            let newCount =
                match digit % 2 = 0 && gcd digit n <> 1 with
                | true -> count + 1
                | false -> count
            loop (current / 10) newCount
    loop n 0