let rec gcd a b = 
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

let maxDigitNotDivBy3 n =
    let rec loop current max =
        match current with
        | 0 -> max
        | _ ->
            let digit = current % 10
            let newMax =
                match digit % 3 <> 0 with
                | true -> if digit > max then digit else max
                | false -> max
            loop (current / 10) newMax
    loop n -1


let complexProduct n =
    let minDiv = smallestDivisor n
    
    // Часть 1: Максимальное число, не взаимно простое и не делящееся на minDiv
    let maxNonCoprime =
        let rec loop current max =
            match current with
            | 0 -> max
            | _ ->
                let digit = current % 10
                let newMax =
                    match gcd digit n <> 1 && digit % minDiv <> 0 with
                    | true -> if digit > max then digit else max
                    | false -> max
                loop (current / 10) newMax
        loop n -1
    
    // Часть 2: Сумма цифр < 5
    let sumDigitsLess5 =
        let rec loop current sum =
            match current with
            | 0 -> sum
            | _ ->
                let digit = current % 10
                let newSum =
                    match digit < 5 with
                    | true -> sum + digit
                    | false -> sum
                loop (current / 10) newSum
        loop n 0
    
    // Результат
    maxNonCoprime * sumDigitsLess5

let testMethods () =
    let num = 36
    printfn "Метод 1 для %d: %d" num (countEvenNonCoprimes num)
    printfn "Метод 2 для %d: %d" num (maxDigitNotDivBy3 num)
    printfn "Метод 3 для %d: %d" num (complexProduct num)

testMethods()