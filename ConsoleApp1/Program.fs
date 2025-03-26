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
    
    
    maxNonCoprime * sumDigitsLess5

let getFunction = function
    | 1 -> countEvenNonCoprimes
    | 2 -> maxDigitNotDivBy3
    | 3 -> complexProduct
    | _ -> failwith "Неверный номер функции"

let mainWithCurrying () =
    let processInput (fnNumber, arg) = 
        getFunction fnNumber arg
    let printResult result = printfn "Результат: %d" result
    
    printfn "Введите кортеж (номер функции, аргумент):"
    let input = System.Console.ReadLine()
    let parsed = 
        try 
            input.Trim('(', ')').Split(',') 
            |> Array.map int 
            |> fun arr -> (arr.[0], arr.[1]) 
            |> Some
        with _ -> None
    
    match parsed with
    | Some (num, arg) -> processInput (num, arg) |> printResult
    | None -> printfn "Ошибка ввода"

mainWithCurrying()

// Main с суперпозицией
let mainWithComposition () =
    let parseInput = 
        fun s -> 
            s.Trim('(', ')').Split(',') 
            |> Array.map int 
            |> fun arr -> (arr.[0], arr.[1])
    
    let processInput = 
        parseInput 
        >> (fun (num, arg) -> getFunction num arg)
    
    let printResult = 
        printfn "Результат: %d"
    
    printfn "Введите кортеж (номер функции, аргумент):"
    System.Console.ReadLine() 
    |> processInput 
    |> printResult

// Вызов
mainWithComposition()