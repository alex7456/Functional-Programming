

// Рекурсия вниз (с телом-выражением)
let rec sumOfDigitsDown n =
    match n with
    | 0 -> 0 // Базовый случай
    | _ -> sumOfDigitsDown (n / 10) + (n % 10) // Рекурсивный случай


let main () =
    printfn "Введите число:"
    let number = System.Console.ReadLine() |> int 

    let sum = sumOfDigitsDown number 
    printfn "Сумма цифр числа %d равна %d" number sum 

main()