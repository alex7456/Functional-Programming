let digitTraversal number operation initial =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            loop (n / 10) (operation acc digit)
    loop number initial

let testDigitTraversal () =
    let number = 12345
    
    let sum = digitTraversal number (+) 0
    printfn "Сумма цифр числа %d: %d" number sum
    
    let product = digitTraversal number (*) 1
    printfn "Произведение цифр числа %d: %d" number product
    
    let minDigit = digitTraversal number (fun acc x -> min acc x) System.Int32.MaxValue
    printfn "Минимальная цифра числа %d: %d" number minDigit
    
    let maxDigit = digitTraversal number (fun acc x -> max acc x) System.Int32.MinValue
    printfn "Максимальная цифра числа %d: %d" number maxDigit

// Явный вызов тестовой функции
testDigitTraversal()
// Добавьте эту строку, чтобы консоль не закрывалась сразу
System.Console.ReadKey() |> ignore