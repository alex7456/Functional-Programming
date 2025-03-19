let digitSum2 n = 
    let rec sumCifr1 n curSum = 
        if n = 0 then curSum
        else
            let n1 = n/10
            let cifr = n%10
            let newSum = curSum + cifr
            sumCifr1 n1 newSum
    sumCifr1 n 0

let main () =

    let sum2 = digitSum2 324
    System.Console.Write "сумма цифр числа 324: "
    System.Console.Write sum2

    System.Console.ReadKey()


main()