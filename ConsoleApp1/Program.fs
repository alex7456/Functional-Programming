open System
let readList n =
    let rec readLines n acc =
        match n with
        | 0 -> acc
        | k -> 
            let el = Console.ReadLine() |> int 
            let newAcc = acc@[el]
            readLines  (k-1) newAcc
    readLines n []

let rec printList list =
    match list with
        | [] -> Console.ReadKey()
        | head :: tail ->
            Console.WriteLine(head.ToString())
            printList tail

let rec reduce_list list (f: int->int->int) (condition: int->bool) acc =
    match list with
    | [] -> acc
    | head::tail ->
        let current = head
        let flag = condition current
        let newAcc = if condition current then f acc current else acc
        reduce_list tail f condition newAcc

let main =
    let sumEven = reduce_list [1;2;3;4;5;6] (+) (fun x -> x % 2 = 0) 0
    System.Console.WriteLine("Сумма четных: ")
    System.Console.Write sumEven

main