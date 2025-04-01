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

let min_list list = 
    match list with
    | [] -> 0
    | head::tail -> reduce_list list (fun a b -> if a < b then a else b) (fun a -> true) head

let sum_even list = reduce_list list (+) (fun a -> a%2 = 0) 0

let count_odd list = reduce_list list (fun a b -> a+1) (fun a -> a%2 = 1) 0

let main =
    let test = [5; 3; 8; 1; 4; 6]
    System.Console.Write("Минимум в списке: ")
    System.Console.WriteLine(min_list test)

    System.Console.Write("Сумма четных в списке: ")
    System.Console.WriteLine(sum_even test)

    System.Console.Write("Количество нечетных в списке: ")
    System.Console.WriteLine(count_odd test)

main