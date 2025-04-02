open System

let readList n =
    let rec readLines n acc =
        match n with
        | 0 -> acc
        | k -> 
            let el = Console.ReadLine() |> int 
            let newAcc = acc @ [el]
            readLines (k - 1) newAcc
    readLines n []

let rec printList list =
    match list with
    | [] -> Console.ReadKey() |> ignore
    | head :: tail ->
        Console.WriteLine(head.ToString())
        printList tail

let rec reduce_list list (f: int -> int -> int) (condition: int -> bool) acc =
    match list with
    | [] -> acc
    | head :: tail ->
        let newAcc = if condition head then f acc head else acc
        reduce_list tail f condition newAcc

let min_list list =
    match list with
    | [] -> 0
    | head :: tail -> reduce_list list (fun a b -> if a < b then a else b) (fun _ -> true) head

let sum_even list = reduce_list list (+) (fun a -> a % 2 = 0) 0

let count_odd list = reduce_list list (fun a _ -> a + 1) (fun a -> a % 2 = 1) 0


let rec frequency list num count =
    match list with
    | [] -> count
    | head :: tail ->
        let newCount = if head = num then count + 1 else count
        frequency tail num newCount

let rec freq_list list main_list cur_list =
    match list with
    | [] -> cur_list
    | head :: tail ->
        let freq_elem = frequency main_list head 0
        let new_list = cur_list @ [freq_elem]
        freq_list tail main_list new_list

let pos list el =
    let rec pos_inner list el num =
        match list with
        | [] -> 0
        | head :: tail ->
            if head = el then num
            else pos_inner tail el (num + 1)
    pos_inner list el 1

let get_from_list list pos =
    let rec get list num cur_num =
        match list with
        | [] -> 0
        | head :: tail ->
            if num = cur_num then head
            else get tail num (cur_num + 1)
    get list pos 1

let max_list list =
    let rec find_max lst current_max =
        match lst with
        | [] -> current_max
        | head :: tail ->
            let new_max = if head > current_max then head else current_max
            find_max tail new_max
    match list with
    | [] -> 0
    | head :: tail -> find_max tail head

let find_most_frequent list =
    let fL = freq_list list list []
    let maxFreq = max_list fL
    let index = pos fL maxFreq
    get_from_list list index

let main =
    let test = [5; 3; 8; 1; 4; 6; 5; 3; 5]

    System.Console.Write("Минимум в списке: ")
    System.Console.WriteLine(min_list test)

    System.Console.Write("Сумма четных в списке: ")
    System.Console.WriteLine(sum_even test)

    System.Console.Write("Количество нечетных в списке: ")
    System.Console.WriteLine(count_odd test)

    System.Console.Write("Самый частый элемент: ")
    System.Console.WriteLine(find_most_frequent test)

main
