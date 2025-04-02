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

let find_most_frequent list =
    let rec loop lst checked max_elem max_freq =
        match lst with
        | [] -> max_elem
        | head :: tail ->
            if List.contains head checked then
                loop tail checked max_elem max_freq
            else
                let freq = frequency list head 0
                if freq > max_freq then
                    loop tail (head :: checked) head freq
                else
                    loop tail (head :: checked) max_elem max_freq
    loop list [] 0 0


type BinaryTree =
    | Empty
    | Node of string * BinaryTree * BinaryTree

let rec insert tree value =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then Node(v, insert left value, right)
        else Node(v, left, insert right value)

let rec inOrder tree =
    match tree with
    | Empty -> []
    | Node(value, left, right) ->
        inOrder left @ [value] @ inOrder right

let testTree =
    Empty
    |> fun t -> insert t "mango"
    |> fun t -> insert t "apple"
    |> fun t -> insert t "peach"
    |> fun t -> insert t "banana"


let main =
    let test = [5; 3; 8; 1; 4; 6; 5; 3; 5]

    Console.Write("Минимум в списке: ")
    Console.WriteLine(min_list test)

    Console.Write("Сумма чётных в списке: ")
    Console.WriteLine(sum_even test)

    Console.Write("Количество нечётных в списке: ")
    Console.WriteLine(count_odd test)

    Console.Write("Самый частый элемент: ")
    Console.WriteLine(find_most_frequent test)

    Console.WriteLine("\nСодержимое строки в двоичном дереве (in-order):")
    inOrder testTree |> List.iter (printfn "%s")

main
