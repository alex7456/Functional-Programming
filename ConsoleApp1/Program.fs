﻿open System

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

let find_most_frequent_list_based list =
    list
    |> List.countBy id         
    |> List.maxBy snd          
    |> fst                
    
let count_squares list =
    list
    |> List.filter (fun x -> List.exists (fun y -> y * y = x) list)
    |> List.length


let sum_of_digits n =
    n
    |> abs
    |> string
    |> Seq.map (fun ch -> int ch - int '0')
    |> Seq.sum

let count_divisors n =
    let n = abs n
    [1 .. n] |> List.filter (fun x -> n % x = 0) |> List.length

let sort_b list =
    list
    |> List.sortBy (fun x -> (sum_of_digits x, -abs x))

let sort_c list =
    list
    |> List.sortByDescending (fun x -> (count_divisors x, abs x))

let make_triples a b c =
    let sortedA = List.sortDescending a
    let sortedB = sort_b b
    let sortedC = sort_c c
    List.zip3 sortedA sortedB sortedC


let sort_strings_by_length (lst: string list) : string list =
    lst |> List.sortBy String.length


let readStringList n =
    let rec read acc n =
        match n with
        | 0 -> acc
        | _ ->
            let line = System.Console.ReadLine()
            read (acc @ [line]) (n - 1)
    read [] n


let solve_1_4_list lst =
    lst
    |> List.mapi (fun i v -> (i, v))           
    |> List.sortByDescending snd               
    |> List.map fst                            

let solve_1_14_list (a: int) (b: int) (lst: int list) : int =
    lst
    |> List.filter (fun x -> x >= a && x <= b)
    |> List.length


let solve_1_24_list (lst: int list) : (int * int) =
    let sorted = lst |> List.sortDescending
    match sorted with
    | x :: y :: _ -> (x, y)
    | _ -> failwith "Недостаточно элементов"


let solve_1_34_list (a: int) (b: int) (lst: int list) : int list =
    lst |> List.filter (fun x -> x >= a && x <= b)


let solve_1_44_list (lst: obj list) : bool =
    let isInt (x: obj) = x :? int
    let isFloat (x: obj) = x :? float
    let rec check prevType rest =
        match rest with
        | [] -> true
        | head :: tail ->
            let currType =
                if isInt head then "int"
                elif isFloat head then "float"
                else "other"
            if currType = "other" || currType = prevType then false
            else check currType tail
    match lst with
    | [] -> true
    | x :: xs ->
        let firstType =
            if isInt x then "int"
            elif isFloat x then "float"
            else "other"
        check firstType xs


let solve_1_54_list (lst: int list) : int list =
    lst
    |> List.countBy id                  
    |> List.filter (fun (_, count) -> count > 3)
    |> List.map fst


let find_pythagorean_triples (lst: int list) : (int * int * int) list =
    lst
    |> List.distinct
    |> List.sort
    |> fun sorted ->
        [ for a in sorted do
            for b in sorted do
                for c in sorted do
                    if a < b && b < c && a * a + b * b = c * c then
                        yield (a, b, c) ]


let filter_divisible_by_3 (arr: int[]) : int[] =
    arr |> Array.filter (fun x -> x % 3 = 0)


let is_palindrome (s: string) : bool =
    let clean = s.ToLower().Replace(" ", "")
    clean = new string(List.rev (Seq.toList clean) |> List.toArray)
    
let countCharFrequency (s: string) : Map<char, int> =
    s.ToCharArray()
    |> Array.countBy id
    |> Map.ofArray

let getCharRelativeFrequency (freqMap: Map<char, int>) (total: int) (ch: char) : float =
    match Map.tryFind ch freqMap with
    | Some count -> float count / float total
    | None -> 0.0

let quadraticDeviation (str: string) (totalFreq: Map<char, float>) : float =
    let charCounts = countCharFrequency str
    let mostFreqChar, _ =
        charCounts
        |> Map.toList
        |> List.maxBy snd

    let strLen = str.Length
    let localFreq = getCharRelativeFrequency charCounts strLen mostFreqChar
    let globalFreq = Map.tryFind mostFreqChar totalFreq |> Option.defaultValue 0.0
    let deviation = localFreq - globalFreq
    deviation * deviation

let getGlobalFrequencies (lines: string list) : Map<char, float> =
    let all = String.Concat(lines)
    let len = all.Length
    countCharFrequency all
    |> Map.map (fun _ count -> float count / float len)

let sortByQuadraticDeviation (lines: string list) : string list =
    let globalFreqs = getGlobalFrequencies lines
    lines
    |> List.sortBy (fun s -> quadraticDeviation s globalFreqs)


let main =
    let test = [5; 3; 8; 1; 4; 6; 5; 3; 5;2]

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

    Console.Write("Самый частый элемент (List): ")
    Console.WriteLine(find_most_frequent_list_based test)

    Console.Write("Количество квадратов в списке: ")
    Console.WriteLine(count_squares test)

    let a = [3; 1; 5]
    let b = [22; 4; 11]  
    let c = [10; 5; 6]   

    let result = make_triples a b c

    printfn "Результат:"
    result |> List.iter (fun (x, y, z) -> printfn "(%d, %d, %d)" x y z)

    Console.Write("Введите количество строк: ")
    let n = Console.ReadLine() |> int
    Console.WriteLine("Введите строки:")
    let stringList = readStringList n

    let sorted = sort_strings_by_length stringList

    Console.WriteLine("\nОтсортированные строки по длине:")
    sorted |> List.iter (printfn "%s")


    printfn "1.4: %A" (solve_1_4_list [3; 1; 4; 2])
    printfn "1.14: %d" (solve_1_14_list 2 5 [1; 2; 3; 6])
    printfn "1.24: %A" (solve_1_24_list [10; 5; 8; 1])
    printfn "1.34: %A" (solve_1_34_list 2 6 [1; 3; 5; 7])
    printfn "1.44: %b" (solve_1_44_list [box 1; box 1.2; box 3; box 4.0])
    printfn "1.54: %A" (solve_1_54_list [1;1;1;1;2;2;2;3;3;3;3;3])

    let triples = find_pythagorean_triples test
    printfn "\nПифагоровы тройки:"
    match triples with
    | [] -> printfn "Не найдено."
    | _ -> triples |> List.iter (fun (a, b, c) -> printfn "(%d, %d, %d)" a b c)



    let A = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12|]
    let result = filter_divisible_by_3 A
    printfn "Элементы, делящиеся на 3: %A" result


    Console.Write("\nВведите строку для проверки на палиндром: ")
    let inputStr = Console.ReadLine()
    if is_palindrome inputStr then
    Console.WriteLine("Это палиндром.")
    else
    Console.WriteLine("Это не палиндром.")

    let lines = ["apple"; "banana"; "grape"; "avocado"]
    let sorted = sortByQuadraticDeviation lines
    printfn "Отсортированные строки:"
    sorted |> List.iter (printfn "%s")

main
