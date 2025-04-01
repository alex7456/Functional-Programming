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

let main =
    let list = readList 5
    printList list
    

main
