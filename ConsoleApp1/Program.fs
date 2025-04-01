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

let main =
    readList 5
 
main
