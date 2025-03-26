let reactToLanguage language =
    match language with
    | "F#" | "Prolog" -> "Ты — подлиза!"
    | "Python" -> "Неплохой выбор, но где функциональщина?"
    | "Java" -> "ООП — это серьёзно!"
    | "C++" -> "Сложно, но интересно!"
    | _ -> "Хороший вкус!"



let mainSuperposition () =
    let readInput () = System.Console.ReadLine()
    let processInput = readInput >> reactToLanguage
    printfn "Введите ваш любимый язык:"
    processInput () |> printfn "%s"

let mainCurrying () =
    let readAndReact reactFunc =
        printfn "Введите ваш любимый язык:"
        System.Console.ReadLine() |> reactFunc |> printfn "%s"
    readAndReact reactToLanguage

mainSuperposition()  