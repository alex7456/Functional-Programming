let reactToLanguage language =
    match language with
    | "F#" | "Prolog" -> "Ты — подлиза!"
    | "Python" -> "Неплохой выбор, но где функциональщина?"
    | "Java" -> "ООП — это серьёзно!"
    | "C++" -> "Сложно, но интересно!"
    | _ -> "Хороший вкус!"

// Пример вызова:
printfn "%s" (reactToLanguage "F#")  // Выведет: "Ты — подлиза!"