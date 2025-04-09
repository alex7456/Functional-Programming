open System

// Определяем типы сообщений
type Message =
    | PrintMessage of string
    | DoubleNumber of int
    | Stop

// Создаем агента
let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop () =
        async {
            let! msg = inbox.Receive()

            match msg with
            | PrintMessage text ->
                printfn "Получено сообщение: %s" text
            | DoubleNumber num ->
                printfn "Число удвоено: %d" (num * 2)
            | Stop ->
                printfn "Агент завершает работу."
                return ()  // Завершаем выполнение
            return! loop ()  // Возвращаем к рекурсии для ожидания сообщений
        }
    loop ())  // Начинаем цикл обработки сообщений

// Отправка сообщений в агент
agent.Post(PrintMessage "Hello, world!")  // Сообщение для печати
agent.Post(DoubleNumber 5)                // Сообщение для удвоения числа
agent.Post(DoubleNumber 10)               // Сообщение для удвоения числа
agent.Post(Stop)                          // Сообщение для завершения работы агента

// Ожидаем ввода, чтобы не закрыть консоль сразу
Console.ReadLine() 
