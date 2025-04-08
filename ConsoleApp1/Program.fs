open FParsec

type Expr =
    | Num of int
    | Add of Expr * Expr
    | Sub of Expr * Expr

let numParser: Parser<Expr, unit> =
    pint32 |>> fun n -> Num n

let addParser: Parser<Expr, unit> =
    (spaces >>. numParser .>> spaces) .>>. (pchar '+' .>> spaces) .>>. (spaces >>. numParser)
    |>> fun ((left, _), right) -> Add (left, right)

let subParser: Parser<Expr, unit> =
    (spaces >>. numParser .>> spaces) .>>. (pchar '-' .>> spaces) .>>. (spaces >>. numParser)
    |>> fun ((left, _), right) -> Sub (left, right)

let exprParser: Parser<Expr, unit> =
    addParser <|> subParser <|> numParser

let parseExpression (input: string) =
    match run exprParser input with
    | Success(result, _, _) -> Some result
    | Failure(msg, _, _) -> None

let input1 = "3 + 4"
let input2 = "10 - 3"
let input3 = "5"

let result1 = parseExpression input1
let result2 = parseExpression input2
let result3 = parseExpression input3

printfn "Результат 1: %A" result1
printfn "Результат 2: %A" result2
printfn "Результат 3: %A" result3
