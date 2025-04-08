
open System
(*
[<AbstractClass>]
type GeometricFigure() =
    abstract member CalculateArea: unit -> float
    override this.ToString() = 
        sprintf "Площадь: %.2f" (this.CalculateArea())

type IPrint =
    abstract member Print: unit -> unit

type Rectangle(width: float, height: float) =
    inherit GeometricFigure()

    member val Width = width with get, set
    member val Height = height with get, set

    override this.CalculateArea() =
        this.Width * this.Height

    override this.ToString() =
        sprintf "Прямоугольник: Ширина = %.2f, Высота = %.2f, Площадь = %.2f" this.Width this.Height (this.CalculateArea())

    interface IPrint with
        member this.Print() =
            printfn "%s" (this.ToString())

type Square(sideLength: float) =
    inherit Rectangle(sideLength, sideLength)

    override this.ToString() =
        sprintf "Квадрат: Сторона = %.2f, Площадь = %.2f" this.Width (this.CalculateArea())

    interface IPrint with
        member this.Print() =
            printfn "%s" (this.ToString())

type Circle(radius: float) =
    inherit GeometricFigure()

    member val Radius = radius with get, set

    override this.CalculateArea() =
        Math.PI * this.Radius * this.Radius

    override this.ToString() =
        sprintf "Круг: Радиус = %.2f, Площадь = %.2f" this.Radius (this.CalculateArea())

    interface IPrint with
        member this.Print() =
            printfn "%s" (this.ToString())

type GeometricFigureType =
    | Rectangle of width: float * height: float
    | Square of sideLength: float
    | Circle of radius: float

let calculateArea figure =
    match figure with
    | Rectangle (width, height) -> width * height
    | Square (sideLength) -> sideLength * sideLength
    | Circle (radius) -> Math.PI * radius * radius

let printFigure figure =
    match figure with
    | Rectangle (width, height) ->
        printfn "Прямоугольник: Ширина = %.2f, Высота = %.2f, Площадь = %.2f" width height (calculateArea figure)
    | Square (sideLength) ->
        printfn "Квадрат: Сторона = %.2f, Площадь = %.2f" sideLength (calculateArea figure)
    | Circle (radius) ->
        printfn "Круг: Радиус = %.2f, Площадь = %.2f" radius (calculateArea figure)

let rectangle = Rectangle(5.0, 10.0)
let square = Square(4.0)
let circle = Circle(3.0)

printFigure rectangle
printFigure square
printFigure circle

let fig1 = Rectangle(5.0, 10.0)
let fig2 = Square(4.0)
let fig3 = Circle(3.0)

printfn "Площадь прямоугольника: %.2f" (calculateArea fig1)
printfn "Площадь квадрата: %.2f" (calculateArea fig2)
printfn "Площадь круга: %.2f" (calculateArea fig3)
*)

open System

// Определяем тип Maybe, который может быть Just или Nothing
type Maybe<'a> =
    | Just of 'a
    | Nothing

// Реализация модуля для функтора
module Functor =
    let map f maybe =
        match maybe with
        | Just x -> Just (f x)
        | Nothing -> Nothing

// Реализация модуля для аппликативного функтора
module Applicative =
    let pure x = Just x

    let apply f maybe =
        match f, maybe with
        | Just f', Just x -> Just (f' x)
        | _ -> Nothing

// Реализация модуля для монады
module Monad =
    let bind maybe f =
        match maybe with
        | Just x -> f x
        | Nothing -> Nothing

    let returnMaybe x = Just x

// Определение функций для тестирования
let increment x = x + 1
let double x = x * 2

// Реализация ToString для типа Maybe
let maybeToString (maybe: Maybe<'a>) =
    match maybe with
    | Just x -> sprintf "Just %A" x
    | Nothing -> "Nothing"

// Применение операцій для тестирования
let resultFunctor = Functor.map increment (Just 5)
let resultNothingFunctor = Functor.map increment Nothing

let resultApplicative = Applicative.apply (Just increment) (Just 5)
let resultNothingApplicative = Applicative.apply (Just increment) Nothing

let resultMonad = Monad.bind (Just 5) (fun x -> Just (x * 2))
let resultNothingMonad = Monad.bind Nothing (fun x -> Just (x * 2))

// Вывод результатов с помощью функции maybeToString
Console.WriteLine("Результат функтора: " + maybeToString resultFunctor)
Console.WriteLine("Результат функтора для Nothing: " + maybeToString resultNothingFunctor)
Console.WriteLine("Результат аппликативного функтора: " + maybeToString resultApplicative)
Console.WriteLine("Результат аппликативного функтора для Nothing: " + maybeToString resultNothingApplicative)
Console.WriteLine("Результат монады: " + maybeToString resultMonad)
Console.WriteLine("Результат монады для Nothing: " + maybeToString resultNothingMonad)
 