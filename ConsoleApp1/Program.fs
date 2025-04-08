open System

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
