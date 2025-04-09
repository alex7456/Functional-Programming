



open System
open System.Text.RegularExpressions

type Passport(seria: string, number: string, fullName: string, issueDate: DateTime, issuedBy: string) =
    let mutable _seria = seria
    let mutable _number = number
    let mutable _fullName = fullName
    let mutable _issueDate = issueDate
    let mutable _issuedBy = issuedBy

    member this.ValidateSeria() =
        let seriaPattern = @"^\d{4}$" 
        Regex.IsMatch(string _seria, seriaPattern)

    member this.ValidateNumber() =
        let numberPattern = @"^\d{6}$" 
        Regex.IsMatch(string _number, numberPattern)

    member this.Validate() =
        this.ValidateSeria() && this.ValidateNumber()

    member this.Seria 
        with get() = _seria
        and set(value) =
            if Regex.IsMatch(string value, @"^\d{4}$") then _seria <- value
            else failwith "Неверный формат серии паспорта."

    member this.Number 
        with get() = _number
        and set(value) =
            if Regex.IsMatch(string value, @"^\d{6}$") then _number <- value
            else failwith "Неверный формат номера паспорта."

    member this.FullName 
        with get() = _fullName
        and set(value) = _fullName <- value

    member this.IssueDate
        with get() = _issueDate
        and set(value) = _issueDate <- value

    member this.IssuedBy
        with get() = _issuedBy
        and set(value) = _issuedBy <- value

    member this.PrintDocument() =
        if this.Validate() then
            printfn "Паспорт гражданина РФ:"
            printfn "Серия: %s" this.Seria
            printfn "Номер: %s" this.Number
            printfn "ФИО владельца: %s" this.FullName
            printfn "Дата выдачи: %O" this.IssueDate
            printfn "Кем выдан: %s" this.IssuedBy
        else
            printfn "Ошибка: Неверный формат данных паспорта."

    member this.Equals(other: Passport) =
        this.Seria = other.Seria && this.Number = other.Number

let passport1 = new Passport("1234", "567890", "Иванов Иван Иванович", DateTime(2020, 5, 10), "ОВД Ленина")
let passport2 = new Passport("1234", "567890", "Петров Петр Петрович", DateTime(2021, 6, 15), "ОВД Центральный")

passport1.PrintDocument()
passport2.PrintDocument()

if passport1.Equals(passport2) then
    printfn "Документы одинаковые."
else
    printfn "Документы разные."

