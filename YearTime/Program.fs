open System
open System.Windows.Forms

// Главная форма
let mainForm = new Form(Text = "Определение времени года", Width = 400, Height = 200)

let months = 
    [ "Январь"; "Февраль"; "Март"; "Апрель"; "Май"; "Июнь"; "Июль"; "Август"; "Сентябрь"; "Октябрь"; "Ноябрь"; "Декабрь" ]

// Создание ComboBox для выбора месяца
let comboBox = new ComboBox(DropDownStyle = ComboBoxStyle.DropDownList, Width = 150, Left = 100, Top = 50)
comboBox.Items.AddRange(months |> Array.ofList)
comboBox.SelectedIndex <- 0  // Установим Январь по умолчанию

// Создание кнопки
let button = new Button(Text = "Определить время года", Width = 200, Left = 100, Top = 100)

// Метка для вывода сообщения
let label = new Label(Width = 200, Left = 100, Top = 140)

button.Click.Add(fun _ ->
    let month = comboBox.SelectedItem.ToString()
    let season =
        match month with
        | "Декабрь" | "Январь" | "Февраль" -> "Зима"
        | "Март" | "Апрель" | "Май" -> "Весна"
        | "Июнь" | "Июль" | "Август" -> "Лето"
        | "Сентябрь" | "Октябрь" | "Ноябрь" -> "Осень"
        | _ -> "Неизвестно"

    label.Text <- sprintf "Время года: %s" season
)

// Добавляем элементы на форму
mainForm.Controls.Add(comboBox)
mainForm.Controls.Add(button)
mainForm.Controls.Add(label)

// Запуск формы
Application.Run(mainForm)
