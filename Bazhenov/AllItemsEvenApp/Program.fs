open System
open System.Windows.Forms

type MainForm() as this =
    inherit Form()

    let textBox = new TextBox(Location = System.Drawing.Point(20, 20), Width = 250)
    let resultLabel = new Label(Location = System.Drawing.Point(20, 80), Width = 250, Text = "Результат: ")
    let button = new Button(Location = System.Drawing.Point(20, 50), Text = "Проверить", Width = 100)

    do
        this.Text <- "Проверка списка на чётность"
        this.Size <- System.Drawing.Size(300, 150)

        this.Controls.Add(textBox)
        this.Controls.Add(resultLabel)
        this.Controls.Add(button)

        button.Click.Add(fun _ ->
            try
                let inputText = textBox.Text.Split([|','|]) |> Array.map (fun s -> Int32.Parse(s.Trim()))

                let allEven = inputText |> Array.forall (fun x -> x % 2 = 0)

                if allEven then
                    resultLabel.Text <- "Результат: Все элементы чётные."
                else
                    resultLabel.Text <- "Результат: Не все элементы чётные."
            with
            | :? FormatException ->
                MessageBox.Show("Пожалуйста, вводите только числа, разделённые запятыми.", "Ошибка ввода", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            | ex -> MessageBox.Show(ex.Message, "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
        )

[<EntryPoint>]
let main argv =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.Run(new MainForm())
    0
