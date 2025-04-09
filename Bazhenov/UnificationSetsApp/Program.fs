open System
open System.Windows.Forms
open System.Linq

type MainForm() as this =
    inherit Form()

    let textBox1 = new TextBox(Location = System.Drawing.Point(20, 20), Width = 250)
    let textBox2 = new TextBox(Location = System.Drawing.Point(20, 60), Width = 250)
    let resultLabel = new Label(Location = System.Drawing.Point(20, 120), Width = 250, Text = "Результат: ")
    let button = new Button(Location = System.Drawing.Point(20, 90), Text = "Объединить", Width = 100)

    do
        this.Text <- "Объединение массивов"
        this.Size <- System.Drawing.Size(300, 200)

        this.Controls.Add(textBox1)
        this.Controls.Add(textBox2)
        this.Controls.Add(resultLabel)
        this.Controls.Add(button)

        button.Click.Add(fun _ ->
            try
                let input1 = textBox1.Text.Split([|','|]) |> Array.map (fun s -> Int32.Parse(s.Trim()))
                let input2 = textBox2.Text.Split([|','|]) |> Array.map (fun s -> Int32.Parse(s.Trim()))

                let union = input1.Union(input2).ToArray()

                resultLabel.Text <- sprintf "Результат: %A" union
            with
            | :? FormatException ->
                MessageBox.Show("Пожалуйста, вводите только числа, разделенные запятыми.", "Ошибка ввода", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
            | ex -> MessageBox.Show(ex.Message, "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Error) |> ignore
        )

[<EntryPoint>]
let main argv =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.Run(new MainForm())
    0
