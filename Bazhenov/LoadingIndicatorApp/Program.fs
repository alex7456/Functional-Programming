open System
open System.Windows.Forms
open System.Threading

type MainForm() as this =
    inherit Form()

    let textBox = new TextBox(Text = "", Location = System.Drawing.Point(20, 20), Width = 250)
    let progressBar = new ProgressBar(Location = System.Drawing.Point(20, 60), Width = 250)
    let statusLabel = new Label(Location = System.Drawing.Point(20, 90), Text = "Введите текст:")

    do
        this.Text <- "Программа с индикатором загрузки"
        this.Size <- System.Drawing.Size(300, 150)

        this.Controls.Add(textBox)
        this.Controls.Add(progressBar)
        this.Controls.Add(statusLabel)

        progressBar.Minimum <- 0
        progressBar.Maximum <- 100
        progressBar.Value <- 0

        textBox.TextChanged.Add(fun _ ->
            let inputText = textBox.Text

            let (progress, maxProgress) = 
                if String.IsNullOrEmpty(inputText) then
                    (0, 100) 
                else
                    let length = Math.Min(inputText.Length * 10, 100) 
                    (length, 100)  

            progressBar.Value <- progress
            statusLabel.Text <- sprintf "Введите текст: (%d%%)" progress
        )

[<EntryPoint>]
let main argv =
    Application.EnableVisualStyles()
    Application.SetCompatibleTextRenderingDefault(false)
    Application.Run(new MainForm())
    0
