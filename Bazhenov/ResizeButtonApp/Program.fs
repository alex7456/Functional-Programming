open System
open System.Windows.Forms
open System.Drawing

[<EntryPoint>]
let main argv =
    let form = new Form(Text = "Ползунок и кнопка", Size = Size(400, 200))

    let button = new Button(Text = "Нажми меня", Location = Point(50, 50), Width = 100, Height = 30)

    let trackBar = new TrackBar()
    trackBar.Location <- Point(50, 100)
    trackBar.Width <- 200
    trackBar.Minimum <- 50
    trackBar.Maximum <- 300
    trackBar.Value <- button.Width
    trackBar.TickFrequency <- 10

    trackBar.Scroll.Add(fun _ ->
        button.Width <- trackBar.Value
    )

    form.Controls.Add(button)
    form.Controls.Add(trackBar)

    Application.Run(form)
    0
