namespace WpfFSharpApp

open System.Windows

type MainWindow() as this =
    inherit Window()

    do
        let resourceLocater = new System.Uri("/WpfFSharpApp;component/MainWindow.xaml", System.UriKind.Relative)
        System.Windows.Application.LoadComponent(this, resourceLocater)

        let button = this.FindName("MyButton") :?> Controls.Button
        let slider = this.FindName("MySlider") :?> Controls.Slider

        slider.ValueChanged.Add(fun _ ->
            button.Width <- slider.Value
        )
