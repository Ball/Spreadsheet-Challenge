namespace SheetApplication
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>
    public partial class Window1
    {
        public Microsoft.FSharp.Collections.List<ViewModel.RowViewModel> SheetModel
        {
            get { return (Microsoft.FSharp.Collections.List<ViewModel.RowViewModel>) DataContext; }
        }
        public Window1()
        {
            InitializeComponent();
            DataContext = ViewModel.row_models(8);
        }
    }
}
