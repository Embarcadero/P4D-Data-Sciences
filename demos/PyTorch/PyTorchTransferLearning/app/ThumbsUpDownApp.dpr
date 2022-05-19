program ThumbsUpDownApp;









uses
  System.StartUpCopy,
  FMX.Forms,
  Frame.CameraLayout in 'view\frame\Frame.CameraLayout.pas' {CameraLayoutFrame: TFrame},
  Frame.Menu in 'view\frame\Frame.Menu.pas' {MenuFrame: TFrame},
  Form.DataCollection in 'view\form\Form.DataCollection.pas' {DataCollectionForm},
  Form.Main in 'view\form\Form.Main.pas' {MainForm},
  Form.DataCollection.ThumbsUp in 'view\form\Form.DataCollection.ThumbsUp.pas' {ThumbsUpForm},
  Form.DataCollection.ThumbsDown in 'view\form\Form.DataCollection.ThumbsDown.pas' {ThumbsDownForm},
  Frame.TrainingClassSelection in 'view\frame\Frame.TrainingClassSelection.pas' {TrainingClassSelectionFrame: TFrame},
  Remote.ClientClasses in 'remote\Remote.ClientClasses.pas',
  Remote.ClientModule in 'remote\Remote.ClientModule.pas' {ClientModule: TDataModule},
  Form.TrainModel in 'view\form\Form.TrainModel.pas' {TrainModelForm},
  Form.TrainedModel in 'view\form\Form.TrainedModel.pas' {TrainedModelForm},
  BitmapHelper in 'utils\BitmapHelper.pas',
  Frame.CustomCameraLayout in 'view\frame\Frame.CustomCameraLayout.pas' {CustomCameraLayoutFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TClientModule, ClientModule);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
