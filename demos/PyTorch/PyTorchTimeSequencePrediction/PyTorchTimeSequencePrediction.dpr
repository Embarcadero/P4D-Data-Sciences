program PyTorchTimeSequencePrediction;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  SineWave in 'SineWave.pas',
  Model in 'Model.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
