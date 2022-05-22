program ThumbsUpDownPythonInstall;

uses
  System.StartUpCopy,
  FMX.Forms,
  InstallForm in 'InstallForm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
