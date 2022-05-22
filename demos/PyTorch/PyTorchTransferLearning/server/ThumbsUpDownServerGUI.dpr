program ThumbsUpDownServerGUI;
{$APPTYPE GUI}

uses
  FMX.Forms,
  Web.WebReq,
  IdHTTPWebBrokerBridge,
  MainForm in 'MainForm.pas' {Form1},
  TrainingClassMethods in 'TrainingClassMethods.pas' {TrainingClass: TDSServerModule},
  ServerContainerUnit1 in 'ServerContainerUnit1.pas' {ServerContainer1: TDataModule},
  WebModuleUnit1 in 'WebModuleUnit1.pas' {WebModule1: TWebModule},
  ProcErrorCode in 'ProcErrorCode.pas';

{$R *.res}

begin
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
