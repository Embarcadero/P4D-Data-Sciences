unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PythonEngine,
  PyEnvironment, PyEnvironment.Embeddable, PyCommon, PyModule, PyPackage,
  PyTorch, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, NumPy;

type
  TForm1 = class(TForm)
    PyTorch1: TPyTorch;
    PyEmbeddedEnvironment1: TPyEmbeddedEnvironment;
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Button1: TButton;
    NumPy1: TNumPy;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedEnvironment1Error(Sender: TObject;
      const AException: Exception);
    procedure PyEmbeddedEnvironment1PluginInstall(const APlugin: TObject;
      const AInfo: TPyPluginInfo);
    procedure PyEmbeddedEnvironment1Ready(Sender: TObject;
      const APythonVersion: string);
    procedure PyTorch1InstallError(Sender: TObject; AException: Exception;
      var AAbort: Boolean);
    procedure PyTorch1BeforeInstall(Sender: TObject);
    procedure PyTorch1AfterInstall(Sender: TObject);
    procedure PyTorch1AfterImport(Sender: TObject);
    procedure PyTorch1BeforeImport(Sender: TObject);
    procedure NumPy1AfterImport(Sender: TObject);
    procedure NumPy1AfterInstall(Sender: TObject);
    procedure NumPy1BeforeImport(Sender: TObject);
    procedure NumPy1BeforeInstall(Sender: TObject);
    procedure NumPy1InstallError(Sender: TObject; AException: Exception;
      var AAbort: Boolean);
  private
    FCancelable: IAsyncResult;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Threading,
  FMX.DialogService;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not FCancelable.IsCompleted and not FCancelable.IsCancelled then
    FCancelable.Cancel();
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not FCancelable.IsCompleted then begin
    CanClose := false;
    if not FCancelable.IsCancelled then begin
      TDialogService.MessageDialog('Background operation will be canceled. Wait...',
        TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1, nil);
      FCancelable.Cancel();
      TTask.Run(procedure() begin
        FCancelable.AsyncWaitEvent.WaitFor();
        TThread.Queue(nil, procedure() begin
          Close();
        end);
      end);
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCancelable := PyEmbeddedEnvironment1.ActivateAsync(
    PyEmbeddedEnvironment1.SetupAsync());
end;

procedure TForm1.NumPy1AfterImport(Sender: TObject);
begin
  Memo1.Lines.Add('Numpy has been imported.');
end;

procedure TForm1.NumPy1AfterInstall(Sender: TObject);
begin
  Memo1.Lines.Add('Numpy has been installed.');
end;

procedure TForm1.NumPy1BeforeImport(Sender: TObject);
begin
  Memo1.Lines.Add('Importing numpy.');
end;

procedure TForm1.NumPy1BeforeInstall(Sender: TObject);
begin
  Memo1.Lines.Add('Installing numpy.');
end;

procedure TForm1.NumPy1InstallError(Sender: TObject; AException: Exception;
  var AAbort: Boolean);
begin
  Memo1.Lines.Add(Format('Failed to install numpy. %s', [AException.ToString()]));
end;

procedure TForm1.PyEmbeddedEnvironment1AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  Memo1.Lines.Add(Format('Python %s is active.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add('Python setup done.');
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeActivate(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Activating Python %s.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Seting up Python.', [APythonVersion]));
end;

procedure TForm1.PyEmbeddedEnvironment1Error(Sender: TObject;
  const AException: Exception);
begin
  Memo1.Lines.Add(Format('Environment error: %s.', [AException.ToString()]));
end;

procedure TForm1.PyEmbeddedEnvironment1PluginInstall(const APlugin: TObject;
  const AInfo: TPyPluginInfo);
begin
  Memo1.Lines.Add(Format('Installing plugin: %s', [AInfo.Name]));
end;

procedure TForm1.PyEmbeddedEnvironment1Ready(Sender: TObject;
  const APythonVersion: string);
begin
  Memo1.Lines.Add(Format('Python %s is ready.', [APythonVersion]));
end;

procedure TForm1.PyTorch1AfterImport(Sender: TObject);
begin
  Memo1.Lines.Add(Format('Torch has been imported.', []));
end;

procedure TForm1.PyTorch1AfterInstall(Sender: TObject);
begin
  Memo1.Lines.Add('Torch has been installed.');
end;

procedure TForm1.PyTorch1BeforeImport(Sender: TObject);
begin
  Memo1.Lines.Add('Importing torch.');
end;

procedure TForm1.PyTorch1BeforeInstall(Sender: TObject);
begin
  Memo1.Lines.Add('Installing torch.');
end;

procedure TForm1.PyTorch1InstallError(Sender: TObject; AException: Exception;
  var AAbort: Boolean);
begin
  Memo1.Lines.Add(Format('Failed to install torch. %s', [AException.ToString()]));
  AAbort := true;
end;

end.
