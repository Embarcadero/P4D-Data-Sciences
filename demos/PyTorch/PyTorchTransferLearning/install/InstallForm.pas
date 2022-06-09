(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit InstallForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, TorchVision,
  PyTorch, OpenCV, PyCommon, PyModule, PyPackage, NumPy, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python38, PythonEngine, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.PythonGUIInputOutput, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, System.Threading;

type
  TForm2 = class(TForm)
    PyEngine: TPythonEngine;
    PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38;
    PyNumPy: TNumPy;
    PyOpenCV: TOpenCV;
    PyTorch: TPyTorch;
    PyTorchVision: TTorchVision;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    mmLog: TMemo;
    procedure PyEmbeddedResEnvironment381BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment381AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyNumPyBeforeInstall(Sender: TObject);
    procedure PyNumPyAfterInstall(Sender: TObject);
    procedure PyTorchVisionBeforeInstall(Sender: TObject);
    procedure PyTorchBeforeInstall(Sender: TObject);
    procedure PyOpenCVBeforeInstall(Sender: TObject);
    procedure PyOpenCVAfterInstall(Sender: TObject);
    procedure PyTorchAfterInstall(Sender: TObject);
    procedure PyTorchVisionAfterInstall(Sender: TObject);
    procedure PyNumPyInstallError(Sender: TObject; AErrorMessage: string);
    procedure PyTorchVisionInstallError(Sender: TObject; AErrorMessage: string);
    procedure PyTorchInstallError(Sender: TObject; AErrorMessage: string);
    procedure PyOpenCVInstallError(Sender: TObject; AErrorMessage: string);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FTask: ITask;
    function IsTaskRunning(): boolean;
    procedure Log(const AMsg: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

{ TForm2 }

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsTaskRunning() then begin
    ShowMessage('Waiting for operations...');
    FTask.Cancel();
    while IsTaskRunning() do begin
      FTask.Wait(100);
      Application.ProcessMessages();
    end;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FTask := TTask.Run(procedure() begin
    PyEmbeddedResEnvironment381.Setup(PyEmbeddedResEnvironment381.PythonVersion);
    FTask.CheckCanceled();
    TThread.Synchronize(nil, procedure() begin
      PyEmbeddedResEnvironment381.Activate(PyEmbeddedResEnvironment381.PythonVersion);
    end);

    FTask.CheckCanceled();
    PyNumPy.Install();

    FTask.CheckCanceled();
    PyOpenCV.Install();

    FTask.CheckCanceled();
    PyTorch.Install();

    FTask.CheckCanceled();
    PyTorchVision.Install();

    FTask.CheckCanceled();
    TThread.Queue(nil, procedure() begin
      PyNumPy.Install();
      PyOpenCV.Install();
      PyTorch.Install();
      PyTorchVision.Install();
      Log('All done!');
    end);
  end);
end;

function TForm2.IsTaskRunning: boolean;
begin
  Result := not (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception]);
end;

procedure TForm2.Log(const AMsg: string);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil, procedure() begin
      mmLog.Lines.Add('-> ' + AMsg);
    end)
  else
    mmLog.Lines.Add('-> ' + AMsg);
end;

procedure TForm2.PyOpenCVAfterInstall(Sender: TObject);
begin
  Log(Format('%s has been installed.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyOpenCVBeforeInstall(Sender: TObject);
begin
  Log(Format('Installing %s...', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyOpenCVInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(Format('%s installation failed.' + #13#10 + '%s', [
    TPyPackage(Sender).PyModuleName,
    AErrorMessage]));
end;

procedure TForm2.PyEmbeddedResEnvironment381AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Log(Format('Python%s has been successfully installed.', [APythonVersion]));
end;

procedure TForm2.PyEmbeddedResEnvironment381BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  Log(Format('Installing Python%s...', [APythonVersion]));
end;

procedure TForm2.PyNumPyAfterInstall(Sender: TObject);
begin
  Log(Format('%s has been installed.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyNumPyBeforeInstall(Sender: TObject);
begin
  Log(Format('Installing %s...', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyNumPyInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(Format('%s installation failed.' + #13#10 + '%s', [
    TPyPackage(Sender).PyModuleName,
    AErrorMessage]));
end;

procedure TForm2.PyTorchAfterInstall(Sender: TObject);
begin
  Log(Format('%s has been installed.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyTorchBeforeInstall(Sender: TObject);
begin
  Log(Format('Installing %s... It may take some time.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyTorchInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(Format('%s installation failed.' + #13#10 + '%s', [
    TPyPackage(Sender).PyModuleName,
    AErrorMessage]));
end;

procedure TForm2.PyTorchVisionAfterInstall(Sender: TObject);
begin
  Log(Format('%s has been installed.', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyTorchVisionBeforeInstall(Sender: TObject);
begin
  Log(Format('Installing %s...', [TPyPackage(Sender).PyModuleName]));
end;

procedure TForm2.PyTorchVisionInstallError(Sender: TObject;
  AErrorMessage: string);
begin
  Log(Format('%s installation failed.' + #13#10 + '%s', [
    TPyPackage(Sender).PyModuleName,
    AErrorMessage]));
end;

end.
