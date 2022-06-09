(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit CompModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, TorchVision, PyTorch, PyCommon,
  PyModule, PyPackage, NumPy, PyEnvironment, PyEnvironment.Embeddable,
  PyEnvironment.Embeddable.Res,
  System.Zip, OpenCV, PyEnvironment.Embeddable.Res.Python38;

type
  TPyComps = class(TDataModule)
    PyEngine: TPythonEngine;
    PyNumPy: TNumPy;
    PyTorch: TPyTorch;
    PyTorchVision: TTorchVision;
    PyIO: TPythonInputOutput;
    PyOpenCV: TOpenCV;
    PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38;
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOReceiveData(Sender: TObject; var Data: AnsiString);
    procedure PyNumPyAfterInstall(Sender: TObject);
    procedure PyNumPyBeforeInstall(Sender: TObject);
    procedure PyTorchVisionBeforeInstall(Sender: TObject);
    procedure PyTorchVisionAfterInstall(Sender: TObject);
    procedure PyTorchBeforeInstall(Sender: TObject);
    procedure PyTorchAfterInstall(Sender: TObject);
    procedure PyOpenCVBeforeInstall(Sender: TObject);
    procedure PyOpenCVAfterInstall(Sender: TObject);
    procedure PyOpenCVInstallError(Sender: TObject; AErrorMessage: string);
    procedure PyEmbeddedResEnvironment381AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment381BeforeSetup(Sender: TObject;
      const APythonVersion: string);
  public
    procedure CheckEngine();
  end;

var
  PyComps: TPyComps;

implementation

uses
  Pipe;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TPyComps.CheckEngine;
begin
  if not PyEngine.Initialized and not PyEngine.IsHandleValid() then
    raise EDLLLoadError.Create('The Python interpreter could not be loaded.');
end;

procedure TPyComps.PyOpenCVAfterInstall(Sender: TObject);
begin
  TPipeIO.Write('OpenCV has been installed');
end;

procedure TPyComps.PyOpenCVBeforeInstall(Sender: TObject);
begin
  TPipeIO.Write('Installing OpenCV');
end;

procedure TPyComps.PyOpenCVInstallError(Sender: TObject; AErrorMessage: string);
begin
  TPipeIO.Write(AErrorMessage);
end;

procedure TPyComps.PyEmbeddedResEnvironment381AfterActivate(Sender: TObject;
  const APythonVersion: string; const AActivated: Boolean);
begin
  TPipeIO.Write('Python is ready');
end;

procedure TPyComps.PyEmbeddedResEnvironment381BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  TPipeIO.Write('Setting up Python' + APythonVersion);
end;

procedure TPyComps.PyIOReceiveData(Sender: TObject; var Data: AnsiString);
begin
  TPipeIO.Write(String(Data));
end;

procedure TPyComps.PyIOSendData(Sender: TObject; const Data: AnsiString);
begin
  TPipeIO.Write(String(Data));
end;

procedure TPyComps.PyNumPyAfterInstall(Sender: TObject);
begin
  TPipeIO.Write('NumPy has been installed');
end;

procedure TPyComps.PyNumPyBeforeInstall(Sender: TObject);
begin
  TPipeIO.Write('Installing NumPy');
end;

procedure TPyComps.PyTorchAfterInstall(Sender: TObject);
begin
  TPipeIO.Write('PyTorch has been installed');
end;

procedure TPyComps.PyTorchBeforeInstall(Sender: TObject);
begin
  TPipeIO.Write('Installing PyTorch');
end;

procedure TPyComps.PyTorchVisionAfterInstall(Sender: TObject);
begin
  TPipeIO.Write('PyTorch Vision has been installed');
end;

procedure TPyComps.PyTorchVisionBeforeInstall(Sender: TObject);
begin
  TPipeIO.Write('Installing PyTorch Vision');
end;

end.
