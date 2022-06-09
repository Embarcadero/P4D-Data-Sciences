(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit nltk1_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Zip, System.Threading, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Memo.Types, NumPy, PythonEngine, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python310, PyCommon, PyModule, PyPackage,
  NLTK, FMX.Layouts, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm16 = class(TForm)
    NLTK1: TNLTK;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    PythonEngine1: TPythonEngine;
    Button1: TButton;
    StatusBar1: TStatusBar;
    lbMsg: TLabel;
    lbDesc: TLabel;
    Memo1: TMemo;
    lbTokens: TListBox;
    lbTagged: TListBox;
    lbEntities: TListBox;
    AniIndicator1: TAniIndicator;
    NumPy1: TNumPy;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure PyEmbeddedResEnvironment3101AfterActivate(Sender: TObject;
      const APythonVersion: string; const AActivated: Boolean);
    procedure PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101BeforeActivate(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
      const APythonVersion: string);
    procedure PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
      ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
      Header: TZipHeader; Position: Int64);
    procedure NLTK1AfterInstall(Sender: TObject);
    procedure NLTK1BeforeInstall(Sender: TObject);
    procedure NLTK1InstallError(Sender: TObject; AErrorMessage: string);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NumPy1AfterInstall(Sender: TObject);
    procedure NumPy1BeforeInstall(Sender: TObject);
    procedure NumPy1InstallError(Sender: TObject; AErrorMessage: string);
  private
    FTask: ITask;
    procedure UpdateInstallationStatus(const AStatus, ADescription: string);
    function IsTaskRunning(): boolean;
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

uses
  FMX.DialogService,
  VarPyth;

{$R *.fmx}

// This is a new method in VarPyth, but I'll put it here just in case....
function VarPyToStrings(const AValue : Variant; const AStrings: TStrings): Integer;
begin
  Assert(Assigned(AStrings));
  if VarIsPythonList(AValue) then
    GetPythonEngine.PyListToStrings(
      ExtractPythonObjectFrom(AValue), AStrings)
  else
    raise Exception.Create('Python List expected: ' + _type(AValue));
  Result := AStrings.Count;
end;

procedure TForm16.Button1Click(Sender: TObject);
begin
  with NLTK1 do begin
    nltk.download('punkt');
    nltk.download('averaged_perceptron_tagger');
    nltk.download('maxent_ne_chunker');
    nltk.download('words');
    nltk.download('treebank');

    var tokens := nltk.word_tokenize(memo1.lines.Text);
    VarPyToStrings(tokens, lbTokens.Items);

    var tagged := nltk.pos_tag(tokens);
    VarPyToStrings(tagged, lbTagged.Items);

    var entities := nltk.chunk.ne_chunk(tagged);
    VarPyToStrings(entities, lbEntities.Items);
    var t := nltk.corpus.treebank.parsed_sents('wsj_0001.mrg');
    for var i in VarPyIterate(t) do
     // i.draw(); // Requires Tkinter....
  end;
end;

procedure TForm16.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IsTaskRunning() then begin
    ShowMessage('Waiting for operations...');
    FTask.Cancel();
    while IsTaskRunning() do begin
      FTask.Wait(100);
      //Avoid synchronization deadlock
      Application.ProcessMessages();
    end;
  end;
end;

procedure TForm16.FormCreate(Sender: TObject);
begin
  Button1.Enabled := false;
  AniIndicator1.Enabled := true;
  AniIndicator1.Visible := true;
  FTask := TTask.Run(procedure() begin
    PyEmbeddedResEnvironment3101.Setup(PyEmbeddedResEnvironment3101.PythonVersion);
    FTask.CheckCanceled();
    TThread.Synchronize(nil, procedure() begin
      PyEmbeddedResEnvironment3101.Activate(PyEmbeddedResEnvironment3101.PythonVersion);
    end);
    FTask.CheckCanceled();
    NumPy1.Install();
    FTask.CheckCanceled();
    NLTK1.Install();
    FTask.CheckCanceled();
    TThread.Queue(nil, procedure() begin
      NumPy1.Import();
      NLTK1.Import();
      AniIndicator1.Visible := false;
      AniIndicator1.Enabled := false;
      Button1.Enabled := true;
      UpdateInstallationStatus('Ready', String.Empty);
    end);
  end);
end;

function TForm16.IsTaskRunning: boolean;
begin
  Result := not (FTask.Status in [TTaskStatus.Completed, TTaskStatus.Exception]);
end;

procedure TForm16.NLTK1AfterInstall(Sender: TObject);
begin
  UpdateInstallationStatus('NLTK', 'Package has been installed.');
end;

procedure TForm16.NLTK1BeforeInstall(Sender: TObject);
begin
  UpdateInstallationStatus('NLTK', 'Installing package.');
end;

procedure TForm16.NLTK1InstallError(Sender: TObject; AErrorMessage: string);
begin
  UpdateInstallationStatus('NLTK', AErrorMessage);
end;

procedure TForm16.NumPy1AfterInstall(Sender: TObject);
begin
  UpdateInstallationStatus('NumPy', 'Package has been installed.');
end;

procedure TForm16.NumPy1BeforeInstall(Sender: TObject);
begin
  UpdateInstallationStatus('NumPy', 'Installing package.');
end;

procedure TForm16.NumPy1InstallError(Sender: TObject; AErrorMessage: string);
begin
  UpdateInstallationStatus('NumPy', AErrorMessage);
end;

procedure TForm16.PyEmbeddedResEnvironment3101AfterActivate(
  Sender: TObject; const APythonVersion: string;
  const AActivated: Boolean);
begin
  if AActivated then
    UpdateInstallationStatus('Activate:', 'Python has been activated.')
  else
    UpdateInstallationStatus('Activate:', 'Failed to activate Python.');
end;

procedure TForm16.PyEmbeddedResEnvironment3101AfterSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus('Setup done.', String.Empty);
end;

procedure TForm16.PyEmbeddedResEnvironment3101BeforeActivate(
  Sender: TObject; const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Activating');
end;

procedure TForm16.PyEmbeddedResEnvironment3101BeforeSetup(Sender: TObject;
  const APythonVersion: string);
begin
  UpdateInstallationStatus(Format('Python %s', [APythonVersion]), 'Setting up...');
end;

procedure TForm16.PyEmbeddedResEnvironment3101ZipProgress(Sender: TObject;
  ADistribution: TPyCustomEmbeddableDistribution; FileName: string;
  Header: TZipHeader; Position: Int64);
begin
  UpdateInstallationStatus('Extracting:', FileName);
end;

procedure TForm16.UpdateInstallationStatus(const AStatus,
  ADescription: string);
begin
  TThread.Synchronize(nil, procedure() begin
    lbMsg.Text := AStatus;
    lbMsg.Repaint;
    lbDesc.Text := ADescription;
    lbDesc.Repaint;
  end);
end;

end.
