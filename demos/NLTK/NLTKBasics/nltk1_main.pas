unit nltk1_main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Zip,
  FMX.Memo.Types, NumPy, PythonEngine, FMX.PythonGUIInputOutput,
  PyEnvironment.AddOn, PyEnvironment.AddOn.EnsurePip, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python310, PyCommon, PyModule, PyPackage, NLTK,
  FMX.Layouts, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm16 = class(TForm)
    NLTK1: TNLTK;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    PythonEngine1: TPythonEngine;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    StatusBar1: TStatusBar;
    lbMsg: TLabel;
    lbDesc: TLabel;
    Memo1: TMemo;
    ListBox1: TListBox;
    PythonType1: TPythonType;
    NumPy1: TNumPy;
    ListBox2: TListBox;
    ListBox3: TListBox;
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
  private
    { Private declarations }
    procedure UpdateInstallationStatus(const AStatus, ADescription: string);
  public
    { Public declarations }
  end;

var
  Form16: TForm16;

implementation

uses
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
    VarPyToStrings(tokens, ListBox1.Items);

    var tagged := nltk.pos_tag(tokens);
    VarPyToStrings(tagged, ListBox2.Items);

    var entities := nltk.chunk.ne_chunk(tagged);
    VarPyToStrings(entities, ListBox3.Items);
    var t := nltk.corpus.treebank.parsed_sents('wsj_0001.mrg');
    for var i in VarPyIterate(t) do
     // i.draw(); // Requires Tkinter....
  end;
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
  lbMsg.Text := AStatus;
  lbMsg.Repaint;
  lbDesc.Text := ADescription;
  lbDesc.Repaint;
end;

end.
