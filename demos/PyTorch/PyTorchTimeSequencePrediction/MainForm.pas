(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PythonEngine, FMX.PythonGUIInputOutput, PyTorch, PyCommon, PyModule,
  PyPackage, NumPy, MatplotLib, PyEnvironment, PyEnvironment.Embeddable,
  PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python310,
  PyEnvironment.AddOn, PyEnvironment.AddOn.EnsurePip;

type
  //Original reference: https://github.com/pytorch/examples/tree/master/time_sequence_prediction
  TForm1 = class(TForm)
    Memo1: TMemo;
    Layout1: TLayout;
    btnGenerate: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    NumPy1: TNumPy;
    PyTorch1: TPyTorch;
    btnTrain: TButton;
    MatplotLib1: TMatplotLib;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    PyEnvironmentAddOnEnsurePip1: TPyEnvironmentAddOnEnsurePip;
    PyEnvironmentAddOn1: TPyEnvironmentAddOn;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnTrainClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SineWave, Model;

{$R *.fmx}

procedure TForm1.btnGenerateClick(Sender: TObject);
begin
  TSineWave.Generate(NumPy1, PyTorch1);
  btnTrain.Enabled := true;
  Memo1.Lines.Add('Data has been generated. Proceed with training process.');
end;

procedure TForm1.btnTrainClick(Sender: TObject);
begin
  TModel.Train(NumPy1, MatplotLib1, PyTorch1);
  Memo1.Lines.Add('The trainning process has finished. Check out generated pdfs.');
end;

end.
