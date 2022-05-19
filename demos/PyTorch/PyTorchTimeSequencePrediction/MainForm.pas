(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'MainForm'         Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  PyTorch - Time Sequence Pediction                     *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)
(* This source code is distributed with no WARRANTY, for no reason or use.*)
(* Everyone is allowed to use and change this code free for his own tasks *)
(* and projects, as long as this header and its copyright text is intact. *)
(* For changed versions of this code, which are public distributed the    *)
(* following additional conditions have to be fullfilled:                 *)
(* 1) The header has to contain a comment on the change and the author of *)
(*    it.                                                                 *)
(* 2) A copy of the changed source has to be sent to the above E-Mail     *)
(*    address or my then valid address, if this is possible to the        *)
(*    author.                                                             *)
(* The second condition has the target to maintain an up to date central  *)
(* version of the component. If this condition is not acceptable for      *)
(* confidential or legal reasons, everyone is free to derive a component  *)
(* or to generate a diff file to my or other original sources.            *)
(**************************************************************************)
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
