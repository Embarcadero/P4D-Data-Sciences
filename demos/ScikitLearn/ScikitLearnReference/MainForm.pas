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
(*  Functionality:  Basic Reference Examples                              *)
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PythonEngine,
  FMX.PythonGUIInputOutput, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, PyCommon, PyModule, ScikitLearn, FMX.StdCtrls,
  FMX.Layouts, MatplotLib, NumPy, PyPackage;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    ScikitLearn1: TScikitLearn;
    Layout1: TLayout;
    Button1: TButton;
    GroupBox1: TGroupBox;
    rbDecisionTreeRegression: TRadioButton;
    NumPy1: TNumPy;
    MatplotLib1: TMatplotLib;
    procedure Button1Click(Sender: TObject);
  private
    //https://scikit-learn.org/stable/auto_examples/tree/plot_tree_regression.html#sphx-glr-auto-examples-tree-plot-tree-regression-py
    procedure DoDecisionTreeRegression();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  VarPyth;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if rbDecisionTreeRegression.IsChecked then begin
    DoDecisionTreeRegression();
  end;
end;

procedure TForm1.DoDecisionTreeRegression;
begin
  var mm := MainModule;
  var bm := BuiltinModule;
  with ScikitLearn1, NumPy1, MatplotLib1 do begin
    mm.rng := np.random.RandomState(1);
    mm.x := np.sort(5 * mm.rng.rand(80, 1), axis := 0);
    mm.y := np.sin(mm.x).ravel();
    mm.op := 3 * (0.5 - mm.rng.rand(16));
    mm.sl := VarPythonCreate([bm.slice(None(), None(), 5)], stTuple);    
    PythonEngine.ExecString('y[sl] = y[sl] + op');

    //Fit regression model
    mm.regr_1 := tree.DecisionTreeRegressor(max_depth := 2);
    mm.regr_2 := tree.DecisionTreeRegressor(max_depth := 5);
    mm.regr_1.fit(mm.x, mm.y);
    mm.regr_2.fit(mm.x, mm.y);
    
    //Predict
    mm.ar := np.arange(0.0, 5.0, 0.01);
    mm.x_test := mm.ar[VarPythonCreate([Ellipsis(), np.newaxis], stTuple)];
    mm.y_1 := mm.regr_1.predict(mm.x_test);
    mm.y_2 := mm.regr_2.predict(mm.x_test);

    //Plot the result
    plt.figure();
    plt.scatter(mm.x, mm.y, s:=20, edgecolor:='black', c:='darkorange', label:='data');
    plt.plot(mm.x_test, mm.y_1, color:='cornflowerblue', label:='max_depth:=2', linewidth:=2);
    plt.plot(mm.x_test, mm.y_2, color:='yellowgreen', label:='max_depth:=5', linewidth:=2);
    plt.xlabel('data');
    plt.ylabel('target');
    plt.title('Decision Tree Regression');
    plt.legend();
    plt.show();
  end;
end;

end.
