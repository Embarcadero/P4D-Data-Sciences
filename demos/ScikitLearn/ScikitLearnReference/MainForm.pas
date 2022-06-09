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
