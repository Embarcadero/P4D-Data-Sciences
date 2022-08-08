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
  PyCommon, PyModule, MatplotLib, PythonEngine, FMX.PythonGUIInputOutput,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, NumPy, FMX.StdCtrls,
  FMX.Layouts, PyPackage, System.Generics.Collections, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python310, PyQT5;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    MatplotLib1: TMatplotLib;
    NumPy1: TNumPy;
    Layout1: TLayout;
    btnScatterMasked: TButton;
    btnScatterSymbol: TButton;
    btnInterpolations: TButton;
    PyEmbeddedResEnvironment3101: TPyEmbeddedResEnvironment310;
    PyQT51: TPyQT5;
    procedure btnScatterMaskedClick(Sender: TObject);
    procedure btnScatterSymbolClick(Sender: TObject);
    procedure btnInterpolationsClick(Sender: TObject);
  private
    procedure DisableButtons();
    procedure EnableButtons();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  VarPyth, PyUtils;

procedure TForm1.btnScatterMaskedClick(Sender: TObject);
begin
  //https://matplotlib.org/stable/gallery/lines_bars_and_markers/scatter_masked.html#sphx-glr-gallery-lines-bars-and-markers-scatter-masked-py
  DisableButtons();
  try
    var mm := MainModule;
    var op := Import('operator');
    with MatplotLib1, NumPy1 do begin
      mm.np := np;
      np.random.seed(19680801);
      var N := 100;
      var r0 := 0.6;
      var x := 0.9 * np.random.rand(N);
      var y := 0.9 * np.random.rand(N);
      mm.rand := np.random.rand(N);
      //                     np.random.rand(N))**2
      var area := 20 * op.pow(np.random.rand(N), 2);
      var c := np.sqrt(area);
      //                     x ** 2 + y ** 2
      var r := np.sqrt(op.pow(x, 2) + op.pow(y, 2));
      //                                r < r0
      var area1 := np.ma.masked_where(op.lt(r, r0), area);
      //                                r >= r0
      var area2 := np.ma.masked_where(op.ge(r, r0), area);

      TPyEx.ExecuteMasked(procedure begin
        plt.scatter(x, y, s := area1, marker := '^', c := c);
        plt.scatter(x, y, s := area2, marker := 'o', c := c);
      end);

      //Show the boundary between the regions:
      var theta := np.arange(0, np.pi / 2, 0.01);
      plt.plot(r0 * np.cos(theta), r0 * np.sin(theta));
      plt.show();
    end;
  finally
    EnableButtons();
  end;
end;

procedure TForm1.btnScatterSymbolClick(Sender: TObject);
begin
  //https://matplotlib.org/stable/gallery/lines_bars_and_markers/scatter_symbol.html#sphx-glr-gallery-lines-bars-and-markers-scatter-symbol-py
  DisableButtons();
  try
    var bm := BuiltinModule;
    var mm := MainModule;
    var op := Import('operator');
    with MatplotLib1, NumPy1 do begin
      var np := np;
      np.random.seed(19680801);
      var x := np.arange(0.0, 50.0, 2.0);
      //*[x.shape] symbol = unpack tuple and each element is passed to each argument
      //once shape has a single element, we can get the first element and send as argument
      //             x ** 1.3 + np.random.rand(*x.shape) * 30.0
      var y := op.pow(x, 1.3) + np.random.rand(x.shape.GetItem(0)) * 30.0;
      //                      *x.shape
      var s := np.random.rand(x.shape.GetItem(0)) * 800 + 500;
      plt.scatter(x, y, s, c :='g', alpha := 0.5, marker := '$\clubsuit$', label := 'Luck');
      plt.xlabel('Leprechauns');
      plt.ylabel('Gold');
      plt.legend(loc := 'upper left');
      plt.show();
    end;
  finally
    EnableButtons();
  end;
end;

procedure TForm1.btnInterpolationsClick(Sender: TObject);
begin
  //https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html#sphx-glr-gallery-images-contours-and-fields-interpolation-methods-py
  DisableButtons();
  try
    var bm := BuiltinModule;
    var mm := MainModule;
    with MatplotLib1, NumPy1 do begin
      mm.np := np;
      var methods := VarArrayof([
        None, 'none', 'nearest', 'bilinear', 'bicubic', 'spline16',
        'spline36', 'hanning', 'hamming', 'hermite', 'kaiser', 'quadric',
        'catrom', 'gaussian', 'bessel', 'mitchell', 'sinc', 'lanczos']);

      // Fixing random state for reproducibility
      np.random.seed(19680801);

      mm.grid := np.random.rand(4, 4);

      var dict := TPyEx.Dictionary([
        TDictItem.Create('xticks', VarArrayOf([])),
        TDictItem.Create('yticks', VarArrayOf([]))
      ]);

      var plots := plt.subplots(nrows := 3, ncols := 6,
        figsize:= TPyEx.Tuple([9, 6]), subplot_kw := dict);
      mm.fig := plots.GetItem(0);
      mm.axs := plots.GetItem(1);

      for var LZip in bm.zip(mm.axs.flat, methods).GetEnumerator() do begin
        var ax := LZip.GetItem(0);
        var interp_method := LZip.GetItem(1);
        ax.imshow(mm.grid, interpolation := interp_method, cmap := 'viridis');
        ax.set_title(bm.str(interp_method));
      end;

      plt.tight_layout();
      plt.show();
    end;
  finally
    EnableButtons();
  end;
end;

procedure TForm1.DisableButtons;
begin
  btnScatterMasked.Enabled := false;
  btnScatterSymbol.Enabled := false;
  btnInterpolations.Enabled := false;
end;

procedure TForm1.EnableButtons;
begin
  btnScatterMasked.Enabled := true;
  btnScatterSymbol.Enabled := true;
  btnInterpolations.Enabled := true;
end;

end.
