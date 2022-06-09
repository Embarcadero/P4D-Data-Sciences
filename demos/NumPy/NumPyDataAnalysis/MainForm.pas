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
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, 
  FMX.Dialogs, PyCommon, PyModule, NumPy, PythonEngine, FMX.PythonGUIInputOutput,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, PyPackage;

type
  (*https://www.machinelearningplus.com/python/101-numpy-exercises-python/*)
  //NumPy Data Analysis exercises resolved in the P4D style.
  TForm2 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    NumPy1: TNumPy;
    Memo1: TMemo;
    PythonModule1: TPythonModule;
    procedure FormCreate(Sender: TObject);
  private
    bm: variant;
    mm: variant;
    op: variant;
    //Interop methods
    function maxx(ASelf, AArgs: PPyObject): PPyObject; cdecl;
    //Helper methods
    procedure RestorePrintOpts(const APrintOpts: variant);
  private
    /// <summary>
    ///  1. Import numpy as np and see the version
    /// </summary>
    procedure Exercise1();
    /// <summary>
    ///  2. How to create a 1D array?
    /// </summary>
    procedure Exercise2();
    /// <summary>
    ///   3. How to create a boolean array?
    /// </summary>
    procedure Exercise3();
    /// <summary>
    ///   4. How to extract items that satisfy a given condition from 1D array?
    /// </summary>
    procedure Exercise4();
    /// <summary>
    ///   5. How to replace items that satisfy a condition with another value in numpy array?
    /// </summary>
    procedure Exercise5();
    /// <summary>
    ///   6. How to replace items that satisfy a condition without affecting the original array?
    /// </summary>
    procedure Exercise6();
    /// <summary>
    ///   7. How to reshape an array?
    /// </summary>
    procedure Exercise7();
    /// <summary>
    ///   8. How to stack two arrays vertically?
    /// </summary>
    procedure Exercise8();
    /// <summary>
    ///   9. How to stack two arrays horizontally?
    /// </summary>
    procedure Exercise9();
    /// <summary>
    ///   10. How to generate custom sequences in numpy without hardcoding?
    /// </summary>
    procedure Exercise10();
    /// <summary>
    ///   11. How to get the common items between two python numpy arrays?
    /// </summary>
    procedure Exercise11();
    /// <summary>
    ///   12. How to remove from one array those items that exist in another?
    /// </summary>
    procedure Exercise12();
    /// <summary>
    ///   13. How to get the positions where elements of two arrays match?
    /// </summary>
    procedure Exercise13();
    /// <summary>
    ///   14. How to extract all numbers between a given range from a numpy array?
    /// </summary>
    procedure Exercise14();
    /// <summary>
    ///   15. How to make a python function that handles scalars to work on numpy arrays?
    /// </summary>
    procedure Exercise15();
    /// <summary>
    ///   16. How to swap two columns in a 2d numpy array?
    /// </summary>
    procedure Exercise16();
    /// <summary>
    ///   17. How to swap two rows in a 2d numpy array?
    /// </summary>
    procedure Exercise17();
    /// <summary>
    ///   18. How to reverse the rows of a 2D array?
    /// </summary>
    procedure Exercise18();
    /// <summary>
    ///   19. How to reverse the columns of a 2D array?
    /// </summary>
    procedure Exercise19();
    /// <summary>
    ///   20. How to create a 2D array containing random floats between 5 and 10?
    /// </summary>
    procedure Exercise20();
    /// <summary>
    ///   21. How to print only 3 decimal places in python numpy array?
    /// </summary>
    procedure Exercise21();
    /// <summary>
    ///   22. How to pretty print a numpy array by suppressing the scientific notation (like 1e10)?
    /// </summary>
    procedure Exercise22();
    /// <summary>
    ///   23. How to limit the number of items printed in output of numpy array?
    /// </summary>
    procedure Exercise23();
    /// <summary>
    ///   24. How to print the full numpy array without truncating
    /// </summary>
    procedure Exercise24();
    /// <summary>
    ///   25. How to import a dataset with numbers and texts keeping the text intact in python numpy?
    /// </summary>
    procedure Exercise25();
    /// <summary>
    ///   26. How to extract a particular column from 1D array of tuples?
    /// </summary>
    procedure Exercise26();
    /// <summary>
    ///   27. How to convert a 1d array of tuples to a 2d numpy array?
    /// </summary>
    procedure Exercise27();
    /// <summary>
    ///   28. How to compute the mean, median, standard deviation of a numpy array?
    /// </summary>
    procedure Exercise28();
    /// <summary>
    ///   29. How to normalize an array so the values range exactly between 0 and 1?
    /// </summary>
    procedure Exercise29();
    /// <summary>
    ///   30. How to compute the softmax score?
    /// </summary>
    procedure Exercise30();
    /// <summary>
    ///   31. How to find the percentile scores of a numpy array?
    /// </summary>
    procedure Exercise31();
    /// <summary>
    ///   32. How to insert values at random positions in an array?
    /// </summary>
    procedure Exercise32();
    /// <summary>
    ///   33. How to find the position of missing values in numpy array?
    /// </summary>
    procedure Exercise33();
    /// <summary>
    ///   34. How to filter a numpy array based on two or more conditions?
    /// </summary>
    procedure Exercise34();
  end;

var
  Form2: TForm2;

implementation

uses
  WrapDelphi, VarPyth, PyUtils;

{$R *.fmx}

const
  Eval: function(const APythonExpression: AnsiString): Variant = VarPythonEval;

procedure TForm2.Exercise1;
begin
  bm.print('Q. Import numpy as np and print the version number.');
  with NumPy1 do begin
    bm.print(np.__version__);
  end;
  bm.print('');
end;

procedure TForm2.Exercise2;
begin
  bm.print('Q. Create a 1D array of numbers from 0 to 9');
  with NumPy1 do begin
    bm.print(np.arange(10));
  end;
  bm.print('');
end;

procedure TForm2.Exercise3;
begin
  bm.print('Q. Create a 3×3 numpy array of all True’s');
  with NumPy1 do begin
    bm.print(np.full(TPyEx.Tuple([3, 3]), true, dtype := bm.bool));
  end;
  bm.print('');
end;

procedure TForm2.Exercise4;
begin
  bm.print('Q. Extract all odd numbers from arr');
  with NumPy1 do begin
    var arr := np.array(TPyEx.Tuple([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    mm.arr := arr;
    bm.print('by Python eval');
    bm.print(Eval('arr[arr %2 == 1]'));
    bm.print('by Delphi');
    var list := TPyEx.List([]);
    for var val in arr.GetEnumerator() do begin
      if Odd(VarToStr(val).ToInteger()) then
        list.append(VarToStr(val).ToInteger());
    end;
    bm.print(list);
  end;
  bm.print('');
end;

procedure TForm2.Exercise5;
begin
  bm.print('Q. Replace all odd numbers in arr with -1');
  with NumPy1, PythonEngine do begin
    var arr := np.array(TPyEx.Tuple([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    mm.arr := arr;
    bm.print('by Python eval');
    ExecString(AnsiString('arr[arr % 2 == 1] = -1'));
    bm.print(mm.arr);
    bm.print('by Delphi');
    arr := np.array(TPyEx.Tuple([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    var ix := 0;
    for var val in arr.GetEnumerator() do begin
      if Odd(VarToStr(val).ToInteger()) then
        arr.Values[ix] := -1;
      Inc(ix);
    end;
    bm.print(arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise6;
begin
  bm.print('Q. Replace all odd numbers in arr with -1 without changing arr');
  with NumPy1 do begin
    var arr := np.array(TPyEx.Tuple([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    arr := np.arange(10);
    mm.arr := arr;
    bm.print('by Python eval');
    var &out := np.where(Eval('arr % 2 == 1'), -1, arr);
    bm.print(arr);
    bm.print(&out);
    bm.print('by Delphi');
    arr := np.array(TPyEx.Tuple([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    arr := np.arange(10);
    &out := np.where(op.eq(op.mod(arr, 2), 1), -1, arr);
    bm.print(arr);
    bm.print(&out);
  end;
  bm.print('');
end;

procedure TForm2.Exercise7;
begin
  bm.print('Q. Convert a 1D array to a 2D array with 2 rows');
  with NumPy1 do begin
    var arr := np.arange(10);
    bm.print(arr.reshape(2, -1));
  end;
  bm.print('');
end;

procedure TForm2.Exercise8;
begin
  bm.print('Q. Stack arrays a and b vertically');
  with NumPy1 do begin
    var a := np.arange(10).reshape(2, -1);
    var b := np.repeat(1, 10).reshape(2, -1);
    bm.print('# Method 1:');
    bm.print(np.concatenate(TPyEx.Tuple([a, b]), axis := 0));
    bm.print('# Method 2:');
    bm.print(np.vstack(TPyEx.Tuple([a, b])));
    bm.print('# Method 3:');
    bm.print(np.r_[(TPyEx.Tuple([a, b]))]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise9;
begin
  bm.print('Q. Stack the arrays a and b horizontally.');
  with NumPy1 do begin
    var a := np.arange(10).reshape(2, -1);
    var b := np.repeat(1, 10).reshape(2, -1);
    bm.print('# Method 1:');
    bm.print(np.concatenate(TPyEx.Tuple([a, b]), axis := 1));
    bm.print('# Method 2:');
    bm.print(np.hstack(TPyEx.Tuple([a, b])));
    bm.print('# Method 3:');
    bm.print(np.c_[(TPyEx.Tuple([a, b]))]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise10;
begin
  bm.print('Q. Create the following pattern without hardcoding. Use only numpy functions and the below input array a.');
  with NumPy1 do begin
    var a := np.array(TPyEx.Tuple([1, 2, 3]));
    bm.print(np.r_[(TPyEx.Tuple([np.repeat(a, 3), np.tile(a, 3)]))]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise11;
begin
  bm.print('Q. Get the common items between a and b.');
  with NumPy1 do begin
    var a := np.array(TPyEx.List([1, 2, 3, 4, 5, 6]));
    var b := np.array(TPyEx.List([7, 2, 10, 2, 7, 4, 9, 4, 9, 8]));
    bm.print(np.intersect1d(a, b));
  end;
  bm.print('');
end;

procedure TForm2.Exercise12;
begin
  bm.print('Q. From array a remove all items present in array b.');
  with NumPy1 do begin
    var a := np.array(TPyEx.List([1, 2, 3, 4, 5]));
    var b := np.array(TPyEx.List([5, 6, 7, 8, 9]));
    bm.print(np.setdiff1d(a, b));
  end;
  bm.print('');
end;

procedure TForm2.Exercise13;
begin
  bm.print('Q. Get the positions where elements of a and b match.');
  with NumPy1 do begin
    var a := np.array(TPyEx.List([1, 2, 3, 2, 3, 4, 3, 4, 5, 6]));
    var b := np.array(TPyEx.List([7, 2, 10, 2, 7, 4, 9, 4, 9, 8]));
    bm.print(np.where(op.eq(a, b)));
  end;
  bm.print('');
end;

procedure TForm2.Exercise14;
begin
  bm.print('Q. Get all items between 5 and 10 from a.');
  with NumPy1 do begin
    var a := np.arange(15);
    bm.print('# Method 1:');
    //                           (a >= 5) & (a <= 10)
    var ix := np.where(op.and_(op.ge(a, 5), op.le(a, 10)));
    bm.print(a.Values[ix]);
    bm.print('# Method 2:');
    ix := np.where(np.logical_and(op.ge(a, 5), op.le(a, 10)));
    bm.print(a.Values[ix]);
    bm.print('# Method 3:');
    bm.print(a.Values[op.and_(op.ge(a, 5), op.le(a, 10))]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise15;
begin
  bm.print('Q. Convert the function maxx that works on two scalars, to work on two arrays.');
  //em.maxx or TPyEx.Closure
  with NumPy1 do begin
    //em.maxx
    with PythonModule1 do begin
      AddDelphiMethod('maxx', maxx, String.Empty);
      Initialize();
      try
        var em := VarPyth.Import('exercises');
        mm.pair_max := np.vectorize(em.maxx, otypes := TPyEx.List([bm.float]));
        var a := np.array(TPyEx.List([5, 7, 9, 8, 6, 4, 5]));
        var b := np.array(TPyEx.List([6, 3, 4, 8, 9, 7, 1]));
        bm.print(mm.pair_max(a, b)); //advice: add a break point at maxx method.
      finally
        Finalize();
      end;
    end;
    //or TPyEx.Closure
    mm.pair_max := np.vectorize(TPyEx.Closure(function(AArg: variant): variant begin
      if AArg.Values[0] > AArg.Values[1] then
        Result := AArg.Values[0]
      else
        Result := AArg.Values[1]
    end), otypes := TPyEx.List([bm.float]));
    var a := np.array(TPyEx.List([5, 7, 9, 8, 6, 4, 5]));
    var b := np.array(TPyEx.List([6, 3, 4, 8, 9, 7, 1]));
    bm.print(mm.pair_max(a, b)); //advice: add a break point at maxx method.
  end;
  bm.print('');
end;

procedure TForm2.Exercise16;
begin
  bm.print('Q. Swap columns 1 and 2 in the array arr.');
  with NumPy1 do begin
    mm.arr := np.arange(9).reshape(3, 3);
    var arr :=  mm.arr[(TPyEx.Tuple([Ellipsis(), TPyEx.List([1, 0, 2])]))]; //arr[:, [1, 0, 2]]
    bm.print(arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise17;
begin
  bm.print('Q. Swap rows 1 and 2 in the array arr:');
  with NumPy1 do begin
    mm.arr := np.arange(9).reshape(3, 3);
    var arr :=  mm.arr[(TPyEx.Tuple([TPyEx.List([1, 0, 2]), Ellipsis()]))]; //arr[[1, 0, 2], :]
    bm.print(arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise18;
begin
  bm.print('Q. Reverse the rows of a 2D array arr.');
  with NumPy1 do begin
    mm.arr := np.arange(9).reshape(3, 3);
    var arr :=  mm.arr[(TPyEx.Tuple([bm.slice(None(), None(), -1)]))]; //arr[::-1]
    bm.print(arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise19;
begin
  bm.print('Q. Reverse the columns of a 2D array arr.');
  with NumPy1 do begin
    mm.arr := np.arange(9).reshape(3, 3);
    var arr :=  mm.arr[(TPyEx.Tuple([Ellipsis(), bm.slice(None(), None(), -1)]))]; //arr[:, ::-1]
    bm.print(arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise20;
begin
  bm.print('Q. Create a 2D array of shape 5x3 to contain random decimal numbers between 5 and 10.');
  with NumPy1 do begin
    bm.print('# Method 1:');
    var rand_arr := np.random.randint(low := 5, high := 10, size := TPyEx.Tuple([5, 3])) + np.random.random(TPyEx.Tuple([5, 3]));
    bm.print(rand_arr);
    bm.print('# Method 2:');
    rand_arr := np.random.uniform(5, 10, size := TPyEx.Tuple([5, 3]));
    bm.print(rand_arr);
  end;
  bm.print('');
end;

procedure TForm2.Exercise21;
begin
  bm.print('Q. Print or show only 3 decimal places of the numpy array rand_arr.');
  with NumPy1 do begin
    mm.rand_arr := np.random.random(TPyEx.Tuple([5, 3]));
    //Limit to 3 decimal places
    var print_opts := np.get_printoptions();
    try
      np.set_printoptions(precision := 3);
      bm.print(mm.rand_arr[TPyEx.Tuple([bm.slice(None(), 4)])]); //rand_arr[:4]
    finally
      RestorePrintOpts(print_opts);
    end;
  end;
  bm.print('');
end;

procedure TForm2.Exercise22;
begin
  bm.print('Q. Pretty print rand_arr by suppressing the scientific notation (like 1e10)');
  with NumPy1 do begin
    //Reset printoptions to default
    var print_opts := np.get_printoptions();
    try
      np.set_printoptions(suppress := false);
      np.random.seed(100);
      var rand_arr := np.random.random(TPyEx.List([3, 3])); //1e3
      bm.print(rand_arr);
      np.set_printoptions(suppress := true, precision := 6);
      bm.print(rand_arr);
    finally
      RestorePrintOpts(print_opts);
    end;
  end;
  bm.print('');
end;

procedure TForm2.Exercise23;
begin
  bm.print('Q. Limit the number of items printed in python numpy array a to a maximum of 6 elements.');
  with NumPy1 do begin
    var print_opts := np.get_printoptions();
    try
      np.set_printoptions(threshold := 6);
      bm.print(np.arange(15));
    finally
      RestorePrintOpts(print_opts);
    end;
  end;
  bm.print('');
end;

procedure TForm2.Exercise24;
begin
  bm.print('Q. Print the full numpy array a without truncating.');
  with NumPy1 do begin
    var print_opts := np.get_printoptions();
    try
      np.set_printoptions(threshold := 6);
      var a := np.arange(15);
      np.set_printoptions(threshold := SysModule.maxsize);
      bm.print(a);
    finally
      RestorePrintOpts(print_opts);
    end;
  end;
  bm.print('');
end;

procedure TForm2.Exercise25;
begin
  bm.print('Q. Import the iris dataset keeping the text intact.');
  with NumPy1 do begin
    np.set_printoptions(precision := 3);
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris := np.genfromtxt(url, delimiter := ',', dtype := 'object');
    var names := TPyEx.Tuple(['sepallength', 'sepalwidth', 'petallength', 'petalwidth', 'species']);
    //Print first 3 rows
    bm.print(mm.iris[TPyEx.Tuple([bm.slice(None(), 3)])]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise26;
begin
  bm.print('Q. Extract the text column species from the 1D iris imported in previous question.');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    var iris_1d := np.genfromtxt(url, delimiter := ',', dtype := None());
    mm.iris_1d := iris_1d;
    bm.print(mm.iris_1d.shape);
    bm.print('by Python eval');
    mm.species := np.array(Eval('[row[4] for row in iris_1d]'));
    bm.print(mm.species[TPyEx.Tuple([bm.slice(None(), 5)])]);
    bm.print('by Delphil');
    var list := TPyEx.List([]);
    for var row in iris_1d.GetEnumerator() do
    begin
      list.append(row.Values[4])
    end;
    mm.species := np.array(list);
    bm.print(mm.species[TPyEx.Tuple([bm.slice(None(), 5)])]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise27;
begin
  bm.print('Q. Convert the 1D iris to 2D array iris_2d by omitting the species text field.');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris_1d := np.genfromtxt(url, delimiter := ',', dtype := None());

    bm.print('# Method 1: Convert each row to a list and get the first 4 items');
    //by Python eval
    mm.iris_2d := np.array(Eval('[row.tolist()[:4] for row in iris_1d]'));
    //or even in Delphi    
    var list := TPyEx.List([]);
    for var row in mm.iris_1d.GetEnumerator() do
    begin
      list.append(row.tolist().GetSlice(Ellipsis, 4));
    end;
    //Compare PythonEval result and Delphi generated result  
    Assert(VarToStr(np.array_equal(mm.iris_2d, np.array(list))).ToBoolean(), 'Expressions results are not equal.');
    
    bm.print(mm.iris_2d[TPyEx.Tuple([bm.slice(None(), 4)])]);
    bm.print('# Method 2: Import only the first 4 columns from source url');
    mm.iris_2d := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0, 1, 2, 3]));
    bm.print(mm.iris_2d[TPyEx.Tuple([bm.slice(None(), 4)])]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise28;
begin
  bm.print('Q. Find the mean, median, standard deviation of iris''s sepallength (1st column)');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris := np.genfromtxt(url, delimiter := ',', dtype := 'object');
    var sepal_length := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0]));
    var mu := np.mean(sepal_length);
    var med := np.median(sepal_length);
    var sd := np.std(sepal_length);
    bm.print(mu, med, sd);
  end;
  bm.print('');
end;

procedure TForm2.Exercise29;
begin
  bm.print('Q. Create a normalized form of iris''s sepallength whose values range exactly between 0 and 1 so that the minimum has value 0 and maximum has value 1.');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    var sepal_length := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0]));
    var smax := sepal_length.max();
    var smin := sepal_length.min();
    var s := (sepal_length - smin) / (smax - smin);
    //or
    s := (sepal_length - smin) / sepal_length.ptp();
    bm.print(s);
  end;
  bm.print('');
end;

procedure TForm2.Exercise30;
begin
  bm.print('Q. Compute the softmax score of sepallength.');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris := np.genfromtxt(url, delimiter := ',', dtype := 'object');
    //by Python eval
    var sepal_length := np.array(Eval('[float(row[0]) for row in iris]'));
    //or even in Delphi
    var list := TPyEx.List([]);
    for var row in mm.iris.GetEnumerator() do begin
      list.append(bm.float(row.Values[0]));  
    end;
    //Compare PythonEval result and Delphi generated result  
    Assert(VarToStr(np.array_equal(sepal_length, np.array(list))).ToBoolean(), 'Expressions results are not equal.');
     
    var softmax := function(const AValue: variant): variant
    begin
      var e_x := np.exp(AValue - np.max(AValue));
      Result := e_x / e_x.sum(axis := 0);
    end;
    bm.print(softmax(sepal_length));
  end;
  bm.print('');
end;

procedure TForm2.Exercise31;
begin
  bm.print('Q. Find the 5th and 95th percentile of iris''s sepallength');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    var sepal_length := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0]));
    bm.print(np.percentile(sepal_length, q := TPyEx.List([5, 95])));
  end;
  bm.print('');
end;

procedure TForm2.Exercise32;
begin
  bm.print('Q. Insert np.nan values at 20 random positions in iris_2d dataset');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris_2d := np.genfromtxt(url, delimiter := ',', dtype := 'object');
    mm.npNAN := np.NAN;
    
    bm.print('# Method 1:');
    var resp := np.where(mm.iris_2d); //Response with many results (tuple) e.g. i, j = np.where(iris_2d)
    var i := resp.Values[0];
    var j := resp.Values[1];
    
    np.random.seed(100);
    mm.c1 := np.random.choice(i, 20);
    mm.c2 := np.random.choice(j, 20);

    //by Python execstr     
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');      
    
    bm.print('# Method 2:');
    np.random.seed(100);
    mm.c1 := np.random.randint(150, size := 20);
    mm.c2 := np.random.randint(4, size := 20);
    //by Python execstr     
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');
    bm.print(mm.iris_2d[TPyEx.Tuple([bm.slice(None(), 10)])]);
  end;
  bm.print('');
end;

procedure TForm2.Exercise33;
begin
  bm.print('Q. Find the number and position of missing values in iris_2d''s sepallength (1st column)');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris_2d := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0,1,2,3]));
    mm.c1 := np.random.randint(150, size := 20);
    mm.c2 := np.random.randint(4, size := 20);
    PythonEngine.ExecString('iris_2d[c1, c2] = npNAN');
    bm.print('Number of missing values: \n', np.isnan(mm.iris_2d[TPyEx.Tuple([Ellipsis(), 0])]).sum());
    bm.print('Position of missing values: \n', np.where(np.isnan(mm.iris_2d[TPyEx.Tuple([Ellipsis(), 0])])));
  end;
  bm.print('');
end;

procedure TForm2.Exercise34;
begin
  bm.print('Q. Filter the rows of iris_2d that has petallength (3rd column) > 1.5 and sepallength (1st column) < 5.0');
  with NumPy1 do begin
    var url := 'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data';
    mm.iris_2d := np.genfromtxt(url, delimiter := ',', dtype := 'float', usecols := TPyEx.List([0,1,2,3]));
    mm.c1 := mm.iris_2d[TPyEx.Tuple([Ellipsis(), 2])];
    //             c1 > 1.5
    mm.c1 := op.gt(mm.c1, 1.5);
    mm.c2 := mm.iris_2d[TPyEx.Tuple([Ellipsis(), 0])];
    //             c2 < 5.0 
    mm.c2 := op.lt(mm.c2, 5.0);
    //                             c1 & c2
    bm.print(mm.iris_2d[op.and_(mm.c1, mm.c2)]);
  end;
  bm.print('');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  bm := BuiltinModule;
  mm := MainModule;
  op := Import('operator');
  Exercise1();
  Exercise2();
  Exercise3();
  Exercise4();
  Exercise5();
  Exercise6();
  Exercise7();
  Exercise8();
  Exercise9();
  Exercise10();
  Exercise11();
  Exercise12();
  Exercise13();
  Exercise14();
  Exercise15();
  Exercise16();
  Exercise17();
  Exercise18();
  Exercise19();
  Exercise20();
  Exercise21();
  Exercise22();
  Exercise23();
  Exercise24();
  Exercise25();
  Exercise26();
  Exercise27();
  Exercise28();
  Exercise29();
  Exercise30();
  Exercise31();
  Exercise32();
  Exercise33();
  Exercise34();
end;

function TForm2.maxx(ASelf, AArgs: PPyObject): PPyObject;
var
  LX, LY: integer;
begin
  with NumPy1.PythonEngine do begin
    if PyArg_ParseTuple(AArgs, 'ii:maxx', @LX, @LY) <> 0 then
      begin
        if (LX >= LY) then
          Result := PyLong_FromLong(LX)
        else
          Result := PyLong_FromLong(LY);
      end
    else
      Result := nil;
  end;
end;

procedure TForm2.RestorePrintOpts(const APrintOpts: variant);
begin
  with PythonEngine1 do begin
    var LArgs := PyTuple_New(0);
    try
      //simulates a keyword argument
      //np.set_printoptions(**LPrintOpts)
      PyEval_CallObjectWithKeywords(
        ExtractPythonObjectFrom(NumPy1.np.set_printoptions),
        LArgs,
        ExtractPythonObjectFrom(APrintOpts));
    finally
      Py_XDecRef(LArgs)
    end;
  end;
end;

end.
