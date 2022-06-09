(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit SineWave;

interface

uses
  NumPy, PyTorch;

type
  TSineWave = class
  public
    class procedure Generate(const ANumPy: TNumPy; const APyTorch: TPyTorch);
  end;

implementation

uses
  PyUtils, VarPyth;

{ TSineWave }

class procedure TSineWave.Generate(const ANumPy: TNumPy;
  const APyTorch: TPyTorch);
begin
  var np := ANumPy.np;
  var torch := APyTorch.torch;
  var bm := BuiltinModule;

  np.random.seed(2);

  const T = 20;
  const L = 1000;
  const N = 100;

  var x := np.empty(TPyEx.Tuple([N, L]), 'int64');
  var r := np.array(bm.range(L)) + np.random.randint(- 4 * T, 4 * T, N).reshape(N, 1);
  x.SetSlice(Ellipsis(), Ellipsis(), r); //x[:] :=
  var data := np.sin(x / 1.0 / T).astype('float64');
  torch.save(data, bm.open('traindata.pt', 'wb'));
end;

end.
