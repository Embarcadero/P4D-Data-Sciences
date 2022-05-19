(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'SineWave'         Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  Generate data trainning data                          *)
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
