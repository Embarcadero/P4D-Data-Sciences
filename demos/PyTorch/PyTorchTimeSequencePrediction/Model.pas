(**************************************************************************)
(*                                                                        *)
(* Module:  Unit 'Model'            Copyright (c) 2021                    *)
(*                                                                        *)
(*                                  Lucas Moura Belo - lmbelo             *)
(*                                  lucas.belo@live.com                   *)
(*                                  Brazil                                *)
(*                                                                        *)
(*  Project page:                   https://github.com/lmbelo/P4D_AI_ML   *)
(**************************************************************************)
(*  Functionality:  Time Sequence Prediction Model                        *)
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
unit Model;

interface

uses
  NumPy, MatplotLib, PyTorch, System.SysUtils;

type
  TModel = class
  public
    class procedure Train(const ANumPy: TNumPy; const AMatplotLib: TMatplotLib;
      const APyTorch: TPyTorch);
  end;

implementation

uses
  System.Variants, PythonEngine, VarPyth, PyUtils;

{ TModel }

class procedure TModel.Train(const ANumPy: TNumPy;
  const AMatplotLib: TMatplotLib; const APyTorch: TPyTorch);
begin
  var bm := BuiltinModule;
  var torch := APyTorch.torch;
  var nn := torch.nn;
  var optim := torch.optim;
  var np := ANumPy.np;
  AMatplotLib.matplot.use('Agg');
  var plt := AMatplotLib.plt;
  var sequence := Import('sequence');

  var argparse := Import('argparse');
  var parser := argparse.ArgumentParser();
  parser.add_argument('--steps', type := bm.int, default := 15, help := 'steps to run');
  var opt := parser.parse_args();

  np.random.seed(0);
  torch.manual_seed(0);

  //load data and make training set
  var data := torch.load('traindata.pt');
  var input := torch.from_numpy(data.GetItem(TPyEx.Tuple([bm.slice(3, None()), bm.slice(None(), -1)]))); //data[3:, :-1]
  var target := torch.from_numpy(data.GetItem(TPyEx.Tuple([bm.slice(3, None()), bm.slice(1, None())]))); //data[3:, 1:]
  var test_input := torch.from_numpy(data.GetItem(TPyEx.Tuple([bm.slice(None(), 3), bm.slice(None(), -1)]))); //data[:3, :-1]
  var test_target := torch.from_numpy(data.GetItem(TPyEx.Tuple([bm.slice(None(), 3), bm.slice(1, None())]))); //data[:3, 1:]

  //build the model
  var seq := sequence.Sequence();
  seq.double();
  var criterion := nn.MSELoss();
  //use LBFGS as optimizer sine we can load the whole data to train
  var optimizer := optim.LBFGS(seq.parameters(), lr := 0.8);
  //begin to train
  for var I in bm.range(opt.steps).GetEnumerator() do begin
    bm.print('STEP: ', I); 
    
    var closure: TypeClosure := function(AParams: variant): variant begin    
      optimizer.zero_grad();
      var out := seq.__call__(input);
      var loss := criterion.__call__(out, target);
      bm.print('loss: ', loss.item());
      loss.backward();
      result := loss;
    end;

    optimizer.step(TPyEx.Closure(closure));

    //begin to predict, no need to track gradient here
    const future = 1000;
    var y: variant;
    var no_grad := torch.no_grad();
    try
      no_grad.__enter__();      
      var pred := seq.__call__(test_input, future := future);
      var loss := criterion.__call__(pred.GetItem(TPyEx.Tuple([Ellipsis(), bm.slice(None(),-future)])), test_target); //pred[:, :-future]
      bm.print('test loss: ', loss.item());
      y := pred.detach().numpy();
    finally
      no_grad.__exit__(None(), None(), None());
    end;
    
    plt.figure(figsize := TPyEx.Tuple([30, 10]));
    plt.title('Predict future values for time sequences' + #10 + '(Dashlines are predicted values)', fontsize := 30);
    plt.xlabel('x', fontsize := 20);
    plt.ylabel('y', fontsize := 20);
    plt.xticks(fontsize := 20);
    plt.yticks(fontsize := 20);

    var draw := procedure(yi, color: variant) begin
      plt.plot(np.arange(input.size(1)), yi.GetItem(TPyEx.Tuple([bm.slice(None, input.size(1))])), color, linewidth := 2.0);
      var x := np.arange(input.size(1), input.size(1) + future);
      var y := yi.GetSlice(input.size(1), Ellipsis());
      plt.plot(x, y, color + ':', linewidth := 2.0);
    end;
    
    draw(y.GetItem(0), 'r');
    draw(y.GetItem(1), 'g');
    draw(y.GetItem(2), 'b'); 
    plt.savefig(Format('predict%d.pdf', [VarToStr(I).ToInteger()]));
    plt.close();
  end;
end;

end.
