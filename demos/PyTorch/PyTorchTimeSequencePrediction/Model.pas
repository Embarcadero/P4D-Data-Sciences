(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


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
