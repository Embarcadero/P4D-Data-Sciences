(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit TrainModel;

interface

type
  TTrainModel = class
  private
    function GetPythonExePath: string;
  private
    function CreateDataSet(const AImageFolder: string): variant;
    procedure SplitDataSetTrainAndTest(const ADataSet: variant;
      out ATrainDataSet, ATestDataSet: variant);
    procedure CreateDataLoaders(const ATrainDataSet, ATestDataSet: variant;
      out ATrainLoader, ATestLoader: variant);
    procedure DefineNeuralNetwork(out ADevice, AModel: variant);
    procedure TrainNeuralNetwork(const ATrainLoader, ATestLoader,
      ATrainDataSet, ATestDataSet, ADevice, AModel: variant;
      const ATrainedModelPath: string);
  public
    procedure Train(const ADataSetPath, ATrainedModelPath: string);
  end;

implementation

uses
  System.SysUtils, System.StrUtils, System.IOUtils, System.Variants,
  PythonEngine, VarPyth,
  PyUtils,
  CompModule;

type
  TDynamicDllCrack = class(TDynamicDll)
  end;

{ TTrainModel }

function TTrainModel.GetPythonExePath: string;
begin
  {$IFDEF MSWINDOWS}
  Result := TPath.Combine(
    TDynamicDllCrack(PyComps.PyEngine).GetDllPath(),
    'python.exe'
  );
  {$ELSE}
    raise ENotImplemented.Create('Not implemented');
  {$ENDIF}
end;

function TTrainModel.CreateDataSet(const AImageFolder: string): variant;
begin
  BuiltinModule.print('Creating the dataset');
  with PyComps.PyTorchVision do begin
    var datasets := torchvision.datasets;
    var transforms := torchvision.transforms;

    Result := datasets.ImageFolder(
      AImageFolder,
      transforms.Compose(TPyEx.List([
        transforms.ColorJitter(0.1, 0.1, 0.1, 0.1),
        transforms.Resize(TPyEx.Tuple([224, 224])),
        transforms.ToTensor(),
        transforms.Normalize(TPyEx.List([0.485, 0.456, 0.406]), TPyEx.List([0.229, 0.224, 0.225]))
      ]))
    );
  end;
end;

procedure TTrainModel.SplitDataSetTrainAndTest(const ADataSet: variant;
  out ATrainDataSet, ATestDataSet: variant);
begin
  BuiltinModule.print('Spliting train and test dataset');
  with PyComps.PyTorch do begin
    var tuple := torch.utils.data.random_split(ADataSet,
      TPyEx.List([ADataSet.length - 50, 50]));
    ATrainDataSet := tuple.GetItem(0);
    ATestDataSet := tuple.GetItem(1);
  end;
end;

procedure TTrainModel.CreateDataLoaders(const ATrainDataSet,
  ATestDataSet: variant; out ATrainLoader, ATestLoader: variant);
begin
  BuiltinModule.print('Defining loaders');
  with PyComps.PyTorch do begin
    ATrainLoader := torch.utils.data.DataLoader(
      ATrainDataSet,
      batch_size := 16,
      shuffle := true,
      num_workers := 4
    );

    ATestLoader := torch.utils.data.DataLoader(
      ATestDataSet,
      batch_size := 16,
      shuffle := true,
      num_workers := 4
    );
  end;
end;

procedure TTrainModel.DefineNeuralNetwork(out ADevice, AModel: variant);
begin
  BuiltinModule.print('Defining the neural network');
  with PyComps.PyTorch, PyComps.PyTorchVision do begin
    var models := torchvision.models;
    var model := models.alexnet(pretrained := true);
    model.classifier.SetItem(6, torch.nn.Linear(model.classifier.GetItem(6).in_features, 2));
    //cuda is only available for a portion of NVIDIA graphic cards.
    //It powers up the model training, reducing its time.
    var device_string := IfThen(torch.cuda.is_available(), 'cuda', 'cpu');
    BuiltinModule.print('Device: ', device_string);
    ADevice := torch.device(device_string);
    AModel := model.to(ADevice);
  end;
end;

procedure TTrainModel.TrainNeuralNetwork(const ATrainLoader, ATestLoader,
  ATrainDataSet, ATestDataSet, ADevice, AModel: variant;
  const ATrainedModelPath: string);
const
  NUM_EPOCHS = 30;
begin
  var best_accuracy := 0.0;
  with PyComps.PyTorch do begin
    var op := VarPyth.Import('operator');
    var F := torch.nn.functional;
    var optimizer := torch.optim.SGD(AModel.parameters(), lr := 0.001, momentum := 0.9);

    var bm := BuiltinModule();

    bm.print('Running epochs');
    for var epoch in bm.range(NUM_EPOCHS).GetEnumerator() do begin
      bm.print('Epoch: ', epoch);
      //seting up the new process to launch the python app instead of this one (PyTorchThumbsUpDown),
      //so we're able to spawn to the new process.
      //note: this is only required when num_workers > 0 in the DataLoader.
      var spawn := VarPyth.Import('multiprocessing.spawn');
      spawn.set_executable(GetPythonExePath());
      //the iter method triggered here by the GetEnumerator will create new workers, defined in the DataLoader.
      var LBatchNum := 1;
      for var tl_tuple in ATrainLoader.GetEnumerator() do begin
        bm.print('Train batch: ' + LBatchNum.ToString());
        var images := tl_tuple.GetItem(0);
        var labels := tl_tuple.GetItem(1);

        images := images.to(ADevice);
        labels := labels.to(ADevice);

        optimizer.zero_grad();
        var outputs := AModel.__call__(images);
        var loss: variant;
        TPyEx.ExecuteMasked(procedure() begin
          loss := F.cross_entropy(outputs, labels);
        end);
        loss.backward();
        optimizer.step();

        Inc(LBatchNum);
      end;

      LBatchNum := 1;
      var test_error_count := 0.0;
      for var tl_tuple in ATestLoader.GetEnumerator() do begin
        bm.print('Test batch: ' + LBatchNum.ToString());
        var images := tl_tuple.GetItem(0);
        var labels := tl_tuple.GetItem(1);

        images := images.to(ADevice);
        labels := labels.to(ADevice);

        var outputs := AModel.__call__(images);
        test_error_count := test_error_count - bm.float((torch.sum(torch.abs(labels - outputs.argmax(1)))));
        Inc(LBatchNum);
      end;

      var test_accuracy := 1.0 - (bm.float(test_error_count) / bm.float(ATestDataSet.length));
      bm.print(op.mod('Accuracy: %d: %f', TPyEx.Tuple([epoch, test_accuracy])));
      if (test_accuracy > best_accuracy) then begin
        torch.save(AModel.state_dict(), ATrainedModelPath);
        best_accuracy := test_accuracy;
      end;
    end;
  end;
end;

procedure TTrainModel.Train(const ADataSetPath, ATrainedModelPath: string);
begin
  var train_dataset: variant;
  var test_dataset: variant;
  var train_loader: variant;
  var test_loader: variant;
  var device: variant;
  var model: variant;

  var dataset := CreateDataSet(ADataSetPath);
  SplitDataSetTrainAndTest(dataset, train_dataset, test_dataset);
  CreateDataLoaders(train_dataset, test_dataset, train_loader, test_loader);
  DefineNeuralNetwork(device, model);
  TrainNeuralNetwork(train_loader, test_loader, train_dataset, test_dataset,
    device, model, ATrainedModelPath);
end;


end.
