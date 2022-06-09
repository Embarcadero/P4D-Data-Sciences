(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit LoadModel;

interface

uses
  CompModule;

type
  TLoadModel = class
  private
    FModel: variant;
    FDevice: variant;
    FNormalize: variant;
    function PreProcess(const AImagePath: string): variant;
  public
    constructor Create(const AModelPath: string);
    function ProbabilisticClassification(const AImagePath: string): extended;
  end;

implementation

uses
  System.StrUtils, VarPyth, PyUtils;

{ TLoadModel }

constructor TLoadModel.Create(const AModelPath: string);
begin
  with PyComps.PyTorch, PyComps.PyTorchVision, PyComps.PyNumPy do begin
    FModel := torchvision.models.alexnet(pretrained := false);
    var linear := torch.nn.Linear(FModel.classifier.GetItem(6).in_features, 2);
    FModel.classifier.SetItem(6, linear);
    FModel.load_state_dict(torch.load(AModelPath));

    var device_string := IfThen(torch.cuda.is_available(), 'cuda', 'cpu');
    FDevice := torch.device(device_string);
    FModel := FModel.to(Fdevice);

    var mean := 255.0 * np.array(TPyEx.List([0.485, 0.456, 0.406]));
    var stdev := 255.0 * np.array(TPyEx.List([0.229, 0.224, 0.225]));
    FNormalize := torchvision.transforms.Normalize(mean, stdev);
  end;  
end;

function TLoadModel.PreProcess(const AImagePath: string): variant;
begin            
  with PyComps.PyTorch, PyComps.PyTorchVision, PyComps.PyNumPy, PyComps.PyOpenCV do begin
    var x := cv2.imread(AImagePath, 0);
    x := cv2.resize(x, TPyEx.Tuple([224, 224]));
    x := cv2.cvtColor(x, cv2.COLOR_BGR2RGB);
    x := x.transpose(TPyEx.Tuple([2, 0, 1]));
    x := torch.from_numpy(x).float();
    x := FNormalize.__call__(x);
    x := x.to(FDevice);
    x := x.GetItem(TPyEx.List([None, Ellipsis]));
    Result := x;
  end;
end;

function TLoadModel.ProbabilisticClassification(
  const AImagePath: string): extended;
begin
  with PyComps.PyTorch, PyComps.PyTorchVision, PyComps.PyNumPy do begin
    var F := torch.nn.functional;
    var x := PreProcess(AImagePath);
    var y := FModel.__call__(x);
    y := F.softmax(y, dim := 1);
    Result := BuiltinModule.float(y.flatten().GetItem(0));
  end;
end;

end.
