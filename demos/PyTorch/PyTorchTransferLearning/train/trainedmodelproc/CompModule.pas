unit CompModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, PyTorch, PyCommon, PyModule,
  PyPackage, TorchVision, NumPy, OpenCV, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39, PyEnvironment.Embeddable.Res.Python38;

type
  TPyComps = class(TDataModule)
    PyEngine: TPythonEngine;
    PyIO: TPythonInputOutput;
    PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38;
    PyNumPy: TNumPy;
    PyOpenCV: TOpenCV;
    PyTorch: TPyTorch;
    PyTorchVision: TTorchVision;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PyComps: TPyComps;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
