unit CompModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, PyTorch, PyCommon, PyModule,
  PyPackage, TorchVision, NumPy, OpenCV;

type
  TPyComps = class(TDataModule)
    PyEngine: TPythonEngine;
    PyIO: TPythonInputOutput;
    PyTorchVision: TTorchVision;
    PyTorch: TPyTorch;
    PyNumPy: TNumPy;
    PyOpenCV: TOpenCV;
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
