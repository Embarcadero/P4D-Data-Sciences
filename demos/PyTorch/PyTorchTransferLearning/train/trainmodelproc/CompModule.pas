unit CompModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, TorchVision, PyTorch, PyCommon,
  PyModule, PyPackage, NumPy;

type
  TPyComps = class(TDataModule)
    PyEngine: TPythonEngine;
    PyNumPy: TNumPy;
    PyTorch: TPyTorch;
    PyTorchVision: TTorchVision;
    PyIO: TPythonInputOutput;
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOReceiveData(Sender: TObject; var Data: AnsiString);
  private
    FOutMonitor: TProc<string>;
    FInMonitor: TProc<string>;
  public
    constructor Create(AOwner: TComponent; const AIn, AOut: TProc<string>); reintroduce; overload;

    procedure CheckEngine();
    procedure Monitor(const AIn, AOut: TProc<string>);
  end;

var
  PyComps: TPyComps;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TPyComps.CheckEngine;
begin
  if not PyEngine.Initialized and not PyEngine.IsHandleValid() then
    raise EDLLLoadError.Create('The Python interpreter could not be loaded.');
end;

constructor TPyComps.Create(AOwner: TComponent; const AIn, AOut: TProc<string>);
begin
  Monitor(AIn, AOut);
  inherited Create(AOwner);
end;

procedure TPyComps.Monitor(const AIn, AOut: TProc<string>);
begin
  FInMonitor := AIn;
  FOutMonitor := AOut;
end;

procedure TPyComps.PyIOReceiveData(Sender: TObject; var Data: AnsiString);
begin
  if Assigned(FInMonitor) then
    FInMonitor(String(Data));
end;

procedure TPyComps.PyIOSendData(Sender: TObject; const Data: AnsiString);
begin
  if Assigned(FOutMonitor) then
    FOutMonitor(String(Data));
end;

end.
