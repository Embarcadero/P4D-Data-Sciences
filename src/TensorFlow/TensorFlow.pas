unit TensorFlow;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TTensorFlow = class(TPyManagedPackage)
  private
    function GetKeras: variant;
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
  public
    property tf: variant read AsVariant;
    property keras: variant read GetKeras;
  end;

implementation

uses
  PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip;

{ TTensorFlow }

function TTensorFlow.AsVariant: variant;
begin
  Result := inherited;
end;

function TTensorFlow.GetKeras: variant;
begin
  Result := PyModule['keras'].AsVariant();
end;

procedure TTensorFlow.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'tensorflow';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('tensorflow'));
  end;
end;

end.
