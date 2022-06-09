unit TorchVision;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TTorchVision = class(TPyManagedPackage)
  private
    function AsVariant: variant;
   protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property torchvision: variant read AsVariant;
  end;

implementation

uses
  PyPackage.Manager.ManagerKind, PyPackage.Manager.pip;

{ TTorchVision }

function TTorchVision.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TTorchVision.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TTorchVision.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'torchvision';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('torchvision'));
  end;
end;

end.
