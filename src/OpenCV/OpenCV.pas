unit OpenCV;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TOpenCV = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
  public
    property cv2: variant read AsVariant;
  end;

implementation

uses
  PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip;

{ TOpenCV }

function TOpenCV.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TOpenCV.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'cv2';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('opencv-python'));
  end;
end;

end.
