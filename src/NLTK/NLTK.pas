unit NLTK;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TNLTK = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
  public
    property nltk: variant read AsVariant;
  end;

implementation

uses
  PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip;

{ TNLTK }

function TNLTK.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TNLTK.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'nltk';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('nltk'));
  end;
end;

end.
