(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit SciPy;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TSciPy = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property np: variant read AsVariant;
  end;

implementation

uses
  System.Variants,
  PyPackage.Manager.ManagerKind,
  PyPackage.Manager.Pip,
  PyPackage.Manager.Conda;

{ TSciPy }

function TSciPy.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TSciPy.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TSciPy.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'scipy';
    //NumPy from PIP
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('scipy'));
    //NumPy from Conda
    PackageManagers.Add(
      TPyPackageManagerKind.conda,
      TPyPackageManagerConda.Create('scipy'));
  end;
end;

end.
