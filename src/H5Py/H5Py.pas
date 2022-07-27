(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit H5Py;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TH5Py = class(TPyManagedPackage)
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

{ TH5Py }

function TH5Py.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TH5Py.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TH5Py.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'h5py';
    //NumPy from PIP
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('h5py'));
    //NumPy from Conda
    PackageManagers.Add(
      TPyPackageManagerKind.conda,
      TPyPackageManagerConda.Create('h5py'));
  end;
end;

end.
