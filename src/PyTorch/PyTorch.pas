(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit PyTorch;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TPyTorch = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property torch: variant read AsVariant;
  end;

implementation

uses
  System.Variants, PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip;

{ TPyTorch }

procedure TPyTorch.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'torch';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('torch'));
      //torch is available as pytorch under conda
//    PackageManagers.Add(
//      TPyPackageManagerType.conda,
//      TPyPackageManagerConda.Create('pytorch'));
  end;
end;

function TPyTorch.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TPyTorch.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

end.
