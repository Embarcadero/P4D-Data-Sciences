(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit MatplotLib;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TMatplotLib = class(TPyManagedPackage)
  private
    function GetPyPlot: variant;
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property matplot: variant read AsVariant;
    property plt: variant read GetPyPlot;
  end;

implementation

uses
  PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip,
  VarPyth, System.Variants;

{ TMatplotLib }

function TMatplotLib.AsVariant: variant;
begin
  Result := inherited;
end;

function TMatplotLib.GetPyPlot: variant;
begin
  Result := PyModule['pyplot'].AsVariant();
end;

procedure TMatplotLib.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TMatplotLib.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'matplotlib';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('matplotlib'));
  end;
end;

end.
