(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit ScikitLearn;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TScikitLearn = class(TPyManagedPackage)
  private
    function GetTree: variant;
    function GetDataSets: variant;
    function GetDecomposition: variant;
    function GetMetrics: variant;
    function GetModelSelection: variant;
    function GetSvm: variant;
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    property sklearn: variant read AsVariant;
    property tree: variant read GetTree;
    property model_selection: variant read GetModelSelection;
    property datasets: variant read GetDataSets;
    property metrics: variant read GetMetrics;
    property decomposition: variant read GetDecomposition;
    property svm: variant read GetSvm;
  end;

implementation

uses
  System.Variants,
  PyPackage.Manager.ManagerKind, PyPackage.Manager.Pip;

{ TScikitLearn }

function TScikitLearn.AsVariant: variant;
begin
  Result := inherited;
end;

function TScikitLearn.GetDataSets: variant;
begin
  Result := PyModule['datasets'].AsVariant();
end;

function TScikitLearn.GetDecomposition: variant;
begin
  Result := PyModule['decomposition'].AsVariant();
end;

function TScikitLearn.GetMetrics: variant;
begin
  Result := PyModule['metrics'].AsVariant();
end;

function TScikitLearn.GetModelSelection: variant;
begin
  Result := PyModule['model_selection'].AsVariant();
end;

function TScikitLearn.GetSvm: variant;
begin
  Result := PyModule['svm'].AsVariant();
end;

function TScikitLearn.GetTree: variant;
begin
  Result := PyModule['tree'].AsVariant();
end;

procedure TScikitLearn.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TScikitLearn.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageName := 'sklearn';
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create('scikit-learn'));
  end;
end;

end.
