(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit CompModule;

interface

uses
  System.SysUtils, System.Classes, PythonEngine, PyTorch, PyCommon, PyModule,
  PyPackage, TorchVision, NumPy, OpenCV, PyEnvironment,
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39, PyEnvironment.Embeddable.Res.Python38;

type
  TPyComps = class(TDataModule)
    PyEngine: TPythonEngine;
    PyIO: TPythonInputOutput;
    PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38;
    PyNumPy: TNumPy;
    PyOpenCV: TOpenCV;
    PyTorch: TPyTorch;
    PyTorchVision: TTorchVision;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PyComps: TPyComps;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

end.
