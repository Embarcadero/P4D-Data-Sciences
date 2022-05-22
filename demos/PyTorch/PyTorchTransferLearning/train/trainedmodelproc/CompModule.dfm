object PyComps: TPyComps
  Height = 267
  Width = 389
  object PyEngine: TPythonEngine
    AutoLoad = False
    DllName = 'python39.dll'
    APIVersion = 1013
    RegVersion = '3.9'
    UseLastKnownVersion = False
    IO = PyIO
    Left = 169
    Top = 16
  end
  object PyIO: TPythonInputOutput
    UnicodeIO = False
    RawOutput = False
    Left = 168
    Top = 80
  end
  object PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38
    AutoLoad = True
    PythonVersion = '3.8'
    PythonEngine = PyEngine
    EnvironmentPath = 'python'
    Left = 168
    Top = 144
  end
  object PyNumPy: TNumPy
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Left = 72
    Top = 200
  end
  object PyOpenCV: TOpenCV
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Left = 256
    Top = 200
  end
  object PyTorch: TPyTorch
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Managers.Pip.InstallOptions.ExtraIndexUrl = 'https://download.pytorch.org/whl/cu113'
    Left = 200
    Top = 200
  end
  object PyTorchVision: TTorchVision
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Managers.Pip.InstallOptions.ExtraIndexUrl = 'https://download.pytorch.org/whl/cu113'
    Left = 136
    Top = 200
  end
end
