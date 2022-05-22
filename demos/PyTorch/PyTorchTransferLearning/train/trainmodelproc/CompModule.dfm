object PyComps: TPyComps
  Height = 265
  Width = 477
  object PyEngine: TPythonEngine
    AutoLoad = False
    FatalAbort = False
    FatalMsgDlg = False
    IO = PyIO
    Left = 233
    Top = 24
  end
  object PyIO: TPythonInputOutput
    OnSendData = PyIOSendData
    OnReceiveData = PyIOReceiveData
    UnicodeIO = False
    RawOutput = False
    Left = 232
    Top = 80
  end
  object PyEmbeddedResEnvironment381: TPyEmbeddedResEnvironment38
    BeforeSetup = PyEmbeddedResEnvironment381BeforeSetup
    AfterActivate = PyEmbeddedResEnvironment381AfterActivate
    AutoLoad = True
    PythonVersion = '3.8'
    PythonEngine = PyEngine
    EnvironmentPath = 'python'
    Left = 232
    Top = 136
  end
  object PyNumPy: TNumPy
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    BeforeInstall = PyNumPyBeforeInstall
    AfterInstall = PyNumPyAfterInstall
    Left = 136
    Top = 192
  end
  object PyOpenCV: TOpenCV
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    BeforeInstall = PyOpenCVBeforeInstall
    OnInstallError = PyOpenCVInstallError
    AfterInstall = PyOpenCVAfterInstall
    Left = 320
    Top = 192
  end
  object PyTorch: TPyTorch
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Managers.Pip.InstallOptions.ExtraIndexUrl = 'https://download.pytorch.org/whl/cu113'
    BeforeInstall = PyTorchBeforeInstall
    AfterInstall = PyTorchAfterInstall
    Left = 264
    Top = 192
  end
  object PyTorchVision: TTorchVision
    PythonEngine = PyEngine
    PyEnvironment = PyEmbeddedResEnvironment381
    ManagerKind = pip
    Managers.Pip.InstallOptions.ExtraIndexUrl = 'https://download.pytorch.org/whl/cu113'
    BeforeInstall = PyTorchVisionBeforeInstall
    AfterInstall = PyTorchVisionAfterInstall
    Left = 200
    Top = 192
  end
end
