object PyComps: TPyComps
  Height = 265
  Width = 477
  PixelsPerInch = 96
  object PyEngine: TPythonEngine
    FatalAbort = False
    FatalMsgDlg = False
    IO = PyIO
    Left = 233
    Top = 24
  end
  object PyNumPy: TNumPy
    PythonEngine = PyEngine
    ManagerKind = pip
    Left = 168
    Top = 144
  end
  object PyTorch: TPyTorch
    PythonEngine = PyEngine
    ManagerKind = pip
    Managers.Pip.PackageVersion = '==1.7.1+cu110'
    Managers.Pip.InstallOptions.FindLinks = 'https://download.pytorch.org/whl/torch_stable.html'
    Left = 296
    Top = 144
  end
  object PyTorchVision: TTorchVision
    AutoImport = False
    PythonEngine = PyEngine
    ManagerKind = pip
    Managers.Pip.PackageVersion = '==0.8.2+cu110'
    Managers.Pip.InstallOptions.FindLinks = 'https://download.pytorch.org/whl/torch_stable.html'
    Left = 232
    Top = 144
  end
  object PyIO: TPythonInputOutput
    OnSendData = PyIOSendData
    OnReceiveData = PyIOReceiveData
    UnicodeIO = False
    RawOutput = False
    Left = 232
    Top = 80
  end
end
