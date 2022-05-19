object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 598
  ClientWidth = 1270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 27
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 1270
    Height = 598
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 317
    Top = 8
  end
  object NumPy1: TNumPy
    PythonEngine = PythonEngine1
    ManagerKind = pip
    Left = 544
    Top = 144
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo1
    Left = 544
    Top = 8
  end
end
