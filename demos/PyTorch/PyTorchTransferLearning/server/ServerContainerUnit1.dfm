object ServerContainer1: TServerContainer1
  Height = 165
  Width = 410
  PixelsPerInch = 96
  object DSServer1: TDSServer
    Left = 96
    Top = 11
  end
  object DSServerClass1: TDSServerClass
    OnGetClass = DSServerClass1GetClass
    Server = DSServer1
    Left = 200
    Top = 11
  end
end
