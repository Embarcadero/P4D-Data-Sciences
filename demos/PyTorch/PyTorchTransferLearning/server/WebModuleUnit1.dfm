object WebModule1: TWebModule1
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end>
  Height = 333
  Width = 414
  object DSHTTPWebDispatcher1: TDSHTTPWebDispatcher
    Filters = <>
    WebDispatch.PathInfo = 'datasnap*'
    Left = 56
    Top = 19
  end
  object DSServerMetaDataProvider1: TDSServerMetaDataProvider
    Left = 56
    Top = 80
  end
  object DSProxyDispatcher1: TDSProxyDispatcher
    Left = 56
    Top = 128
  end
end
