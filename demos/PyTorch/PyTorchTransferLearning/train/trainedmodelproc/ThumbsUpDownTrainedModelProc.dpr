program ThumbsUpDownTrainedModelProc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  Windows,
  LoadModel in 'LoadModel.pas',
  CompModule in 'CompModule.pas' {PyComps: TDataModule},
  Pipe in '..\Pipe.pas';

begin
  try
    var LDebug := false;
    var LModelPath: string;
    for var I := 0 to ParamCount do begin
      if ParamStr(I).Trim().StartsWith('-o') then begin
        LModelPath := ParamStr(I).Replace('-o', String.Empty);
      end else if (ParamStr(I).Trim().Equals('DEBUG')) then
        LDebug := true;
    end;

    if LDebug then begin
      //If you want to attach this process to a debuger
      if ParamCount = 3 then begin
        var LTimer := 30;
        while (DebugHook = 0) and (LTimer > 0) do begin
          //You have 30 seconds to attach the debugger
          Sleep(1000);
          Dec(LTimer);
        end;
        if (DebugHook <> 0) then
          DebugBreak();
      end;
    end;

    PyComps := TPyComps.Create(nil);
    try
      var LLoadModel := TLoadModel.Create(LModelPath);
      try
        var LCmd := TPipeIO.Read();
        while (LCmd = 'RUN') do begin
          try
            TPipeIO.Write('WAITING');
            var LImagePath := TPipeIO.Instance.Read();
            if not TFile.Exists(LImagePath) then begin
              TPipeIO.Write('ERROR: File not found.');
            end else begin
              TPipeIO.Write(
                LLoadModel.ProbabilisticClassification(LImagePath).ToString());
            end;
            LCmd := TPipeIO.Read();
          except
            on E: Exception do begin
              Write('ERROR: ' + E.Message);
            end;
          end;
        end;
      finally
        LLoadModel.Free();
      end;
    finally
      PyComps.Free();
    end;
  except
    on E: Exception do
      TPipeIO.Write('ERROR: ' + E.Message);
  end;
end.
