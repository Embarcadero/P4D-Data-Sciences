program ThumbsUpDownTrainedModelProc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  System.JSON,
  Windows,
  LoadModel in 'LoadModel.pas',
  CompModule in 'CompModule.pas' {PyComps: TDataModule},
  Pipe in '..\Pipe.pas';

const
  IMAGES_FOLDER = 'training_data';

procedure Log(const AText: string);
begin
  var LFile := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), 'recognition.txt');
  TFile.AppendAllText(LFile, Now().ToString() + ': ' + AText + #13#10);
end;

function ProcessCommand(const ALoadModel: TLoadModel): boolean;
begin
  Result := true;
  var LOutput := TJSONObject.Create();
  try
    var LCmd := TPipeIO.Read();
    Log(Format('Command received: %s', [LCmd]));
    var LInput := TJSONObject.ParseJSONValue(LCmd);
    if LInput is TJSONObject then begin
      var LValue: string;
      if TJSONObject(LInput).TryGetValue<string>('classify', LValue) then begin
        var LClassification :=
          ALoadModel.ProbabilisticClassification(LValue).ToString();
        LOutput.AddPair('classification', LClassification);
      end else if TJSONObject(LInput).TryGetValue<string>('quit', LValue) then begin
        Result := false;
      end else
        LOutput.AddPair('error', 'Invalid command: ' + LValue);
    end else begin
      LOutput.AddPair('error', 'Invalid command');
    end;

    Log(Format('Command sent: %s', [LOutput.ToJSON()]));
    TPipeIO.Write(LOutput.ToJSON());
  finally
    LOutput.Free();
  end;
end;

begin
  var LOutput := TJSONObject.Create();
  try
    try
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
            while ProcessCommand(LLoadModel) do begin
              //
            end;
            LOutput.AddPair('quit', 'leaving...');
          finally
            LLoadModel.Free();
          end;
        finally
          PyComps.Free();
        end;
      except
        on E: Exception do begin
          LOutput.AddPair('error', E.Message);
        end;
      end;
    finally
      Log(LOutput.ToJSON());
      TPipeIO.Write(LOutput.ToJSON());
    end;
  finally
    LOutput.Free();
  end;
end.
