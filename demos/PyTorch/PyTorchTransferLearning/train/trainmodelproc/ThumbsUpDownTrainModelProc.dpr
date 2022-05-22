program ThumbsUpDownTrainModelProc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  CompModule in 'CompModule.pas' {PyComps: TDataModule},
  TrainModel in 'TrainModel.pas',
  PythonEngine,
  ProcErrorCode in '..\..\server\ProcErrorCode.pas',
  Pipe in '..\Pipe.pas';

begin
  if (ParamCount < 3) then
    raise Exception.Create('Invalid parameters.');

  try
    var LTrainModel := TTrainModel.Create();
    try
      var profile := ParamStr(1);
      var dataset_path := ParamStr(2); //In
      var trained_model_path := ParamStr(3); //Out

      TPipeIO.Write('Training profile: ' + profile);
      TPipeIO.Write('Reading images from: ' + dataset_path);
      TPipeIO.Write('Writing model to: ' + trained_model_path);

      //If you want to attach this process to a debuger
      if ParamCount = 4 then begin
        TPipeIO.Write('Hanging on debugger...');
        var LTimer := 30;
        while (DebugHook = 0) and (LTimer > 0) do begin
          //You have 30 seconds to attach the debugger
          Sleep(1000);
          Dec(LTimer);
        end;
        {$IFDEF MSWINDOWS}
        if (DebugHook <> 0) then begin
          TPipeIO.Write('Waiting for debugger command to continue');
          DebugBreak();
        end else
          TPipeIO.Write('Debugger not attached');
        {$ENDIF MSWINDOWS}
      end;

      PyComps := TPyComps.Create(nil);
      try
        PyComps.CheckEngine();
        LTrainModel.Train(dataset_path, trained_model_path);
      finally
        FreeAndNil(PyComps);
      end;
    finally
      LTrainModel.Free();
    end;
  except
    on E: EPythonError do begin
      TPipeIO.Write(E.ClassName + ': ' + E.Message);
      ExitCode := cPYTHON_ERROR_EXIT_CODE;
    end;
    on E: EDLLLoadError do begin
      TPipeIO.Write(E.ClassName + ': ' + E.Message);
      ExitCode := cPYTHON_DLL_ERROR_EXIT_CODE;
    end;
    on E: EDLLImportError do begin
      TPipeIO.Write(E.ClassName + ': ' + E.Message);
      ExitCode := cPYTHON_DLL_MAP_ERROR_EXIT_CODE;
    end;
    on E: Exception do begin
      TPipeIO.Write(E.ClassName + ': ' + E.Message);
      ExitCode := cGENERIC_ERROR_EXIT_CODE;
    end;
  end;
end.
