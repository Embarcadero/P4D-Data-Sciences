program ThumbsUpDownTrainModelProc;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Windows,
  CompModule in 'CompModule.pas' {PyComps: TDataModule},
  TrainModel in 'TrainModel.pas',
  PythonEngine,
  ProcErrorCode in '..\..\server\ProcErrorCode.pas';

begin
  try
    PyComps := TPyComps.Create(nil,
      procedure(AText: string) begin
        WriteLn(AText);
      end,
      procedure(AText: string) begin
        WriteLn(AText);
      end);
    try
      PyComps.CheckEngine();

      var LTrainModel := TTrainModel.Create();
      try
        if ParamCount >= 3 then begin
          var profile := ParamStr(1);
          var dataset_path := ParamStr(2); //In
          var trained_model_path := ParamStr(3); //Out

          Writeln('Training profile: ' + profile);
          Writeln('Reading images from: ' + dataset_path);
          Writeln('Writing model to: ' + trained_model_path);

          //If you want to attach this process to a debuger
          if ParamCount = 4 then begin
            Writeln('Hanging on debugger...');
            var LTimer := 30;
            while (DebugHook = 0) and (LTimer > 0) do begin
              //You have 30 seconds to attach the debugger
              Sleep(1000);
              Dec(LTimer);
            end;
            if (DebugHook <> 0) then
              DebugBreak()
            else
              Writeln('Debugger not attached');
          end;

          LTrainModel.Train(dataset_path, trained_model_path);
        end;
      finally
        LTrainModel.Free();
      end;
    finally
      FreeAndNil(PyComps);
    end;
  except
    on E: EPythonError do begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := cPYTHON_ERROR_EXIT_CODE;
    end;
    on E: EDLLLoadError do begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := cPYTHON_DLL_ERROR_EXIT_CODE;
    end;
    on E: EDLLImportError do begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := cPYTHON_DLL_MAP_ERROR_EXIT_CODE;
    end;
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := cGENERIC_ERROR_EXIT_CODE;
    end;
  end;
end.
