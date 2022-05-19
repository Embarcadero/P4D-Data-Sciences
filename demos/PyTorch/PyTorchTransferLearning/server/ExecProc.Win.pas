unit ExecProc.Win;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

//Ref: https://docs.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output

type
  TRedirect = (stdin, stdout);
  TRecirections = set of TRedirect;
  TReader = TFunc<string>;
  TWriter = TProc<string>;

  TExecCmdWin = class
  private
    FSecurityAttributes: TSecurityAttributes;
    FStartupInfo: TStartupInfo;
    FProcessInfo: TProcessInformation;
    //Pipes
    //StdOut
    FStdOutPipeRead: THandle;
    FStdOutPipeWrite: THandle;
    //StdIn
    FStdInPipeRead: THandle;
    FStdInPipeWrite: THandle;
    //Job
    FJob: THandle;
  private
    function GetStatus: cardinal;
    function GetIsAlive: boolean;
    function GetExitCode: cardinal;
  public
    constructor Create(const ACmd: string; const ARedirections: TRecirections; const AShow: boolean = false);
    destructor Destroy(); override;

    procedure Redirect(out AReader: TReader; out AWriter: TWriter);
    procedure Kill();
    function LoopStdOut(AStdOut: TProc<string>; ATerminate: TFunc<boolean>): integer;
    procedure OnProcessExit(AOnProcExit: TProc<integer>);

    class function ExecCmd(const ACmd: string;
      const AStdOutPipeReader: TProc<string>;
      const ATerminate: TFunc<boolean>): integer; static;

    property Status: cardinal read GetStatus;
    property IsAlive: boolean read GetIsAlive;
    property ExitCode: cardinal read GetExitCode;
  end;

const
  cCANCELATION_SIGNAL_EXIT_CODE = $001A;

implementation

uses
  Math;

{ TExecCmdWin }

constructor TExecCmdWin.Create(const ACmd: string; const ARedirections: TRecirections; const AShow: boolean = false);
begin
  //Set the bInheritedHandle to true so pipe handles are inherited
  FSecurityAttributes := Default(TSecurityAttributes);
  with FSecurityAttributes do begin
    nLength := SizeOf(FSecurityAttributes);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;

  //Create the startup information to the process
  FStartupInfo := Default(TStartupInfo);
  with FStartupInfo do
  begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
    if AShow then
      wShowWindow := SW_SHOW
    else
      wShowWindow := SW_HIDE;
  end;

  //Create a pipe for the child process's stdout
  if (TRedirect.stdout in ARedirections) then begin
    if not CreatePipe(FStdOutPipeRead, FStdOutPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();

    //Ensure the read hangle to the pipe for STDOUT is not inherited
//    if not SetHandleInformation(FStdOutPipeRead, HANDLE_FLAG_INHERIT, 0) then
//      RaiseLastOSError();

    //Redirect to our pipe
    FStartupInfo.hStdOutput := FStdOutPipeWrite;
    FStartupInfo.hStdError := FStdOutPipeWrite;
  end else begin
    FStartupInfo.hStdOutput := GetStdHandle(STD_OUTPUT_HANDLE);
    FStartupInfo.hStdError := GetStdHandle(STD_ERROR_HANDLE);
  end;

  if (TRedirect.stdin in ARedirections) then begin
    //Create a pipe for the child process's STDIN
    if not CreatePipe(FStdInPipeRead, FStdInPipeWrite, @FSecurityAttributes, 0) then
      RaiseLastOSError();

    //Ensure the write handle to the pipe for STDIN is not inherited
//    if not SetHandleInformation(FStdInPipeWrite, HANDLE_FLAG_INHERIT, 0) then
//      RaiseLastOSError();

    //Redirect to our pipe
    FStartupInfo.hStdInput := FStdInPipeRead;
  end else begin
    FStartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  end;

  //Create a job that kills the subprocess when parent dies
  FJob := CreateJobObject(nil, nil);
  if (FJob <> 0) then begin
    var LExInfo: TJobObjectExtendedLimitInformation;
    LExInfo.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if not SetInformationJobObject(FJob, JobObjectExtendedLimitInformation,
      @LExInfo, SizeOf(TJobObjectExtendedLimitInformation)) then
        RaiseLastOSError();
  end;

  //Create the process
  var LCmd := ACmd;
  UniqueString(LCmd);
  if not CreateProcess(nil, PWideChar(LCmd), nil, nil, True, 0, nil, nil,
    FStartupInfo, FProcessInfo) then
      RaiseLastOSError();

  // Close handles to the stdin and stdout pipes no longer needed by the child process.
  // If they are not explicitly closed, there is no way to recognize that the child process has ended.
  CloseHandle(FStdOutPipeWrite);
  CloseHandle(FStdInPipeRead);

  //Assign the process to the job. It takes the proc. down when parent is killed.
  AssignProcessToJobObject(FJob, FProcessInfo.hProcess);
end;

destructor TExecCmdWin.Destroy;
begin
  if IsAlive then
    Kill();
  CloseHandle(FProcessInfo.hThread);
  CloseHandle(FProcessInfo.hProcess);
  CloseHandle(FStdOutPipeRead);
  inherited;
end;

function TExecCmdWin.GetExitCode: cardinal;
begin
  if not GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(Result)) then
    RaiseLastOSError();
end;

function TExecCmdWin.GetIsAlive: boolean;
begin
  Result := WaitForSingleObject(FProcessInfo.hProcess, 0) = WAIT_TIMEOUT;
end;

function TExecCmdWin.GetStatus: cardinal;
begin
  Result := WaitForSingleObject(FProcessInfo.hProcess, 0);
end;

class function TExecCmdWin.ExecCmd(const ACmd: string;
  const AStdOutPipeReader: TProc<string>;
  const ATerminate: TFunc<boolean>): integer;
begin
  var LProc := TExecCmdWin.Create(ACmd, [TRedirect.stdout], true);
  try
    Result := LProc.LoopStdOut(AStdOutPipeReader, ATerminate);
  finally
    LProc.Free();
  end;
end;

procedure TExecCmdWin.Redirect(out AReader: TReader; out AWriter: TWriter);
const
  BUFFSIZE = 4096;
type
  TBuffArr = array[0..BUFFSIZE - 1] of AnsiChar;
var
  LBytesRead: cardinal;
  LBytesWritten: cardinal;
  LBuffer: TBuffArr;
begin
  AReader := function(): string begin
    Result := String.Empty;
    ReadFile(FStdOutPipeRead, LBuffer, BUFFSIZE, LBytesRead, nil);
    if (LBytesRead > 0) then begin
      LBuffer[LBytesRead] := #0;
      Result := Result + String(LBuffer);
    end;
  end;

  AWriter := procedure(AIn: string) begin
    var LIn := AnsiString(AIn);
    WriteFile(FStdInPipeWrite, LIn[1], Length(LIn), LBytesWritten, nil);
  end;
end;

procedure TExecCmdWin.Kill;
begin
  if (Status = WAIT_TIMEOUT) then
    if not TerminateProcess(FProcessInfo.hProcess, cCANCELATION_SIGNAL_EXIT_CODE) then
      RaiseLastOSError();
end;

function TExecCmdWin.LoopStdOut(AStdOut: TProc<string>;
  ATerminate: TFunc<boolean>): integer;
var
  LReader: TReader;
  LWriter: TWriter;
begin
  Redirect(LReader, LWriter);
  TThread.CreateAnonymousThread(procedure() begin
    while (Status = WAIT_TIMEOUT) do begin
      AStdOut(LReader());
    end;
  end).Start();

  repeat
    Sleep(100);
  until ATerminate() or (Status <> WAIT_TIMEOUT);

  Kill();

  if not GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(Result)) then
    RaiseLastOSError();
end;

procedure TExecCmdWin.OnProcessExit(AOnProcExit: TProc<integer>);
begin
  if Assigned(AOnProcExit) then
    TThread.CreateAnonymousThread(procedure() begin
      while (Status = WAIT_TIMEOUT) do begin
        Sleep(100);
      end;

      var AResultCode: integer;
      GetExitCodeProcess(FProcessInfo.hProcess, Cardinal(AResultCode));
      AOnProcExit(AResultCode);
    end).Start();
end;

end.
