(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit TrainingClassMethods;

interface

uses System.SysUtils, System.Classes, System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  Datasnap.DSServer, Datasnap.DSAuth, FMX.Forms,
  PyTools.ExecCmd;

type  
  TExecCmdAdapter = class;
  
  TTrainingClass = class(TDSServerModule)
  private
    procedure BuildDir(const ADir: string);
    function BuildImagesFolder(): string;
    function BuildProfileFolder(const ABaseDir, AProfile: string): string;
    function BuildClassFolder(const ABaseDir, ATrainingClass: string): string;
    function BuildImagePath(const ABaseDir, AImageName: string): string;
    function GetCount(const ABaseDir: string): integer;
    function GetModelPath(const AProfile: string): string;
    function IsSessionValid(const ASessionId: string): boolean;

    procedure ProcessTrainModelResult(const AProfile, ASessionId, AChannel, ACallbackId: string; const AResultCode: integer);
    function HasTrainingSession(const AProfile: string): boolean;
    function InternalContainsTrainedModel(const AProfile: string): boolean;

    function GetTrainedProcName(const AProfile: string): string;
    procedure LoadTrainedModelProc(const AProfile: string);
    function GetTrainedModelProc(const AProfile: string): TExecCmdAdapter;
  public
    const IMAGES_FOLDER = 'training_data';
  public
    { Public declarations }
    function LoadProfile(const AProfile: string): TJSONValue;
    function CountClass(const AProfile, ATrainingClass: string): integer;
    function SendImage(const AProfile, ATrainingClass, AImageName: string;
      const AImage: TStream): integer;
    procedure Clear(const AProfile, ATrainingClass: string);
    function TrainModel(const AProfile: string): TJSONValue;
    function ContainsTrainedModel(const AProfile: string): TJSONValue;
    function Recognize(const AProfile: string; const AImage: TStream): TJSONValue;
  end;

  TExecCmdAdapter = class
  strict private
    FExecCmd: IExecCmd;
    FReader: TReader;
    FWriter: TWriter;
  public
    constructor Create(const AExecCmd: IExecCmd; const AReader: TReader; const AWriter: TWriter);
    
    property ExecCmd: IExecCmd read FExecCmd;
    property Reader: TReader read FReader;
    property Writer: TWriter read FWriter;
  end;
  
implementation

uses
  System.IOUtils, ServerContainerUnit1, DSSession, ProcErrorCode;

const
  CHANNEL_SUFIX = '_TRAIN_MODEL_CALLBACK';

{ TExecCmdAdapter }

constructor TExecCmdAdapter.Create(const AExecCmd: IExecCmd; const AReader: TReader;
  const AWriter: TWriter);
begin
  FExecCmd := AExecCmd;
  FReader := AReader;
  FWriter := AWriter;
end;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

{ TTrainingClass }

function TTrainingClass.BuildClassFolder(const ABaseDir,
  ATrainingClass: string): string;
begin
  Result := TPath.Combine(ABaseDir, ATrainingClass);
  BuildDir(Result);
end;

procedure TTrainingClass.BuildDir(const ADir: string);
begin
  if not TDirectory.Exists(ADir) then
    TDirectory.CreateDirectory(ADir);
end;

function TTrainingClass.BuildImagePath(const ABaseDir, AImageName: string): string;
begin
  Result := TPath.Combine(ABaseDir, AImageName);
end;

function TTrainingClass.BuildProfileFolder(const ABaseDir,
  AProfile: string): string;
begin
  Result := TPath.Combine(ABaseDir, AProfile);
  BuildDir(Result);
end;

function TTrainingClass.GetCount(const ABaseDir: string): integer;
begin
  Result := Length(TDirectory.GetFiles(ABaseDir));
end;

function TTrainingClass.GetModelPath(const AProfile: string): string;
begin
  Result := TPath.Combine(BuildProfileFolder(
    BuildImagesFolder(), AProfile), 'best_model.pth');
end;

function TTrainingClass.BuildImagesFolder(): string;
begin
  var LBinDir := TPath.GetDirectoryName(ParamStr(0));
  var LRootDir := TDirectory.GetParent(LBinDir);
  Result := TPath.Combine(LRootDir, IMAGES_FOLDER, false);
  BuildDir(Result);
end;

function TTrainingClass.GetTrainedProcName(const AProfile: string): string;
begin
  Result := AProfile + '_TRAINED_MODEL_PROC';
end;

function TTrainingClass.InternalContainsTrainedModel(
  const AProfile: string): boolean;
begin
  Result := TFile.Exists(GetModelPath(AProfile));
end;

function TTrainingClass.IsSessionValid(const ASessionId: string): boolean;
begin
  Result := Assigned(TDSSessionManager.Instance.Session[ASessionId])
    and TDSSessionManager.Instance.Session[ASessionId].IsValid;
end;

function TTrainingClass.HasTrainingSession(const AProfile: string): boolean;
begin
  var LChannel := AProfile + CHANNEL_SUFIX;
  Result := TDSSessionManager.GetThreadSession().HasData(LChannel);
end;

procedure TTrainingClass.LoadTrainedModelProc(const AProfile: string);
const
  PROC = 'ThumbsUpDownTrainedModelProc.exe';
  DEBUG_FLAG = 'DEBUG';
var
  LReader: TReader;
  LWriter: TWriter;
begin
  var LProcName := GetTrainedProcName(AProfile);

  if not TFile.Exists(PROC) then
    raise Exception.Create('Trained model application not found.');

  var LModel := GetModelPath(AProfile);
  if not TFile.Exists(LModel) then
    raise Exception.Create('Profile doesn''t has a trained model.');

  var LExec := TExecCmdService
    .Cmd(PROC, ['-o' + LModel, '-mCHILD_PROC' {, DEBUG_FLAG}])
      .Run(LReader, LWriter, [TRedirect.stdin, TRedirect.stdout]);

  TDSSessionManager.GetThreadSession().PutObject(LProcName,
    TExecCmdAdapter.Create(LExec, LReader, LWriter));
end;

function TTrainingClass.GetTrainedModelProc(const AProfile: string): TExecCmdAdapter;
const
  PROC = 'ThumbsUpDownTrainedModelProc.exe';
begin
  var LProcName := GetTrainedProcName(AProfile);
  if not TDSSessionManager.GetThreadSession().HasObject(LProcName) then
    LoadTrainedModelProc(AProfile);

  Result := TExecCmdAdapter(TDSSessionManager.GetThreadSession().GetObject(LProcName));

  //If the process has died in some manner
  if not Result.ExecCmd.IsAlive then begin
    TDSSessionManager.GetThreadSession().RemoveObject(LProcName, true);
    Result := GetTrainedModelProc(AProfile);
  end;
end;

procedure TTrainingClass.ProcessTrainModelResult(const AProfile, ASessionId, AChannel,
  ACallbackId: string; const AResultCode: integer);
begin
  var LModel := GetModelPath(AProfile);
  if (AResultCode = 0) and TFile.Exists(LModel) then begin
    DSServer.BroadcastMessage(AChannel, ACallbackId,
      TJSONObject.Create(TJSONPair.Create('done', true)));

    //Starts up the trained module proc to reduce delay time on first recognition call
    LoadTrainedModelProc(AProfile);
  end else begin
    //Has created the model file, but process has finished unsuccessfully,
    //so we delete it to avoid using a broken model
    if TFile.Exists(LModel) then
      TFile.Delete(LModel);

    var LMsg: string;
    case AResultCode of
      cGENERIC_ERROR_EXIT_CODE:
        LMsg := mGENERIC_ERROR_EXIT_MSG;
      cPYTHON_ERROR_EXIT_CODE:
        LMsg := mPYTHON_ERROR_EXIT_MSG;
      cPYTHON_DLL_ERROR_EXIT_CODE:
        LMsg := mPYTHON_DLL_ERROR_EXIT_MSG;
      cPYTHON_DLL_MAP_ERROR_EXIT_CODE:
        LMsg := mPYTHON_DLL_MAP_ERROR_EXIT_MSG;
    end;

    if not LMsg.IsEmpty() then
      LMsg := LMsg + ' Error code: ' + AResultCode.ToString()  
    else
      LMsg := 'An unknown error has occurred';
                                                 
    DSServer.BroadcastMessage(AChannel, ACallbackId,
      TJSONObject.Create(TJSONPair.Create('pipe', LMsg)));

    DSServer.BroadcastMessage(AChannel, ACallbackId,
      TJSONObject.Create(TJSONPair.Create('done', false)));
  end;

  if IsSessionValid(ASessionId) then
    TDSSessionManager.Instance.Session[ASessionId].RemoveData(AChannel);
end;

procedure TTrainingClass.Clear(const AProfile, ATrainingClass: string);
begin
  TDirectory.Delete(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass), true);
end;

function TTrainingClass.ContainsTrainedModel(
  const AProfile: string): TJSONValue;
begin
  case InternalContainsTrainedModel(AProfile) of
    true: Result := TJSONTrue.Create();
    false: Result := TJSONFalse.Create();
  end;
end;

function TTrainingClass.CountClass(const AProfile,
  ATrainingClass: string): integer;
begin
  Result := GetCount(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass));
end;

function TTrainingClass.LoadProfile(const AProfile: string): TJSONValue;
begin
  var LIsTrained := InternalContainsTrainedModel(AProfile);

  Result := TJSONObject.Create(
    TJSONPair.Create('contains_trained_model', LIsTrained));

  //Starts up the trained module proc to reduce delay time on first recognition call
  if LIsTrained then
    LoadTrainedModelProc(AProfile);
end;

function TTrainingClass.SendImage(const AProfile, ATrainingClass, AImageName: string;
  const AImage: TStream): integer;
begin
  var LDir := BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass);
  var LStream := TFileStream.Create(BuildImagePath(LDir, AImageName), fmCreate or fmOpenWrite);
  try
    LStream.Seek(0, soFromEnd);
    LStream.CopyFrom(AImage, AImage.Size)
  finally
    LStream.Free();
  end;

  Result := GetCount(LDir);
end;

function TTrainingClass.TrainModel(const AProfile: string): TJSONValue;
const
  PROC = 'ThumbsUpDownTrainModelProc.exe';
  DEBUG_FLAG = 'd';
begin
  if not TFile.Exists(PROC) then
    raise Exception.Create('Train model application not found.');

  var LChannel := AProfile + CHANNEL_SUFIX;
  var LCallBackId := LChannel + '_' + TDSSessionManager.GetThreadSession().SessionName;

  //Only one training execution per profile session
  if HasTrainingSession(AProfile) then begin
    Result := TJSONObject.Create();
    TJSONObject(Result).AddPair(TJSONPair.Create(
      'error',
      'A training set is already running to the current session.'));
    TJSONObject(Result).AddPair(TJSONPair.Create(
      'callback_id',
      LCallBackId));
    Exit;
  end;

  TDSSessionManager.GetThreadSession().PutData(LChannel, DateTimeToStr(Now));

  var LModel := GetModelPath(AProfile);

  if TFile.Exists(LModel) then
    TFile.Delete(LModel);

  var LArgs: TArray<string> := [
    AProfile,
    BuildProfileFolder(BuildImagesFolder(), AProfile),
    LModel
    {,DEBUG_FLAG}]; //The debug flag to hang on the proc. until you attach the debugger...

  var LStdOutLogPath := TPath.Combine(BuildProfileFolder(BuildImagesFolder(), AProfile), 'stdout.log');
  if TFile.Exists(LStdOutLogPath) then
    TFile.Delete(LStdOutLogPath);

  var LSessionActive := true;
  var LSessionPredicate: TPredicate<string> := function(ASessionId: string): boolean begin
    Result := LSessionActive
      and IsSessionValid(ASessionId)
      and not Application.Terminated;
  end;

  var LSessionId := TDSSessionManager.GetThreadSession().SessionName;
  TThread.CreateAnonymousThread(
    procedure()
    var
      LReader: TReader;
      LWriter: TWriter;
      LOutput: string;
    begin
      var LExec := TExecCmdService
        .Cmd(PROC, LArgs)
          .Run(LReader, LWriter, [TRedirect.stdout]);

      repeat
        LOutput := LReader();
        if not LOutput.IsEmpty() then begin
          DSServer.BroadcastMessage(LChannel, LCallBackId,
            TJSONObject.Create(TJSONPair.Create('pipe', LOutput)));
          TFile.AppendAllText(LStdOutLogPath, LOutput);
        end;
      until LOutput.IsEmpty() or not LSessionPredicate(LSessionId);

      if LExec.IsAlive and not LSessionPredicate(LSessionId) then
        LExec.Kill();

      ProcessTrainModelResult(AProfile, LSessionId, LChannel, LCallbackId, LExec.Wait());
  end).Start();

  TDSSessionManager.Instance.AddSessionEvent(
    procedure(Sender: TObject; const EventType: TDSSessionEventType; const Session: TDSSession) begin
      case EventType of
        SessionClose: begin
          LSessionActive := false;
        end;
      end;
    end);

  Result := TJSONObject.Create(TJSONPair.Create('callback_id', LCallBackId));
end;

function TTrainingClass.Recognize(const AProfile: string;
  const AImage: TStream): TJSONValue;
begin
  var LImagePath := TPath.GetTempFileName();
  var LStream := TFileStream.Create(LImagePath, fmCreate or fmOpenWrite);
  try
    LStream.Seek(0, soFromEnd);
    LStream.CopyFrom(AImage, AImage.Size)
  finally
    LStream.Free();
  end;

  var LBuildResponse := function(const AStdOut: string): TJSONValue begin
    Result := TJSONObject.Create();
    var LResponse := TJSONObject.ParseJSONValue(AStdOut);
    if LResponse is TJSONObject then begin
      var LValue: string;
      if TJSONObject(LResponse).TryGetValue<string>('classification', LValue) then begin
        TJSONObject(Result).AddPair('success', 'Image classified');
        TJSONObject(Result).AddPair('value', LValue.ToDouble());
      end else if TJSONObject(LResponse).TryGetValue<string>('error', LValue) then begin
        TJSONObject(Result).AddPair('error', LValue);
        TJSONObject(Result).AddPair('value', 0);
      end;
    end;
  end;

  //We keep the trained model up and send requests as needed.
  //Note: starting up this process may take to much time. We can keep it alive, though.
  var LProc := GetTrainedModelProc(AProfile);
  var LReader := LProc.Reader;
  var LWriter := LProc.Writer;  

  var LInput := TJSONObject.Create(TJSONPair.Create('classify', LImagePath));
  try
    LWriter(LInput.ToJSON());
  finally
    LInput.Free();
  end;
  Result := LBuildResponse(LReader());
end;

end.

