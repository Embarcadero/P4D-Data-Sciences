unit TrainingClassMethods;

interface

uses System.SysUtils, System.Classes, System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  Datasnap.DSServer, Datasnap.DSAuth, FMX.Forms,
  ExecProc.Win;

type
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

    function GetTrainedProcName(const AProfile: string): string;
    procedure LoadTrainedModelProc(const AProfile: string);
    function GetTrainedModelProc(const AProfile: string): TExecCmdWin;
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
    function Recognize(const AProfile: string; const AImage: TStream): TJSONValue;
  end;

implementation

uses
  System.IOUtils, ServerContainerUnit1, DSSession, ProcErrorCode;

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

function TTrainingClass.IsSessionValid(const ASessionId: string): boolean;
begin
  Result := Assigned(TDSSessionManager.Instance.Session[ASessionId])
    and TDSSessionManager.Instance.Session[ASessionId].IsValid;
end;

procedure TTrainingClass.LoadTrainedModelProc(const AProfile: string);
const
  PROC = 'ThumbsUpDownTrainedModelProc.exe';
begin
  var LProcName := GetTrainedProcName(AProfile);

  if not TFile.Exists(PROC) then
    raise Exception.Create('Trained model application not found.');

  var LModel := GetModelPath(AProfile);
  if not TFile.Exists(LModel) then
    raise Exception.Create('Profile doesn''t has a trained model.');

  var LCmd := PROC
    + ' -o' + LModel
    + ' -mCHILD_PROC';
//    + ' ' + DEBUG_FLAG; //The debug flag to hang on the proc. until you attach the debugger...

  TDSSessionManager.GetThreadSession().PutObject(
    LProcName,
    TExecCmdWin.Create(LCmd, [TRedirect.stdout, TRedirect.stdin]));
end;

function TTrainingClass.GetTrainedModelProc(const AProfile: string): TExecCmdWin;
const
  PROC = 'ThumbsUpDownTrainedModelProc.exe';
  DEBUG_FLAG = 'DEBUG';
begin
  var LProcName := GetTrainedProcName(AProfile);
  if not TDSSessionManager.GetThreadSession().HasObject(LProcName) then
    LoadTrainedModelProc(AProfile);

  Result := TExecCmdWin(TDSSessionManager.GetThreadSession().GetObject(LProcName));

  //If the process has died in some manner
  if not Result.IsAlive then begin
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
      DSServer.BroadcastMessage(AChannel, ACallbackId,
        TJSONObject.Create(
          TJSONPair.Create(
            'pipe',
            LMsg + ' Error code: ' + AResultCode.ToString())));

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

function TTrainingClass.CountClass(const AProfile,
  ATrainingClass: string): integer;
begin
  Result := GetCount(BuildClassFolder(BuildProfileFolder(BuildImagesFolder(), AProfile), ATrainingClass));
end;

function TTrainingClass.LoadProfile(const AProfile: string): TJSONValue;
begin
  var LModel := GetModelPath(AProfile);
  var LContainsTrainedModel := TFile.Exists(LModel);

  Result := TJSONObject.Create();
  with Result as TJSONObject do begin
    AddPair('contains_trained_model', LContainsTrainedModel);
  end;

  //Starts up the trained module proc to reduce delay time on first recognition call
  if LContainsTrainedModel then
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
  CHANNEL_SUFIX = '_TRAIN_MODEL_CALLBACK';
  PROC = 'ThumbsUpDownTrainModelProc.exe';
  DEBUG_FLAG = 'd';
begin
  if not TFile.Exists(PROC) then
    raise Exception.Create('Train model application not found.');

  var LChannel := AProfile + CHANNEL_SUFIX;
  var LCallBackId := LChannel + '_' + TDSSessionManager.GetThreadSession().SessionName;

  //Only one training execution per profile session
  if TDSSessionManager.GetThreadSession().HasData(LChannel) then begin
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

  var LCmd := PROC
    + ' ' + AProfile
    + ' ' + BuildProfileFolder(BuildImagesFolder(), AProfile)
    + ' ' + LModel
    + ' ' + DEBUG_FLAG; //The debug flag to hang on the proc. until you attach the debugger...

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
  TThread.CreateAnonymousThread(procedure() begin
    var LResultCode := TExecCmdWin.ExecCmd(LCmd,
      procedure(AText: string) begin
        DSServer.BroadcastMessage(LChannel, LCallBackId,
          TJSONObject.Create(TJSONPair.Create('pipe', AText)));
        TFile.AppendAllText(LStdOutLogPath, AText);
      end,
      function(): boolean begin
        Result := not LSessionPredicate(LSessionId);
      end);

    ProcessTrainModelResult(AProfile, LSessionId, LChannel, LCallbackId, LResultCode);
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
    var LProb: extended := -1;
    Result := TJSONObject.Create();

    with TJSONObject(Result) do begin
      if (AStdOut.StartsWith('ERROR')) then begin
        AddPair('error', AStdOut);
      end else if TryStrToFloat(AStdOut, LProb) then begin
        AddPair('success', 'Image classified');
      end;
      AddPair('value', LProb);
    end;
  end;

  //We keep the trained model up and send requests as needed.
  //Note: starting up this process may take to much time. We can keep it alive, though.
  var LProc := GetTrainedModelProc(AProfile);
  var LWriter: TWriter;
  var LReader: TReader;

  LProc.Redirect(LReader, LWriter);
  LWriter('RUN');
  var LStdOut := LReader();
  if (LStdOut = 'WAITING') then begin
    LWriter(LImagePath);
    LStdOut := LReader();
    Result := LBuildResponse(LStdOut);
  end else
    Result := LBuildResponse(String.Empty);
end;

end.

