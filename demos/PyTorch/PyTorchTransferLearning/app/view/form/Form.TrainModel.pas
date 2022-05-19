unit Form.TrainModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, Datasnap.DSClientRest,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, System.JSON;

type
  TTrainModelForm = class(TForm)
    ToolBar1: TToolBar;
    lbProfile: TLabel;
    mmPipe: TMemo;
    btnTrain: TButton;
    StyleBook1: TStyleBook;
    procedure btnTrainClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FProfile: string;
    FCallback: TDSRestClientCallback;
    function GetCallBackChannel(): TDSRestClientChannel;
  public
    constructor Create(AOwner: TComponent; const AProfile: string); reintroduce;
    property Profile: string read FProfile;
  end;

var
  TrainModelForm: TTrainModelForm;

implementation

uses
  Remote.ClientModule;

{$R *.fmx}

procedure TTrainModelForm.btnTrainClick(Sender: TObject);
begin
  var LMsg: string;
  var LResult := ClientModule.TrainingClassClient.TrainModel(FProfile, LMsg);
  var LResultStr: string;

  if LResult.TryGetValue<string>('error', LResultStr) then
    ShowMessage(LResultStr);

  if LResult.TryGetValue<string>('callback_id', LResultStr) then begin
    mmPipe.Lines.Text := 'Trying to connect to the pipe...';
    mmPipe.Lines.Add('');

    FCallback := TDSRestClientCallback.Create(GetCallBackChannel, LResultStr,
      function(AValue: TJSONValue; ADataType: string): boolean begin
        TThread.Synchronize(nil, procedure() begin
          var LValue: string;
          var LStatus: boolean;
          if AValue is TJSONObject then begin
            if AValue.TryGetValue<string>('pipe', LValue) then
              mmPipe.Lines.Text := mmPipe.Lines.Text + LValue
            else if AValue.TryGetValue<boolean>('done', LStatus) then begin
              if LStatus then
                mmPipe.Lines.Add('Model successfully trained.')
              else
                mmPipe.Lines.Add('Model not trained. Check the pipe lines for errors.');
            end else if AValue.TryGetValue<string>('error', LValue) then begin
              mmPipe.Lines.Add('Error: ' + LValue);
            end;
            mmPipe.GoToTextEnd();
          end else
            mmPipe.Lines.Add(AValue.ToString());;
        end);
        Result := true;
      end);

     GetCallBackChannel.RegisterCallback(FCallback);
  end;
end;

constructor TTrainModelForm.Create(AOwner: TComponent; const AProfile: string);
begin
  inherited Create(AOwner);
  FProfile := AProfile;
  lbProfile.Text := 'Profile: ' + AProfile;
end;

procedure TTrainModelForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TTrainModelForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCallback) then begin
    GetCallBackChannel.UnregisterCallback(FCallback);
  end;
end;

function TTrainModelForm.GetCallBackChannel: TDSRestClientChannel;
const
  TRAIN_MODEL_CHANNEL_SUFFIX = '_TRAIN_MODEL_CALLBACK';
begin
  Result := ClientModule.CallbackChannel[FProfile + TRAIN_MODEL_CHANNEL_SUFFIX];
end;

end.
