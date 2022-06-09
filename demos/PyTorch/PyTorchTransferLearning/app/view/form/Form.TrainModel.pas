(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FProfile: string;
    FCallback: TDSRestClientCallback;
    function GetCallBackChannel(): TDSRestClientChannel;

    procedure DoOnDisconnect(Sender: TObject);
    procedure DoOnChannelChange(const EventItem: TDSRESTChannelEventItem);
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

  if Assigned(FCallback) then
    GetCallBackChannel().UnregisterCallback(FCallback);

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

     GetCallBackChannel().RegisterCallback(FCallback);
  end;
end;

constructor TTrainModelForm.Create(AOwner: TComponent; const AProfile: string);
begin
  inherited Create(AOwner);
  FProfile := AProfile;
  lbProfile.Text := 'Profile: ' + AProfile;
end;

procedure TTrainModelForm.DoOnChannelChange(
  const EventItem: TDSRESTChannelEventItem);
begin
  if (EventItem.CallbackId = FCallback.CallbackId) then
    case EventItem.EventType of
      rChannelCreate: ;
      rChannelClose: mmPipe.Lines.Add('Callback channel was closed');
      rChannelClosedByServer: mmPipe.Lines.Add('Callback channel was closed by server');
      rCallbackAdded: ;
      rCallbackRemoved: mmPipe.Lines.Add('Callback channel was removed');
    end;
end;

procedure TTrainModelForm.DoOnDisconnect(Sender: TObject);
begin
  ShowMessage('You have been disconnected from the pipe. Try to reconnect.');
end;

procedure TTrainModelForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TTrainModelForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  try
    if Assigned(FCallback) then begin
      GetCallBackChannel().UnregisterCallback(FCallback);
    end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TTrainModelForm.FormCreate(Sender: TObject);
begin
  with GetCallBackChannel() do begin
    OnDisconnect := DoOnDisconnect;
    OnChannelStateChange := DoOnChannelChange;
  end;
end;

function TTrainModelForm.GetCallBackChannel: TDSRestClientChannel;
const
  TRAIN_MODEL_CHANNEL_SUFFIX = '_TRAIN_MODEL_CALLBACK';
begin
  Result := ClientModule.CallbackChannel[FProfile + TRAIN_MODEL_CHANNEL_SUFFIX];
end;

end.
