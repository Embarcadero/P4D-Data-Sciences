(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Form.TrainedModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Media, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Components, FMX.Layouts, System.JSON, Frame.CustomCameraLayout;

type
  TTrainedModelForm = class(TForm)
    blLinks: TBindingsList;
    StyleBook1: TStyleBook;
    cclFrame: TCustomCameraLayoutFrame;
    loBottom: TLayout;
    swCamera: TSwitch;
    loLeftSide: TLayout;
    loRecComps: TLayout;
    tbThumbs: TTrackBar;
    imgThumbsUp: TImage;
    imgThumbsDown: TImage;
    LinkControlToPropertyActive: TLinkControlToProperty;
    btnRecognize: TSpeedButton;
    imgRecognize: TImage;
    btnLive: TButton;
    procedure CustomCameraLayoutFrame1ccCameraCompSampleBufferReady(
      Sender: TObject; const ATime: TMediaTime);
    procedure btnRecognizeClick(Sender: TObject);
    procedure btnLiveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FProfile: string;
    FIsLive: boolean;
    FImageAccess: integer;
    function Recognize(const ABitmap: TBitmap): Extended; overload; inline;
    function Recognize(const AStream: TStream): Extended; overload; inline;
    procedure StartLiveRecognition();
  public
    property Profile: string read FProfile write FProfile;
  end;

var
  TrainedModelForm: TTrainedModelForm;

implementation

uses
  System.SyncObjs,
  DateUtils,
  BitmapHelper, Remote.ClientModule;

{$R *.fmx}

procedure TTrainedModelForm.btnLiveClick(Sender: TObject);
begin
  btnRecognize.Visible := not btnLive.IsPressed;
  if btnLive.IsPressed and not FIsLive then
    StartLiveRecognition();
end;

procedure TTrainedModelForm.btnRecognizeClick(Sender: TObject);
begin
  tbThumbs.Value := Recognize(cclFrame.imgCamera.Bitmap);
end;

procedure TTrainedModelForm.CustomCameraLayoutFrame1ccCameraCompSampleBufferReady(
  Sender: TObject; const ATime: TMediaTime);
begin
  TThread.Synchronize(TThread.CurrentThread, procedure() begin
    if TInterlocked.CompareExchange(FImageAccess, 1, 0) = 1 then begin
      cclFrame.ccCameraComp.SampleBufferToBitmap(cclFrame.imgCamera.Bitmap, true);
      FImageAccess := 0;
    end;
  end);
end;

procedure TTrainedModelForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TTrainedModelForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  cclFrame.StopStreaming();
  var LSpins := 0;
  while FIsLive do begin
    Sleep(100);
    Inc(LSpins);
    if (LSpins = 30) then
      ShowMessage('Closing live recognition');
  end;
  CanClose := true;
end;

procedure TTrainedModelForm.FormCreate(Sender: TObject);
begin
  FIsLive := false;
  FImageAccess := 0;
end;

function TTrainedModelForm.Recognize(const AStream: TStream): Extended;
begin
  var LResult := ClientModule.TrainingClassClient.Recognize(Profile, AStream);
  if LResult is TJSONObject then begin
    var LError: string;
    if LResult.TryGetValue<string>('error', LError) then begin
      raise Exception.Create(LError);
    end else begin
      var LValue: Extended;
      if LResult.TryGetValue<Extended>('value', LValue) then begin
        Result := LValue * 100; //The probability goes from 0 to 1 (decimals).
        //nearest to 0 represents thumbsdown, whereas nearest to 1 thumbsup.
      end else
        Result := 50;
    end;
  end else
    Result := 50;
end;

function TTrainedModelForm.Recognize(const ABitmap: TBitmap): Extended;
begin
  var LStream := TMemoryStream.Create();
  try
    ABitmap.ToJpg(LStream);
    Result := Recognize(LStream);
  finally
    LStream.Free();
  end;
end;

procedure TTrainedModelForm.StartLiveRecognition;
begin
  FIsLive := true;
  TThread.CreateAnonymousThread(procedure() begin
    try
      var LStream := TMemoryStream.Create();
      try
        while btnLive.IsPressed and cclFrame.IsStreaming() do begin
          LStream.Clear();
          if TInterlocked.CompareExchange(FImageAccess, 1, 0) = 1 then begin
            cclFrame.imgCamera.Bitmap.ToJpg(LStream);
            FImageAccess := 0;
            try
              var LValue := Recognize(LStream);
              //We don't want last recognition anymore.
              //WARNING: May result in no indicator recognition in slow connections...
              //Local network 5GHz is strongly recomended
              TThread.RemoveQueuedEvents(TThread.Current); //Removing this line may result in false live recognition
              //Puts the recognition indicator into the print queue
              TThread.Queue(nil, procedure() begin
                tbThumbs.Value := LValue;
              end);
            except
              on E: Exception do begin
                TThread.Synchronize(nil, procedure() begin
                  ShowMessage(E.Message);
                end);
              end;
            end;
          end else
            Sleep(1);
        end;
      finally
        LStream.Free();
      end;
    finally
      FIsLive := false;
    end;
  end).Start();
end;

end.
