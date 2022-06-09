(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Form.DataCollection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Media, FMX.Layouts,
  System.Messaging, System.Permissions, System.Actions, FMX.ActnList,
  FMX.StdActns, FMX.MediaLibrary.Actions, System.ImageList, FMX.ImgList,
  Frame.CameraLayout, FMX.ListBox, Frame.CustomCameraLayout;

type
  TTrainingClass = (ThumbsUp, ThumbsDown);

  TDataCollectionForm = class(TForm)
    camLayout: TCameraLayoutFrame;
    sbCameraLayout: TStyleBook;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure camLayoutlbiClearClick(Sender: TObject);
  private
    FProfile: string;
    procedure SaveImageAsync(Sender: TObject; AImageName: string; AImage: TBitmap);
    procedure DoUpdateCounter();
    procedure ConvertBmpToJpg(const AImg: TBitmap; const AStream: TStream);
  public
    constructor Create(AOwner: TComponent; const AProfile: string); reintroduce;

    function GetTrainingClass(): TTrainingClass; virtual; abstract;
  public
    property Profile: string read FProfile;
  end;

  TTrainingClassHelper = record helper for TTrainingClass
  public
    function ToString(): string;
  end;

var
  DataCollectionForm: TDataCollectionForm;

implementation

uses
  System.IOUtils, FMX.DialogService, FMX.Consts, Remote.ClientModule, BitmapHelper;

{$R *.fmx}

constructor TDataCollectionForm.Create(AOwner: TComponent;
  const AProfile: string);
begin
  FProfile := AProfile;
  inherited Create(AOwner);
end;

procedure TDataCollectionForm.camLayoutlbiClearClick(Sender: TObject);
begin
  camLayout.lbiClearClick(Sender);
  if not camLayout.IsSaving() then begin
    var LMessage := Format('Are you sure you want to clean all "%s" data?', [
        GetTrainingClass().ToString()]);

    TDialogService.MessageDialog(LMessage,
      TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, -1,
      procedure(const AResult: TModalResult) begin
        if (AResult = mrYes) then begin
          ClientModule.TrainingClassClient.Clear(Profile, GetTrainingClass().ToString());
          DoUpdateCounter();
        end;
      end)
  end else
    ShowMessage('Wait saving process to finish.');
end;

procedure TDataCollectionForm.ConvertBmpToJpg(const AImg: TBitmap;
  const AStream: TStream);
begin
  AImg.ToJpg(AStream);
end;

procedure TDataCollectionForm.DoUpdateCounter;
begin
  var LCount := ClientModule.TrainingClassClient.CountClass(Profile,
      GetTrainingClass().ToString());
  camLayout.UpdateCounter(LCount);
end;

procedure TDataCollectionForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TDataCollectionForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := true;
  if camLayout.IsSaving() then begin
    ShowMessage('Saving in progress.');
    CanClose := false;
  end;
end;

procedure TDataCollectionForm.FormCreate(Sender: TObject);
begin
  camLayout.OnSaveAsync := SaveImageAsync;
  DoUpdateCounter();
end;

procedure TDataCollectionForm.SaveImageAsync(Sender: TObject; AImageName: string;
  AImage: TBitmap);
begin
  var LCount := 0;
  var LStream := TMemoryStream.Create();
  try
    ConvertBmpToJpg(AImage, LStream);
    LCount := ClientModule.TrainingClassClient.SendImage(Profile,
      GetTrainingClass().ToString(), TPath.ChangeExtension(AImageName, '.jpg'), LStream);
  finally
    LStream.Free();
  end;

  TThread.Queue(nil, procedure() begin
    camLayout.UpdateCounter(LCount);
  end);
end;

{ TTrainingClassHelper }

function TTrainingClassHelper.ToString: string;
begin
  Result := String.Empty;
  case Self of
    ThumbsUp: Result := 'ThumbsUp';
    ThumbsDown: Result := 'ThumbsDown';
  end;
end;

end.
