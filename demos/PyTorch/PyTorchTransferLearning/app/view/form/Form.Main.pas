(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Permissions, System.Messaging, Form.DataCollection, Frame.Menu,
  Frame.TrainingClassSelection, Form.TrainModel, Form.TrainedModel,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    frmMenu: TMenuFrame;
    frmClassSelection: TTrainingClassSelectionFrame;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure frmMenubtnCollectDataClick(Sender: TObject);
    procedure frmMenubtnQuitClick(Sender: TObject);
    procedure frmClassSelectionbtnSelectClick(Sender: TObject);
    procedure frmMenubtnTrainModelClick(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure frmMenubtnContinueClick(Sender: TObject);
    procedure frmMenubtnLiveRecognitionClick(Sender: TObject);
    procedure frmMenulblHostClick(Sender: TObject);
    procedure frmMenubtnMenuClick(Sender: TObject);
  private
    function GetFormValues(): string;
    procedure SetFormValues(const AValues: string);
    procedure ConfigureHost();
  private
    procedure ApplicationEventChangedHandler(const Sender: TObject; const AMessage: TMessage);
    procedure ActivateCameraPermissionRequestResult(Sender: TObject; const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
    procedure DisplayRationale(Sender: TObject; const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
    procedure CheckPermissions;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.IOUtils, System.Generics.Collections, System.JSON,
{$IFDEF ANDROID}
  Androidapi.Helpers,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
{$ENDIF}
  FMX.Platform, FMX.DialogService, Remote.ClientModule;

{$R *.fmx}

{ TMainForm }

procedure TMainForm.ActivateCameraPermissionRequestResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
begin
  var LGranted := true;
  var LNotGranted := String.Empty;
  for var I := Low(APermissions) to High(APermissions) do begin
    if APermissions[I] = JStringToString(TJManifest_permission.JavaClass.CAMERA) then begin
      if not (AGrantResults[I] = TPermissionStatus.Granted) then begin
        LGranted := false;
        LNotGranted := LNotGranted + sLineBreak + 'Camera';
      end;
    end else if (APermissions[I] = JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE)) then begin
      if not (AGrantResults[I] = TPermissionStatus.Granted) then begin
        LGranted := false;
        LNotGranted := LNotGranted + sLineBreak + 'External Storage';
      end;
    end else if (APermissions[I] = JStringToString(TJManifest_permission.JavaClass.INTERNET)) then begin
      if not (AGrantResults[I] = TPermissionStatus.Granted) then begin
        LGranted := false;
        LNotGranted := LNotGranted + sLineBreak + 'Internet';
      end;
    end;
  end;

  if LGranted then
    frmMenu.EnableActions()
  else
    TDialogService.ShowMessage(
      'Cannot start the camera because the required permission(s) has not been granted.'
    + sLineBreak
    + LNotGranted)
end;

procedure TMainForm.ApplicationEventChangedHandler(const Sender: TObject;
  const AMessage: TMessage);
begin
  case TApplicationEventMessage(AMessage).Value.Event of
    TApplicationEvent.FinishedLaunching:
      CheckPermissions();
  end;
end;

procedure TMainForm.CheckPermissions;
begin
  {$IFDEF ANDROID}
  var LPermissionCamera := JStringToString(TJManifest_permission.JavaClass.CAMERA);
  var LPermissionStorage := JStringToString(TJManifest_permission.JavaClass.WRITE_EXTERNAL_STORAGE);
  var LPermissionInternet := JStringToString(TJManifest_permission.JavaClass.INTERNET);
  if not PermissionsService.IsEveryPermissionGranted([LPermissionCamera, LPermissionStorage]) then begin
    PermissionsService.RequestPermissions([
      LPermissionCamera, LPermissionStorage, LPermissionInternet],
      ActivateCameraPermissionRequestResult, DisplayRationale);
  end else
    frmMenu.EnableActions();
  {$ENDIF}
end;

procedure TMainForm.ConfigureHost;
begin
  TDialogService.InputQuery('Host options:', ['Host'], [ClientModule.GetHost()],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOk) then
        ClientModule.SetHost(AValues[0]);
    end);
end;

procedure TMainForm.DisplayRationale(Sender: TObject;
  const APermissions: TClassicStringDynArray; const APostRationaleProc: TProc);
begin
  // Show an explanation to the user *asynchronously* - don't block this thread waiting for the user's response!
  // After the user sees the explanation, invoke the post-rationale routine to request the permissions
  TDialogService.ShowMessage('The app needs to access the camera in order to work',
    procedure(const AResult: TModalResult)
    begin
      APostRationaleProc;
    end);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  frmMenu.loActions.Visible := false;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventChangedHandler);
  frmMenu.DisableActions();

  SaveState.StoragePath := TPath.GetHomePath();
  if (SaveState.Stream.Size > 0) then begin
    var LReader := TBinaryReader.Create(SaveState.Stream, TEncoding.UTF8);
    try
      SetFormValues(LReader.ReadString());
    finally
      LReader.Free();
    end;
  end;
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  if frmMenu.ceProfile.Items.IndexOf(frmMenu.ceProfile.Text) = -1 then begin
    frmMenu.ceProfile.Items.Add(frmMenu.ceProfile.Text);
  end;

  SaveState.Stream.Clear();
  var LWriter := TBinaryWriter.Create(SaveState.Stream);
  try
    LWriter.Write(GetFormValues());
  finally
    LWriter.Free();
  end;
end;

procedure TMainForm.frmClassSelectionbtnSelectClick(Sender: TObject);
begin
  var LForm := frmClassSelection.CreateDataCollectionForm(Self, frmMenu.ceProfile.Text);
  LForm.Show();
  frmClassSelection.Visible := false;
end;

procedure TMainForm.frmMenubtnCollectDataClick(Sender: TObject);
begin
  frmClassSelection.Visible := true;
end;

procedure TMainForm.frmMenubtnContinueClick(Sender: TObject);
var
  LContaisTrainedModel: boolean;
begin
  if frmMenu.ceProfile.Text.Trim().IsEmpty() then
    raise Exception.Create('Select or create a profile.');

  if not ClientModule.HasValidHost() then begin
    ConfigureHost();
    Exit();
  end;

  //Initialize the profile on the server side
  var LResponse := ClientModule.TrainingClassClient.LoadProfile(frmMenu.ceProfile.Text);
  //Check for a trained model
  if LResponse.TryGetValue<boolean>('contains_trained_model', LContaisTrainedModel) then
    frmMenu.btnLiveRecognition.Enabled := LContaisTrainedModel;

  frmMenu.loActions.Visible := true;
end;

procedure TMainForm.frmMenubtnLiveRecognitionClick(Sender: TObject);
begin
  var LForm := TTrainedModelForm.Create(Self);
  LForm.Profile := frmMenu.ceProfile.Text;
  LForm.Show();
end;

procedure TMainForm.frmMenubtnMenuClick(Sender: TObject);
begin
  frmMenu.btnMenuClick(Sender);
end;

procedure TMainForm.frmMenubtnQuitClick(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TMainForm.frmMenubtnTrainModelClick(Sender: TObject);
begin
  var LForm := TTrainModelForm.Create(Self, frmMenu.ceProfile.Text);
  LForm.ShowModal(procedure(AModalResult: TModalResult) begin
    if ClientModule.TrainingClassClient.ContainsTrainedModel(frmMenu.ceProfile.Text) is TJSONTrue then
      frmMenu.btnLiveRecognition.Enabled := true;
  end);
end;

procedure TMainForm.frmMenulblHostClick(Sender: TObject);
begin
  frmMenu.lblHostClick(Sender);
  ConfigureHost();
end;

function TMainForm.GetFormValues: string;
begin
  var LValues := TJSONArray.Create();
  try
    LValues.Add(frmMenu.ceProfile.Items.Text);
    LValues.Add(ClientModule.GetHost());
    Result := LValues.ToJSON();
  finally
    LValues.Free();
  end;
end;

procedure TMainForm.SetFormValues(const AValues: string);
begin
  var LValues := TJSONObject.ParseJSONValue(AValues) as TJSONArray;
  try
    if not Assigned(LValues) then
      Exit;
    frmMenu.ceProfile.Items.Text := LValues[0].AsType<string>;
    ClientModule.SetHost(LValues[1].AsType<string>);
  finally
    LValues.Free();
  end;
end;

end.
