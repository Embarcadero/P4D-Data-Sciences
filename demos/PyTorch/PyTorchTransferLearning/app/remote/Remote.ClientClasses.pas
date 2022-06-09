(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


//
// Created by the DataSnap proxy generator.
// 21/05/2022 15:06:46
//

unit Remote.ClientClasses;

interface

uses System.JSON, Datasnap.DSProxyRest, Datasnap.DSClientRest, Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, Datasnap.DSProxy, System.Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, Data.DBXCDSReaders, Data.DBXJSONReflect;

type
  TTrainingClassClient = class(TDSAdminRestClient)
  private
    FLoadProfileCommand: TDSRestCommand;
    FLoadProfileCommand_Cache: TDSRestCommand;
    FCountClassCommand: TDSRestCommand;
    FSendImageCommand: TDSRestCommand;
    FClearCommand: TDSRestCommand;
    FTrainModelCommand: TDSRestCommand;
    FTrainModelCommand_Cache: TDSRestCommand;
    FContainsTrainedModelCommand: TDSRestCommand;
    FContainsTrainedModelCommand_Cache: TDSRestCommand;
    FRecognizeCommand: TDSRestCommand;
    FRecognizeCommand_Cache: TDSRestCommand;
  public
    constructor Create(ARestConnection: TDSRestConnection); overload;
    constructor Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    function LoadProfile(AProfile: string; const ARequestFilter: string = ''): TJSONValue;
    function LoadProfile_Cache(AProfile: string; const ARequestFilter: string = ''): IDSRestCachedJSONValue;
    function CountClass(AProfile: string; ATrainingClass: string; const ARequestFilter: string = ''): Integer;
    function SendImage(AProfile: string; ATrainingClass: string; AImageName: string; AImage: TStream; const ARequestFilter: string = ''): Integer;
    procedure Clear(AProfile: string; ATrainingClass: string);
    function TrainModel(AProfile: string; const ARequestFilter: string = ''): TJSONValue;
    function TrainModel_Cache(AProfile: string; const ARequestFilter: string = ''): IDSRestCachedJSONValue;
    function ContainsTrainedModel(AProfile: string; const ARequestFilter: string = ''): TJSONValue;
    function ContainsTrainedModel_Cache(AProfile: string; const ARequestFilter: string = ''): IDSRestCachedJSONValue;
    function Recognize(AProfile: string; AImage: TStream; const ARequestFilter: string = ''): TJSONValue;
    function Recognize_Cache(AProfile: string; AImage: TStream; const ARequestFilter: string = ''): IDSRestCachedJSONValue;
  end;

const
  TTrainingClass_LoadProfile: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONValue')
  );

  TTrainingClass_LoadProfile_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TTrainingClass_CountClass: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'ATrainingClass'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 6; TypeName: 'Integer')
  );

  TTrainingClass_SendImage: array [0..4] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'ATrainingClass'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImageName'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImage'; Direction: 1; DBXType: 33; TypeName: 'TStream'),
    (Name: ''; Direction: 4; DBXType: 6; TypeName: 'Integer')
  );

  TTrainingClass_Clear: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'ATrainingClass'; Direction: 1; DBXType: 26; TypeName: 'string')
  );

  TTrainingClass_TrainModel: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONValue')
  );

  TTrainingClass_TrainModel_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TTrainingClass_ContainsTrainedModel: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONValue')
  );

  TTrainingClass_ContainsTrainedModel_Cache: array [0..1] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

  TTrainingClass_Recognize: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImage'; Direction: 1; DBXType: 33; TypeName: 'TStream'),
    (Name: ''; Direction: 4; DBXType: 37; TypeName: 'TJSONValue')
  );

  TTrainingClass_Recognize_Cache: array [0..2] of TDSRestParameterMetaData =
  (
    (Name: 'AProfile'; Direction: 1; DBXType: 26; TypeName: 'string'),
    (Name: 'AImage'; Direction: 1; DBXType: 33; TypeName: 'TStream'),
    (Name: ''; Direction: 4; DBXType: 26; TypeName: 'String')
  );

implementation

function TTrainingClassClient.LoadProfile(AProfile: string; const ARequestFilter: string): TJSONValue;
begin
  if FLoadProfileCommand = nil then
  begin
    FLoadProfileCommand := FConnection.CreateCommand;
    FLoadProfileCommand.RequestType := 'GET';
    FLoadProfileCommand.Text := 'TTrainingClass.LoadProfile';
    FLoadProfileCommand.Prepare(TTrainingClass_LoadProfile);
  end;
  FLoadProfileCommand.Parameters[0].Value.SetWideString(AProfile);
  FLoadProfileCommand.Execute(ARequestFilter);
  Result := TJSONValue(FLoadProfileCommand.Parameters[1].Value.GetJSONValue(FInstanceOwner));
end;

function TTrainingClassClient.LoadProfile_Cache(AProfile: string; const ARequestFilter: string): IDSRestCachedJSONValue;
begin
  if FLoadProfileCommand_Cache = nil then
  begin
    FLoadProfileCommand_Cache := FConnection.CreateCommand;
    FLoadProfileCommand_Cache.RequestType := 'GET';
    FLoadProfileCommand_Cache.Text := 'TTrainingClass.LoadProfile';
    FLoadProfileCommand_Cache.Prepare(TTrainingClass_LoadProfile_Cache);
  end;
  FLoadProfileCommand_Cache.Parameters[0].Value.SetWideString(AProfile);
  FLoadProfileCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONValue.Create(FLoadProfileCommand_Cache.Parameters[1].Value.GetString);
end;

function TTrainingClassClient.CountClass(AProfile: string; ATrainingClass: string; const ARequestFilter: string): Integer;
begin
  if FCountClassCommand = nil then
  begin
    FCountClassCommand := FConnection.CreateCommand;
    FCountClassCommand.RequestType := 'GET';
    FCountClassCommand.Text := 'TTrainingClass.CountClass';
    FCountClassCommand.Prepare(TTrainingClass_CountClass);
  end;
  FCountClassCommand.Parameters[0].Value.SetWideString(AProfile);
  FCountClassCommand.Parameters[1].Value.SetWideString(ATrainingClass);
  FCountClassCommand.Execute(ARequestFilter);
  Result := FCountClassCommand.Parameters[2].Value.GetInt32;
end;

function TTrainingClassClient.SendImage(AProfile: string; ATrainingClass: string; AImageName: string; AImage: TStream; const ARequestFilter: string): Integer;
begin
  if FSendImageCommand = nil then
  begin
    FSendImageCommand := FConnection.CreateCommand;
    FSendImageCommand.RequestType := 'POST';
    FSendImageCommand.Text := 'TTrainingClass."SendImage"';
    FSendImageCommand.Prepare(TTrainingClass_SendImage);
  end;
  FSendImageCommand.Parameters[0].Value.SetWideString(AProfile);
  FSendImageCommand.Parameters[1].Value.SetWideString(ATrainingClass);
  FSendImageCommand.Parameters[2].Value.SetWideString(AImageName);
  FSendImageCommand.Parameters[3].Value.SetStream(AImage, FInstanceOwner);
  FSendImageCommand.Execute(ARequestFilter);
  Result := FSendImageCommand.Parameters[4].Value.GetInt32;
end;

procedure TTrainingClassClient.Clear(AProfile: string; ATrainingClass: string);
begin
  if FClearCommand = nil then
  begin
    FClearCommand := FConnection.CreateCommand;
    FClearCommand.RequestType := 'GET';
    FClearCommand.Text := 'TTrainingClass.Clear';
    FClearCommand.Prepare(TTrainingClass_Clear);
  end;
  FClearCommand.Parameters[0].Value.SetWideString(AProfile);
  FClearCommand.Parameters[1].Value.SetWideString(ATrainingClass);
  FClearCommand.Execute;
end;

function TTrainingClassClient.TrainModel(AProfile: string; const ARequestFilter: string): TJSONValue;
begin
  if FTrainModelCommand = nil then
  begin
    FTrainModelCommand := FConnection.CreateCommand;
    FTrainModelCommand.RequestType := 'GET';
    FTrainModelCommand.Text := 'TTrainingClass.TrainModel';
    FTrainModelCommand.Prepare(TTrainingClass_TrainModel);
  end;
  FTrainModelCommand.Parameters[0].Value.SetWideString(AProfile);
  FTrainModelCommand.Execute(ARequestFilter);
  Result := TJSONValue(FTrainModelCommand.Parameters[1].Value.GetJSONValue(FInstanceOwner));
end;

function TTrainingClassClient.TrainModel_Cache(AProfile: string; const ARequestFilter: string): IDSRestCachedJSONValue;
begin
  if FTrainModelCommand_Cache = nil then
  begin
    FTrainModelCommand_Cache := FConnection.CreateCommand;
    FTrainModelCommand_Cache.RequestType := 'GET';
    FTrainModelCommand_Cache.Text := 'TTrainingClass.TrainModel';
    FTrainModelCommand_Cache.Prepare(TTrainingClass_TrainModel_Cache);
  end;
  FTrainModelCommand_Cache.Parameters[0].Value.SetWideString(AProfile);
  FTrainModelCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONValue.Create(FTrainModelCommand_Cache.Parameters[1].Value.GetString);
end;

function TTrainingClassClient.ContainsTrainedModel(AProfile: string; const ARequestFilter: string): TJSONValue;
begin
  if FContainsTrainedModelCommand = nil then
  begin
    FContainsTrainedModelCommand := FConnection.CreateCommand;
    FContainsTrainedModelCommand.RequestType := 'GET';
    FContainsTrainedModelCommand.Text := 'TTrainingClass.ContainsTrainedModel';
    FContainsTrainedModelCommand.Prepare(TTrainingClass_ContainsTrainedModel);
  end;
  FContainsTrainedModelCommand.Parameters[0].Value.SetWideString(AProfile);
  FContainsTrainedModelCommand.Execute(ARequestFilter);
  Result := TJSONValue(FContainsTrainedModelCommand.Parameters[1].Value.GetJSONValue(FInstanceOwner));
end;

function TTrainingClassClient.ContainsTrainedModel_Cache(AProfile: string; const ARequestFilter: string): IDSRestCachedJSONValue;
begin
  if FContainsTrainedModelCommand_Cache = nil then
  begin
    FContainsTrainedModelCommand_Cache := FConnection.CreateCommand;
    FContainsTrainedModelCommand_Cache.RequestType := 'GET';
    FContainsTrainedModelCommand_Cache.Text := 'TTrainingClass.ContainsTrainedModel';
    FContainsTrainedModelCommand_Cache.Prepare(TTrainingClass_ContainsTrainedModel_Cache);
  end;
  FContainsTrainedModelCommand_Cache.Parameters[0].Value.SetWideString(AProfile);
  FContainsTrainedModelCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONValue.Create(FContainsTrainedModelCommand_Cache.Parameters[1].Value.GetString);
end;

function TTrainingClassClient.Recognize(AProfile: string; AImage: TStream; const ARequestFilter: string): TJSONValue;
begin
  if FRecognizeCommand = nil then
  begin
    FRecognizeCommand := FConnection.CreateCommand;
    FRecognizeCommand.RequestType := 'POST';
    FRecognizeCommand.Text := 'TTrainingClass."Recognize"';
    FRecognizeCommand.Prepare(TTrainingClass_Recognize);
  end;
  FRecognizeCommand.Parameters[0].Value.SetWideString(AProfile);
  FRecognizeCommand.Parameters[1].Value.SetStream(AImage, FInstanceOwner);
  FRecognizeCommand.Execute(ARequestFilter);
  Result := TJSONValue(FRecognizeCommand.Parameters[2].Value.GetJSONValue(FInstanceOwner));
end;

function TTrainingClassClient.Recognize_Cache(AProfile: string; AImage: TStream; const ARequestFilter: string): IDSRestCachedJSONValue;
begin
  if FRecognizeCommand_Cache = nil then
  begin
    FRecognizeCommand_Cache := FConnection.CreateCommand;
    FRecognizeCommand_Cache.RequestType := 'POST';
    FRecognizeCommand_Cache.Text := 'TTrainingClass."Recognize"';
    FRecognizeCommand_Cache.Prepare(TTrainingClass_Recognize_Cache);
  end;
  FRecognizeCommand_Cache.Parameters[0].Value.SetWideString(AProfile);
  FRecognizeCommand_Cache.Parameters[1].Value.SetStream(AImage, FInstanceOwner);
  FRecognizeCommand_Cache.ExecuteCache(ARequestFilter);
  Result := TDSRestCachedJSONValue.Create(FRecognizeCommand_Cache.Parameters[2].Value.GetString);
end;

constructor TTrainingClassClient.Create(ARestConnection: TDSRestConnection);
begin
  inherited Create(ARestConnection);
end;

constructor TTrainingClassClient.Create(ARestConnection: TDSRestConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ARestConnection, AInstanceOwner);
end;

destructor TTrainingClassClient.Destroy;
begin
  FLoadProfileCommand.DisposeOf;
  FLoadProfileCommand_Cache.DisposeOf;
  FCountClassCommand.DisposeOf;
  FSendImageCommand.DisposeOf;
  FClearCommand.DisposeOf;
  FTrainModelCommand.DisposeOf;
  FTrainModelCommand_Cache.DisposeOf;
  FContainsTrainedModelCommand.DisposeOf;
  FContainsTrainedModelCommand_Cache.DisposeOf;
  FRecognizeCommand.DisposeOf;
  FRecognizeCommand_Cache.DisposeOf;
  inherited;
end;

end.

