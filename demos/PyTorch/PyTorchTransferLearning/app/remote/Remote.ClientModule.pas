(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Remote.ClientModule;

interface

uses
  System.SysUtils, System.Classes, Remote.ClientClasses, Datasnap.DSClientRest,
  System.Generics.Collections;

type
  TClientModule = class(TDataModule)
    DSRestConnection1: TDSRestConnection;
  private
    FInstanceOwner: Boolean;
    FTrainingClassClient: TTrainingClassClient;
    FCallbacks: TObjectDictionary<string, TDSRestClientChannel>;
    function GetTrainingClassClient: TTrainingClassClient;
    function GetCallbackChannel(const AChannel: string): TDSRestClientChannel;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HasValidHost(): boolean;
    procedure SetHost(const AHost: string);
    function GetHost(): string;

    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property TrainingClassClient: TTrainingClassClient read GetTrainingClassClient write FTrainingClassClient;
    property CallbackChannel[const AChannel: string]: TDSRestClientChannel read GetCallbackChannel;
end;

var
  ClientModule: TClientModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

constructor TClientModule.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := False;
  FCallbacks := TObjectDictionary<string, TDSRestClientChannel>.Create([doOwnsValues]);
end;

destructor TClientModule.Destroy;
begin
  FCallbacks.Free();
  FTrainingClassClient.Free;
  inherited;
end;

function TClientModule.GetCallbackChannel(
  const AChannel: string): TDSRestClientChannel;
begin
  if not FCallbacks.ContainsKey(AChannel) then
    FCallbacks.Add(AChannel, TDSRestClientChannel.Create('', AChannel, DSRestConnection1));
  Result := FCallbacks.Items[AChannel];
end;

function TClientModule.GetTrainingClassClient: TTrainingClassClient;
begin
  if FTrainingClassClient = nil then
    FTrainingClassClient:= TTrainingClassClient.Create(DSRestConnection1, FInstanceOwner);
  Result := FTrainingClassClient;
end;

function TClientModule.HasValidHost: boolean;
begin
  Result := not DSRestConnection1.Host.IsEmpty();
end;

function TClientModule.GetHost: string;
begin
  Result := DSRestConnection1.Host + ':' + DSRestConnection1.Port.ToString();
end;

procedure TClientModule.SetHost(const AHost: string);
var
  LHostAndPort: TArray<string>;
begin
  LHostAndPort := AHost.Split([':']);
  if Length(LHostAndPort) = 1 then
    DSRestConnection1.Host := LHostAndPort[0]
  else if Length(LHostAndPort) = 2 then begin
    DSRestConnection1.Host := LHostAndPort[0];
    DSRestConnection1.Port := LHostAndPort[1].ToInteger();
  end;
end;

end.
