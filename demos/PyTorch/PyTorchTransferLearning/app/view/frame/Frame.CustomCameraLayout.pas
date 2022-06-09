(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Frame.CustomCameraLayout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Media, FMX.Objects;

type
  TCustomCameraLayoutFrame = class(TFrame)
    ccCameraComp: TCameraComponent;
    recBackgroud: TRectangle;
    imgCamera: TImage;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function IsStreaming(): boolean;
    procedure StartStreaming;
    procedure StopStreaming;
  end;

implementation

uses
  System.Messaging, FMX.Platform;

{$R *.fmx}

{ TCustomCameraLayoutFrame }

constructor TCustomCameraLayoutFrame.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage,
    procedure(const Sender: TObject; const M: TMessage) begin
      case TApplicationEventMessage(M).Value.Event of
        TApplicationEvent.BecameActive: StartStreaming();
        TApplicationEvent.WillBecomeInactive: StopStreaming();
      end;
    end);
end;

destructor TCustomCameraLayoutFrame.Destroy;
begin
  inherited;
end;

function TCustomCameraLayoutFrame.IsStreaming: boolean;
begin
  Result := ccCameraComp.Active;
end;

procedure TCustomCameraLayoutFrame.StartStreaming;
begin
  ccCameraComp.Active := true;
end;

procedure TCustomCameraLayoutFrame.StopStreaming;
begin
  ccCameraComp.Active := false;
end;

end.
