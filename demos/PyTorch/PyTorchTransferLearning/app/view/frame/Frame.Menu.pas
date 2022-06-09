(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Frame.Menu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ComboEdit, FMX.Layouts, FMX.ListBox;

type
  TMenuFrame = class(TFrame)
    loContainer: TLayout;
    loCollapse: TLayout;
    loProfile: TLayout;
    ceProfile: TComboEdit;
    lbProfile: TLabel;
    loActions: TLayout;
    btnCollectData: TButton;
    btnLiveRecognition: TButton;
    btnQuit: TButton;
    btnTrainModel: TButton;
    btnContinue: TSpeedButton;
    sbActions: TScrollBox;
    ToolBar1: TToolBar;
    btnMenu: TSpeedButton;
    lbMenu: TListBox;
    lblHost: TListBoxItem;
    procedure btnMenuClick(Sender: TObject);
    procedure lblHostClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;

    procedure EnableActions();
    procedure DisableActions();
  end;

implementation

{$R *.fmx}

procedure TMenuFrame.btnMenuClick(Sender: TObject);
begin
  lbMenu.Visible := not lbMenu.Visible;
end;

constructor TMenuFrame.Create(AOwner: TComponent);
begin
  inherited;
  lbMenu.Visible := false;
end;

procedure TMenuFrame.DisableActions;
begin
  btnCollectData.Enabled := false;
  btnLiveRecognition.Enabled := false;
end;

procedure TMenuFrame.EnableActions;
begin
  btnCollectData.Enabled := true;
  btnLiveRecognition.Enabled := true;
end;

procedure TMenuFrame.lblHostClick(Sender: TObject);
begin
  lbMenu.Visible := false;
end;

end.
