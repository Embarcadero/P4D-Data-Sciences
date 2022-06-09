(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Frame.TrainingClassSelection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, Form.DataCollection, FMX.Layouts,
  FMX.Ani;

type
  TTrainingClassSelectionFrame = class(TFrame)
    pnlBackground: TPanel;
    recContent: TRectangle;
    lbTitle: TLabel;
    lnLower: TLine;
    rbThumbsUp: TRadioButton;
    rbThumbsDown: TRadioButton;
    btnSelect: TSpeedButton;
    lnUpper: TLine;
    loBox: TLayout;
    faOpacity: TFloatAnimation;
    loClasses: TLayout;
  private
    { Private declarations }
  public
    function CreateDataCollectionForm(AOwner: TComponent;
      const AProfile: string): TDataCollectionForm;
  end;

implementation

uses
  Form.DataCollection.ThumbsUp, Form.DataCollection.ThumbsDown;

{$R *.fmx}

{ TTrainingClassSelectionFrame }

function TTrainingClassSelectionFrame.CreateDataCollectionForm(
  AOwner: TComponent; const AProfile: string): TDataCollectionForm;
begin
  if rbThumbsUp.IsChecked then
    Result := TThumbsUpForm.Create(AOwner, AProfile)
  else if rbThumbsDown.IsChecked then
    Result := TThumbsDownForm.Create(AOwner, AProfile)
  else
    Result := nil;
end;

end.
