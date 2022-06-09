(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Form.DataCollection.ThumbsUp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Form.DataCollection, Frame.CameraLayout, FMX.Objects, Frame.CustomCameraLayout;

type
  TThumbsUpForm = class(TDataCollectionForm)
  private
    { Private declarations }
  public
    function GetTrainingClass(): TTrainingClass; override;
  end;

var
  ThumbsUpForm: TThumbsUpForm;

implementation

{$R *.fmx}

{ TDataCollectionForm1 }

function TThumbsUpForm.GetTrainingClass: TTrainingClass;
begin
  Result := TTrainingClass.ThumbsUp;
end;

end.
