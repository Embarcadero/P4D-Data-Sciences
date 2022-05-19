unit BitmapHelper;

interface

uses
  FMX.Graphics, System.Classes;

type
  TBitmapHelper = class helper for TBitmap
  public
    procedure ToJpg(const AStream: TStream);
    function Clone(): TBitmap;
  end;

implementation

uses
  FMX.Surfaces, FMX.Types, FMX.Consts;

{ TBitmapHelper }

function TBitmapHelper.Clone: TBitmap;
begin
 Result := TBitmap.Create(Self.Width, Self.Height);
 Result.CopyFromBitmap(Self);
end;

procedure TBitmapHelper.ToJpg(const AStream: TStream);
begin
  var LSurface := TBitmapSurface.Create();
  try
    LSurface.Assign(Self);
    if not TBitmapCodecManager.SaveToStream(AStream, LSurface, '.jpg') then
      raise EBitmapSavingFailed.Create(SBitmapSavingFailed);
  finally
    LSurface.Free();
  end;
end;

end.
