unit TorchVisionReg;

interface

procedure Register();

implementation

uses
  Classes, TorchVision;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TTorchVision]);
end;

end.
