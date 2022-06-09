unit PyTorchReg;

interface

procedure Register();

implementation

uses
  Classes, PyTorch;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TPyTorch]);
end;

end.
