unit MatplotLibReg;

interface

procedure Register();

implementation

uses
  Classes, MatplotLib;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TMatplotLib]);
end;

end.
