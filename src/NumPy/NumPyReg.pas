unit NumPyReg;

interface

procedure Register();

implementation

uses
  Classes, NumPy;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TNumPy]);
end;

end.
