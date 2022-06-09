unit OpenCVReg;

interface

procedure Register();

implementation

uses
  Classes, OpenCV;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TOpenCV]);
end;

end.
