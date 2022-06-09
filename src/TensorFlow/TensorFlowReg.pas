unit TensorFlowReg;

interface

procedure Register();

implementation

uses
  Classes, TensorFlow;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TTensorFlow]);
end;

end.
