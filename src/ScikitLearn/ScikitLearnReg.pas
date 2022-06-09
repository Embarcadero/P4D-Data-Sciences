unit ScikitLearnReg;

interface

procedure Register();

implementation

uses
  Classes, ScikitLearn;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TScikitLearn]);
end;

end.
