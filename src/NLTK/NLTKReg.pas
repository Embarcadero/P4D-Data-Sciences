unit NLTKReg;

interface

procedure Register();

implementation

uses
  Classes, NLTK;

procedure Register();
begin
  RegisterComponents('Python - Data Science Ecosystem', [TNLTK]);
end;

end.
