program nltk_basics;

uses
  System.StartUpCopy,
  FMX.Forms,
  nltk1_main in 'nltk1_main.pas' {Form16};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm16, Form16);
  Application.Run;
end.
