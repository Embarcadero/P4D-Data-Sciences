(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


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
