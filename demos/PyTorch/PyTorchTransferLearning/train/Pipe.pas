(********************************************************
 * Part of Python for Delphi - Data Sciences libraries  *
 *                                                      *
 * Copyright (c) 2022 by Embarcadero Technologies       *
 * Licensed under the MIT License                       *
 *                                                      *
 * For full license text and more information visit:    *
 * https://github.com/Embarcadero/P4D-Data-Sciences     *
 ********************************************************)


unit Pipe;

interface

type
  TPipeIO = class
  private
    class var FInstance: TPipeIO;
  private
    FStdIn: THandle;
    FStdOut: THandle;
  private
    function InternalRead(): string;
    procedure InternalWrite(const AText: string);
  public
    constructor Create();
    destructor Destroy(); override;

    class constructor Create();
    class destructor Destroy();

    class function Read(): string;
    class procedure Write(const AText: string);
  public
    class property Instance: TPipeIO read FInstance;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF MSWINDOWS}
  System.SysUtils;

const
  BUFFSIZE = 4096;

type
  TBuffArr = array[0..BUFFSIZE - 1] of AnsiChar;

{ TPipeIO }

class constructor TPipeIO.Create;
begin
  FInstance := TPipeIO.Create();
end;

class destructor TPipeIO.Destroy;
begin
  FreeAndNil(FInstance);
end;

constructor TPipeIO.Create;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  FStdIn := GetStdHandle(STD_INPUT_HANDLE);
  FStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ENDIF MSWINDOWS}
end;

destructor TPipeIO.Destroy;
begin
  FStdIn := 0;
  FStdOut := 0;
  inherited;
end;

function TPipeIO.InternalRead: string;
begin
  {$IFDEF MSWINDOWS}
  var LBuffer: TBuffArr;
  var LBytesRead: cardinal;
  ReadFile(FStdIn, LBuffer, BUFFSIZE, LBytesRead, nil);
  if (LBytesRead > 0) then begin
    SetString(Result, LBuffer, LBytesRead);
  end;
  {$ELSE}
  Readln(Result);
  {$ENDIF}
end;

procedure TPipeIO.InternalWrite(const AText: string);
begin
  {$IFDEF MSWINDOWS}
  var LText := AnsiString(AText + sLineBreak);
  var LBytesWritten: cardinal;
  WriteFile(FStdOut, LText[1], Length(LText), LBytesWritten, nil);
  {$ELSE}
  Writeln(AText);
  {$ENDIF}
end;

class function TPipeIO.Read: string;
begin
  Result := FInstance.InternalRead();
end;

class procedure TPipeIO.Write(const AText: string);
begin
  FInstance.InternalWrite(AText);
end;

end.
