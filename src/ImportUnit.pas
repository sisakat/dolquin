unit ImportUnit;

{$mode objfpc}{$H+}
{$link stb_image.o}
{$linklib c}
{$linklib m}

interface

uses
  Graphics, ctypes;

type
  TImageImporter = class(TObject)
  public
    function Load(FileName : String) : TImage;
  end;

function load_image(
  FileName     : pcchar   ;
  x            : pcint    ;
  y            : pcint    
) : pcchar; cdecl; external;

implementation

uses
  Classes, Contnrs, SysUtils;

function TImageImporter.Load(FileName : String) : TImage;
var
  i : Integer;
  x, y : Integer;
  Data : pcchar;
  Color : TPixel;
begin
  try
    x := 16;
    y := 16;

    Data := load_image(
      pcchar(PChar(PAnsiChar(AnsiString(FileName)))),
      pcint(@x),
      pcint(@y)
    );

    Result := TImage.Create(x, y);
    Result.Fill([255, 0, 255, 255]);

    if (Data <> nil) then
    begin
      for i:=0 to x * y - 1 do
      begin
        Color[0] := Data[i * 3 + 0];
        Color[1] := Data[i * 3 + 1];
        Color[2] := Data[i * 3 + 2];
        Result.PixelAtIdx[x * y - 1 - i] := Color;
      end; // for i
    end; // if ()
  except
    on E: Exception do
    begin
      raise Exception.Create(Format('Could not load file %s: %s', [FileName, E.Message]));
    end;
  end; // try..except
end; // Load()

begin
end.