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
    constructor Create;
    destructor  Destroy; override;

    function Load(FileName : String) : TImage;
  end;

function load_image(
  FileName     : pcchar   ;
  x            : pcint    ;
  y            : pcint    ;
  c            : pcint    
) : pcchar; cdecl; external;

procedure free_image(
  Image : pcchar
); cdecl;  external;

implementation

uses
  Classes, Contnrs, SysUtils;

constructor TImageImporter.Create;
begin
  inherited;
end; // Create()

destructor TImageImporter.Destroy;
begin
  inherited;
end; // Destroy()

function TImageImporter.Load(FileName : String) : TImage;
var
  i       : Integer;
  x, y, c : Integer;
  Data    : pcchar ;
  Color   : TPixel ;
begin
  try
    x := 16;
    y := 16;
    c := 3 ;

    Data := load_image(
      pcchar(PChar(PAnsiChar(AnsiString(FileName)))),
      pcint(@x),
      pcint(@y),
      pcint(@c)
    );

    try
      Result := TImage.Create(x, y);
      Result.Fill([255, 0, 255, 255]);

      if (Data <> nil) then
      begin
        for i:=0 to x * y - 1 do
        begin
          Color[0] := Data[i * 4 + 0];
          Color[1] := Data[i * 4 + 1];
          Color[2] := Data[i * 4 + 2];
          Color[3] := Data[i * 4 + 3];
          Result.PixelAtIdx[x * y - 1 - i] := Color;
        end; // for i
      end; // if ()
    finally
      free_image(Data);
    end; // try..finally
  except
    on E: Exception do
    begin
      raise Exception.Create(Format('Could not load file %s: %s', [FileName, E.Message]));
    end;
  end; // try..except
end; // Load()

begin
end.