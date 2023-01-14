unit Export;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

type
  TImageExporter = class(TObject)
  public
    procedure Export(Image : TImage; FileName : String); virtual; abstract;
  end;

type
  TPPMExporter = class(TImageExporter)
  public
    procedure Export(Image : TImage; FileName : String); override;
  end;

implementation

uses
  Classes, Contnrs, SysUtils;

procedure TPPMExporter.Export(Image : TImage; FileName : String);
var
  i          : Integer;
  FileStream : TFileStream;
  s          : String;
  Color      : TColor;
begin
  try
    FileStream := TFileStream.Create(FileName, fmCreate);
    s := Format('P3 %d %d %d', [Image.Width, Image.Height, 255]) + sLineBreak;
    FileStream.Write(PChar(s)^, Length(s));

    for i:=0 to Image.Length-1 do
    begin
      Color := Image.PixelAtIdx[i];
      s := Format('%d %d %d%s', [Color[0], Color[1], Color[2], sLineBreak]);
      FileStream.Write(PChar(s)^, Length(s));
    end; // for

    FileStream.Free;
  except
    on E: Exception do
    begin
      raise Exception.Create(Format('Could not write file %s', [FileName]));
    end;
  end; // try..except
end; // Export()

begin
end.