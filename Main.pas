program Main;

{$mode objfpc}{$H+}

uses
  Export, Graphics, SysUtils;

var
  Image    : TImage        ;
  Exporter : TImageExporter;
  Painter  : TPainter      ;
begin
  Image    := TImage      .Create(1920, 1080);
  Exporter := TPPMExporter.Create            ;
  Painter  := TPainter    .Create(Image     );
  try
    Image.Fill([0, 0, 0, 0]);
    Painter.DrawRect  (100, 100, 50, 50, [125, 125, 125, 255]);
    Painter.DrawCircle(100, 100, 50,     [180, 0  , 0  , 255]);
    Painter.DrawLine  (255, 255, 0  , 0, [0  , 255, 0  , 255]);
    Painter.DrawLine  (0  , 255, 128, 0, [0  , 0  , 255, 255]);
    WriteLn('Exporting...');
    Exporter.Export(Image, 'output.ppm');
  finally
    FreeAndNil(Painter );
    FreeAndNil(Exporter);
    FreeAndNil(Image   );
  end;
end.