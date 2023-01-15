program Main;

{$mode objfpc}{$H+}

uses
  Export, Graphics, SysUtils;

var
  Image       : TImage        ;
  PPMExporter : TImageExporter;
  PNGExporter : TImageExporter;
  Painter     : TPainter      ;
begin
  Image       := TImage      .Create(256, 256);
  PPMExporter := TPPMExporter.Create          ;
  PNGExporter := TPNGExporter.Create          ;
  Painter     := TPainter    .Create(Image   );
  try
    Image.Fill([0, 0, 0, 0]);
    Painter.DrawRectangle(100, 100, 50, 50, [125, 125, 125, 255]);
    Painter.DrawCircle   (100, 100, 50,     [180, 0  , 0  , 255]);
    Painter.DrawLine     (255, 255, 0  , 0, [0  , 255, 0  , 255]);
    Painter.DrawLine     (0  , 255, 128, 0, [0  , 0  , 255, 255]);
    Painter.DrawTriangle (0  , 0  ,
                          100, 100,
                          230, 10 ,
                          [255, 255, 255, 255]);
    WriteLn('Exporting...');
    PPMExporter.Export(Image, 'output.ppm');
    PNGExporter.Export(Image, 'output.png');
  finally
    FreeAndNil(Painter    );
    FreeAndNil(PNGExporter);
    FreeAndNil(PPMExporter);
    FreeAndNil(Image      );
  end;
end.