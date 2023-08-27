program Main;

{$mode objfpc}{$H+}

uses
  Export, Graphics, SysUtils, Math;

var
  Image       : TImage        ;
  PPMExporter : TImageExporter;
  PNGExporter : TImageExporter;
  Painter     : TPainter      ;
begin
  WriteLn('Creating image...');
  Image       := TImage      .Create(512, 512);
  PPMExporter := TPPMExporter.Create          ;
  PNGExporter := TPNGExporter.Create          ;
  Painter     := TPainter    .Create(Image   );
  try
    Image.Fill([20, 20, 20, 255]);

    Painter.DrawRectangle(128, 128, 200, 100, [255, 255, 255, 255]);
    Painter.DrawRectangle(150, 150, 200, 100, [0  , 0  , 0  , 128]);
    Painter.DrawTriangle (130, 130, 190, 130, 130, 250, [0, 0, 255, 128]);
    
    WriteLn('Exporting...');
    PNGExporter.Export(Image, 'output.png');
    WriteLn('Done.');
  finally
    FreeAndNil(Painter    );
    FreeAndNil(PNGExporter);
    FreeAndNil(PPMExporter);
    FreeAndNil(Image      );
  end;
end.