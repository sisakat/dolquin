program Main;

{$mode objfpc}{$H+}

uses
  Export, Graphics, SysUtils, LinearAlgebra, Math, Mesh, MeshReaderObj, RendererUnit;

var
  PPMExporter : TImageExporter;
  PNGExporter : TImageExporter;
  ObjReader   : TObjReader    ;
  Renderer    : TRenderer     ;
begin
  WriteLn('Creating image...');
  PPMExporter := TPPMExporter.Create;
  PNGExporter := TPNGExporter.Create;
  ObjReader   := TObjReader  .Create;
  Renderer    := TRenderer   .Create(2048, 2048);
  try
    ObjReader.Load('african_head.obj');
    Renderer.Render(ObjReader.Mesh);    
    WriteLn('Exporting...');
    PNGExporter.Export(Renderer.Image, 'output.png');
    WriteLn('Done.');
  finally
    FreeAndNil(Renderer   );
    FreeAndNil(PNGExporter);
    FreeAndNil(PPMExporter);
  end;
end.