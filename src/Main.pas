program Main;

{$mode objfpc}{$H+}

uses
  ExportUnit, 
  Graphics, 
  SysUtils, 
  LinearAlgebra, 
  Math, 
  Mesh, 
  MeshReaderObj,
  RendererUnit, 
  ImportUnit,
  DateUtils;

var
  Importer    : TImageImporter;
  PPMExporter : TImageExporter;
  PNGExporter : TImageExporter;
  ObjReader   : TObjReader    ;
  Renderer    : TRenderer     ;
  Texture     : TImage        ;
  FromTime    : TDateTime     ;
  ToTime      : TDateTime     ;
  DiffMillis  : Integer       ;
begin
  WriteLn('Creating image...');
  Importer    := TImageImporter.Create;
  PPMExporter := TPPMExporter  .Create;
  PNGExporter := TPNGExporter  .Create;
  ObjReader   := TObjReader    .Create;
  Renderer    := TRenderer     .Create(512, 512);
  try
    WriteLn('Loading model...');
    ObjReader.Load('african_head.obj');

    WriteLn('Loading texture...');
    Texture := Importer.Load('african_head_diffuse.tga');

    WriteLn('Rendering...');
    FromTime := Now;
    Renderer.DepthTest       := TRUE;
    Renderer.BackFaceCulling := TRUE;
    Renderer.Lighting        := TRUE;
    Renderer.ColorBlendMode := COLOR_BLEND_MODE_ONE_MINUS_ALPHA;
    Renderer.ClearColor([0, 0, 0, 0]); // transparent
    Renderer.BindTexture(Texture);
    Renderer.Render(ObjReader.Mesh);   
    ToTime := Now;

    DiffMillis := MilliSecondsBetween(FromTime, ToTime);
    WriteLn(Format('Render time: %dms (%f fps)', [DiffMillis, 1000/DiffMillis]));
    
    WriteLn('Exporting...');
    PNGExporter.Export(Renderer.Image, 'output.png');
    WriteLn('Done.');
  finally
    FreeAndNil(Renderer   );
    FreeAndNil(ObjReader  );
    FreeAndNil(PNGExporter);
    FreeAndNil(PPMExporter);
    FreeAndNil(Importer   );
  end;
end.