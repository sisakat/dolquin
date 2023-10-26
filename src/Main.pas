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
  DateUtils,
  WindowUnit;

var
  Time        : Double        ;
  Importer    : TImageImporter;
  PPMExporter : TImageExporter;
  PNGExporter : TImageExporter;
  ObjReader   : TObjReader    ;
  Renderer    : TRenderer     ;
  Texture     : TImage        ;
  FromTime    : TDateTime     ;
  ToTime      : TDateTime     ;
  DiffMillis  : Integer       ;
  CameraPos   : TVector3D     ;
begin
  WriteLn('Creating image...');
  Importer    := TImageImporter.Create;
  PPMExporter := TPPMExporter  .Create;
  PNGExporter := TPNGExporter  .Create;
  ObjReader   := TObjReader    .Create;
  Renderer    := TRenderer     .Create(512, 512);
  CameraPos   := Z_AXIS;
  try
    WriteLn('Loading model...');
    ObjReader.Load('african_head.obj');

    WriteLn('Loading texture...');
    Texture := Importer.Load('african_head_diffuse.tga');

    WriteLn('Rendering...');
    Time := 0.0;
    ShowWindow(Renderer.Image);
    while UpdateWindow do
    begin
      FromTime := Now;
      Renderer.DepthTest       := TRUE;
      Renderer.BackFaceCulling := TRUE;
      Renderer.Lighting        := TRUE;
      Renderer.ColorBlendMode := COLOR_BLEND_MODE_ONE_MINUS_ALPHA;
      Renderer.ClearColor([0, 0, 0, 0]); // transparent
      Renderer.ClearDepthBuffer;
      // Renderer.BindTexture(Texture);
      Renderer.Render(ObjReader.Mesh);

      CameraPos[0] := Sin(Time);
      CameraPos[1] := 0.0;
      CameraPos[2] := 1.0;
      Renderer.Camera.Eye := CameraPos;

      ToTime := Now;
      DiffMillis := MilliSecondsBetween(FromTime, ToTime);
      WriteLn(Format('Render time: %dms (%f fps)', [DiffMillis, 1000/DiffMillis]));
      Time := Time + 0.01;
    end; // while
    CloseWindow;

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