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
  Renderer    := TRenderer     .Create(640, 460);
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

      CameraPos[0] := Sin(Time);
      CameraPos[1] := 0.0;
      CameraPos[2] := 2.0;
      Renderer.Camera.Eye := CameraPos;

      Renderer.DepthTest       := TRUE;
      Renderer.BackFaceCulling := TRUE;
      Renderer.Lighting        := TRUE;
      Renderer.ColorBlendMode := COLOR_BLEND_MODE_NONE;
      Renderer.ClearColor([0, 0, 0, 0]); // transparent
      Renderer.ClearDepthBuffer;
      Renderer.BindTexture(Texture);
      Renderer.Viewport(0, 0, Renderer.Image.Width, Renderer.Image.Height);
      Renderer.PerspectiveProj(0.1, 100.0, 75.0);
      Renderer.Render(ObjReader.Mesh);

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