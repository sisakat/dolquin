unit RendererUnit;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra, Mesh, Graphics;

type
  TRenderer = class(TObject)
  public
  private
    FImage   : TImage  ;
    FPainter : TPainter;
    
  public
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Render(Mesh : TMesh);

    property Image : TImage read FImage;
  end;

implementation

uses
  Math, SysUtils;

constructor TRenderer.Create(Width : Integer; Height : Integer);
begin
  inherited Create;
  FImage   := TImage  .Create(Width, Height);
  FPainter := TPainter.Create(FImage       );
end; // Create()

destructor TRenderer.Destroy;
begin
  inherited;
end; // Destroy()

procedure TRenderer.Render(Mesh : TMesh);
const
  Z_AXIS     : TVector3D = (0.0, 0.0, 1.0);
var
  i : Integer;
  Vector3D : TVector3D;
  Color : TColor;
  IndexVector : TIndexVector  ;
  x0, x1, x2  : Integer       ;
  y0, y1, y2  : Integer       ;
  d           : Double        ;
begin
  FImage.Fill([0, 0, 0, 255]);
  FPainter.ColorBlendMode := COLOR_BLEND_MODE_NONE;
  for i:=0 to Mesh.IndexCount-1 do
  begin
    IndexVector := Mesh.Index[i];
    Vector3D := Mesh.Vertex[IndexVector[0][0]];
    Color := [255, 255, 255, 255];

    x0 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y0 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    Vector3D := Mesh.Vertex[IndexVector[0][1]];
    x1 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y1 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    Vector3D := Mesh.Vertex[IndexVector[0][2]];
    x2 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y2 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    Vector3D := VectorNormalize(
    VectorCross(
        VectorSubtract(Mesh.Vertex[IndexVector[0][0]], Mesh.Vertex[IndexVector[0][1]]), 
        VectorSubtract(Mesh.Vertex[IndexVector[0][0]], Mesh.Vertex[IndexVector[0][2]])
    )
    );
    d := VectorMultiply(Vector3D, Z_AXIS);
    if (d < 0.0) then Continue;
    FPainter.DrawTriangle(x0, y0, x1, y1, x2, y2, [Floor(255 * d), 0, 0, 255]);
    FPainter.DrawLine(x0, y0, x1, y1, Color);

    Vector3D := Mesh.Vertex[IndexVector[0][0]];
    x0 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y0 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    Vector3D := Mesh.Vertex[IndexVector[0][2]];
    x1 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y1 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    FPainter.DrawLine(x0, y0, x1, y1, Color);

    Vector3D := Mesh.Vertex[IndexVector[0][1]];
    x0 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y0 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    Vector3D := Mesh.Vertex[IndexVector[0][2]];
    x1 := Floor((Vector3D[_X_] + 1) * Image.Width / 2.0);
    y1 := Floor((Vector3D[_Y_] + 1) * Image.Height / 2.0);
    FPainter.DrawLine(x0, y0, x1, y1, Color);
  end; // for i
end; // Render()

begin
end.