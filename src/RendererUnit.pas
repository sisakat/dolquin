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
    FColor   : TColor  ;

  protected
    procedure RenderTriangle(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
    
  public
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Render(Mesh : TMesh);
    procedure ClearColor(Color : TColor);

    property Image : TImage read FImage;
  end;

implementation

uses
  Math, SysUtils;

const
  Z_AXIS : TVector3D = (0.0, 0.0, 1.0);

constructor TRenderer.Create(Width : Integer; Height : Integer);
begin
  inherited Create;
  FImage   := TImage  .Create(Width, Height);
  FPainter := TPainter.Create(FImage       );
  FColor   := [255, 255, 255, 255];

  FImage.Fill([0, 0, 0, 255]);
  FPainter.ColorBlendMode := COLOR_BLEND_MODE_NONE;
end; // Create()

destructor TRenderer.Destroy;
begin
  inherited;
end; // Destroy()

procedure TRenderer.RenderTriangle(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
var
  x0, x1, x2 : Integer  ;
  y0, y1, y2 : Integer  ;
  d          : Double   ;
  Color      : TColor   ;
  Vector3D   : TVector3D;
begin
  x0 := Floor((v0[_X_] + 1) * Image.Width  / 2.0);
  y0 := Floor((v0[_Y_] + 1) * Image.Height / 2.0);
  x1 := Floor((v1[_X_] + 1) * Image.Width  / 2.0);
  y1 := Floor((v1[_Y_] + 1) * Image.Height / 2.0);
  x2 := Floor((v2[_X_] + 1) * Image.Width  / 2.0);
  y2 := Floor((v2[_Y_] + 1) * Image.Height / 2.0);

  Vector3D := VectorCross    (VectorSubtract(v0, v1), VectorSubtract(v0, v2));
  Vector3D := VectorNormalize(Vector3D);
  d        := VectorMultiply (Vector3D, Z_AXIS);
  Color    := [Floor(255 * d), Floor(255 * d), Floor(255 * d), 255];

  if (d > 0.0) then
  begin
    FPainter.DrawTriangle(x0, y0, x1, y1, x2, y2, Color);
  end; // if ()
end; // RenderTriangle()

procedure TRenderer.Render(Mesh : TMesh);
var
  i           : Integer     ;
  IndexVector : TIndexVector;
begin
  for i:=0 to Mesh.IndexCount-1 do
  begin
    IndexVector := Mesh.Index[i];
    RenderTriangle(
      Mesh.Vertex[IndexVector[0][0]],
      Mesh.Vertex[IndexVector[0][1]],
      Mesh.Vertex[IndexVector[0][2]]
    );
  end; // for i
end; // Render()

procedure TRenderer.ClearColor(Color : TColor);
begin
  FImage.Fill(Color);
end; // ClearColor()

begin
end.