unit RendererUnit;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra, Mesh, Graphics;

type
  TRenderer = class(TObject)
  public
  private
    FImage          : TRenderImage   ;
    FColor          : TColor         ;
    FColorBlendMode : TColorBlendMode;
    FTexture        : TImage         ;
    FIndexVector    : TIndexVector   ;
    FMesh           : TMesh          ;

  protected
    procedure Rasterize     (v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
    procedure RenderTriangle(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
    
  public
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Render(Mesh : TMesh);
    procedure ClearColor(Color : TColor);
    procedure ClearDepthBuffer;

    procedure BindTexture(Image : TImage);

    property Image : TRenderImage read FImage;
    property ColorBlendMode : TColorBlendMode read FColorBlendMode write FColorBlendMode;
    property DrawColor : TColor read FColor write FColor;
  end;

implementation

uses
  Math, SysUtils;

const
  Z_AXIS : TVector3D = (0.0, 0.0, 1.0);

constructor TRenderer.Create(Width : Integer; Height : Integer);
begin
  inherited Create;
  FImage          := TRenderImage.Create(Width, Height);
  FColor          := [255, 255, 255, 255];
  FColorBlendMode := COLOR_BLEND_MODE_NONE;
  FTexture        := nil;

  ClearDepthBuffer;
  ClearColor([0, 0, 0, 255]);
end; // Create()

destructor TRenderer.Destroy;
begin
  inherited;
end; // Destroy()

procedure TRenderer.Rasterize(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
  function Sign(
    x0 : Integer; y0 : Integer;
    x1 : Integer; y1 : Integer;
    x2 : Integer; y2 : Integer
  ) : Double;
  begin
    Result := (x0 - x2) * (y1 - y2) - (x1 - x2) * (y0 - y2);
  end;

  function PointInTriangle(
    p0 : Integer; p1 : Integer;
    x0 : Integer; y0 : Integer;
    x1 : Integer; y1 : Integer;
    x2 : Integer; y2 : Integer
  ) : Boolean;
  var
    d0, d1, d2     : Double ;
    HasNeg, HasPos : Boolean;
  begin
    d0 := Sign(p0, p1, x0, y0, x1, y1);
    d1 := Sign(p0, p1, x1, y1, x2, y2);
    d2 := Sign(p0, p1, x2, y2, x0, y0);
    HasNeg := (d0 < 0) or (d1 < 0) or (d2 < 0);
    HasPos := (d0 > 0) or (d1 > 0) or (d2 > 0);
    Result := not (HasNeg and HasPos);
  end;

  procedure BoundingBox(
    x0 : Integer; y0 : Integer;
    x1 : Integer; y1 : Integer;
    x2 : Integer; y2 : Integer;
    var bx0 : Integer; var by0 : Integer;
    var bx1 : Integer; var by1 : Integer
  );
  begin
    bx0 := Min(Min(x0, x1), x2);
    by0 := Min(Min(y0, y1), y2);
    bx1 := Max(Max(x0, x1), x2);
    by1 := Max(Max(y0, y1), y2);
  end;

var
  x0, x1, y0, y1, x2, y2 : Integer  ;
  x  , y                 : Integer  ;
  z                      : Double   ;
  bx0, bx1, by0, by1     : Integer  ;
  Vector3D               : TVector3D;
  uv                     : TVector3D;
  Color, Pixel           : TColor   ;
  d                      : Double   ;
begin
  Vector3D := VectorCross    (VectorSubtract(v0, v1), VectorSubtract(v0, v2));
  Vector3D := VectorNormalize(Vector3D);
  d        := VectorMultiply (Vector3D, Z_AXIS);
  if (d < 0.0) then Exit;

  x0 := Floor((v0[_X_] + 1) * FImage.Width  / 2.0);
  y0 := Floor((v0[_Y_] + 1) * FImage.Height / 2.0);
  x1 := Floor((v1[_X_] + 1) * FImage.Width  / 2.0);
  y1 := Floor((v1[_Y_] + 1) * FImage.Height / 2.0);
  x2 := Floor((v2[_X_] + 1) * FImage.Width  / 2.0);
  y2 := Floor((v2[_Y_] + 1) * FImage.Height / 2.0);

  BoundingBox(x0, y0, x1, y1, x2, y2, bx0, by0, bx1, by1);
  for y:=by0 to by1 do
  begin
    for x:=bx0 to bx1 do
    begin
      if (PointInTriangle(x, y, x0, y0, x1, y1, x2, y2)) then
      begin
        if not (FImage.InBounds(x, y)) then Continue;
        Vector3D := Barycentric(x0, y0, x1, y1, x2, y2, x, y);
        if (Vector3D[_X_] < 0) or (Vector3D[_Y_] < 0) or (Vector3D[_Z_] < 0) then
          Continue;

        // Find the Z-value of the pixel by multiplying the
        // triangle vertices with the barycentric coordinates of the pixel.
        z := 0.0;
        z := z + v0[_Z_] * Vector3D[_X_];
        z := z + v1[_Z_] * Vector3D[_Y_];
        z := z + v2[_Z_] * Vector3D[_Z_];

        if (FImage.Depth[x, y] < z) then
        begin
          FImage.Depth[x, y] := z;

          // Retrieve color from texture
          if (FTexture <> nil) then
          begin
            uv[_X_] := 0.0;
            uv[_Y_] := 0.0;
            uv[_X_] := uv[_X_] + FMesh.TexIndex[FIndexVector.TexIndices[0]][_X_] * Vector3D[_X_];
            uv[_X_] := uv[_X_] + FMesh.TexIndex[FIndexVector.TexIndices[1]][_X_] * Vector3D[_Y_];
            uv[_X_] := uv[_X_] + FMesh.TexIndex[FIndexVector.TexIndices[2]][_X_] * Vector3D[_Z_];

            uv[_Y_] := uv[_Y_] + FMesh.TexIndex[FIndexVector.TexIndices[0]][_Y_] * Vector3D[_X_];
            uv[_Y_] := uv[_Y_] + FMesh.TexIndex[FIndexVector.TexIndices[1]][_Y_] * Vector3D[_Y_];
            uv[_Y_] := uv[_Y_] + FMesh.TexIndex[FIndexVector.TexIndices[2]][_Y_] * Vector3D[_Z_];

            Pixel := FTexture.Pixel[Floor(uv[_X_] * FTexture.Width ), 
                                    Floor(uv[_Y_] * FTexture.Height)];
            Color := [Floor(Pixel[_X_] * d), 
                      Floor(Pixel[_Y_] * d), 
                      Floor(Pixel[_Z_] * d), 
                      255];
          end
          else
          begin
            Color := [Floor(FColor[_X_] * d), Floor(FColor[_Y_] * d), Floor(FColor[_Z_] * d), FColor[_W_]];
          end; // if ()

          FImage.Pixel[x, y] := BlendColor(Color, FImage.Pixel[x, y], FColorBlendMode);
        end; // if ()
      end; // if ()
    end; // for
  end; // for
end; // Rasterize()

procedure TRenderer.RenderTriangle(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
begin
  Rasterize(v0, v1, v2);
end; // RenderTriangle()

procedure TRenderer.Render(Mesh : TMesh);
var
  i : Integer;
begin
  FMesh := Mesh;
  for i:=0 to Mesh.IndexCount-1 do
  begin
    FIndexVector := Mesh.Index[i];
    RenderTriangle(
      Mesh.Vertex[FIndexVector.VerIndices[0]],
      Mesh.Vertex[FIndexVector.VerIndices[1]],
      Mesh.Vertex[FIndexVector.VerIndices[2]]
    );
  end; // for i
  FMesh := nil;
end; // Render()

procedure TRenderer.ClearColor(Color : TColor);
begin
  FImage.Fill(Color);
end; // ClearColor()

procedure TRenderer.ClearDepthBuffer;
var
  i : Integer;
begin
  for i:=0 to FImage.Width * FImage.Height - 1  do
  begin
    FImage.DepthAtIdx[i] := -INFINITY;
  end; // for i
end; // ClearDeapthBuffer()

procedure TRenderer.BindTexture(Image : TImage);
begin
  FTexture := Image;
end; // BindTexture()

begin
end.