unit RendererUnit;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra, Mesh, Graphics;

type
  TCamera = class(TObject)
  private
    FEye    : TVector3D;
    FCenter : TVector3D;
    FUp     : TVector3D;

    function GetViewMatrix : TMatrix4D;
  public
    constructor Create;
    destructor  Destroy; override;

    property ViewMatrix : TMatrix4D read GetViewMatrix;
    property Eye        : TVector3D read FEye write FEye;
  end;

type
  TRenderer = class(TObject)
  private
    FCamera          : TCamera        ;
    FImage           : TRenderImage   ;
    FColor           : TColor         ;
    FColorBlendMode  : TColorBlendMode;
    FTexture         : TImage         ;
    FIndexVector     : TIndexVector   ;
    FMesh            : TMesh          ;
    FDepthTest       : Boolean        ;
    FLighting        : Boolean        ;
    FBackfaceCulling : Boolean        ;

    FModelMatrix     : TMatrix4D      ;
    FViewMatrix      : TMatrix4D      ;
    FProjMatrix      : TMatrix4D      ;
    FViewportMatrix  : TMatrix4D      ;

    FViewportWidth   : Integer        ;
    FViewportHeight  : Integer        ;

  protected
    procedure Rasterize     (v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
    procedure RenderTriangle(v0 : TVector4D; v1 : TVector4D; v2 : TVector4D);
    
  public   
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Render          (Mesh : TMesh  );
    procedure ClearColor      (Color : TColor);
    procedure ClearDepthBuffer(              );
    procedure BindTexture     (Image : TImage);
    procedure Viewport        (X : Integer; Y : Integer; Width : Integer; Height : Integer);
    procedure PerspectiveProj (NearPlane : Double; FarPlane : Double; FieldOfView : Double);
    procedure ModelMatrix     (M : TMatrix4D );

    property Camera : TCamera read FCamera;
    
    // The rendered image (including depth-buffer)
    property Image : TRenderImage read FImage; 

    // Color blending mode when using textures or colors with alpha values.
    property ColorBlendMode  : TColorBlendMode read FColorBlendMode  write FColorBlendMode;
    
    // The base color to use when rendering a primitive.
    property DrawColor : TColor read FColor write FColor;

    // Writing into and checking the depth-buffer when rendering triangles.
    property DepthTest : Boolean read FDepthTest write FDepthTest;

    // Light calculations for shaded rendering based on the vertex normal.
    property Lighting : Boolean read FLighting write FLighting;
    
    // A given triangle that is facing away from the camera will not be rendered.
    property BackfaceCulling : Boolean read FBackfaceCulling write FBackfaceCulling;
  end;

implementation

uses
  Math, SysUtils;

constructor TCamera.Create;
begin
  inherited;
  FEye    := Z_AXIS        ;
  FCenter := ZERO_VECTOR_3D;
  FUp     := Y_AXIS        ;
end; // Create()

destructor TCamera.Destroy;
begin
  inherited;
end; // Destroy()

function TCamera.GetViewMatrix : TMatrix4D;
begin
  Result := LookAt(FEye, FCenter, FUp);
end; // GetViewMatrix()

constructor TRenderer.Create(Width : Integer; Height : Integer);
var
  i : Integer;
begin
  inherited Create;
  FCamera          := TCamera.Create;
  FImage           := TRenderImage.Create(Width, Height);
  FColor           := [255, 255, 255, 255];
  FColorBlendMode  := COLOR_BLEND_MODE_NONE;
  FDepthTest       := TRUE;
  FLighting        := TRUE;
  FBackfaceCulling := TRUE;
  FTexture         := nil;
  FModelMatrix     := IdentityMatrix4D;
  FViewMatrix      := IdentityMatrix4D;
  FProjMatrix      := IdentityMatrix4D;
  FViewportMatrix  := IdentityMatrix4D;
  FViewMatrix      := FCamera.ViewMatrix;
  ClearDepthBuffer;
  ClearColor([0, 0, 0, 255]);
end; // Create()

destructor TRenderer.Destroy;
begin
  inherited;
  FreeAndNil(FCamera);
end; // Destroy()

procedure TRenderer.Rasterize(v0 : TVector3D; v1 : TVector3D; v2 : TVector3D);
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
  if Lighting or BackfaceCulling then
  begin
    Vector3D := VectorCross    (VectorSubtract(v0, v1), VectorSubtract(v0, v2));
    Vector3D := VectorNormalize(Vector3D);
    d        := VectorMultiply (Vector3D, Z_AXIS);
  end
  else
    d := 1.0;
  
  if BackfaceCulling and (d < 0.0) then Exit;
  if not Lighting then d := 1.0;

  x0 := Floor(v0[_X_]);
  y0 := Floor(v0[_Y_]);
  x1 := Floor(v1[_X_]);
  y1 := Floor(v1[_Y_]);
  x2 := Floor(v2[_X_]);
  y2 := Floor(v2[_Y_]);

  BoundingBox(x0, y0, x1, y1, x2, y2, bx0, by0, bx1, by1);
  for y:=by0 to by1 do
  begin
    for x:=bx0 to bx1 do
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

      if (FImage.Depth[x, y] > z) or not DepthTest then
      begin
        if DepthTest then 
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

        if (FColorBlendMode = COLOR_BLEND_MODE_NONE) then
          FImage.Pixel[x, y] := Color
        else
          FImage.Pixel[x, y] := BlendColor(Color, FImage.Pixel[x, y], FColorBlendMode);
      end; // if ()
    end; // for
  end; // for
end; // Rasterize()

procedure TRenderer.RenderTriangle(v0 : TVector4D; v1 : TVector4D; v2 : TVector4D);
begin
  v0 := MatrixMultiply(MatrixMultiply(MatrixMultiply(MatrixMultiply(FModelMatrix, FViewMatrix), FProjMatrix), FViewportMatrix), v0);
  v1 := MatrixMultiply(MatrixMultiply(MatrixMultiply(MatrixMultiply(FModelMatrix, FViewMatrix), FProjMatrix), FViewportMatrix), v1);
  v2 := MatrixMultiply(MatrixMultiply(MatrixMultiply(MatrixMultiply(FModelMatrix, FViewMatrix), FProjMatrix), FViewportMatrix), v2);
  Rasterize(To3D(v0), To3D(v1), To3D(v2));
end; // RenderTriangle()

procedure TRenderer.Render(Mesh : TMesh);
var
  i : Integer;
begin
  FViewMatrix := FCamera.ViewMatrix;
  FMesh := Mesh;
  for i:=0 to Mesh.IndexCount-1 do
  begin
    FIndexVector := Mesh.Index[i];
    RenderTriangle(
      To4D(Mesh.Vertex[FIndexVector.VerIndices[0]]),
      To4D(Mesh.Vertex[FIndexVector.VerIndices[1]]),
      To4D(Mesh.Vertex[FIndexVector.VerIndices[2]])
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
  for i:=0 to FImage.Width * FImage.Height - 1 do
  begin
    FImage.DepthAtIdx[i] := 255.0;
  end; // for i
end; // ClearDeapthBuffer()

procedure TRenderer.BindTexture(Image : TImage);
begin
  FTexture := Image;
end; // BindTexture()

procedure TRenderer.Viewport(X : Integer; Y : Integer; Width : Integer; Height : Integer);
const
  Depth : Double = 255.0;
begin
  FViewportWidth       := Width           ;
  FViewportHeight      := Height          ;
  FViewportMatrix      := IdentityMatrix4D;
  FViewportMatrix[0,0] :=     Width  / 2.0;
  FViewportMatrix[1,1] :=     Height / 2.0;
  FViewportMatrix[0,3] := X + Width  / 2.0;
  FViewportMatrix[1,3] := Y + Height / 2.0;
  FViewportMatrix[2,2] :=     Depth  / 2.0;
  FViewportMatrix[2,3] :=     Depth  / 2.0;
  WriteLn(Format('W: %d H: %d', [Width, Height]));
end; // Viewport()

procedure TRenderer.PerspectiveProj(NearPlane : Double; FarPlane : Double; FieldOfView : Double);
var
  e : Double;
begin
  e := 1.0 / Tan(DegToRad(FieldOfView / 2.0));
  FProjMatrix := IdentityMatrix4D;
  FProjMatrix[0,0] := e;
  FProjMatrix[1,1] := e / (FViewportHeight / FViewportWidth);
  FProjMatrix[2,2] := -(    FarPlane + NearPlane) / (FarPlane - NearPlane);
  FProjMatrix[2,3] := -(2 * FarPlane * NearPlane) / (FarPlane - NearPlane);
  FProjMatrix[3,2] := -1.0;
  FProjMatrix[3,3] := 0.0;
end; // PerspectiveProj()

procedure TRenderer.ModelMatrix(M : TMatrix4D );
begin
  FModelMatrix := M;
end; // ModelMatrix()

begin
end.