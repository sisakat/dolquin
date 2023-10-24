unit Graphics;

{$mode objfpc}{$H+}

interface

type
  TColor = Array of Byte;

type
  TPixel    = Array[0..3] of Byte;
  TPixelPtr = ^TPixel            ;

type
  TColorBlendMode = (
    COLOR_BLEND_MODE_NONE, 
    COLOR_BLEND_MODE_ONE_MINUS_ALPHA
  );

type
  TImage = class(TObject)
  private
    FPixels : Array of TPixel;
    FDepth  : Array of Double;
    FWidth  : Integer        ;
    FHeight : Integer        ;

    function  GetLength   : Integer  ;
    function  GetPixelPtr : TPixelPtr;
    procedure SetPixel(Idx : Integer;              Value : TColor); overload;
    procedure SetPixel(w   : Integer; h : Integer; Value : TColor); overload;
    function  GetPixel(Idx : Integer             ) : TColor; overload;
    function  GetPixel(w   : Integer; h : Integer) : TColor; overload;
    function  GetDepth(Idx : Integer             ) : Double; overload;
    function  GetDepth(w   : Integer; h : Integer) : Double; overload;
    procedure SetDepth(Idx : Integer;              Value : Double); overload;
    procedure SetDepth(w   : Integer; h : Integer; Value : Double); overload;
  public
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Fill(Color : TColor);
    function  InBounds(x : Integer; y : Integer) : Boolean;

    property Width      : Integer read FWidth     ;
    property Height     : Integer read FHeight    ;
    property Length     : Integer read GetLength  ;

    property PixelPtr                             : TPixelPtr read GetPixelPtr            ;
    property PixelAtIdx[i : Integer             ] : TColor    read GetPixel write SetPixel;
    property Pixel     [w : Integer; h : Integer] : TColor    read GetPixel write SetPixel;
    property DepthAtIdx[i : Integer             ] : Double    read GetDepth write SetDepth;
    property Depth     [w : Integer; h : Integer] : Double    read GetDepth write SetDepth;
  end;

type
  TPainter = class(TObject)
  private
    FImage          : TImage         ;
    FColorBlendMode : TColorBlendMode;
  public
    constructor Create(Image : TImage);

    procedure DrawRectangle(x  : Integer; y  : Integer; Width  : Integer; Height : Integer; Color : TColor);
    procedure DrawCircle   (x  : Integer; y  : Integer; Radius : Integer;                   Color : TColor);
    procedure DrawLine     (x0 : Integer; y0 : Integer; x1     : Integer; y1     : Integer; Color : TColor);
    procedure DrawTriangle (x0 : Integer; y0 : Integer;
                            x1 : Integer; y1 : Integer;
                            x2 : Integer; y2 : Integer;
                            Color : TColor);

    destructor  Destroy; override;

    property ColorBlendMode : TColorBlendMode read FColorBlendMode write FColorBlendMode;
  end;

function BlendColor(
  ColorTOP : TColor; 
  ColorBOT : TColor; 
  ColorBlendMode : TColorBlendMode = COLOR_BLEND_MODE_NONE
) : TPixel;

implementation

uses
  Math, SysUtils;

// ---------------------------------------
// TImage
// ---------------------------------------

constructor TImage.Create(Width : Integer; Height : Integer);
begin
  inherited Create;
  FWidth      := Width     ;
  FHeight     := Height    ;
  SetLength(FPixels, Width * Height);
  SetLength(FDepth , Width * Height);
end; // Create()

destructor TImage.Destroy;
begin
  inherited;
end; // Destroy()

procedure TImage.Fill(Color : TColor);
var
  i : Integer;
begin
  for i:=0 to Length-1 do
  begin
    PixelAtIdx[i] := Color;
  end; // for
end; // Fill()

function TImage.InBounds(x : Integer; y : Integer) : Boolean;
begin
  Result := (x >= 0) and (x < Width) and (y >= 0) and (y < Height);
end; // InBounds()

function TImage.GetLength : Integer;
begin
  Result := Width * Height;
end; // GetLength()

function TImage.GetPixelPtr : TPixelPtr;
begin
  Result := @FPixels[0];
end;

procedure TImage.SetPixel(Idx : Integer; Value : TColor);
begin
  Move(Value[0], FPixels[Idx], SizeOf(TPixel));
end; // SetPixel()

procedure TImage.SetPixel(w : Integer; h : Integer; Value : TColor);
begin
  Move(Value[0], FPixels[Width * h + w], SizeOf(TPixel));
end; // SetPixel()

function  TImage.GetPixel(Idx : Integer) : TColor;
begin
  Result := FPixels[Idx];
end; // GetPixel()

function TImage.GetPixel(w : Integer; h : Integer) : TColor;
begin
  Result := FPixels[Width * h + w];
end; // GetPixel()

procedure TImage.SetDepth(Idx : Integer; Value : Double);
begin
  FDepth[Idx] := Value;
end; // SetDepth()

procedure TImage.SetDepth(w : Integer; h : Integer; Value : Double);
begin
  SetDepth(Width * h + w, Value);
end; // SetDepth()

function TImage.GetDepth(Idx : Integer) : Double;
begin
  Result := FDepth[Idx];
end; // GetDepth()

function TImage.GetDepth(w : Integer; h : Integer) : Double;
begin
  Result := GetDepth(Width * h + w);
end; // GetDepth()

// ---------------------------------------
// TPainter
// ---------------------------------------

constructor TPainter.Create(Image : TImage);
begin
  inherited Create;
  FImage := Image;
  FColorBlendMode := COLOR_BLEND_MODE_ONE_MINUS_ALPHA;
end; // Create()

destructor TPainter.Destroy;
begin
  inherited;
end; // Destroy()

procedure TPainter.DrawRectangle(x : Integer; y : Integer; Width : Integer; Height : Integer; Color : TColor);
var
  dy, dx : Integer;
begin
  for dy:=0 to Height-1 do
  begin
    if y + dy >= FImage.Height then Break;
    for dx:=0 to Width-1 do
    begin
      if x + dx >= FImage.Width then Break;
      FImage.Pixel[x + dx, y + dy] := BlendColor(Color, FImage.Pixel[x + dx, y + dy], FColorBlendMode);
    end; // for
  end; // for
end; // DrawRect()

procedure TPainter.DrawCircle(x : Integer; y : Integer; Radius : Integer; Color : TColor);
var
  dy, dx : Integer;
begin
  for dy:=-Radius to Radius-1 do
  begin
    if (y + dy >= FImage.Height) or (y + dy < 0) then Continue;
    for dx:=-Radius to Radius-1 do
      begin
        if (x + dx >= FImage.Width) or (x + dx < 0) then Continue;
        if (dx * dx + dy * dy <= Radius * Radius) then
        begin
          FImage.Pixel[x + dx, y + dy] := BlendColor(Color, FImage.Pixel[x + dx, y + dy], FColorBlendMode);
        end; // if ()
      end; // for
  end; // for
end; // DrawCircle()

procedure TPainter.DrawLine(x0 : Integer; y0 : Integer; x1 : Integer; y1 : Integer; Color : TColor);
  procedure Swap(var a : Integer; var b : Integer);
  var
    c : Integer;
  begin
    c := a;
    a := b;
    b := c;
  end;
var
  i : Integer;
  y : Integer;
  k : Double ;
  d : Double ;
  s : Integer;
  t : Integer;
  Swapped : Boolean;
begin
  // Check if there is no movement in x (k = 0)
  if (x1 - x0 = 0) then
  begin
    for y:=y0 to y1-1 do
    begin
      if not FImage.InBounds(x0, y) then Continue;
      FImage.PixelAtIdx[x0, y] := BlendColor(Color, FImage.Pixel[x0, y], FColorBlendMode);
    end;
  end
  else
  begin
    Swapped := FALSE;

    // Parameters for regular line equation
    k := (y1 - y0) / (x1 - x0);
    d := y0 - k * x0;

    // Reverse the direction if specified in wrong order
    if (x0 > x1) then
    begin
      Swap(x0, x1);
      Swapped := TRUE;
    end;

    if (y0 > y1) then
    begin
     Swap(y0, y1);
    end;

    // Output for debug   
    // WriteLn(Format('x0=%d', [x0]));
    // WriteLn(Format('x1=%d', [x1]));
    // WriteLn(Format('y0=%d', [y0]));
    // WriteLn(Format('y1=%d', [y1]));
    // WriteLn(Format('k=%f', [k]));
    // WriteLn(Format('d=%f', [k]));
    
    // Sign change when we have negative slope
    s := 1;
    if (k < 0) then s := -1;

    for i:=x0 to x1-1 do
    begin
      t := Max((Round(k * (i + 1) + d)-s) * s, Round(k * i + d) * s);
      for y:=Round(k * i + d) * s to t do 
      begin
        if not FImage.InBounds(i, y * s) then Continue;
        FImage.PixelAtIdx[i, y * s] := BlendColor(Color, FImage.Pixel[i, y * s], FColorBlendMode);
      end; // for
    end; // for
  end; // if ()
end; // DrawLine()

procedure TPainter.DrawTriangle (
  x0 : Integer; y0 : Integer;
  x1 : Integer; y1 : Integer;
  x2 : Integer; y2 : Integer;
  Color : TColor);
  
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
  x  , y             : Integer;
  bx0, bx1, by0, by1 : Integer;
begin
  BoundingBox(x0, y0, x1, y1, x2, y2, bx0, by0, bx1, by1);
  for y:=by0 to by1 do
  begin
    for x:=bx0 to bx1 do
    begin
      if (PointInTriangle(x, y, x0, y0, x1, y1, x2, y2)) then
      begin
        if not (FImage.InBounds(x, y)) then Continue;
        FImage.PixelAtIdx[x, y] := BlendColor(Color, FImage.Pixel[x, y], FColorBlendMode);
      end; // if ()
    end; // for
  end; // for
end; // DrawTriangle()

// ---------------------------------------
// Utilities
// ---------------------------------------

function BlendColor(
  ColorTOP : TColor;
  ColorBOT : TColor;
  ColorBlendMode : TColorBlendMode
) : TPixel;
var
  Alpha : Double;
begin
  case ColorBlendMode of
    COLOR_BLEND_MODE_NONE:
    begin
      Result[0] := ColorTOP[0]; 
      Result[1] := ColorTOP[1];
      Result[2] := ColorTOP[2];
      Result[3] := 255;
    end;
    COLOR_BLEND_MODE_ONE_MINUS_ALPHA:
    begin
      Alpha := ColorTOP[3] / 255.0;
      Result[0] := Floor(ColorTOP[0] * Alpha) + Floor(ColorBOT[0] * (1.0 - Alpha));
      Result[1] := Floor(ColorTOP[1] * Alpha) + Floor(ColorBOT[1] * (1.0 - Alpha));
      Result[2] := Floor(ColorTOP[2] * Alpha) + Floor(ColorBOT[2] * (1.0 - Alpha));
      Result[3] := Max(ColorTOP[3], ColorBOT[3]);
    end; 
  end;
end; // BlendColor()

begin
end.