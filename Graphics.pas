unit Graphics;

{$mode objfpc}{$H+}

interface

type
  TColor = Array of Byte;

type
  TPixel = Array[0..3] of Byte;

type
  TImage = class(TObject)
  private
    FPixels : Array of TPixel;
    FWidth  : Integer        ;
    FHeight : Integer        ;

    function  GetLength : Integer;
    procedure SetPixel(Idx : Integer;              Value : TColor); overload;
    procedure SetPixel(w   : Integer; h : Integer; Value : TColor); overload;
    function  GetPixel(Idx : Integer             ) : TColor; overload;
    function  GetPixel(w   : Integer; h : Integer) : TColor; overload;
  public
    constructor Create(Width : Integer; Height : Integer);
    destructor  Destroy; override;

    procedure Fill(Color : TColor);
    function  InBounds(x : Integer; y : Integer) : Boolean;

    property Width      : Integer read FWidth     ;
    property Height     : Integer read FHeight    ;
    property Length     : Integer read GetLength  ;

    property PixelAtIdx[i : Integer             ] : TColor read GetPixel write SetPixel;
    property Pixel     [w : Integer; h : Integer] : TColor read GetPixel write SetPixel;
  end;

type
  TPainter = class(TObject)
  private
    FImage : TImage;
  public
    constructor Create(Image : TImage);

    procedure DrawRect  (x  : Integer; y  : Integer; Width  : Integer; Height : Integer; Color : TColor);
    procedure DrawCircle(x  : Integer; y  : Integer; Radius : Integer;                   Color : TColor);
    procedure DrawLine  (x0 : Integer; y0 : Integer; x1     : Integer; y1     : Integer; Color : TColor);

    destructor  Destroy; override;
  end;

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

// ---------------------------------------
// TPainter
// ---------------------------------------

constructor TPainter.Create(Image : TImage);
begin
  inherited Create;
  FImage := Image;
end; // Create()

destructor TPainter.Destroy;
begin
  inherited;
end; // Destroy()

procedure TPainter.DrawRect(x : Integer; y : Integer; Width : Integer; Height : Integer; Color : TColor);
var
  dy, dx : Integer;
begin
  for dy:=0 to Height-1 do
  begin
    if y + dy >= FImage.Height then Break;
    for dx:=0 to Width-1 do
    begin
      if x + dx >= FImage.Width then Break;
      FImage.Pixel[x + dx, y + dy] := Color;
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
          FImage.Pixel[x + dx, y + dy] := Color;
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
begin
  // Check if there is no movement in x (k = 0)
  if (x1 - x0 = 0) then
  begin
    for y:=y0 to y1-1 do
    begin
      if not FImage.InBounds(x0, y) then Continue;
      FImage.PixelAtIdx[x0, y] := Color;
    end;
  end
  else
  begin
    // Reverse the direction if specified in wrong order
    if (x0 > x1) and (y0 > y1) then
    begin
      Swap(x0, x1);
      Swap(y0, y1);
    end;

    // Parameters for regular line equation
    k := (y1 - y0) / (x1 - x0);
    d := y0 - k * x0;

    // Sign change when we have negative slope
    s := 1;
    if (k < 0) then s := -1;

    for i:=x0 to x1-1 do
    begin
      for y:=Round(k * i + d) * s to (Round(k * (i + 1) + d)-s) * s do 
      begin
        if not FImage.InBounds(i, y * s) then Continue;
        FImage.PixelAtIdx[i, y * s] := Color;
      end; // for
    end; // for
  end; // if ()
end; // DrawLine()

begin
end.