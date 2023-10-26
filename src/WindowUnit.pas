unit WindowUnit;

{$mode objfpc}{$H+}
{$link window.o}
{$linklib c}
{$linklib glfw}
{$linklib GL}
{$linklib GLEW}

interface

uses
  ctypes, Graphics;
  
function window_init(  
  Buffer : pcchar;
  Width  : cint;
  Height : cint
) : cint; cdecl; external;

function window_update() : cint; cdecl; external;
procedure window_destroy(); cdecl; external;

procedure ShowWindow(Image : TImage);
procedure CloseWindow;
function UpdateWindow : Boolean;

implementation

uses
  Classes, Contnrs, SysUtils;

procedure ShowWindow(Image : TImage);
begin
  window_init(pcchar(Image.PixelPtr), Image.Width, Image.Height);
end; // ShowWindow()

procedure CloseWindow;
begin
  window_destroy;
end; // CloseWindow()

function UpdateWindow : Boolean;
begin
  Result := window_update = 1;
end; // UpdateWindow()

begin
end.