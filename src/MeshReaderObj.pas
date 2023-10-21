unit MeshReaderObj;

{$mode objfpc}{$H+}

interface

uses
  Mesh;

type
  TObjReader = class(TObject)
  private
    FMesh : TMesh;

  public
    constructor Create;
    destructor  Destroy; override;

    procedure Load(FileName : String);

    property Mesh : TMesh read FMesh;
  end;

implementation

uses
  Classes, Math, SysUtils, LinearAlgebra;

constructor TObjReader.Create;
begin
    inherited;
    FMesh := nil;
end; // Create()

destructor TObjReader.Destroy;
begin
    inherited;
    if (FMesh <> nil) then
      FreeAndNil(FMesh);
end; // Destroy()

procedure TObjReader.Load(FileName : String);
var
  i            : Integer;
  StringList   : TStringList;
  Line         : String;
  StringArray  : TStringArray;
  Vector3D     : TVector3D;
  Vector3I     : TVector3I;
  VertexCount  : Integer;
  IndexCount   : Integer;
  NormalCount  : Integer;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);

    VertexCount := 0;
    IndexCount  := 0;
    
    for i:=0 to StringList.Count-1 do
    begin
      Line := StringList[i];
      if (Pos('v ', Line) > 0) then
      begin
        // Vertex data
        VertexCount := VertexCount + 1;
      end
      else
      if (Pos('f ', Line) > 0) then
      begin
        // Index data
        IndexCount := IndexCount + 1;
      end; // if ()
    end; // for i

    FMesh := TMesh.Create(VertexCount, IndexCount);

    VertexCount := 0;
    IndexCount  := 0;
    NormalCount := 0;

    for i:=0 to StringList.Count-1 do
    begin
      Line := StringList[i];
      Line := StringReplace(Line, '  ', ' ', [rfReplaceAll]);
      if (Pos('v ', Line) > 0) then
      begin
        // Vertex data
        StringArray := Line.Substring(2).Split(' ');
        Vector3D[_X_] := StrToFloat(StringArray[_X_]) * -1;
        Vector3D[_Y_] := StrToFloat(StringArray[_Y_]) * -1;
        Vector3D[_Z_] := StrToFloat(StringArray[_Z_]) * -1;
        FMesh.Vertex[VertexCount] := Vector3D;
        VertexCount := VertexCount + 1;
      end
      else
      if (Pos('vn ', Line) > 0) then
      begin
        // Normal data
        StringArray := Line.Substring(3).Split(' ');
        Vector3D[_X_] := StrToFloat(StringArray[_X_]);
        Vector3D[_Y_] := StrToFloat(StringArray[_Y_]);
        Vector3D[_Z_] := StrToFloat(StringArray[_Z_]);
        FMesh.Normal[NormalCount] := Vector3D;
        NormalCount := NormalCount + 1;
      end
      else
      if (Pos('f ', Line) > 0) then
      begin
        // Index data
        StringArray := Line.Substring(2).Split(' ');
        Vector3I[_X_] := StrToInt(StringArray[0].Split('/')[0]) - 1;
        Vector3I[_Y_] := StrToInt(StringArray[1].Split('/')[0]) - 1;
        Vector3I[_Z_] := StrToInt(StringArray[2].Split('/')[0]) - 1;
        FMesh.Index[IndexCount] := Vector3I;
        IndexCount := IndexCount + 1;
      end; // if ()
    end; // for i
  finally
    FreeAndNil(StringList);
  end; // try..finally
end; // Load()

begin
end.