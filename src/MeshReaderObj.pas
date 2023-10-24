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
  i, j                : Integer;
  StringList          : TStringList;
  Line                : String;
  StringArray         : TStringArray;
  VertexTextureNormal : TStringArray;
  Vector3D            : TVector3D;
  IndexVector         : TIndexVector;
  VertexCount         : Integer;
  IndexCount          : Integer;
  NormalCount         : Integer;
  TexIndexCount       : Integer;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);

    VertexCount   := 0;
    IndexCount    := 0;
    NormalCount   := 0;
    TexIndexCount := 0;
    
    for i:=0 to StringList.Count-1 do
    begin
      Line := StringList[i];
      Line := StringReplace(Line, '  ', ' ', [rfReplaceAll]);

      if (Pos('v ', Line) > 0) then
      begin
        // Vertex data
        VertexCount := VertexCount + 1; 
      end
      else
      if (Pos('vn ', Line) > 0) then
      begin
        NormalCount := NormalCount + 1;
      end
      else
      if (Pos('vt ', Line) > 0) then
      begin
        TexIndexCount := TexIndexCount + 1;
      end
      else
      if (Pos('f ', Line) > 0) then
      begin
        // Index data
        IndexCount  := IndexCount  + 1;
      end; // if ()
    end; // for i

    FMesh := TMesh.Create(VertexCount, IndexCount, NormalCount, TexIndexCount);

    VertexCount   := 0;
    IndexCount    := 0;
    NormalCount   := 0;
    TexIndexCount := 0;

    for i:=0 to StringList.Count-1 do
    begin
      Line := StringList[i];
      Line := StringReplace(Line, '  ', ' ', [rfReplaceAll]);
      if (Pos('v ', Line) > 0) then
      begin
        // Vertex data
        StringArray := Line.Substring(2).Split(' ');
        Vector3D[_X_] := StrToFloat(StringArray[_X_]);
        Vector3D[_Y_] := StrToFloat(StringArray[_Y_]);
        Vector3D[_Z_] := StrToFloat(StringArray[_Z_]);
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
      if (Pos('vt ', Line) > 0) then
      begin 
        // Texture Index data
        StringArray := Line.Substring(3).Split(' ');
        Vector3D[_X_] := StrToFloat(StringArray[_X_]);
        Vector3D[_Y_] := StrToFloat(StringArray[_Y_]);
        Vector3D[_Z_] := 0.0;
        FMesh.TexIndex[TexIndexCount] := Vector3D;
        TexIndexCount := TexIndexCount + 1;
      end
      else
      if (Pos('f ', Line) > 0) then
      begin
        // Index data
        StringArray := Line.Substring(2).Split(' ');
        IndexVector[0][0] := 0;
        IndexVector[0][1] := 0;
        IndexVector[0][2] := 0;

        for j:=0 to 2 do
        begin
          VertexTextureNormal := StringArray[j].Split('/');
          IndexVector[0][j] := StrToInt(VertexTextureNormal[0]) - 1;

          if (Length(VertexTextureNormal) > 1) and (VertexTextureNormal[1] <> '') then
            IndexVector[1][j] := StrToInt(VertexTextureNormal[1]) - 1
          else
            IndexVector[1][j] := 0;

          if (Length(VertexTextureNormal) > 2) and (VertexTextureNormal[2] <> '') then
            IndexVector[2][j] := StrToInt(VertexTextureNormal[2]) - 1
          else
            IndexVector[2][j] := 0;
        end; // for j

        FMesh.Index[IndexCount] := IndexVector;
        IndexCount := IndexCount + 1;
      end; // if ()
    end; // for i
  finally
    FreeAndNil(StringList);
  end; // try..finally
end; // Load()

begin
end.