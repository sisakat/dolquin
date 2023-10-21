unit Mesh;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra;

type
  TMesh = class(TObject)
  private
    FVertices   : array of TVector3D;
    FIndices    : array of TVector3I;
    FNormals    : array of TVector3D;
    FTexIndices : array of TVector3D;

    function GetVertex  (i : Integer) : TVector3D;
    function GetIndex   (i : Integer) : TVector3I;
    function GetNormal  (i : Integer) : TVector3D;
    function GetTexIndex(i : Integer) : TVector3D;

    procedure SetVertex  (i : Integer; v : TVector3D);
    procedure SetIndex   (i : Integer; v : TVector3I);
    procedure SetNormal  (i : Integer; v : TVector3D);
    procedure SetTexIndex(i : Integer; v : TVector3D);

    function GetVertexCount   : Integer;
    function GetIndexCount    : Integer;
    
  public
    constructor Create(NumVertices : Integer; NumIndices : Integer);
    destructor  Destroy; override;

    property Vertex  [i : Integer]  : TVector3D read GetVertex   write SetVertex  ;
    property Index   [i : Integer]  : TVector3I read GetIndex    write SetIndex   ;
    property Normal  [i : Integer]  : TVector3D read GetNormal   write SetNormal  ;
    property TexIndex[i : Integer]  : TVector3D read GetTexIndex write SetTexIndex;
    property VertexCount   : Integer read GetVertexCount  ;
    property IndexCount    : Integer read GetIndexCount   ;  
  end;

implementation

uses
  Math, SysUtils;

constructor TMesh.Create(NumVertices : Integer; NumIndices : Integer);
begin
  inherited Create;

  SetLength(FVertices  , NumVertices);
  SetLength(FIndices   , NumIndices );
  SetLength(FNormals   , NumIndices * 3);
  SetLength(FTexIndices, NumIndices * 3);
end; // Create()

destructor TMesh.Destroy;
begin
  inherited;
end; // Destroy()

function TMesh.GetVertex(i : Integer) : TVector3D;
begin
  Result := FVertices[i];
end; // GetVertex()

function TMesh.GetIndex(i : Integer) : TVector3I;
begin
  Result := FIndices[i];
end; // GetIndex()

function TMesh.GetNormal(i : Integer) : TVector3D;
begin
  Result := FNormals[i];
end; // GetNormal()

function TMesh.GetTexIndex(i : Integer) : TVector3D;
begin
  Result := FTexIndices[i];
end; // GetTexIndex()

procedure TMesh.SetVertex(i : Integer; v : TVector3D);
begin
  FVertices[i] := v;
end; // SetVertex()

procedure TMesh.SetIndex(i : Integer; v : TVector3I);
begin
  FIndices[i] := v;
end; // SetIndex()

procedure TMesh.SetNormal(i : Integer; v : TVector3D);
begin
  FNormals[i] := v;
end; // SetNormal()

procedure TMesh.SetTexIndex(i : Integer; v : TVector3D);
begin
  FTexIndices[i] := v;
end; // SetTexIndex()

function TMesh.GetVertexCount : Integer;
begin
  Result := Length(FVertices);
end; // GetVertexCount()

function TMesh.GetIndexCount : Integer;
begin
  Result := Length(FIndices);
end; // GetIndexCount()

begin
end.