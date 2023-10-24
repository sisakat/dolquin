unit Mesh;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra;

type
  TIndexVector = array[0..2] of TVector3I;

type
  TMesh = class(TObject)
  public
  private
    FVertices   : array of TVector3D   ;
    FNormals    : array of TVector3D   ;
    FTexIndices : array of TVector3D   ;
    FIndices    : array of TIndexVector;

    function GetVertex  (i : Integer) : TVector3D   ;
    function GetNormal  (i : Integer) : TVector3D   ;
    function GetTexIndex(i : Integer) : TVector3D   ;
    function GetIndex   (i : Integer) : TIndexVector;

    procedure SetVertex  (i : Integer; v : TVector3D   );
    procedure SetNormal  (i : Integer; v : TVector3D   );
    procedure SetTexIndex(i : Integer; v : TVector3D   );
    procedure SetIndex   (i : Integer; v : TIndexVector);

    function GetVertexCount     : Integer;
    function GetIndexCount      : Integer;
    function GetNormalCount     : Integer;
    function GetTexIndicesCount : Integer;
    
  public
    constructor Create(NumVertices   : Integer;
                       NumIndices    : Integer; 
                       NumNormals    : Integer;
                       NumTexIndices : Integer);
    destructor  Destroy; override;

    property Vertex  [i : Integer]  : TVector3D    read GetVertex   write SetVertex  ;
    property Normal  [i : Integer]  : TVector3D    read GetNormal   write SetNormal  ;
    property TexIndex[i : Integer]  : TVector3D    read GetTexIndex write SetTexIndex;
    property Index   [i : Integer]  : TIndexVector read GetIndex    write SetIndex   ;
    property VertexCount     : Integer read GetVertexCount    ;
    property IndexCount      : Integer read GetIndexCount     ;  
    property NormalCount     : Integer read GetNormalCount    ;
    property TexIndicesCount : Integer read GetTexIndicesCount;
  end;

implementation

uses
  Math, SysUtils;

constructor TMesh.Create(NumVertices : Integer; NumIndices : Integer; NumNormals : Integer; NumTexIndices : Integer);
begin
  inherited Create;
  SetLength(FVertices  , NumVertices  );
  SetLength(FIndices   , NumIndices   );
  SetLength(FNormals   , NumNormals   );
  SetLength(FTexIndices, NumTexIndices);
end; // Create()

destructor TMesh.Destroy;
begin
  inherited;
end; // Destroy()

function TMesh.GetVertex(i : Integer) : TVector3D;
begin
  Result := FVertices[i];
end; // GetVertex()

function TMesh.GetNormal(i : Integer) : TVector3D;
begin
  Result := FNormals[i];
end; // GetNormal()

function TMesh.GetTexIndex(i : Integer) : TVector3D;
begin
  Result := FTexIndices[i];
end; // GetTexIndex()

function TMesh.GetIndex(i : Integer) : TIndexVector;
begin
  Result := FIndices[i];
end; // GetIndex()

procedure TMesh.SetVertex(i : Integer; v : TVector3D);
begin
  FVertices[i] := v;
end; // SetVertex()

procedure TMesh.SetNormal(i : Integer; v : TVector3D);
begin
  FNormals[i] := v;
end; // SetNormal()

procedure TMesh.SetTexIndex(i : Integer; v : TVector3D);
begin
  FTexIndices[i] := v;
end; // SetTexIndex()

procedure TMesh.SetIndex(i : Integer; v : TIndexVector);
begin
  FIndices[i] := v;
end; // SetIndex()

function TMesh.GetVertexCount : Integer;
begin
  Result := Length(FVertices);
end; // GetVertexCount()

function TMesh.GetIndexCount : Integer;
begin
  Result := Length(FIndices);
end; // GetIndexCount()

function TMesh.GetNormalCount : Integer;
begin
  Result := Length(FNormals);
end; // GetNormalCount()

function TMesh.GetTexIndicesCount : Integer;
begin
  Result := Length(FTexIndices);
end; // GetTexIndicesCount()

begin
end.