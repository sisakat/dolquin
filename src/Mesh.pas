unit Mesh;

{$mode objfpc}{$H+}

interface

uses LinearAlgebra;

type
  TMesh = class(TObject)
  private
    FVertices : array of TVector3D;
    FIndices  : array of TVector3I;

    function GetVertex(i : Integer) : TVector3D;
    function GetIndex (i : Integer) : TVector3I;

    procedure SetVertex(i : Integer; v : TVector3D);
    procedure SetIndex (i : Integer; v : TVector3I);

    function GetVertexCount : Integer;
    function GetIndexCount  : Integer;
    
  public
    constructor Create(NumVertices : Integer; NumIndices : Integer);
    destructor  Destroy; override;

    property Vertex[i : Integer] : TVector3D read GetVertex write SetVertex;
    property Index [i : Integer] : TVector3I read GetIndex  write SetIndex ;
    property VertexCount : Integer read GetVertexCount;
    property IndexCount  : Integer read GetIndexCount;
  end;

implementation

uses
  Math, SysUtils;

constructor TMesh.Create(NumVertices : Integer; NumIndices : Integer);
begin
  inherited Create;

  SetLength(FVertices, NumVertices);
  SetLength(FIndices , NumIndices );
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

procedure TMesh.SetVertex(i : Integer; v : TVector3D);
begin
  FVertices[i] := v;
end; // SetVertex()

procedure TMesh.SetIndex(i : Integer; v : TVector3I);
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

begin
end.