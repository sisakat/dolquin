unit LinearAlgebra;

{$mode objfpc}{$H+}

interface

const
  _X_ : Integer = 0;
  _Y_ : Integer = 1;
  _Z_ : Integer = 2;
  _W_ : Integer = 3;

type
  TVector3D = array[0..2] of Double;
  TVector4D = array[0..3] of Double;
  TVector3I = array[0..2] of Integer;
  TMatrix4D = array[0..3] of TVector4D;

procedure WriteVector3I(A : TVector3I);
procedure WriteVector3D(A : TVector3D);
function VectorMultiply(A : TVector3D; B : TVector3D   ) : Double;
function VectorMultiply(A : TVector4D; B : TVector4D   ) : Double;
function VectorScale   (A : TVector3D; Scalar : Double ) : TVector3D;
function VectorScale   (A : TVector4D; Scalar : Double ) : TVector4D;

implementation

uses
  Math, SysUtils;
  
procedure WriteVector3I(A : TVector3I);
begin
  WriteLn(Format('(%d, %d, %d)', [A[_X_], A[_Y_], A[_Z_]]));
end; // WriteVector3I()

procedure WriteVector3D(A : TVector3D);
begin
  WriteLn(Format('(%f, %f, %f)', [A[_X_], A[_Y_], A[_Z_]]));
end; // WriteVector3D()

function VectorMultiply(A : TVector3D; B : TVector3D) : Double;
begin
    Result := A[_X_] * B[_X_] + 
              A[_Y_] * B[_Y_] + 
              A[_Z_] * B[_Z_];    
end; // VectorMultiply()

function VectorMultiply(A : TVector4D; B : TVector4D) : Double;
begin
    Result := A[_X_] * B[_X_] + 
              A[_Y_] * B[_Y_] + 
              A[_Z_] * B[_Z_] + 
              A[_W_] * B[_W_];    
end; // VectorMultiply()

function VectorScale(A : TVector3D; Scalar : Double ) : TVector3D;
begin
    Result[_X_] := A[_X_] * Scalar;
    Result[_Y_] := A[_Y_] * Scalar;
    Result[_Z_] := A[_Z_] * Scalar;
end; // VectorScale()

function VectorScale(A : TVector4D; Scalar : Double ) : TVector4D;
begin
    Result[_X_] := A[_X_] * Scalar;
    Result[_Y_] := A[_Y_] * Scalar;
    Result[_Z_] := A[_Z_] * Scalar;
    Result[_W_] := A[_W_] * Scalar;
end; // VectorScale()

begin
end.