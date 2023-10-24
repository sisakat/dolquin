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
  TMatrix3D = array[0..2] of TVector3D;
  TMatrix4D = array[0..3] of TVector4D;

const
  IdentityMatrix3D : TMatrix3D = ((1, 0, 0),
                                  (0, 1, 0),
                                  (0, 0, 1));
  IdentityMatrix4D : TMatrix4D = ((1, 0, 0, 0),
                                  (0, 1, 0, 0),
                                  (0, 0, 1, 0),
                                  (0, 0, 0, 1));

procedure WriteVector3I (A : TVector3I);
procedure WriteVector3D (A : TVector3D);
procedure WriteVector4D (A : TVector4D);
procedure WriteMatrix4D (A : TMatrix4D);

function VectorSubtract (A : TVector3D; B : TVector3D   ) : TVector3D; overload;
function VectorSubtract (A : TVector4D; B : TVector4D   ) : TVector4D; overload;
function VectorMultiply (A : TVector3D; B : TVector3D   ) : Double; overload;
function VectorMultiply (A : TVector4D; B : TVector4D   ) : Double; overload;
function VectorScale    (A : TVector3D; Scalar : Double ) : TVector3D; overload;
function VectorScale    (A : TVector4D; Scalar : Double ) : TVector4D; overload;
function VectorNormalize(A : TVector3D) : TVector3D; overload;
function VectorNormalize(A : TVector4D) : TVector4D; overload;
function VectorLength   (A : TVector3D) : Double; overload;
function VectorLength   (A : TVector4D) : Double; overload;
function VectorCross    (A : TVector3D; B : TVector3D) : TVector3D; overload;
function VectorCross    (A : TVector4D; B : TVector4D) : TVector4D; overload;
function To3D           (A : TVector4D) : TVector3D;
function To4D           (A : TVector3D) : TVector4D;

function MatrixMultiply (A : TMatrix3D; B : TMatrix3D) : TMatrix3D; overload;
function MatrixMultiply (A : TMatrix4D; B : TMatrix4D) : TMatrix4D; overload;
function MatrixMultiply (A : TMatrix4D; V : TVector4D) : TVector4D; overload;

implementation

uses
  Math, SysUtils;
  
procedure WriteVector3I(A : TVector3I);
begin
  WriteLn(Format('[%d, %d, %d]', [A[_X_], A[_Y_], A[_Z_]]));
end; // WriteVector3I()

procedure WriteVector3D(A : TVector3D);
begin
  WriteLn(Format('[%.3f, %.3f, %.3f]', [A[_X_], A[_Y_], A[_Z_]]));
end; // WriteVector3D()

procedure WriteVector4D(A : TVector4D);
begin
  WriteLn(Format('[%.3f %.3f %.3f %.3f]', [A[_X_], A[_Y_], A[_Z_], A[_W_]]));
end; // WriteVector4D()

procedure WriteMatrix4D (A : TMatrix4D);
begin
  WriteLn(Format('[%.3f %.3f %.3f %.3f]', [A[0,0], A[0,1], A[0,2], A[0,3]]));
  WriteLn(Format('[%.3f %.3f %.3f %.3f]', [A[1,0], A[1,1], A[1,2], A[1,3]]));
  WriteLn(Format('[%.3f %.3f %.3f %.3f]', [A[2,0], A[2,1], A[2,2], A[2,3]]));
  WriteLn(Format('[%.3f %.3f %.3f %.3f]', [A[3,0], A[3,1], A[3,2], A[3,3]]));
end; // WriteMatrix4D()

function VectorSubtract(A : TVector3D; B : TVector3D) : TVector3D;
begin
  Result[_X_] := A[_X_] - B[_X_];
  Result[_Y_] := A[_Y_] - B[_Y_];
  Result[_Z_] := A[_Z_] - B[_Z_];
end; // VectorSubtract()

function VectorSubtract(A : TVector4D; B : TVector4D) : TVector4D;
begin
  Result[_X_] := A[_X_] - B[_X_];
  Result[_Y_] := A[_Y_] - B[_Y_];
  Result[_Z_] := A[_Z_] - B[_Z_];
  Result[_W_] := A[_W_] - B[_W_];
end; // VectorSubtract()

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

function VectorNormalize(A : TVector3D) : TVector3D;
var
  Length : Double;
begin
  Length := VectorLength(A);
  Result[_X_] := A[_X_] / Length;
  Result[_Y_] := A[_Y_] / Length;
  Result[_Z_] := A[_Z_] / Length;
end; // VectorNormalize()

function VectorNormalize(A : TVector4D) : TVector4D;
var
  Length : Double;
begin
  Length := VectorLength(A);
  Result[_X_] := A[_X_] / Length;
  Result[_Y_] := A[_Y_] / Length;
  Result[_Z_] := A[_Z_] / Length;
  Result[_W_] := A[_W_] / Length;
end; // VectorNormalize()

function VectorLength(A : TVector3D) : Double;
begin
  Result := Sqrt(A[_X_] * A[_X_] + 
                 A[_Y_] * A[_Y_] + 
                 A[_Z_] * A[_Z_]);
end; // VectorLength()

function VectorLength(A : TVector4D) : Double;
begin
  Result := Sqrt(A[_X_] * A[_X_] + 
                 A[_Y_] * A[_Y_] + 
                 A[_Z_] * A[_Z_] + 
                 A[_W_] * A[_W_]);
end; // VectorLength()

function VectorCross(A : TVector3D; B : TVector3D) : TVector3D;
begin
  Result[_X_] := A[_Y_] * B[_Z_] - A[_Z_] * B[_Y_];
  Result[_Y_] := A[_Z_] * B[_X_] - A[_X_] * B[_Z_];
  Result[_Z_] := A[_X_] * B[_Y_] - A[_Y_] * B[_X_];
end; // VectorCross()

function VectorCross(A : TVector4D; B : TVector4D) : TVector4D;
begin
  Result[_X_] := A[_Y_] * B[_Z_] - A[_Z_] * B[_Y_];
  Result[_Y_] := A[_Z_] * B[_X_] - A[_X_] * B[_Z_];
  Result[_Z_] := A[_X_] * B[_Y_] - A[_Y_] * B[_X_];
  Result[_W_] := A[_W_];
end; // VectorCross()

function To3D(A : TVector4D) : TVector3D;
begin
  Result[_X_] := A[_X_] / A[_W_];
  Result[_Y_] := A[_Y_] / A[_W_];
  Result[_Z_] := A[_Z_] / A[_W_];
end; // To3D()

function To4D(A : TVector3D) : TVector4D;
begin
  Result[_X_] := A[_X_];
  Result[_Y_] := A[_Y_];
  Result[_Z_] := A[_Z_];
  Result[_W_] := 1.0   ;
end; // To4D()

function MatrixMultiply(A : TMatrix3D; B : TMatrix3D) : TMatrix3D;
begin
  // First row
  Result[0,0] := A[0,0] * B[0,0] + A[0,1] * B[1,0] + A[0,2] * B[2,0];
  Result[0,1] := A[0,0] * B[0,1] + A[0,1] * B[1,1] + A[0,2] * B[2,1];
  Result[0,2] := A[0,0] * B[0,2] + A[0,1] * B[1,2] + A[0,2] * B[2,2];
  // Second row
  Result[1,0] := A[1,0] * B[0,0] + A[1,1] * B[1,0] + A[1,2] * B[2,0];
  Result[1,1] := A[1,0] * B[0,1] + A[1,1] * B[1,1] + A[1,2] * B[2,1];
  Result[1,2] := A[1,0] * B[0,2] + A[1,1] * B[1,2] + A[1,2] * B[2,2];
  // Third row
  Result[2,0] := A[2,0] * B[0,0] + A[2,1] * B[1,0] + A[2,2] * B[2,0];
  Result[2,1] := A[2,0] * B[0,1] + A[2,1] * B[1,1] + A[2,2] * B[2,1];
  Result[2,2] := A[2,0] * B[0,2] + A[2,1] * B[1,2] + A[2,2] * B[2,2];
end; // MatrixMultiply()

function MatrixMultiply(A : TMatrix4D; B : TMatrix4D) : TMatrix4D;
begin
  // First row
  Result[0,0] := A[0,0] * B[0,0] + A[0,1] * B[1,0] + A[0,2] * B[2,0] + A[0,3] * B[3,0];
  Result[0,1] := A[0,0] * B[0,1] + A[0,1] * B[1,1] + A[0,2] * B[2,1] + A[0,3] * B[3,1];
  Result[0,2] := A[0,0] * B[0,2] + A[0,1] * B[1,2] + A[0,2] * B[2,2] + A[0,3] * B[3,2];
  Result[0,3] := A[0,0] * B[0,3] + A[0,1] * B[1,3] + A[0,2] * B[2,3] + A[0,3] * B[3,3];
  // Second row
  Result[1,0] := A[1,0] * B[0,0] + A[1,1] * B[1,0] + A[1,2] * B[2,0] + A[1,3] * B[3,0];
  Result[1,1] := A[1,0] * B[0,1] + A[1,1] * B[1,1] + A[1,2] * B[2,1] + A[1,3] * B[3,1];
  Result[1,2] := A[1,0] * B[0,2] + A[1,1] * B[1,2] + A[1,2] * B[2,2] + A[1,3] * B[3,2];
  Result[1,3] := A[1,0] * B[0,3] + A[1,1] * B[1,3] + A[1,2] * B[2,3] + A[1,3] * B[3,3];
  // Third row
  Result[2,0] := A[2,0] * B[0,0] + A[2,1] * B[1,0] + A[2,2] * B[2,0] + A[2,3] * B[3,0];
  Result[2,1] := A[2,0] * B[0,1] + A[2,1] * B[1,1] + A[2,2] * B[2,1] + A[2,3] * B[3,1];
  Result[2,2] := A[2,0] * B[0,2] + A[2,1] * B[1,2] + A[2,2] * B[2,2] + A[2,3] * B[3,2];
  Result[2,3] := A[2,0] * B[0,3] + A[2,1] * B[1,3] + A[2,2] * B[2,3] + A[2,3] * B[3,3];
  // Fourth row
  Result[3,0] := A[3,0] * B[0,0] + A[3,1] * B[1,0] + A[3,2] * B[2,0] + A[3,3] * B[3,0];
  Result[3,1] := A[3,0] * B[0,1] + A[3,1] * B[1,1] + A[3,2] * B[2,1] + A[3,3] * B[3,1];
  Result[3,2] := A[3,0] * B[0,2] + A[3,1] * B[1,2] + A[3,2] * B[2,2] + A[3,3] * B[3,2];
  Result[3,3] := A[3,0] * B[0,3] + A[3,1] * B[1,3] + A[3,2] * B[2,3] + A[3,3] * B[3,3];
end; // MatrixMultiply()

function MatrixMultiply(A : TMatrix4D; V : TVector4D) : TVector4D;
begin
  Result[_X_] := A[0,0] * V[0] + A[0,1] * V[1] + A[0,2] * V[2] + A[0,3] * V[3];
  Result[_Y_] := A[1,0] * V[0] + A[1,1] * V[1] + A[1,2] * V[2] + A[1,3] * V[3];
  Result[_Z_] := A[2,0] * V[0] + A[2,1] * V[1] + A[2,2] * V[2] + A[2,3] * V[3];
  Result[_W_] := A[3,0] * V[0] + A[3,1] * V[1] + A[3,2] * V[2] + A[3,3] * V[3];
end; // MatrixMultiply()

begin
end.
