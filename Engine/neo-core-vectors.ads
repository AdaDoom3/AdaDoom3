
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Ada.Containers.Vectors;

generic
  type Vec_T is private;
package Neo.Core.Vectors is

  -- Base type
  package Unsafe is new Ada.Containers.Vectors (Positive, Vec_T);
  subtype Cursor is Unsafe.Cursor;
  NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

  -- Array conversions
  type Unsafe_Array is array (Positive range <>) of aliased Vec_T with Convention => C;
  type Ptr_Unsafe_Array is access all Unsafe_Array;
  function To_Unsafe_Vector (Item : Unsafe_Array)  return Unsafe.Vector;
  function To_Unsafe_Array  (Item : Unsafe.Vector) return Unsafe_Array;
  procedure Free is new Unchecked_Deallocation (Unsafe_Array, Ptr_Unsafe_Array);

  -- Pointer conversions
  type Ptr_Unsafe_Vector is access all Unsafe.Vector;
  function To_Ptr        is new Unchecked_Conversion (Ptr_Unsafe_Vector, Ptr);
  function To_Ptr_Vector is new Unchecked_Conversion (Ptr, Ptr_Unsafe_Vector);

-- warning: incompatible types in conversion
pragma Warnings (Off);
  function To_Ptr          is new Unchecked_Conversion (Ptr_Unsafe_Array, Ptr); -- Just hack it
  function To_Unsafe_Array is new Unchecked_Conversion (Ptr, Ptr_Unsafe_Array); -- Just hack it
Pragma Warnings (On);

  procedure Free is new Unchecked_Deallocation (Unsafe.Vector, Ptr_Unsafe_Vector);

  -- Wrapped type
  protected type Safe_Vector is
      procedure Clear;
      procedure Set     (Val : Unsafe.Vector);
      procedure Set     (Val : Unsafe_Array);
      procedure Next    (Pos : in out Cursor);
      procedure Replace (Pos :        Cursor; Item : Vec_T);
      procedure Replace (Pos : Positive;      Item : Vec_T);
      procedure Append                       (Item : Vec_T; Count : Positive := 1);
      procedure Prepend                      (Item : Vec_T; Count : Positive := 1);
      procedure Insert  (Before : Positive;   Item : Vec_T; Count : Positive := 1);
      procedure Delete  (Index  : Positive;                 Count : Positive := 1);
      function Has      (Pos    : Cursor)   return Boolean;
      function Get      (Pos    : Cursor)   return Vec_T;
      function Get      (Index  : Positive) return Vec_T;
      function Get                          return Unsafe.Vector;
      function To_Array                     return Unsafe_Array;
      function First                        return Cursor;
      function Length                       return Positive;
    private
      This : Unsafe.Vector;
    end;
end;
