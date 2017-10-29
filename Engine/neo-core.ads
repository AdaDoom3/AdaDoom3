
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with GNAT.Traceback;          use GNAT.Traceback;

-- Separator for the "Core" layer consisting of system and data format independant functionality
package Neo.Core is

  -----------
  -- Stack --
  -----------

  procedure Put_Stack;

  -----------
  -- Split --
  -----------

  -- function Split (Item : Str; On : Str := " ") return Vector_Str_16_Unbound.Unsafe.Vector;
  function Split (Item : Str; On : Str := " ") return Array_Str_Unbound; -- is (Vector_Str_16_Unbound.To_Unsafe_Array (Split (Item, On)));

  -------------
  -- Replace --
  -------------

  function Replace  (Item :        Str;         From : Str; To : Str) return Str is (S (Replace (U (Item), From, To)));
  function Replace  (Item :        Str_Unbound; From : Str; To : Str) return Str_Unbound;
  procedure Replace (Item : in out Str_Unbound; From : Str; To : Str);

  -----------
  -- Count --
  -----------

  function Count (Text : Str;         Item : Char) return Natural;
  function Count (Text : Str_Unbound; Item : Char) return Natural is (Count (S (Text), Item));

  -------------
  -- Vectors --
  -------------
    
  generic
    type Vec_T is private;
  package Neo.Core.Vectors is


    -- Base type
    package Unsafe is new Ada.Containers.Indefinite_Vectors (Positive, Vec_T);
    subtype Cursor is Unsafe.Cursor;
    NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

    -- Array conversions
    type Unsafe_Array is array (Positive range <>) of aliased Vec_T with Convention => C;
    type Ptr_Unsafe_Array is access all Unsafe_Array;
    function To_Unsafe_Vector (Item : Unsafe_Array)  return Unsafe.Vector;
    function To_Unsafe_Array  (Item : Unsafe.Vector) return Unsafe_Array;

  -- warning: incompatible types in conversion
  pragma Warnings (Off);
    function To_Ptr is new Unchecked_Conversion (Ptr_Unsafe_Array, Ptr);
  Pragma Warnings (On);

    -- Wrapped type
    protected type Safe_Vector is
        procedure Clear;
        procedure Set     (Val : Unsafe.Vector);
        procedure Set     (Val : Unsafe_Array);
        procedure Next    (Pos : in out Cursor);
        procedure Replace (Pos :        Cursor; Item : Vec_T);
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

  ---------------------
  -- Multi-Way Trees --
  ---------------------

  generic
    type Tree_T is private;
  package Neo.Core.Trees is

    -- Base type
    package Unsafe is new Ada.Containers.Indefinite_Multiway_Trees (Tree_T, "="); 
    subtype Cursor is Unsafe.Cursor;
    NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

    -- Wrapped type
    protected type Safe_Tree is   
        procedure Clear;
        procedure Replace         (Pos    : Cursor; Item : Tree_T);
        procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T));
        procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T));
        procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor));
        procedure Iterate                          (Process : not null access procedure (Pos : Cursor));
        procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
        procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
        procedure Swap            (I, J   : Cursor);
        procedure Set             (Source : Unsafe.Tree);
        procedure Move            (Source : in out Unsafe.Tree);
        procedure Delete_Leaf     (Pos    : in out Cursor);
        procedure Delete_Subtree  (Pos    : in out Cursor);
        procedure Delete          (Parent         : Cursor);
        procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Positive := 1);
        procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Positive := 1);
        procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Positive := 1);
        procedure Splice_Subtree  (Parent, Before, Pos           : Cursor);
        procedure Splice_Children (Parent, Before, Source_Parent : Cursor);
        procedure Copy_Subtree    (Parent, Before, Source        : Cursor);
        procedure Next            (Pos    : in out Cursor);
        procedure Previous        (Pos    : in out Cursor);
        function Subtree_Nodes    (Pos    : Cursor)                return Positive;
        function Depth            (Pos    : Cursor)                return Positive;
        function "="              (L, R   : Cursor)                return Bool;
        function Is_Root          (Pos    : Cursor)                return Bool;
        function Is_Leaf          (Pos    : Cursor)                return Bool;
        function Has              (Pos    : Cursor)                return Bool;
        function Equals           (Item   : Unsafe.Tree)           return Bool;
        function Find             (Item   : Tree_T)                return Cursor;
        function Get              (Pos    : Cursor)                return Tree_T;
        function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor;
        function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor;
        function Child_Count      (Parent        : Cursor)         return Positive;
        function Child_Depth      (Parent, Child : Cursor)         return Positive;
        function Parent           (Pos    : Cursor)                return Cursor;
        function First            (Parent : Cursor)                return Cursor;
        function First            (Parent : Cursor)                return Tree_T;
        function Last             (Parent : Cursor)                return Tree_T;
        function Last             (Parent : Cursor)                return Cursor;
        function Is_Empty                                          return Bool;
        function Node_Count                                        return Positive;
        function Root                                              return Cursor;
        function Get                                               return Unsafe.Tree;
      private
        This : Unsafe.Tree;
      end;
  end;

  ------------------
  -- Ordered Maps --
  ------------------

  generic
    type Key_T is (<>);
    type Map_T is private;
  package Neo.Core.Ordered is

    -- Base type
    package Unsafe is new Ada.Containers.Indefinite_Ordered_Maps (Key_T, Map_T, "<", "="); use Unsafe;
    subtype Cursor is Unsafe.Cursor;
    NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

    -- Wrapped type
    protected type Safe_Map is
        procedure Clear;
        procedure Set     (Val : Unsafe.Map);
        procedure Next    (Pos : in out Cursor);
        procedure Delete  (Pos : in out Cursor);
        procedure Delete  (Key : Key_T);
        procedure Replace (Pos : Cursor; Item : Map_T);
        procedure Replace (Key : Key_T;  Item : Map_T);
        procedure Insert  (Key : Key_T;  Item : Map_T);
        function Has      (Key : Key_T)  return Bool;
        function Has      (Pos : Cursor) return Bool;
        function Key      (Pos : Cursor) return Key_T;
        function Get      (Pos : Cursor) return Map_T;
        function Get      (Key : Key_T)  return Map_T;
        function Get                     return Unsafe.Map;
        function First                   return Cursor;
        function Length                  return Natural;
      private
        This : Unsafe.Map;
      end;
  end;
              
  ------------
  -- Arrays --
  ------------
  --
  -- Array types tied to vectors to ease conversion
  --

  generic package Vectors             renames Neo.Core.Vectors;
  package Vector_Bool                 is new  Vectors (Bool);
  package Vector_Duration             is new  Vectors (Duration);
  package Vector_Ptr                  is new  Vectors (Ptr);
  package Vector_Str_8_Unbound        is new  Vectors (Str_8_Unbound);
  package Vector_Str_16_Unbound       is new  Vectors (Unbounded_Wide_String);
  package Vector_Str_Unbound          renames Vector_Str_16_Unbound;
  package Vector_Ptr_Str_16           is new  Vectors (Ptr_Str_16);
  package Vector_Ptr_Str_8_C          is new  Vectors (Ptr_Str_8_C);
  package Vector_Ptr_Char_8_C         is new  Vectors (Ptr_Char_8_C);
  package Vector_Char_8_C             is new  Vectors (Char_8_C);
  package Vector_Char_8               is new  Vectors (Char_8);
  package Vector_Char_16_C            is new  Vectors (Char_16_C);
  package Vector_Char_16              is new  Vectors (Char_16);
  package Vector_Int_Ptr              is new  Vectors (Int_Ptr);
  package Vector_Int_8_Unsigned_C     is new  Vectors (Int_8_Unsigned_C);
  package Vector_Int_8_Unsigned       is new  Vectors (Int_8_Unsigned);
  package Vector_Int_8_Signed_C       is new  Vectors (Int_8_Signed_C);
  package Vector_Int_8_Signed         is new  Vectors (Int_8_Signed);
  package Vector_Int_8_Natural        is new  Vectors (Int_8_Natural);
  package Vector_Int_8_Positive       is new  Vectors (Int_8_Positive);
  package Vector_Int_16_Unsigned_C    is new  Vectors (Int_16_Unsigned_C);
  package Vector_Int_16_Unsigned      is new  Vectors (Int_16_Unsigned);
  package Vector_Int_16_Signed_C      is new  Vectors (Int_16_Signed_C);
  package Vector_Int_16_Signed        is new  Vectors (Int_16_Signed);
  package Vector_Int_16_Natural       is new  Vectors (Int_16_Natural);
  package Vector_Int_16_Positive      is new  Vectors (Int_16_Positive);
  package Vector_Int_32_Unsigned_C    is new  Vectors (Int_Unsigned_C);
  package Vector_Int_32_Unsigned      is new  Vectors (Int_Unsigned);
  package Vector_Int_32_Signed_C      is new  Vectors (Int_32_Signed_C);
  package Vector_Int_32_Signed        is new  Vectors (Int_32_Signed);
  package Vector_Int_32_Natural       is new  Vectors (Int_32_Natural);
  package Vector_Natural              renames Vector_Int_32_Natural;
  package Vector_Int_32_Positive      is new  Vectors (Int_32_Positive);
  package Vector_Positive             renames Vector_Int_32_Positive;
  package Vector_Int_64_Unsigned      is new  Vectors (Int_64_Unsigned);
  package Vector_Int_64_Unsigned_C    is new  Vectors (Int_64_Unsigned_C);
  package Vector_Int_64_Signed        is new  Vectors (Int_64_Signed);
  package Vector_Int_64_Signed_C      is new  Vectors (Int_64_Signed_C);
  package Vector_Int_64_Natural       is new  Vectors (Int_64_Natural);
  package Vector_Int_64_Positive      is new  Vectors (Int_64_Positive);
  package Vector_Int_Size_C           is new  Vectors (Int_Size_C);
  package Vector_Real_64_C            is new  Vectors (Real_64_C);
  package Vector_Real_64              is new  Vectors (Real_64);
  package Vector_Real_64_Natural      is new  Vectors (Real_64_Natural);
  package Vector_Real_64_Positive     is new  Vectors (Real_64_Positive);
  package Vector_Real_64_Percent      is new  Vectors (Real_64_Percent);
  package Vector_Real_64_Degree       is new  Vectors (Real_64_Degree);
  package Vector_Real_32_C            is new  Vectors (Real_32_C);
  package Vector_Real_32              is new  Vectors (Real_32);
  package Vector_Real                 renames Vector_Real_32;
  package Vector_Real_32_Natural      is new  Vectors (Real_32_Natural);
  package Vector_Real_32_Positive     is new  Vectors (Real_32_Positive);
  package Vector_Real_32_Percent      is new  Vectors (Real_32_Percent);
  package Vector_Real_32_Degree       is new  Vectors (Real_32_Degree);
  subtype Array_Bool                  is Vector_Bool.Unsafe_Array;
  subtype Array_Duration              is Vector_Duration.Unsafe_Array;
  subtype Array_Ptr                   is Vector_Ptr.Unsafe_Array;
  subtype Array_Ptr_Str_16            is Vector_Ptr_Str_16.Unsafe_Array;
  subtype Array_Ptr_Str_8_C           is Vector_Ptr_Str_8_C.Unsafe_Array;
  subtype Array_Ptr_Char_8_C          is Vector_Ptr_Char_8_C.Unsafe_Array;
  subtype Array_Str_16_Unbound        is Vector_Str_16_Unbound.Unsafe_Array;
  subtype Array_Str_Unbound           is Array_Str_16_Unbound;
  subtype Array_Str_8_Unbound         is Vector_Str_8_Unbound.Unsafe_Array;
  subtype Array_Char_8_C              is Vector_Char_8_C.Unsafe_Array;
  subtype Array_Char_8                is Vector_Char_8.Unsafe_Array;
  subtype Array_Char_16_C             is Vector_Char_16_C.Unsafe_Array;
  subtype Array_Char_16               is Vector_Char_16.Unsafe_Array;
  subtype Array_Int_Ptr               is Vector_Int_Ptr.Unsafe_Array;
  subtype Array_Int_8_Unsigned_C      is Vector_Int_8_Unsigned_C.Unsafe_Array;
  subtype Array_Int_8_Unsigned        is Vector_Int_8_Unsigned.Unsafe_Array;
  subtype Array_Byte_C                is Array_Int_8_Unsigned_C;
  subtype Array_Byte                  is Array_Int_8_Unsigned;
  subtype Array_Int_8_Signed_C        is Vector_Int_8_Signed_C.Unsafe_Array;
  subtype Array_Int_8_Signed          is Vector_Int_8_Signed.Unsafe_Array;
  subtype Array_Int_8_Natural         is Vector_Int_8_Natural.Unsafe_Array;
  subtype Array_Int_8_Positive        is Vector_Int_8_Positive.Unsafe_Array;
  subtype Array_Int_16_Unsigned_C     is Vector_Int_16_Unsigned_C.Unsafe_Array;
  subtype Array_Int_16_Unsigned       is Vector_Int_16_Unsigned.Unsafe_Array;
  subtype Array_Int_16_Signed_C       is Vector_Int_16_Signed_C.Unsafe_Array;
  subtype Array_Int_16_Signed         is Vector_Int_16_Signed.Unsafe_Array;
  subtype Array_Int_16_Natural        is Vector_Int_16_Natural.Unsafe_Array;
  subtype Array_Int_16_Positive       is Vector_Int_16_Positive.Unsafe_Array;
  subtype Array_Int_32_Unsigned_C     is Vector_Int_32_Unsigned_C.Unsafe_Array;
  subtype Array_Int_Unsigned_C        is Array_Int_32_Unsigned_C;
  subtype Array_Int_32_Unsigned       is Vector_Int_32_Unsigned.Unsafe_Array;
  subtype Array_Int_32_Signed_C       is Vector_Int_32_Signed_C.Unsafe_Array;
  subtype Array_Int_32_Signed         is Vector_Int_32_Signed.Unsafe_Array;
  subtype Array_Int                   is Array_Int_32_Signed;
  subtype Array_Int_32_Natural        is Vector_Int_32_Natural.Unsafe_Array;
  subtype Array_Int_32_Positive       is Vector_Int_32_Positive.Unsafe_Array;
  subtype Array_Positive              is Array_Int_32_Positive;
  subtype Array_Int_64_Unsigned       is Vector_Int_64_Unsigned.Unsafe_Array;
  subtype Array_Int_64_Unsigned_C     is Vector_Int_64_Unsigned_C.Unsafe_Array;
  subtype Array_Int_64_Signed         is Vector_Int_64_Signed.Unsafe_Array;
  subtype Array_Int_64                is Array_Int_64_Signed;
  subtype Array_Int_64_Signed_C       is Vector_Int_64_Signed_C.Unsafe_Array;
  subtype Array_Int_64_Natural        is Vector_Int_64_Natural.Unsafe_Array;
  subtype Array_Int_64_Positive       is Vector_Int_64_Positive.Unsafe_Array;
  subtype Array_Int_Size_C            is Vector_Int_Size_C.Unsafe_Array;
  subtype Array_Real_64_C             is Vector_Real_64_C.Unsafe_Array;
  subtype Array_Real_64               is Vector_Real_64.Unsafe_Array;
  subtype Array_Real_64_Natural       is Vector_Real_64_Natural.Unsafe_Array;
  subtype Array_Real_64_Positive      is Vector_Real_64_Positive.Unsafe_Array;
  subtype Array_Real_64_Percent       is Vector_Real_64_Percent.Unsafe_Array;
  subtype Array_Real_64_Degree        is Vector_Real_64_Degree.Unsafe_Array;
  subtype Array_Real_32_C             is Vector_Real_32_C.Unsafe_Array;
  subtype Array_Real_C                is Array_Real_32_C;
  subtype Array_Real_32               is Vector_Real_32.Unsafe_Array;
  subtype Array_Real                  is Array_Real_32;
  subtype Array_Real_32_Natural       is Vector_Real_32_Natural.Unsafe_Array;
  subtype Array_Real_32_Positive      is Vector_Real_32_Positive.Unsafe_Array;
  subtype Array_Real_32_Percent       is Vector_Real_32_Percent.Unsafe_Array;
  subtype Array_Real_32_Degree        is Vector_Real_32_Degree.Unsafe_Array;
  subtype Ptr_Array_Ptr               is Vector_Ptr.Ptr_Unsafe_Array;
  subtype Ptr_Array_Ptr_Str_16        is Vector_Ptr_Str_16.Ptr_Unsafe_Array;
  subtype Ptr_Array_Ptr_Str_8_C       is Vector_Ptr_Str_8_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Str_16_Unbound    is Vector_Str_16_Unbound.Ptr_Unsafe_Array;
  subtype Ptr_Array_Char_8_C          is Vector_Char_8_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Char_8            is Vector_Char_8.Ptr_Unsafe_Array;
  subtype Ptr_Array_Char_16_C         is Vector_Char_16_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Char_16           is Vector_Char_16.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Unsigned_C  is Vector_Int_8_Unsigned_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Unsigned    is Vector_Int_8_Unsigned.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Signed_C    is Vector_Int_8_Signed_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Signed      is Vector_Int_8_Signed.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Natural     is Vector_Int_8_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_8_Positive    is Vector_Int_8_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Unsigned_C is Vector_Int_16_Unsigned_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Unsigned   is Vector_Int_16_Unsigned.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Signed_C   is Vector_Int_16_Signed_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Signed     is Vector_Int_16_Signed.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Natural    is Vector_Int_16_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_16_Positive   is Vector_Int_16_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_32_Unsigned_C is Vector_Int_32_Unsigned_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_Unsigned_C    is Ptr_Array_Int_32_Unsigned_C;
  subtype Ptr_Array_Int_32_Unsigned   is Vector_Int_32_Unsigned.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_32_Signed_C   is Vector_Int_32_Signed_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_32_Signed     is Vector_Int_32_Signed.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_32_Natural    is Vector_Int_32_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_32_Positive   is Vector_Int_32_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Unsigned   is Vector_Int_64_Unsigned.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Unsigned_C is Vector_Int_64_Unsigned_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Signed     is Vector_Int_64_Signed.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Signed_C   is Vector_Int_64_Signed_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Natural    is Vector_Int_64_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_64_Positive   is Vector_Int_64_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Int_Size_C        is Vector_Int_Size_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64_C         is Vector_Real_64_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64           is Vector_Real_64.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64_Natural   is Vector_Real_64_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64_Positive  is Vector_Real_64_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64_Percent   is Vector_Real_64_Percent.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_64_Degree    is Vector_Real_64_Degree.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_32_C         is Vector_Real_32_C.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_C            is Ptr_Array_Real_32_C;
  subtype Ptr_Array_Real_32           is Vector_Real_32.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_32_Natural   is Vector_Real_32_Natural.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_32_Positive  is Vector_Real_32_Positive.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_32_Percent   is Vector_Real_32_Percent.Ptr_Unsafe_Array;
  subtype Ptr_Array_Real_32_Degree    is Vector_Real_32_Degree.Ptr_Unsafe_Array;

  -------------
  -- Sorting --
  -------------

  procedure Sort (Item : in out Array_Str_Unbound);
  procedure Sort (Item : in out Array_Int);
  procedure Sort (Item : in out Array_Int_64);
  procedure Sort (Item : in out Array_Real);
  procedure Sort (Item : in out Array_Real_64);

  -----------------
  -- Hashed Maps --
  -----------------

  generic
    type Map_T is private;
  package Neo.Core.Hashed is

    -- Base type
    package Unsafe is new Ada.Containers.Indefinite_Hashed_Maps (Str_Unbound, Map_T, Wide_Hash, "=");
    subtype Cursor is Unsafe.Cursor;
    NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;

    -- Fetch a sorted list of keys
    function Keys (Val : Unsafe.Map) return Array_Str_Unbound;

    -- Wrapped type
    protected type Safe_Map is
        procedure Clear;
        procedure Set     (Val : Unsafe.Map);
        procedure Next    (Pos : in out Cursor);
        procedure Delete  (Pos : in out Cursor);
        procedure Delete  (Key : Str);
        procedure Replace (Pos : Cursor; Item : Map_T);
        procedure Replace (Key : Str; Item : Map_T);
        procedure Insert  (Key : Str; Item : Map_T);
        function Has      (Key : Str)    return Bool;
        function Has      (Pos : Cursor) return Bool;
        function Key      (Pos : Cursor) return Str;
        function Get      (Key : Str)    return Map_T;
        function Get      (Pos : Cursor) return Map_T;
        function Get                     return Unsafe.Map;
        function Keys                    return Array_Str_Unbound; -- See below
        function First                   return Cursor;
        function Length                  return Natural;
      private
        This : Unsafe.Map;
      end;
  end;
end;
