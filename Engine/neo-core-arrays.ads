
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

with Ada.Containers.Generic_Array_Sort;
with Neo.Core.Vectors;
with Neo.Core.Math; use Neo.Core.Math;

package Neo.Core.Arrays is

  ----------------------
  -- Single Dimension --
  ----------------------
  -- Set of array types tied to vectors for easy back-and-forth translation

  package Vector_Vector_2D            is new Vectors (Vector_2D);
  package Vector_Vector_3D            is new Vectors (Vector_3D);
  package Vector_Vector_4D            is new Vectors (Vector_4D);
  package Vector_Bool                 is new Vectors (Bool);
  package Vector_Duration             is new Vectors (Duration);
  package Vector_Ptr                  is new Vectors (Ptr);
  package Vector_Str_8_Unbound        is new Vectors (Str_8_Unbound);
  package Vector_Str_16_Unbound       is new Vectors (Str_16_Unbound);
  package Vector_Str_Unbound          renames Vector_Str_16_Unbound;
  package Vector_Str_32_Unbound       is new Vectors (Str_32_Unbound);
  package Vector_Ptr_Str_16           is new Vectors (Ptr_Str_16);
  package Vector_Ptr_Str_8_C          is new Vectors (Ptr_Str_8_C);
  package Vector_Ptr_Char_8_C         is new Vectors (Ptr_Char_8_C);
  package Vector_Char_8_C             is new Vectors (Char_8_C);
  package Vector_Char_8               is new Vectors (Char_8);
  package Vector_Char_16_C            is new Vectors (Char_16_C);
  package Vector_Char_16              is new Vectors (Char_16);
  package Vector_Int_Ptr              is new Vectors (Int_Ptr);
  package Vector_Int_8_Unsigned_C     is new Vectors (Int_8_Unsigned_C);
  package Vector_Int_8_Unsigned       is new Vectors (Int_8_Unsigned);
  package Vector_Int_8_Signed_C       is new Vectors (Int_8_Signed_C);
  package Vector_Int_8_Signed         is new Vectors (Int_8_Signed);
  package Vector_Int_8_Natural        is new Vectors (Int_8_Natural);
  package Vector_Int_8_Positive       is new Vectors (Int_8_Positive);
  package Vector_Int_16_Unsigned_C    is new Vectors (Int_16_Unsigned_C);
  package Vector_Int_16_Unsigned      is new Vectors (Int_16_Unsigned);
  package Vector_Int_16_Signed_C      is new Vectors (Int_16_Signed_C);
  package Vector_Int_16_Signed        is new Vectors (Int_16_Signed);
  package Vector_Int_16_Natural       is new Vectors (Int_16_Natural);
  package Vector_Int_16_Positive      is new Vectors (Int_16_Positive);
  package Vector_Int_32_Unsigned_C    is new Vectors (Int_Unsigned_C);
  package Vector_Int_32_Unsigned      is new Vectors (Int_Unsigned);
  package Vector_Int_32_Signed_C      is new Vectors (Int_32_Signed_C);
  package Vector_Int_32_Signed        is new Vectors (Int_32_Signed);
  package Vector_Int_32_Natural       is new Vectors (Int_32_Natural);
  package Vector_Int_32_Positive      is new Vectors (Int_32_Positive);
  package Vector_Int                  renames Vector_Int_32_Signed;
  package Vector_Natural              renames Vector_Int_32_Natural;
  package Vector_Positive             renames Vector_Int_32_Positive;
  package Vector_Int_64_Unsigned      is new Vectors (Int_64_Unsigned);
  package Vector_Int_64_Unsigned_C    is new Vectors (Int_64_Unsigned_C);
  package Vector_Int_64_Signed        is new Vectors (Int_64_Signed);
  package Vector_Int_64_Signed_C      is new Vectors (Int_64_Signed_C);
  package Vector_Int_64_Natural       is new Vectors (Int_64_Natural);
  package Vector_Int_64_Positive      is new Vectors (Int_64_Positive);
  package Vector_Int_Size_C           is new Vectors (Int_Size_C);
  package Vector_Real_64_C            is new Vectors (Real_64_C);
  package Vector_Real_64              is new Vectors (Real_64);
  package Vector_Real_64_Natural      is new Vectors (Real_64_Natural);
  package Vector_Real_64_Positive     is new Vectors (Real_64_Positive);
  package Vector_Real_64_Percent      is new Vectors (Real_64_Percent);
  package Vector_Real_64_Degree       is new Vectors (Real_64_Degree);
  package Vector_Real_32_C            is new Vectors (Real_32_C);
  package Vector_Real_32              is new Vectors (Real_32);
  package Vector_Real_16              is new Vectors (Real_16);
  package Vector_Real                 renames Vector_Real_32;
  package Vector_Real_32_Natural      is new Vectors (Real_32_Natural);
  package Vector_Real_32_Positive     is new Vectors (Real_32_Positive);
  package Vector_Real_32_Percent      is new Vectors (Real_32_Percent);
  package Vector_Real_32_Degree       is new Vectors (Real_32_Degree);
  subtype Array_Vector_2D             is Vector_Vector_2D.Unsafe_Array;
  subtype Array_Vector_3D             is Vector_Vector_3D.Unsafe_Array;
  subtype Array_Vector_4D             is Vector_Vector_4D.Unsafe_Array;
  subtype Array_Bool                  is Vector_Bool.Unsafe_Array;
  subtype Array_Duration              is Vector_Duration.Unsafe_Array;
  subtype Array_Ptr                   is Vector_Ptr.Unsafe_Array;
  subtype Array_Ptr_Str_16            is Vector_Ptr_Str_16.Unsafe_Array;
  subtype Array_Ptr_Str_8_C           is Vector_Ptr_Str_8_C.Unsafe_Array;
  subtype Array_Ptr_Char_8_C          is Vector_Ptr_Char_8_C.Unsafe_Array;
  subtype Array_Str_32_Unbound        is Vector_Str_32_Unbound.Unsafe_Array;
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
  subtype Array_Int_8                 is Vector_Int_8_Signed.Unsafe_Array;
  subtype Array_Int_8_Natural         is Vector_Int_8_Natural.Unsafe_Array;
  subtype Array_Int_8_Positive        is Vector_Int_8_Positive.Unsafe_Array;
  subtype Array_Int_16_Unsigned_C     is Vector_Int_16_Unsigned_C.Unsafe_Array;
  subtype Array_Int_16_Unsigned       is Vector_Int_16_Unsigned.Unsafe_Array;
  subtype Array_Int_16_Signed_C       is Vector_Int_16_Signed_C.Unsafe_Array;
  subtype Array_Int_16_Signed         is Vector_Int_16_Signed.Unsafe_Array;
  subtype Array_Int_16                is Vector_Int_16_Signed.Unsafe_Array;
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
  subtype Array_Natural               is Array_Int_32_Natural;
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
  subtype Array_Real_16               is Vector_Real_16.Unsafe_Array;
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

  ---------------------
  -- Multi Dimension --
  ---------------------

  type Array_x2_Bool              is array (Positive range <>, Positive range <>) of aliased Bool;
  type Array_x2_Duration          is array (Positive range <>, Positive range <>) of aliased Duration;
  type Array_x2_Ptr               is array (Positive range <>, Positive range <>) of aliased Ptr;
  type Array_x2_Ptr_Str_16        is array (Positive range <>, Positive range <>) of aliased Ptr_Str_16;
  type Array_x2_Ptr_Str_8_C       is array (Positive range <>, Positive range <>) of aliased Ptr_Str_8_C;
  type Array_x2_Ptr_Char_8_C      is array (Positive range <>, Positive range <>) of aliased Ptr_Char_8_C;
  type Array_x2_Str_32_Unbound    is array (Positive range <>, Positive range <>) of aliased Str_32_Unbound;
  type Array_x2_Str_16_Unbound    is array (Positive range <>, Positive range <>) of aliased Str_16_Unbound;
  type Array_x2_Str_8_Unbound     is array (Positive range <>, Positive range <>) of aliased Str_8_Unbound;
  type Array_x2_Char_8_C          is array (Positive range <>, Positive range <>) of aliased Char_8_C;
  type Array_x2_Char_8            is array (Positive range <>, Positive range <>) of aliased Char_8;
  type Array_x2_Char_16_C         is array (Positive range <>, Positive range <>) of aliased Char_16_C;
  type Array_x2_Char_16           is array (Positive range <>, Positive range <>) of aliased Char_16;
  type Array_x2_Int_Ptr           is array (Positive range <>, Positive range <>) of aliased Int_Ptr;
  type Array_x2_Int_8_Unsigned_C  is array (Positive range <>, Positive range <>) of aliased Int_8_Unsigned_C;
  type Array_x2_Int_8_Unsigned    is array (Positive range <>, Positive range <>) of aliased Int_8_Unsigned;
  type Array_x2_Int_8_Signed_C    is array (Positive range <>, Positive range <>) of aliased Int_8_Signed_C;
  type Array_x2_Int_8_Signed      is array (Positive range <>, Positive range <>) of aliased Int_8_Signed;
  type Array_x2_Int_8_Natural     is array (Positive range <>, Positive range <>) of aliased Int_8_Natural;
  type Array_x2_Int_8_Positive    is array (Positive range <>, Positive range <>) of aliased Int_8_Positive;
  type Array_x2_Int_16_Unsigned_C is array (Positive range <>, Positive range <>) of aliased Int_16_Unsigned_C;
  type Array_x2_Int_16_Unsigned   is array (Positive range <>, Positive range <>) of aliased Int_16_Unsigned;
  type Array_x2_Int_16_Signed_C   is array (Positive range <>, Positive range <>) of aliased Int_16_Signed_C;
  type Array_x2_Int_16_Signed     is array (Positive range <>, Positive range <>) of aliased Int_16_Signed;
  type Array_x2_Int_16_Natural    is array (Positive range <>, Positive range <>) of aliased Int_16_Natural;
  type Array_x2_Int_16_Positive   is array (Positive range <>, Positive range <>) of aliased Int_16_Positive;
  type Array_x2_Int_32_Unsigned_C is array (Positive range <>, Positive range <>) of aliased Int_32_Unsigned_C;
  type Array_x2_Int_32_Unsigned   is array (Positive range <>, Positive range <>) of aliased Int_32_Unsigned;
  type Array_x2_Int_32_Signed_C   is array (Positive range <>, Positive range <>) of aliased Int_32_Signed_C;
  type Array_x2_Int_32_Signed     is array (Positive range <>, Positive range <>) of aliased Int_32_Signed;
  type Array_x2_Int_32_Natural    is array (Positive range <>, Positive range <>) of aliased Int_32_Natural;
  type Array_x2_Int_32_Positive   is array (Positive range <>, Positive range <>) of aliased Int_32_Positive;
  type Array_x2_Int_64_Unsigned   is array (Positive range <>, Positive range <>) of aliased Int_64_Unsigned;
  type Array_x2_Int_64_Unsigned_C is array (Positive range <>, Positive range <>) of aliased Int_64_Unsigned_C;
  type Array_x2_Int_64_Signed     is array (Positive range <>, Positive range <>) of aliased Int_64_Signed;
  type Array_x2_Int_64_Signed_C   is array (Positive range <>, Positive range <>) of aliased Int_64_Signed_C;
  type Array_x2_Int_64_Natural    is array (Positive range <>, Positive range <>) of aliased Int_64_Natural;
  type Array_x2_Int_64_Positive   is array (Positive range <>, Positive range <>) of aliased Int_64_Positive;
  type Array_x2_Int_Size_C        is array (Positive range <>, Positive range <>) of aliased Int_Size_C;
  type Array_x2_Real_64_C         is array (Positive range <>, Positive range <>) of aliased Real_64_C;
  type Array_x2_Real_64           is array (Positive range <>, Positive range <>) of aliased Real_64;
  type Array_x2_Real_64_Natural   is array (Positive range <>, Positive range <>) of aliased Real_64_Natural;
  type Array_x2_Real_64_Positive  is array (Positive range <>, Positive range <>) of aliased Real_64_Positive;
  type Array_x2_Real_64_Percent   is array (Positive range <>, Positive range <>) of aliased Real_64_Percent;
  type Array_x2_Real_64_Degree    is array (Positive range <>, Positive range <>) of aliased Real_64_Degree;
  type Array_x2_Real_32_C         is array (Positive range <>, Positive range <>) of aliased Real_32_C;
  type Array_x2_Real_32           is array (Positive range <>, Positive range <>) of aliased Real_32;
  type Array_x2_Real_32_Natural   is array (Positive range <>, Positive range <>) of aliased Real_32_Natural;
  type Array_x2_Real_32_Positive  is array (Positive range <>, Positive range <>) of aliased Real_32_Positive;
  type Array_x2_Real_32_Percent   is array (Positive range <>, Positive range <>) of aliased Real_32_Percent;
  type Array_x2_Real_32_Degree    is array (Positive range <>, Positive range <>) of aliased Real_32_Degree;
  type Array_x2_Real_16           is array (Positive range <>, Positive range <>) of aliased Real_16;
  subtype Array_x2_Str_Unbound    is Array_x2_Str_16_Unbound;
  subtype Array_x2_Byte_C         is Array_x2_Int_8_Unsigned_C;
  subtype Array_x2_Byte           is Array_x2_Int_8_Unsigned;
  subtype Array_x2_Int_Unsigned_C is Array_x2_Int_32_Unsigned_C;
  subtype Array_x2_Int_8          is Array_x2_Int_8_Signed;
  subtype Array_x2_Int            is Array_x2_Int_32_Signed;
  subtype Array_x2_Int_16         is Array_x2_Int_16_Signed;
  subtype Array_x2_Positive       is Array_x2_Int_32_Positive;
  subtype Array_x2_Natural        is Array_x2_Int_32_Natural;
  subtype Array_x2_Int_64         is Array_x2_Int_64_Signed;
  subtype Array_x2_Real_C         is Array_x2_Real_32_C;
  subtype Array_x2_Real           is Array_x2_Real_32;

  -------------
  -- Sorting --
  -------------

  procedure Sort (Item : in out Array_Str_Unbound);
  procedure Sort (Item : in out Array_Int);
  procedure Sort (Item : in out Array_Int_64);
  procedure Sort (Item : in out Array_Real);
  procedure Sort (Item : in out Array_Real_64);
end;
