
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
with Neo.Vectors;

package Neo.Arrays is

  ------------
  -- Arrays --
  ------------
  -- Set of array types tied to vectors for easy back-and-forth translation

  package Vector_Bool                 is new Vectors (Bool);
  package Vector_Duration             is new Vectors (Duration);
  package Vector_Ptr                  is new Vectors (Ptr);
  package Vector_Str_16_Unbound       is new Vectors (Unbounded_Wide_String);
  package Vector_Str_Unbound          renames Vector_Str_16_Unbound;
  package Vector_Ptr_Str_16           is new Vectors (Ptr_Str_16);
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
  package Vector_Int_32_Unsigned_C    is new Vectors (Int_32_Unsigned_C);
  package Vector_Int_32_Unsigned      is new Vectors (Int_32_Unsigned);
  package Vector_Int_32_Signed_C      is new Vectors (Int_32_Signed_C);
  package Vector_Int_32_Signed        is new Vectors (Int_32_Signed);
  package Vector_Int_32_Natural       is new Vectors (Int_32_Natural);
  package Vector_Int_32_Positive      is new Vectors (Int_32_Positive);
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
  package Vector_Real_32_Natural      is new Vectors (Real_32_Natural);
  package Vector_Real_32_Positive     is new Vectors (Real_32_Positive);
  package Vector_Real_32_Percent      is new Vectors (Real_32_Percent);
  package Vector_Real_32_Degree       is new Vectors (Real_32_Degree);
  subtype Array_Stream                is Stream_Element_Array;
  subtype Array_Bool                  is Vector_Bool.Unsafe_Array;
  subtype Array_Duration              is Vector_Duration.Unsafe_Array;
  subtype Array_Ptr                   is Vector_Ptr.Unsafe_Array;
  subtype Array_Ptr_Str_16            is Vector_Ptr_Str_16.Unsafe_Array;
  subtype Array_Str_16_Unbound        is Vector_Str_16_Unbound.Unsafe_Array;
  subtype Array_Str_Unbound           is Array_Str_16_Unbound;
  subtype Array_Char_8_C              is Vector_Char_8_C.Unsafe_Array;
  subtype Array_Char_8                is Vector_Char_8.Unsafe_Array;
  subtype Array_Char_16_C             is Vector_Char_16_C.Unsafe_Array;
  subtype Array_Char_16               is Vector_Char_16.Unsafe_Array;
  subtype Array_Int_Ptr               is Vector_Int_Ptr.Unsafe_Array;
  subtype Array_Int_8_Unsigned_C      is Vector_Int_8_Unsigned_C.Unsafe_Array;
  subtype Array_Int_8_Unsigned        is Vector_Int_8_Unsigned.Unsafe_Array;
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
  subtype Array_Int_32_Unsigned       is Vector_Int_32_Unsigned.Unsafe_Array;
  subtype Array_Int_32_Signed_C       is Vector_Int_32_Signed_C.Unsafe_Array;
  subtype Array_Int_32_Signed         is Vector_Int_32_Signed.Unsafe_Array;
  subtype Array_Int                   is Array_Int_32_Signed;
  subtype Array_Int_32_Natural        is Vector_Int_32_Natural.Unsafe_Array;
  subtype Array_Int_32_Positive       is Vector_Int_32_Positive.Unsafe_Array;
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
  subtype Array_Real_32               is Vector_Real_32.Unsafe_Array;
  subtype Array_Real                  is Array_Real_32;
  subtype Array_Real_32_Natural       is Vector_Real_32_Natural.Unsafe_Array;
  subtype Array_Real_32_Positive      is Vector_Real_32_Positive.Unsafe_Array;
  subtype Array_Real_32_Percent       is Vector_Real_32_Percent.Unsafe_Array;
  subtype Array_Real_32_Degree        is Vector_Real_32_Degree.Unsafe_Array;
  subtype Ptr_Array_Ptr               is Vector_Ptr.Ptr_Unsafe_Array;
  subtype Ptr_Array_Ptr_Str_16        is Vector_Ptr_Str_16.Ptr_Unsafe_Array;
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
end;