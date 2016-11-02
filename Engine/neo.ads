
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

with Ada.Unchecked_Conversion;
with Ada.Wide_Text_IO;             use Ada.Wide_Text_IO;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Characters.Latin_1;       use Ada.Characters.Latin_1;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings;                  use Ada.Strings;
with Ada.Calendar.Formatting;      use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;      use Ada.Calendar.Time_Zones;
with Ada.Calendar;                 use Ada.Calendar;
with Ada.Streams;                  use Ada.Streams;
with Interfaces.C;                 use Interfaces.C;
with Interfaces;                   use Interfaces;
with System;                       use System;

-- String constants and base data-types
package Neo is

  ----------------
  -- Versioning --
  ----------------

  NAME_ID : constant Wide_String := "Neo";
  VERSION : constant Wide_String := "0.1a";

  -----------
  -- Paths --
  -----------

  PATH_LOGS            : constant Wide_String := "./Logs/";
  PATH_ASSETS          : constant Wide_String := "./Assets/";
  PATH_LOCALE          : constant Wide_String := "locale.csv";
  PATH_CONFIG          : constant Wide_String := "config.txt";
  PATH_ICON            : constant Wide_String := PATH_ASSETS & "icon";
  PATH_CURSOR_ACTIVE   : constant Wide_String := PATH_ASSETS & "cursor_active";
  PATH_CURSOR_INACTIVE : constant Wide_String := PATH_ASSETS & "cursor_inactive";

  -----------
  -- Types --
  -----------

  subtype Bool                        is Boolean;
  subtype Ptr                         is Address;
  subtype Stream                      is Ada.Streams.Stream_Element;
  subtype Char_8_C                    is Char;
  subtype Char_8                      is Character;
  subtype Char_16_C                   is WChar_T;
  subtype Char_16                     is Wide_Character;
  subtype Str_8_C                     is Char_Array;
  subtype Str_8                       is String;
  subtype Str                         is Wide_String;
  subtype Str_16_Unbound              is Unbounded_Wide_String;
  subtype Str_Unbound                 is Str_16_Unbound;
  subtype Str_8_Unbound               is Unbounded_String;
  subtype Str_16_C                    is WChar_Array;
  subtype Str_16                      is Wide_String;
  subtype Str_C                       is Str_16_C;
  type Int_Ptr                        is mod MEMORY_SIZE;
  type Int_8_Percent                  is range 1..100; for Int_8_Percent'size use 64;
  subtype Int_8_Unsigned              is Unsigned_8;
  subtype Byte                        is Int_8_Unsigned;
  subtype Int_8_Unsigned_C            is Interfaces.C.Unsigned_Char;
  subtype Int_8_Signed                is Short_Short_Integer;
  subtype Int_8_Signed_C              is Interfaces.C.Char;
  subtype Int_8                       is Int_8_Signed;
  subtype Int_8_C                     is Int_8_Signed_C;
  subtype Int_8_Natural               is Int_8_Unsigned;
  subtype Int_8_Positive              is Int_8_Unsigned range 1..Int_8_Unsigned'last;
  subtype Int_16_Unsigned             is Unsigned_16;
  subtype Int_16_Unsigned_C           is Interfaces.C.Unsigned_Short;
  subtype Int_16_Signed               is Short_Integer;
  subtype Int_16_Signed_C             is Interfaces.C.Short;
  subtype Int_16                      is Int_16_Signed;
  subtype Int_16_C                    is Int_16_Signed_C;
  subtype Int_16_Natural              is Int_16_Unsigned;
  subtype Int_16_Positive             is Int_8_Unsigned range 1..Int_8_Unsigned'last;
  subtype Int_32_Unsigned             is Unsigned_32;
  subtype Int_32_Unsigned_C           is Interfaces.C.Unsigned;
  subtype Int_32_Signed               is Integer;
  subtype Int_32_Signed_C             is Interfaces.C.Int;
  subtype Int_32                      is Int_32_Signed;
  subtype Int_32_C                    is Int_32_Signed_C;
  subtype Int_32_Natural              is Natural;
  subtype Int_32_Positive             is Positive;
  subtype Int                         is Int_32;
  subtype Int_C                       is Int_32_C;
  subtype Int_64_Unsigned             is Unsigned_64;
  subtype Int_64_Unsigned_C           is Int_64_Unsigned;
  subtype Int_64_Signed               is Long_Long_Integer;
  subtype Int_64_Signed_C             is Int_64_Signed;
  subtype Int_64                      is Int_64_Signed;
  subtype Int_64_C                    is Int_64_Signed_C;
  subtype Int_64_Natural              is Int_64_Unsigned;
  subtype Int_64_Positive             is Int_64_Signed range 1..Int_64'last;
  subtype Int_Size_C                  is Interfaces.C.Size_T;
  subtype Real_64                     is Long_Float;
  subtype Real_64_C                   is Interfaces.C.Double;
  subtype Real_64_Natural             is Real_64 range 0.0..Real_64'last;
  subtype Real_64_Positive            is Real_64 range 1.0..Real_64'last;
  subtype Real_64_Range               is Real_64 range -100.0..100.0;
  subtype Real_64_Percent             is Real_64 range 0.0..100.0;
  subtype Real_64_Degree              is Real_64 range 1.0..360.0;
  subtype Real_32                     is Float;
  subtype Real_32_C                   is Interfaces.C.C_Float;
  subtype Real_32_Natural             is Real_32 range 0.0..Real_32'last;
  subtype Real_32_Positive            is Real_32 range 1.0..Real_32'last;
  subtype Real_32_Range               is Real_32 range -100.0..100.0;
  subtype Real_32_Percent             is Real_32 range 0.0..100.0;
  subtype Real_32_Degree              is Real_32 range 1.0..360.0;
  subtype Real                        is Real_32;
  type Ptr_Str_16                     is access all Str_16;
  type Ptr_Str_8_C                    is access all Str_8_C;
  type Ptr_Str_8                      is access all Str_8;
  type Ptr_Str_16_C                   is access all Str_16_C;
  type Ptr_Int_Ptr                    is access all Int_Ptr;
  type Ptr_Int_64_Unsigned_C          is access all Int_64_Unsigned_C;
  type Ptr_Int_32_Unsigned_C          is access all Int_32_Unsigned_C;
  type Ptr_Int_32_Unsigned            is access all Int_32_Unsigned;
  type Ptr_Int_32_Signed_C            is access all Int_32_Signed_C;
  type Ptr_Int_16_Unsigned_C          is access all Int_16_Unsigned_C;
  type Ptr_Int_8_Unsigned_C           is access all Int_8_Unsigned_C;
  type Ptr_Real_32_C                  is access all Real_32_C;
  type Ptr_Char_8_C                   is access all Interfaces.C.Char;
  type Ptr_Char_16_C                  is access all Char_16_C;
  type Ptr_Const_Char_8_C             is access constant Char_8_C;
  type Ptr_Const_Char_16_C            is access constant Char_16_C;
  type Ptr_Const_Str_16_C             is access constant Str_16_C;
  type Ptr_Procedure                  is access procedure;
  type Ptr_Procedure_Put              is access procedure (Item : Str_16);
  function To_Ptr_Int_16_Unsigned_C   is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_16_Unsigned_C   is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_32_Unsigned     is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Int_32_Unsigned);
  function To_Ptr_Char_8_C            is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Char_8_C);
  function To_Ptr_Char_16_C           is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Char_16_C);
  function To_Ptr_Const_Char_8_C      is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Const_Char_8_C);
  function To_Ptr_Const_Char_16_C     is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Const_Char_16_C);
  function To_Ptr_Const_Char_16_C     is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr_Const_Char_16_C);
  function To_Ptr                     is new Ada.Unchecked_Conversion (Ptr_Const_Char_16_C,   Ptr);
  function To_Ptr                     is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr);
  function To_Int_32_Unsigned_C       is new Ada.Unchecked_Conversion (Int_32_Signed_C,       Int_32_Unsigned_C);
  function To_Int_32_Unsigned         is new Ada.Unchecked_Conversion (Int_32_Signed_C,       Int_32_Unsigned);
  function To_Int_32_Unsigned         is new Ada.Unchecked_Conversion (Real_32,               Int_32_Unsigned);
  function To_Int_32_Signed_C         is new Ada.Unchecked_Conversion (Int_32_Unsigned_C,     Int_32_Signed_C);
  function To_Int_32_Signed           is new Ada.Unchecked_Conversion (Int_32_Unsigned,       Int_32_Signed);
  function To_Int_16_Signed           is new Ada.Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed);
  function To_Int_16_Signed_C         is new Ada.Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed_C);
  function To_Int_16_Unsigned         is new Ada.Unchecked_Conversion (Char_16_C,             Int_16_Unsigned);
  function To_Int_Ptr                 is new Ada.Unchecked_Conversion (Ptr_Int_16_Unsigned_C, Int_Ptr);
  function To_Int_Ptr                 is new Ada.Unchecked_Conversion (Ptr_Const_Char_16_C,   Int_Ptr);
  function To_Int_Ptr                 is new Ada.Unchecked_Conversion (Ptr_Const_Char_8_C,    Int_Ptr);
  function To_Int_Ptr                 is new Ada.Unchecked_Conversion (Ptr,                   Int_Ptr);
  function To_Real_32                 is new Ada.Unchecked_Conversion (Real_32,               Int_32_Unsigned);
  C_TRUE              : constant Int_32_Signed_C := 1;
  C_FALSE             : constant Int_32_Signed_C := 0;
  CHAR_16_REPLACEMENT : constant Char_8          := '~';
  NULL_CHAR_8         : constant Char_8          := ASCII.NUL;
  NULL_CHAR_8_C       : constant Char_8_C        := Interfaces.C.NUL;
  NULL_CHAR_16        : constant Char_16         := Char_16'Val   (Char_8'Pos   (NULL_CHAR_8));
  NULL_CHAR_16_C      : constant Char_16_C       := Char_16_C'Val (Char_8_C'Pos (NULL_CHAR_8_C));
  NULL_STR_16         : constant Str_16          := "";
  NULL_STR            : constant Str_16          := NULL_STR_16;
  NULL_STR_16_UNBOUND : constant Str_16_Unbound  := NULL_UNBOUNDED_WIDE_STRING;
  NULL_STR_UNBOUND    : constant Str_16_Unbound  := NULL_STR_16_UNBOUND;
  NULL_STR_8          : constant Str_8           := "";
  NULL_STR_8_UNBOUND  : constant Str_8_Unbound   := NULL_UNBOUNDED_STRING;
  WORD_SIZE_IMAGE     : constant Str_8           := WORD_SIZE'Img (WORD_SIZE'Img'First + 1..WORD_SIZE'Img'Last);
  EOL_8               : constant Str_8           := CR & LF;
  TAB_8               : constant Str_8           := "" & HT;
  generic
    type Num_T is mod <>;
  function Generic_To_Str_16 (Item : Num_T; Base : Positive; Do_Pad_Zeros : Bool := True) return Str_16;
  function To_Str_8               (Item : Str_16)              return Str_8;
  function To_Str_8               (Item : Ptr_Const_Char_8_C)  return Str_8;
  function To_Str_8               (Item : Str_8_Unbound)       return Str_8               renames To_String;
  function To_Str_8_Unbound       (Item : Str_8)               return Str_8_Unbound       renames To_Unbounded_String;
  function To_Str_16              (Item : Str_16_Unbound)      return Str_16              renames To_Wide_String;
  function To_Str_16_Unbound      (Item : Str_16)              return Str_16_Unbound      renames To_Unbounded_Wide_String;
  function To_Str_Unbound         (Item : Str_16)              return Str_16_Unbound      renames To_Str_16_Unbound;
  function To_Str_16              (Item : Str_8)               return Str_16;
  function To_Str_16              (Item : Str_16_C)            return Str_16;
  function To_Str_16              (Item : Ptr_Const_Char_16_C) return Str_16;
  function To_Str_8               (Item : Str_8_C)             return Str_8               is (To_Ada                       (Item, True));
  function To_Str_8_C             (Item : Str_8)               return Str_8_C             is (To_C                         (Item, True));
  function To_Str_16              (Item : Char_16)             return Str_16              is                               ("" & Item);
  function To_Str_16              (Item : Str_8_C)             return Str_16              is (To_Str_16                    (To_Str_8 (Item)));
  function To_Str_16              (Item : Ptr_Const_Char_8_C)  return Str_16              is (To_Str_16                    (To_Str_8 (Item)));
  function To_Str_16              (Item : Char_8)              return Str_16              is (To_Str_16                    ("" & Item));
  function To_Str_16_Unbound      (Item : Str_8)               return Str_16_Unbound      is (To_Str_16_Unbound            (To_Str_16 (Item)));
  function To_Str_16_Unbound      (Item : Char_16)             return Str_16_Unbound      is (To_Str_16_Unbound            ("" & Item));
  function To_Str_Unbound         (Item : Str_8)               return Str_16_Unbound      renames To_Str_16_Unbound;
  function To_Str_Unbound         (Item : Char_16)             return Str_16_Unbound      renames To_Str_16_Unbound;
  function To_Str_16_C            (Item : Str_16)              return Str_16_C            is (To_C                         (Item & NULL_CHAR_16));
  function To_Str_16_C            (Item : Str_16_Unbound)      return Str_16_C            is (To_Str_16_C                  (To_Str_16 (Item)));
  function To_Str_C               (Item : Str_16)              return Str_16_C            renames To_Str_16_C;
  function To_Str_C               (Item : Str_16_Unbound)      return Str_16_C            renames To_Str_16_C;
  function To_Str                 (Item : Str_8)               return Str_16              renames To_Str_16;
  function To_Str                 (Item : Str_16_C)            return Str_16              renames To_Str_16;
  function To_Str                 (Item : Ptr_Const_Char_16_C) return Str_16              renames To_Str_16;
  function To_Str                 (Item : Char_16)             return Str_16              renames To_Str_16;
  function To_Str                 (Item : Str_8_C)             return Str_16              renames To_Str_16;
  function To_Str                 (Item : Ptr_Const_Char_8_C)  return Str_16              renames To_Str_16;
  function To_Str                 (Item : Char_8)              return Str_16              renames To_Str_16;
  function To_Str                 (Item : Str_16_Unbound)      return Str_16              renames To_Str_16;
  function To_Ptr_Char_8_C        (Item : Str_8_C)             return Ptr_Char_8_C        is (To_Ptr_Char_8_C              (Item (Item'First)'Address));
  function To_Ptr_Char_16_C       (Item : Str_16)              return Ptr_Char_16_C       is (To_Ptr_Char_16_C             (To_Str_16_C (Item)'Address));
  function To_Ptr_Const_Char_16_C (Item : Str_16_C)            return Ptr_Const_Char_16_C is (To_Ptr_Const_Char_16_C       (Item (Item'First)'Address));
  function To_Ptr_Const_Char_16_C (Item : Str_16)              return Ptr_Const_Char_16_C is (To_Ptr_Const_Char_16_C       (To_C (Item)));
  function To_Ptr_Const_Char_8_C2 (Item : Str_8_C)             return Ptr_Const_Char_8_C  is (To_Ptr_Const_Char_8_C        (Item (Item'First)'Address));
  function To_Ptr_Const_Char_8_C  (Item : Str_8)               return Ptr_Const_Char_8_C  is (To_Ptr_Const_Char_8_C2       (To_C (Item)));
  function To_Char_8              (Item : Char_16)             return Char_8;
  function To_Char_8_C            (Item : Char_8)              return Char_8_C            is (Char_8_C'Val  (Char_8'Pos    (Item)));
  function To_Char_8              (Item : Char_8_C)            return Char_8              is (Char_8'Val    (Char_8_C'Pos  (Item)));
  function To_Char_16_C           (Item : Char_8_C)            return Char_16_C           is (Char_16_C'Val (Char_8_C'Pos  (item)));
  function To_Char_16_C           (Item : Char_8)              return Char_16_C           is (Char_16_C'Val (Char_8'Pos    (item)));
  function To_Char_16             (Item : Char_16_C)           return Char_16             is (Char_16'Val   (Char_16_C'Pos (Item)));
  function To_Char_16             (Item : Char_8_C)            return Char_16             is (Char_16'Val   (Char_8_C'Pos  (Item)));
  function To_Char_16             (Item : Char_8)              return Char_16             is (Char_16'Val   (Char_8'Pos    (Item)));
  function To_Char_8_C            (Item : Char_16_C)           return Char_8_C            is (To_Char_8_C   (To_Char_8     (To_Char_16 (Item))));
  function To_Char_8_C            (Item : Char_16)             return Char_8_C            is (To_Char_8_C   (To_Char_8     (Item)));
  function To_Char_16_C           (Item : Char_16)             return Char_16_C           is (Char_16_C'Val (Char_16'Pos   (item)));
  function To_Char_8              (Item : Char_16_C)           return Char_8              is (To_Char_8     (To_Char_16    (Item)));
  EOL_16        : constant Str     := To_Char_16 (CR) & To_Char_16 (LF);
  EOL           : constant Str     := EOL_16;
  TAB_16        : constant Str     := To_Char_16 (HT) & "";
  TAB           : constant Str     := TAB_16;
  NULL_STR_16_C : constant Str_C   := To_Str_16_C (NULL_STR_16);
  NULL_STR_8_C  : constant Str_8_C := To_Str_8_C  (NULL_STR_8);
  NULL_STR_C    : constant Str_C   := NULL_STR_16_C;
  PATH_SEP      : constant Str     := "/";
  NULL_PTR      : constant Ptr     := NULL_ADDRESS;

  ------------
  -- Status --
  ------------

  -- A mutex or task-safe flag, however you want to look at it...
  protected type Safe_Status with Lock_Free is
      function Occupied return Bool;
      procedure Occupied (Val : Bool);
    private
      Status : Bool := False;
    end;

  -----------
  -- Timer --
  -----------

  type Timer_State is record
      Start      : Time;
      Last       : Duration := 0.0;
      Is_Stopped : Bool     := False;
    end record;
  function Date_Str       return Str;
  function Get_Start_Time return Time;
  function Get_Duration   (Timer :        Timer_State) return Duration;
  procedure Start         (Timer : in out Timer_State);
  procedure Stop          (Timer : in out Timer_State);

  ---------------
  -- Debugging --
  ---------------

  -- Assertion to check imported C function calls. They raise a Program_Error if the value is null or 0
  procedure Assert (Val : Ptr);
  procedure Assert (Val : Bool);
  procedure Assert (Val : Int_C);
  procedure Assert (Val : Int_16_Unsigned_C);
  procedure Assert (Val : Int_32_Unsigned_C);

  -- Ignore procedures swallow the result of C functions that return useless results, it can't be "is null" due to GNAT GPL compiler error.
  procedure Ignore (Val : Bool);              --is null;
  procedure Ignore (Val : Ptr);               --is null;
  procedure Ignore (Val : Int_Ptr);           --is null;
  procedure Ignore (Val : Int_C);             --is null;
  procedure Ignore (Val : Int_16_Unsigned_C); --is null;
  procedure Ignore (Val : Int_32_Unsigned_C); --is null;

  -----------
  -- Color --
  -----------

  type Color_State is record
      Red, Green, Blue : Byte;
    end record;
  COLOR_RED          : constant Color_State := (16#FF#, 16#00#, 16#00#);
  COLOR_TAN          : constant Color_State := (16#D2#, 16#B4#, 16#8C#);
  COLOR_BLUE         : constant Color_State := (16#00#, 16#00#, 16#FF#);
  COLOR_PINK         : constant Color_State := (16#FF#, 16#C0#, 16#CB#);
  COLOR_AQUA         : constant Color_State := (16#00#, 16#FF#, 16#FF#);
  COLOR_GRAY         : constant Color_State := (16#80#, 16#80#, 16#80#);
  COLOR_CYAN         : constant Color_State := (16#00#, 16#FF#, 16#FF#);
  COLOR_TEAL         : constant Color_State := (16#00#, 16#80#, 16#80#);
  COLOR_LIME         : constant Color_State := (16#BF#, 16#FF#, 16#00#);
  COLOR_PUCE         : constant Color_State := (16#CC#, 16#88#, 16#99#);
  COLOR_PLUM         : constant Color_State := (16#84#, 16#31#, 16#79#);
  COLOR_MAUVE        : constant Color_State := (16#E0#, 16#B0#, 16#FF#);
  COLOR_BLACK        : constant Color_State := (16#00#, 16#00#, 16#00#);
  COLOR_WHITE        : constant Color_State := (16#FF#, 16#FF#, 16#FF#);
  COLOR_GREEN        : constant Color_State := (16#00#, 16#FF#, 16#00#);
  COLOR_KHAKI        : constant Color_State := (16#C3#, 16#B0#, 16#91#);
  COLOR_IVORY        : constant Color_State := (16#FF#, 16#FF#, 16#F0#);
  COLOR_BEIGE        : constant Color_State := (16#F5#, 16#F5#, 16#DC#);
  COLOR_WHEAT        : constant Color_State := (16#F5#, 16#DE#, 16#B3#);
  COLOR_CORAL        : constant Color_State := (16#FF#, 16#7F#, 16#50#);
  COLOR_OLIVE        : constant Color_State := (16#80#, 16#80#, 16#00#);
  COLOR_SILVER       : constant Color_State := (16#C0#, 16#C0#, 16#C0#);
  COLOR_YELLOW       : constant Color_State := (16#FF#, 16#FF#, 16#00#);
  COLOR_ORANGE       : constant Color_State := (16#FF#, 16#A5#, 16#00#);
  COLOR_VIOLET       : constant Color_State := (16#EE#, 16#82#, 16#EE#);
  COLOR_PURPLE       : constant Color_State := (16#80#, 16#00#, 16#80#);
  COLOR_SALMON       : constant Color_State := (16#FA#, 16#80#, 16#72#);
  COLOR_INDIGO       : constant Color_State := (16#4B#, 16#00#, 16#82#);
  COLOR_MAROON       : constant Color_State := (16#80#, 16#00#, 16#00#);
  COLOR_GOLDEN       : constant Color_State := (16#FF#, 16#D7#, 16#00#);
  COLOR_MAGENTA      : constant Color_State := (16#FF#, 16#00#, 16#FF#);
  COLOR_FUCHSIA      : constant Color_State := (16#FF#, 16#77#, 16#FF#);
  COLOR_CRIMSON      : constant Color_State := (16#DC#, 16#14#, 16#3C#);
  COLOR_LAVENDER     : constant Color_State := (16#B5#, 16#7E#, 16#DC#);
  COLOR_SKY_BLUE     : constant Color_State := (16#87#, 16#CE#, 16#EB#);
  COLOR_CHARCOAL     : constant Color_State := (16#46#, 16#46#, 16#46#);
  COLOR_HOT_PINK     : constant Color_State := (16#FC#, 16#0F#, 16#C0#);
  COLOR_GOLDENROD    : constant Color_State := (16#DA#, 16#A5#, 16#20#);
  COLOR_NAVY_BLUE    : constant Color_State := (16#00#, 16#00#, 16#80#);
  COLOR_LIGHT_BLUE   : constant Color_State := (16#AD#, 16#D8#, 16#E6#);
  COLOR_ROYAL_BLUE   : constant Color_State := (16#08#, 16#4C#, 16#9E#);
  COLOR_AQUAMARINE   : constant Color_State := (16#7F#, 16#FF#, 16#D4#);
  COLOR_CHARTREUSE   : constant Color_State := (16#7F#, 16#FF#, 16#00#);
  COLOR_FOREST_GREEN : constant Color_State := (16#22#, 16#8B#, 16#22#);
end;