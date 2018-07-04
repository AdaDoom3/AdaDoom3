
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

with GNAT.Compiler_Version;
with GNAT.Traceback.Symbolic;      use GNAT.Traceback.Symbolic;
with GNAT.Traceback;               use GNAT.Traceback;
with Interfaces;                   use Interfaces;
with Interfaces.C;                 use Interfaces.C;
with System;                       use System;
with System.Machine_Code;          use System.Machine_Code;
with Ada.Containers;               use Ada.Containers;
with Ada.Directories;              use Ada.Directories;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Finalization;             use Ada.Finalization;
with Ada.Command_Line;             use Ada.Command_Line;
with Ada.Calendar;                 use Ada.Calendar;
with Ada.Calendar.Formatting;      use Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;       use Ada.Characters.Latin_1;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Task_Identification;      use Ada.Task_Identification;
with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;
with Ada.Streams;                  use Ada.Streams;
with Ada.Streams.Stream_IO;        
with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.Unchecked_Deallocation;   
with Ada.Unchecked_Conversion;
with Is_Debugging;

-- String constants and base data types
package Neo is
  
  -------------
  -- Renames --
  -------------

  package Ada_IO    renames Ada.Wide_Text_IO;
  package Stream_IO renames Ada.Streams.Stream_IO;

  generic procedure Unchecked_Deallocation renames Ada.Unchecked_Deallocation;
  generic function  Unchecked_Conversion   renames Ada.Unchecked_Conversion;
  
  -----------
  -- Types --
  -----------

  -- Characters
  subtype Char_8_C  is Char;
  subtype Char_8    is Character;
  subtype Char_16_C is WChar_T;
  subtype Char_16   is Wide_Character;

  -- Strings
  subtype Str_8_C        is Char_Array;
  subtype Str_8          is String;
  subtype Str_16_Unbound is Unbounded_Wide_String;
  subtype Str_8_Unbound  is Unbounded_String;
  subtype Str_16_C       is WChar_Array;
  subtype Str_16         is Wide_String;
  subtype Str_C          is Str_16_C;

  -- Integers
  type Int_Ptr              is mod MEMORY_SIZE;
  type Int_8_Percent        is range 1..100; for Int_8_Percent'Size use 8;
  subtype Int_8_Unsigned    is Unsigned_8;
  subtype Int_8_Unsigned_C  is Interfaces.C.Unsigned_Char;
  subtype Int_8_Signed      is Short_Short_Integer;
  subtype Int_8_Signed_C    is Interfaces.C.Char;
  subtype Int_8_Natural     is Int_8_Unsigned;
  subtype Int_8_Positive    is Int_8_Unsigned range 1..Int_8_Unsigned'Last;
  subtype Int_16_Unsigned   is Unsigned_16;
  subtype Int_16_Unsigned_C is Interfaces.C.Unsigned_Short;
  subtype Int_16_Signed     is Short_Integer;
  subtype Int_16_Signed_C   is Interfaces.C.Short;
  subtype Int_16_Natural    is Int_16_Unsigned;
  subtype Int_16_Positive   is Int_8_Unsigned range 1..Int_8_Unsigned'Last;
  subtype Int_32_Unsigned   is Unsigned_32;
  subtype Int_32_Unsigned_C is Interfaces.C.Unsigned;
  subtype Int_32_Signed     is Integer;
  subtype Int_32_Signed_C   is Interfaces.C.Int;
  subtype Int_32_Natural    is Natural;
  subtype Int_32_Positive   is Positive;
  subtype Int_64_Unsigned   is Unsigned_64;
  subtype Int_64_Unsigned_C is Int_64_Unsigned;
  subtype Int_64_Signed     is Long_Long_Integer;
  subtype Int_64_Signed_C   is Int_64_Signed;
  subtype Int_64_Natural    is Int_64_Unsigned;
  subtype Int_64_Positive   is Int_64_Signed range 1..Int_64_Signed'Last;
  subtype Int_Size_C        is Interfaces.C.Size_T;

  -- Floating point reals
  subtype Real_64          is Long_Float;
  subtype Real_64_C        is Interfaces.C.Double;
  subtype Real_64_Natural  is Real_64 range 0.0..Real_64'Last;
  subtype Real_64_Positive is Real_64 range 1.0..Real_64'Last;
  subtype Real_64_Percent  is Real_64 range 0.0..100.0;
  subtype Real_64_Range    is Real_64 range -Real_64_Percent'Last..Real_64_Percent'Last;
  subtype Real_64_Degree   is Real_64 range 1.0..360.0;
  subtype Real_32          is Float;
  subtype Real_32_C        is Interfaces.C.C_Float;
  subtype Real_32_Natural  is Real_32 range 0.0..Real_32'Last;
  subtype Real_32_Positive is Real_32 range 1.0..Real_32'Last;
  subtype Real_32_Percent  is Real_32 range 0.0..100.0;
  subtype Real_32_Degree   is Real_32 range 1.0..360.0;

  -- Stream types
  subtype Array_Stream is Stream_Element_Array;

  -- Access types
  subtype Ptr                is Address;
  type Ptr_Ptr               is access all Address;
  type Ptr_Str_16            is access all Str_16;
  type Ptr_Str_8_C           is access all Str_8_C;
  type Ptr_Str_8             is access all Str_8;
  type Ptr_Str_16_C          is access all Str_16_C;
  type Ptr_Int_Ptr           is access all Int_Ptr;
  type Ptr_Int_64_Unsigned_C is access all Int_64_Unsigned_C;
  type Ptr_Int_32_Unsigned_C is access all Int_32_Unsigned_C;
  type Ptr_Int_32_Unsigned   is access all Int_32_Unsigned;
  type Ptr_Int_32_Signed_C   is access all Int_32_Signed_C;
  type Ptr_Int_16_Unsigned_C is access all Int_16_Unsigned_C;
  type Ptr_Int_8_Unsigned_C  is access all Int_8_Unsigned_C;
  type Ptr_Real_32_C         is access all Real_32_C;
  type Ptr_Char_8_C          is access all Char_8_C;
  type Ptr_Char_16_C         is access all Char_16_C;
  type Ptr_Const_Char_8_C    is access constant Char_8_C;
  type Ptr_Const_Char_16_C   is access constant Char_16_C;
  type Ptr_Const_Str_16_C    is access constant Str_16_C;
  type Ptr_Procedure         is access procedure;
  type Ptr_Procedure_Put     is access procedure (Item : Str_16);

  -- Abbreviations for convience
  subtype Bool               is Boolean;
  subtype Char               is Char_16;
  subtype Stream             is Stream_Element;
  subtype Byte               is Int_8_Unsigned;
  subtype Int                is Int_32_Signed;
  subtype Int_C              is Int_32_Signed_C;
  subtype Real               is Real_32;
  subtype Real_C             is Real_32_C;
  subtype Real_Percent       is Real_64_Percent;
  subtype Real_Degree        is Real_64_Degree;
  subtype Int_64             is Int_64_Signed;
  subtype Int_64_C           is Int_64_Signed_C;
  subtype Int_16             is Int_16_Signed;
  subtype Int_16_C           is Int_16_Signed_C;
  subtype Int_8              is Int_8_Signed;
  subtype Int_8_C            is Int_8_Signed_C;
  subtype Int_Unsigned       is Int_32_Unsigned;
  subtype Int_Unsigned_C     is Int_32_Unsigned_C;
  subtype Str                is Str_16;
  subtype Str_Unbound        is Str_16_Unbound;
  subtype Ptr_Int_Unsigned_C is Ptr_Int_32_Unsigned_C;
  subtype Ptr_Real_C         is Ptr_Real_32_C;

  -- Unchecked conversions
  function To_Ptr_Int_16_Unsigned_C is new Unchecked_Conversion (Ptr,                   Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_16_Unsigned_C is new Unchecked_Conversion (Int_Ptr,               Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_32_Unsigned   is new Unchecked_Conversion (Ptr,                   Ptr_Int_32_Unsigned);
  function To_Ptr                   is new Unchecked_Conversion (Ptr_Const_Char_16_C,   Ptr);
  function To_Ptr                   is new Unchecked_Conversion (Int_Ptr,               Ptr);
  function To_Ptr_Char_8_C          is new Unchecked_Conversion (Ptr,                   Ptr_Char_8_C);
  function To_Ptr_Char_16_C         is new Unchecked_Conversion (Ptr,                   Ptr_Char_16_C);
  function To_Ptr_Const_Char_16_C   is new Unchecked_Conversion (Ptr,                   Ptr_Const_Char_16_C);
  function To_Ptr_Const_Char_16_C   is new Unchecked_Conversion (Int_Ptr,               Ptr_Const_Char_16_C);
  function To_Int_64_Unsigned       is new Unchecked_Conversion (Real_64,               Int_64_Unsigned);
  function To_Int_32_Unsigned_C     is new Unchecked_Conversion (Int_32_Signed_C,       Int_Unsigned_C);
  function To_Int_32_Unsigned       is new Unchecked_Conversion (Int_32_Signed_C,       Int_Unsigned);
  function To_Int_32_Unsigned       is new Unchecked_Conversion (Real_32,               Int_Unsigned);
  function To_Int_32_Signed_C       is new Unchecked_Conversion (Int_Unsigned_C,        Int_32_Signed_C);
  function To_Int_32_Signed         is new Unchecked_Conversion (Int_Unsigned,          Int_32_Signed);
  function To_Int_16_Signed         is new Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed);
  function To_Int_16_Signed_C       is new Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed_C);
  function To_Int_16_Unsigned       is new Unchecked_Conversion (Char_16_C,             Int_16_Unsigned);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Int_16_Unsigned_C, Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Const_Char_16_C,   Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr_Char_8_C,    Int_Ptr);
  function To_Int_Ptr               is new Unchecked_Conversion (Ptr,                   Int_Ptr);
  function To_Real_32               is new Unchecked_Conversion (Real_32,               Int_Unsigned);

  -- Prerequisite string constants
  CHAR_16_REPLACEMENT : constant Char_8         := '~';
  NULL_CHAR_8         : constant Char_8         := ASCII.NUL;
  NULL_CHAR_8_C       : constant Char_8_C       := Interfaces.C.NUL;
  NULL_CHAR_16        : constant Char_16        := Char_16'Val   (Char_8'Pos   (NULL_CHAR_8));
  NULL_CHAR_16_C      : constant Char_16_C      := Char_16_C'Val (Char_8_C'Pos (NULL_CHAR_8_C));
  NULL_CHAR           : constant Char_16        := NULL_CHAR_16;
  NULL_CHAR_C         : constant Char_16_C      := NULL_CHAR_16_C;
  NULL_STR_16         : constant Str_16         := "";
  NULL_STR            : constant Str_16         := NULL_STR_16;
  NULL_STR_16_UNBOUND : constant Str_16_Unbound := NULL_UNBOUNDED_WIDE_STRING;
  NULL_STR_UNBOUND    : constant Str_16_Unbound := NULL_STR_16_UNBOUND;
  NULL_STR_8          : constant Str_8          := "";
  NULL_STR_8_UNBOUND  : constant Str_8_Unbound  := NULL_UNBOUNDED_STRING;
  WORD_SIZE_IMAGE     : constant Str_8          := WORD_SIZE'Img (WORD_SIZE'Img'First + 1..WORD_SIZE'Img'Last);
  EOL_8               : constant Str_8          := CR & LF;
  TAB_8               : constant Str_8          := "" & HT;

  -- String conversions
  generic
    type Num_T is mod <>;
  function Generic_To_Str_16_Int (Item : Num_T; Base : Positive; Do_Pad_Zeros : Bool := True) return Str_16; -- For hex or binary strings
  generic
    type Num_T is digits <>;
  function Generic_To_Str_16_Real (Item : Num_T) return Str_16;
  function To_Str_8               (Item : Str_16)              return Str_8          is (To_String (Item, CHAR_16_REPLACEMENT));
  function To_Str_8               (Item : Ptr_Char_8_C)        return Str_8;
  function To_Str_8               (Item : Str_8_Unbound)       return Str_8          renames To_String;
  function To_Str_8_Unbound       (Item : Str_8)               return Str_8_Unbound  renames To_Unbounded_String;
  function To_Str_16              (Item : Str_16_Unbound)      return Str_16         renames To_Wide_String;
  function To_Str_16_Unbound      (Item : Str_16)              return Str_16_Unbound renames To_Unbounded_Wide_String;
  function To_Str_Unbound         (Item : Str_16)              return Str_16_Unbound renames To_Str_16_Unbound;
  function U                      (Item : Str_16)              return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_16              (Item : Str_8)               return Str_16;
  function To_Str_16              (Item : Str_16_C)            return Str_16;
  function To_Str_16              (Item : Ptr_Const_Char_16_C) return Str_16;
  function To_Str_8               (Item : Str_8_C)             return Str_8          is (To_Ada (Item, True));
  function To_Str_8_C             (Item : Str_8)               return Str_8_C        is (To_C   (Item, True));
  function To_Str_8_C             (Item : Str_16)              return Str_8_C        is (To_Str_8_C (To_Str_8 (Item)));
  function To_Str_8_C             (Item : Ptr_Char_8_C)        return Str_8_C        is (To_Str_8_C (To_Str_8 (Item)));
  function To_Str_16              (Item : Char_16)             return Str_16         is ("" & Item);
  function To_Str_16              (Item : Str_8_C)             return Str_16         is (To_Str_16 (To_Str_8 (Item)));
  function To_Str_16              (Item : Ptr_Char_8_C)        return Str_16         is (To_Str_16 (To_Str_8 (Item)));
  function To_Str_16              (Item : Char_8)              return Str_16         is (To_Str_16 ("" & Item));
  function To_Str_16_Unbound      (Item : Str_8)               return Str_16_Unbound is (To_Str_16_Unbound (To_Str_16 (Item)));
  function To_Str_16_Unbound      (Item : Char_16)             return Str_16_Unbound is (To_Str_16_Unbound ("" & Item));
  function To_Str_16_Unbound      (Item : Str_8_C)             return Str_16_Unbound is (To_Str_16_Unbound (To_Str_16 (Item)));
  function To_Str_Unbound         (Item : Str_8_C)             return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound         (Item : Str_8)               return Str_16_Unbound renames To_Str_16_Unbound;
  function To_Str_Unbound         (Item : Char_16)             return Str_16_Unbound renames To_Str_16_Unbound;  
  function To_Str_16_C            (Item : Str_16)              return Str_16_C       is (To_C        (Item & NULL_CHAR_16));
  function To_Str_16_C            (Item : Str_16_Unbound)      return Str_16_C       is (To_Str_16_C (To_Str_16 (Item)));
  function To_Str_C               (Item : Str_16)              return Str_16_C       renames To_Str_16_C;
  function To_Str_C               (Item : Str_16_Unbound)      return Str_16_C       renames To_Str_16_C;
  function To_Str                 (Item : Str_8)               return Str_16         renames To_Str_16;
  function To_Str                 (Item : Str_16_C)            return Str_16         renames To_Str_16;
  function To_Str                 (Item : Ptr_Const_Char_16_C) return Str_16         renames To_Str_16;
  function To_Str                 (Item : Char_16)             return Str_16         renames To_Str_16;
  function To_Str                 (Item : Str_8_C)             return Str_16         renames To_Str_16;
  function To_Str                 (Item : Ptr_Char_8_C)        return Str_16         renames To_Str_16;
  function To_Str                 (Item : Char_8)              return Str_16         renames To_Str_16;
  function To_Str                 (Item : Str_16_Unbound)      return Str_16         renames To_Str_16;
  function S                      (Item : Str_16_C)            return Str_16         renames To_Str_16;
  function S                      (Item : Str_16_Unbound)      return Str_16         renames To_Str_16;
  function To_Ptr_Char_8_C        (Item : in out Str_8_C)      return Ptr_Char_8_C   is (Item (Item'First)'Unchecked_Access);
  function To_Ptr_Char_16_C       (Item : in out Str_16_C)     return Ptr_Char_16_C  is (Item (Item'First)'Unchecked_Access);
  function C                      (Item : in out Str_16_C)     return Ptr_Char_16_C  renames To_Ptr_Char_16_C;
  function C                      (Item : in out Str_8_C)      return Ptr_Char_8_C   renames To_Ptr_Char_8_C;
  function To_Char_8              (Item : Char_16)             return Char_8;
  function To_Char_8_C            (Item : Char_8)              return Char_8_C       is (Char_8_C'Val  (Char_8'Pos    (Item)));
  function To_Char_8              (Item : Char_8_C)            return Char_8         is (Char_8'Val    (Char_8_C'Pos  (Item)));
  function To_Char_16_C           (Item : Char_8_C)            return Char_16_C      is (Char_16_C'Val (Char_8_C'Pos  (item)));
  function To_Char_16_C           (Item : Char_8)              return Char_16_C      is (Char_16_C'Val (Char_8'Pos    (item)));
  function To_Char_16             (Item : Char_16_C)           return Char_16        is (Char_16'Val   (Char_16_C'Pos (Item)));
  function To_Char_16             (Item : Char_8_C)            return Char_16        is (Char_16'Val   (Char_8_C'Pos  (Item)));
  function To_Char_16             (Item : Char_8)              return Char_16        is (Char_16'Val   (Char_8'Pos    (Item)));
  function To_Char_8_C            (Item : Char_16_C)           return Char_8_C       is (To_Char_8_C   (To_Char_8     (To_Char_16 (Item))));
  function To_Char_8_C            (Item : Char_16)             return Char_8_C       is (To_Char_8_C   (To_Char_8     (Item)));
  function To_Char_16_C           (Item : Char_16)             return Char_16_C      is (Char_16_C'Val (Char_16'Pos   (item)));
  function To_Char_8              (Item : Char_16_C)           return Char_8         is (To_Char_8     (To_Char_16    (Item)));

  -- String constants requiring conversions
  EOL_16        : constant Str     := To_Char_16 (CR) & To_Char_16 (LF);
  EOL           : constant Str     := EOL_16;
  TAB_16        : constant Str     := To_Char_16 (HT) & "";
  TAB           : constant Str     := TAB_16;
  NULL_STR_16_C : constant Str_C   := To_Str_16_C (NULL_STR_16);
  NULL_STR_8_C  : constant Str_8_C := To_Str_8_C  (NULL_STR_8);
  NULL_STR_C    : constant Str_C   := NULL_STR_16_C;
  NULL_PTR      : constant Ptr     := NULL_ADDRESS;

  -----------------
  -- Information --
  -----------------

  NAME_ID : constant Str := "Neo";
  VERSION : constant Str := "0.1.0";
  
  package GNAT_Info is new GNAT.Compiler_Version;

  -- We need to get the path separator explicitly because Windows uses backslashes
  function Path_Separator return Char is ((if Index (Current_Directory, "\") = 0 then '/' else '\'));
  function S return Char renames Path_Separator; -- For convience
  
  ---------------
  -- Debugging --
  ---------------

  -- Assertion to check imported C function calls. They raise a Program_Error if the value is null or 0
  procedure Assert (Val : Bool);
  procedure Assert (Val : Ptr);
  procedure Assert (Val : Int_Ptr);
  procedure Assert (Val : Int_C);
  procedure Assert (Val : Int_Unsigned_C);
  procedure Assert (Val : Int_16_Unsigned_C);

  -- ???
  procedure Debug_Assert (Val : Bool);
  procedure Debug_Assert (Val : Ptr);
  procedure Debug_Assert (Val : Int_Ptr);
  procedure Debug_Assert (Val : Int_C);
  procedure Debug_Assert (Val : Int_Unsigned_C);
  procedure Debug_Assert (Val : Int_16_Unsigned_C);
  
  -- Ignore procedures swallow the result of C functions that return useless results
  procedure Ignore (Val : Bool)              is null;
  procedure Ignore (Val : Ptr)               is null;
  procedure Ignore (Val : Int_Ptr)           is null;
  procedure Ignore (Val : Int_C)             is null;
  procedure Ignore (Val : Int_Unsigned_C)    is null;
  procedure Ignore (Val : Int_16_Unsigned_C) is null;
  
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
  
  -------------
  -- Counter --
  -------------
  
  protected type Safe_Counter with Lock_Free is
      function Get return Int;
      procedure Set (Val : Int);
      procedure Increment (Amount : Int); 
      procedure Increment;
      procedure Decrement (Amount : Int);
      procedure Decrement;
    private
      Count : Int := 0;
    end;
    
  ----------
  -- Safe --
  ----------
  
  generic
    type Safe_T is private;
    Initial : Safe_T;
  package Safe is
      protected type T is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;    
  
  generic
    type Safe_T is (<>);
    Initial : Safe_T;
  package Safe_Discrete is
      protected type T with Lock_Free is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;
  
  generic
    type Safe_T is digits <>;
    Initial : Safe_T;
  package Safe_Digits is
      protected type T with Lock_Free is
          function Get return Safe_T;
          procedure Set (Val : Safe_T);
        private
          Data : Safe_T := Initial;
        end;
    end;

  -----------
  -- Timer --
  -----------

  type Timer_State is record
      Start      : Time;
      Last       : Duration := 0.0;
      Is_Stopped : Bool     := False;
    end record;
  function Get_Start_Time return Time;
  function Get_Duration   (Timer :        Timer_State) return Duration;
  procedure Start         (Timer : in out Timer_State);
  procedure Stop          (Timer : in out Timer_State);
  
  -----------
  -- Sizes --
  -----------
  
  GB : constant Int_Ptr := 32768 ** 2;
  MB : constant Int_Ptr := 1024  ** 2;
  kB : constant Int_Ptr := 1024;
  
  -----------
  -- Color --
  -----------

  type Color_State is record Red, Green, Blue, Alpha : Byte := 16#FF#; end record;
  NULL_COLOR         : constant Color_State := (16#FF#, 16#00#, 16#00#, 16#00#);
  COLOR_RED          : constant Color_State := (16#FF#, 16#00#, 16#00#, 16#FF#);
  COLOR_TAN          : constant Color_State := (16#D2#, 16#B4#, 16#8C#, 16#FF#);
  COLOR_BLUE         : constant Color_State := (16#00#, 16#00#, 16#FF#, 16#FF#);
  COLOR_PINK         : constant Color_State := (16#FF#, 16#C0#, 16#CB#, 16#FF#);
  COLOR_AQUA         : constant Color_State := (16#00#, 16#FF#, 16#FF#, 16#FF#);
  COLOR_GRAY         : constant Color_State := (16#80#, 16#80#, 16#80#, 16#FF#);
  COLOR_CYAN         : constant Color_State := (16#00#, 16#FF#, 16#FF#, 16#FF#);
  COLOR_TEAL         : constant Color_State := (16#00#, 16#80#, 16#80#, 16#FF#);
  COLOR_LIME         : constant Color_State := (16#BF#, 16#FF#, 16#00#, 16#FF#);
  COLOR_PUCE         : constant Color_State := (16#CC#, 16#88#, 16#99#, 16#FF#);
  COLOR_PLUM         : constant Color_State := (16#84#, 16#31#, 16#79#, 16#FF#);
  COLOR_MAUVE        : constant Color_State := (16#E0#, 16#B0#, 16#FF#, 16#FF#);
  COLOR_BLACK        : constant Color_State := (16#00#, 16#00#, 16#00#, 16#FF#);
  COLOR_WHITE        : constant Color_State := (16#FF#, 16#FF#, 16#FF#, 16#FF#);
  COLOR_GREEN        : constant Color_State := (16#00#, 16#FF#, 16#00#, 16#FF#);
  COLOR_KHAKI        : constant Color_State := (16#C3#, 16#B0#, 16#91#, 16#FF#);
  COLOR_IVORY        : constant Color_State := (16#FF#, 16#FF#, 16#F0#, 16#FF#);
  COLOR_BEIGE        : constant Color_State := (16#F5#, 16#F5#, 16#DC#, 16#FF#);
  COLOR_WHEAT        : constant Color_State := (16#F5#, 16#DE#, 16#B3#, 16#FF#);
  COLOR_CORAL        : constant Color_State := (16#FF#, 16#7F#, 16#50#, 16#FF#);
  COLOR_OLIVE        : constant Color_State := (16#80#, 16#80#, 16#00#, 16#FF#);
  COLOR_SILVER       : constant Color_State := (16#C0#, 16#C0#, 16#C0#, 16#FF#);
  COLOR_YELLOW       : constant Color_State := (16#FF#, 16#FF#, 16#00#, 16#FF#);
  COLOR_ORANGE       : constant Color_State := (16#FF#, 16#A5#, 16#00#, 16#FF#);
  COLOR_VIOLET       : constant Color_State := (16#EE#, 16#82#, 16#EE#, 16#FF#);
  COLOR_PURPLE       : constant Color_State := (16#80#, 16#00#, 16#80#, 16#FF#);
  COLOR_SALMON       : constant Color_State := (16#FA#, 16#80#, 16#72#, 16#FF#);
  COLOR_INDIGO       : constant Color_State := (16#4B#, 16#00#, 16#82#, 16#FF#);
  COLOR_MAROON       : constant Color_State := (16#80#, 16#00#, 16#00#, 16#FF#);
  COLOR_GOLDEN       : constant Color_State := (16#FF#, 16#D7#, 16#00#, 16#FF#);
  COLOR_MAGENTA      : constant Color_State := (16#FF#, 16#00#, 16#FF#, 16#FF#);
  COLOR_FUCHSIA      : constant Color_State := (16#FF#, 16#77#, 16#FF#, 16#FF#);
  COLOR_CRIMSON      : constant Color_State := (16#DC#, 16#14#, 16#3C#, 16#FF#);
  COLOR_LAVENDER     : constant Color_State := (16#B5#, 16#7E#, 16#DC#, 16#FF#);
  COLOR_SKY_BLUE     : constant Color_State := (16#87#, 16#CE#, 16#EB#, 16#FF#);
  COLOR_CHARCOAL     : constant Color_State := (16#46#, 16#46#, 16#46#, 16#FF#);
  COLOR_HOT_PINK     : constant Color_State := (16#FC#, 16#0F#, 16#C0#, 16#FF#);
  COLOR_NAVY_BLUE    : constant Color_State := (16#00#, 16#00#, 16#80#, 16#FF#);
  COLOR_GOLDEN_ROD   : constant Color_State := (16#DA#, 16#A5#, 16#20#, 16#FF#);
  COLOR_LIGHT_BLUE   : constant Color_State := (16#AD#, 16#D8#, 16#E6#, 16#FF#);
  COLOR_ROYAL_BLUE   : constant Color_State := (16#08#, 16#4C#, 16#9E#, 16#FF#);
  COLOR_AQUAMARINE   : constant Color_State := (16#7F#, 16#FF#, 16#D4#, 16#FF#);
  COLOR_CHARTREUSE   : constant Color_State := (16#7F#, 16#FF#, 16#00#, 16#FF#);
  COLOR_FOREST_GREEN : constant Color_State := (16#22#, 16#8B#, 16#22#, 16#FF#);
end;
