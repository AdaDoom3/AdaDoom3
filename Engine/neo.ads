
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

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Strings.Wide_Unbounded;   use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Fixed;       use Ada.Strings.Wide_Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings;                  use Ada.Strings;
with Ada.Wide_Text_IO;             use Ada.Wide_Text_IO;
with Ada.Wide_Characters.Handling; use Ada.Wide_Characters.Handling;
with Ada.Characters.Latin_1;       use Ada.Characters.Latin_1;
with Ada.Streams;                  use Ada.Streams;
with Ada.Calendar;                 use Ada.Calendar;
with Ada.Containers;               use Ada.Containers;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Finalization;             use Ada.Finalization;
with Interfaces.C;                 use Interfaces.C;
with Interfaces;                   use Interfaces;
with System;                       use System;

-- System independant base code including data types, path settings, parsing, and IO. All of the boiler-plate stuff goes here.
package Neo is

  -----------
  -- Paths --
  -----------

  PATH_LOGS            : constant Wide_String := "./Logs/";
  PATH_ASSETS          : constant Wide_String := "./Assets/";
  PATH_SETTINGS        : constant Wide_String := "./Settings/";
  PATH_ICON            : constant Wide_String := PATH_ASSETS & "icon";
  PATH_CURSOR_ACTIVE   : constant Wide_String := PATH_ASSETS & "cursor_active";
  PATH_CURSOR_INACTIVE : constant Wide_String := PATH_ASSETS & "cursor_inactive";
  PATH_LOCALIZATION    : constant Wide_String := PATH_SETTINGS & "localization.csv";
  PATH_CONFIGURATION   : constant Wide_String := PATH_SETTINGS & "conf.txt";

  -----------
  -- Types --
  -----------
  --
  -- Subtypes act as a "renamed" type of anoth
  subtype Ptr                      is Address;
  subtype Bool                     is Boolean;
  subtype Stream                   is Ada.Streams.Stream_Element;
  subtype Char_8_C                 is Char;
  subtype Char_8                   is Character;
  subtype Char_16_C                is WChar_T;
  subtype Char_16                  is Wide_Character;
  subtype Str_8_C                  is Char_Array;
  subtype Str_8                    is String;
  subtype Str_8_Unbound            is Unbounded_String;
  subtype Str_16_C                 is WChar_Array;
  subtype Str_16                   is Wide_String;
  subtype Str_16_Unbound           is Unbounded_Wide_String;
  subtype Str                      is Str_16;
  subtype Str_C                    is Str_16_C;
  type    Int_Ptr                  is mod MEMORY_SIZE;
  type    Int_8_Percent            is range 1..100; for Int_8_Percent'size use 64;
  subtype Int_8_Unsigned           is Unsigned_8;
  subtype Byte                     is Int_8_Unsigned;
  subtype Int_8_Unsigned_C         is Interfaces.C.Unsigned_Char;
  subtype Int_8_Signed             is Short_Short_Integer;
  subtype Int_8_Signed_C           is Interfaces.C.Char;
  subtype Int_8                    is Int_8_Signed;
  subtype Int_8_C                  is Int_8_Signed_C;
  subtype Int_8_Natural            is Int_8_Unsigned;
  subtype Int_8_Positive           is Int_8_Unsigned range 1..Int_8_Unsigned'last;
  subtype Int_16_Unsigned          is Unsigned_16;
  subtype Int_16_Unsigned_C        is Interfaces.C.Unsigned_Short;
  subtype Int_16_Signed            is Short_Integer;
  subtype Int_16_Signed_C          is Interfaces.C.Short;
  subtype Int_16                   is Int_16_Signed;
  subtype Int_16_C                 is Int_16_Signed_C;
  subtype Int_16_Natural           is Int_16_Unsigned;
  subtype Int_16_Positive          is Int_8_Unsigned range 1..Int_8_Unsigned'last;
  subtype Int_32_Unsigned          is Unsigned_32;
  subtype Int_32_Unsigned_C        is Interfaces.C.Unsigned;
  subtype Int_32_Signed            is Integer;
  subtype Int_32_Signed_C          is Interfaces.C.Int;
  subtype Int_32                   is Int_32_Signed;
  subtype Int_32_C                 is Int_32_Signed_C;
  subtype Int_32_Natural           is Natural;
  subtype Int_32_Positive          is Positive;
  subtype Int                      is Int_32;
  subtype Int_C                    is Int_32_C;
  subtype Int_64_Unsigned          is Unsigned_64;
  subtype Int_64_Unsigned_C        is Int_64_Unsigned;
  subtype Int_64_Signed            is Long_Long_Integer;
  subtype Int_64_Signed_C          is Int_64_Signed;
  subtype Int_64                   is Int_64_Signed;
  subtype Int_64_C                 is Int_64_Signed_C;
  subtype Int_64_Natural           is Int_64_Unsigned;
  subtype Int_64_Positive          is Int_64_Signed range 1..Int_64'last;
  subtype Int_Size_C               is Interfaces.C.Size_T;
  subtype Real_64                  is Long_Float;
  subtype Real_64_C                is Interfaces.C.Double;
  subtype Real_64_Natural          is Real_64 range 0.0..Real_64'last;
  subtype Real_64_Positive         is Real_64 range 1.0..Real_64'last;
  subtype Real_64_Range            is Real_64 range -100.0..100.0;
  subtype Real_64_Percent          is Real_64 range 0.0..100.0;
  subtype Real_64_Degree           is Real_64 range 1.0..360.0;
  subtype Real_32                  is Float;
  subtype Real_32_C                is Interfaces.C.C_Float;
  subtype Real_32_Natural          is Real_32 range 0.0..Real_32'last;
  subtype Real_32_Positive         is Real_32 range 1.0..Real_32'last;
  subtype Real_32_Range            is Real_32 range -100.0..100.0;
  subtype Real_32_Percent          is Real_32 range 0.0..100.0;
  subtype Real_32_Degree           is Real_32 range 1.0..360.0;
  subtype Real                     is Real_32;
  type Ptr_Str_16                  is access all Str_16;
  type Ptr_Str_8_C                 is access all Str_8_C;
  type Ptr_Str_8                   is access all Str_8;
  type Ptr_Str_16_C                is access all Str_16_C;
  type Ptr_Int_Ptr                 is access all Int_Ptr;
  type Ptr_Int_64_Unsigned_C       is access all Int_64_Unsigned_C;
  type Ptr_Int_32_Unsigned_C       is access all Int_32_Unsigned_C;
  type Ptr_Int_32_Unsigned         is access all Int_32_Unsigned;
  type Ptr_Int_32_Signed_C         is access all Int_32_Signed_C;
  type Ptr_Int_16_Unsigned_C       is access all Int_16_Unsigned_C;
  type Ptr_Int_8_Unsigned_C        is access all Int_8_Unsigned_C;
  type Ptr_Real_32_C               is access all Real_32_C;
  type Ptr_Char_8_C                is access all Interfaces.C.Char;
  type Ptr_Char_16_C               is access all Char_16_C;
  type Ptr_Const_Char_8_C          is access constant Char_8_C;
  type Ptr_Const_Char_16_C         is access constant Char_16_C;
  type Ptr_Const_Str_16_C          is access constant Str_16_C;
  type Ptr_Procedure               is access procedure;
  type Ptr_Function_Localize       is access function  (Item : Str_16) return Str_16;
  type Ptr_Procedure_Put           is access procedure (Item : Str_16);
  subtype Array_Stream             is Stream_Element_Array;
  type Array_Bool                  is array (Int_32_Positive range <>) of Bool;
  type Array_Duration              is array (Int_32_Positive range <>) of Duration;
  type Array_Ptr                   is array (Int_32_Positive range <>) of Ptr;
  type Array_Ptr_Str_16            is array (Int_32_Positive range <>) of Ptr_Str_16;
  type Array_Str_16_Unbound        is array (Int_32_Positive range <>) of Str_16_Unbound;
  type Array_Char_8_C              is array (Int_32_Positive range <>) of Char_8_C;
  type Array_Char_8                is array (Int_32_Positive range <>) of Char_8;
  type Array_Char_16_C             is array (Int_32_Positive range <>) of Char_16_C;
  type Array_Char_16               is array (Int_32_Positive range <>) of Char_16;
  type Array_Int_Ptr               is array (Int_32_Positive range <>) of Int_Ptr;
  type Array_Int_8_Unsigned_C      is array (Int_32_Positive range <>) of Int_8_Unsigned_C;
  type Array_Int_8_Unsigned        is array (Int_32_Positive range <>) of Int_8_Unsigned;
  type Array_Int_8_Signed_C        is array (Int_32_Positive range <>) of Int_8_Signed_C;
  type Array_Int_8_Signed          is array (Int_32_Positive range <>) of Int_8_Signed;
  type Array_Int_8_Natural         is array (Int_32_Positive range <>) of Int_8_Natural;
  type Array_Int_8_Positive        is array (Int_32_Positive range <>) of Int_8_Positive;
  type Array_Int_16_Unsigned_C     is array (Int_32_Positive range <>) of Int_16_Unsigned_C;
  type Array_Int_16_Unsigned       is array (Int_32_Positive range <>) of Int_16_Unsigned;
  type Array_Int_16_Signed_C       is array (Int_32_Positive range <>) of Int_16_Signed_C;
  type Array_Int_16_Signed         is array (Int_32_Positive range <>) of Int_16_Signed;
  type Array_Int_16_Natural        is array (Int_32_Positive range <>) of Int_16_Natural;
  type Array_Int_16_Positive       is array (Int_32_Positive range <>) of Int_16_Positive;
  type Array_Int_32_Unsigned_C     is array (Int_32_Positive range <>) of Int_32_Unsigned_C;
  type Array_Int_32_Unsigned       is array (Int_32_Positive range <>) of Int_32_Unsigned;
  type Array_Int_32_Signed_C       is array (Int_32_Positive range <>) of Int_32_Signed_C;
  type Array_Int_32_Signed         is array (Int_32_Positive range <>) of Int_32_Signed;
  type Array_Int_32_Natural        is array (Int_32_Positive range <>) of Int_32_Natural;
  type Array_Int_32_Positive       is array (Int_32_Positive range <>) of Int_32_Positive;
  type Array_Int_64_Unsigned       is array (Int_32_Positive range <>) of Int_64_Unsigned;
  type Array_Int_64_Unsigned_C     is array (Int_32_Positive range <>) of Int_64_Unsigned_C;
  type Array_Int_64_Signed         is array (Int_32_Positive range <>) of Int_64_Signed;
  type Array_Int_64_Signed_C       is array (Int_32_Positive range <>) of Int_64_Signed_C;
  type Array_Int_64_Natural        is array (Int_32_Positive range <>) of Int_64_Natural;
  type Array_Int_64_Positive       is array (Int_32_Positive range <>) of Int_64_Positive;
  type Array_Int_Size_C            is array (Int_32_Positive range <>) of Int_Size_C;
  type Array_Real_64_C             is array (Int_32_Positive range <>) of Real_64_C;
  type Array_Real_64               is array (Int_32_Positive range <>) of Real_64;
  type Array_Real_64_Natural       is array (Int_32_Positive range <>) of Real_64_Natural;
  type Array_Real_64_Positive      is array (Int_32_Positive range <>) of Real_64_Positive;
  type Array_Real_64_Percent       is array (Int_32_Positive range <>) of Real_64_Percent;
  type Array_Real_64_Degree        is array (Int_32_Positive range <>) of Real_64_Degree;
  type Array_Real_32_C             is array (Int_32_Positive range <>) of Real_32_C;
  type Array_Real_32               is array (Int_32_Positive range <>) of Real_32;
  type Array_Real_32_Natural       is array (Int_32_Positive range <>) of Real_32_Natural;
  type Array_Real_32_Positive      is array (Int_32_Positive range <>) of Real_32_Positive;
  type Array_Real_32_Percent       is array (Int_32_Positive range <>) of Real_32_Percent;
  type Array_Real_32_Degree        is array (Int_32_Positive range <>) of Real_32_Degree;
  type Ptr_Array_Ptr               is access all Array_Ptr;
  type Ptr_Array_Ptr_Str_16        is access all Array_Ptr_Str_16;
  type Ptr_Array_Str_16_Unbound    is access all Array_Str_16_Unbound;
  type Ptr_Array_Char_8_C          is access all Array_Char_8_C;
  type Ptr_Array_Char_8            is access all Array_Char_8;
  type Ptr_Array_Char_16_C         is access all Array_Char_16_C;
  type Ptr_Array_Char_16           is access all Array_Char_16;
  type Ptr_Array_Int_8_Unsigned_C  is access all Array_Int_8_Unsigned_C;
  type Ptr_Array_Int_8_Unsigned    is access all Array_Int_8_Unsigned;
  type Ptr_Array_Int_8_Signed_C    is access all Array_Int_8_Signed_C;
  type Ptr_Array_Int_8_Signed      is access all Array_Int_8_Signed;
  type Ptr_Array_Int_8_Natural     is access all Array_Int_8_Natural;
  type Ptr_Array_Int_8_Positive    is access all Array_Int_8_Positive;
  type Ptr_Array_Int_16_Unsigned_C is access all Array_Int_16_Unsigned_C;
  type Ptr_Array_Int_16_Unsigned   is access all Array_Int_16_Unsigned;
  type Ptr_Array_Int_16_Signed_C   is access all Array_Int_16_Signed_C;
  type Ptr_Array_Int_16_Signed     is access all Array_Int_16_Signed;
  type Ptr_Array_Int_16_Natural    is access all Array_Int_16_Natural;
  type Ptr_Array_Int_16_Positive   is access all Array_Int_16_Positive;
  type Ptr_Array_Int_32_Unsigned_C is access all Array_Int_32_Unsigned_C;
  type Ptr_Array_Int_32_Unsigned   is access all Array_Int_32_Unsigned;
  type Ptr_Array_Int_32_Signed_C   is access all Array_Int_32_Signed_C;
  type Ptr_Array_Int_32_Signed     is access all Array_Int_32_Signed;
  type Ptr_Array_Int_32_Natural    is access all Array_Int_32_Natural;
  type Ptr_Array_Int_32_Positive   is access all Array_Int_32_Positive;
  type Ptr_Array_Int_64_Unsigned   is access all Array_Int_64_Unsigned;
  type Ptr_Array_Int_64_Unsigned_C is access all Array_Int_64_Unsigned_C;
  type Ptr_Array_Int_64_Signed     is access all Array_Int_64_Signed;
  type Ptr_Array_Int_64_Signed_C   is access all Array_Int_64_Signed_C;
  type Ptr_Array_Int_64_Natural    is access all Array_Int_64_Natural;
  type Ptr_Array_Int_64_Positive   is access all Array_Int_64_Positive;
  type Ptr_Array_Int_Size_C        is access all Array_Int_Size_C;
  type Ptr_Array_Real_64_C         is access all Array_Real_64_C;
  type Ptr_Array_Real_64           is access all Array_Real_64;
  type Ptr_Array_Real_64_Natural   is access all Array_Real_64_Natural;
  type Ptr_Array_Real_64_Positive  is access all Array_Real_64_Positive;
  type Ptr_Array_Real_64_Percent   is access all Array_Real_64_Percent;
  type Ptr_Array_Real_64_Degree    is access all Array_Real_64_Degree;
  type Ptr_Array_Real_32_C         is access all Array_Real_32_C;
  type Ptr_Array_Real_32           is access all Array_Real_32;
  type Ptr_Array_Real_32_Natural   is access all Array_Real_32_Natural;
  type Ptr_Array_Real_32_Positive  is access all Array_Real_32_Positive;
  type Ptr_Array_Real_32_Percent   is access all Array_Real_32_Percent;
  type Ptr_Array_Real_32_Degree    is access all Array_Real_32_Degree;

  -------------
  -- Console --
  -------------

  -- Task safe input and output
  function Lines                      return Int_64_Natural;
  function Log                        return Str;
  function Input_Entry                return Str;
  function Line_Size                  return Int_32_Positive;
  function Localize     (Item  : Str) return Str;
  procedure Put         (Item  : Char_16);
  procedure Put         (Item  : Str);
  procedure Line        (Item  : Char_16);
  procedure Line        (Item  : Str);
  procedure Line        (Count : Int_32_Positive := 1);
  procedure Line_Size   (Val   : Int_32_Positive);
  procedure Input_Entry (Val   : Str);

  -- Comment here !!!
  procedure Submit      (Text : Str);
  function Autocomplete (Text : Str) return Array_Str_16_Unbound;
  procedure Initialize_Configuration;
  procedure Finalize_Configuration;

  ----------
  -- CVar --
  ----------
  --
  -- The cvar package or "console variable" is the core of a game engine. It represents a changable setting and could be loaded from a
  -- configuration file or set via an in-game or external console window. In the Neo engine this package allows communication between
  -- between tasks and upon instantiation will also be able to parse itself from a console's commandline, so there are 2 ways to set a
  -- cvar: have it be parsed as a string form the console's "Submit" procedure or by directly using the "Set" and "Get" subprograms
  -- within the instantiated package. However, it is limited to only discrete types (integers and enumerations).
  --
  -- Ex.
  --   ...
  --   type Graphics_Kind is (Low_Quality, Medium_Quanlity, High_Quality, Ultra_Quality);
  --   package Graphics_Settings (Name     => "vidquality",
  --                              Help     => "Quality of graphics",
  --                              Var_T    => Graphics_Kind,
  --                              Initial  => Medium_Quanlity,
  --                              Settable => True);
  --   ...
  --   Graphics_Settings.Set (Ultra_Quality);
  --   ...
  --   if Graphics_Settings.Get = Low_Quality then
  --   ... 
  --   Submit ("vidquality low_quality");
  --   ...
  --

  generic
    Name     : Str;
    Help     : Str;
    type Var_T is (<>);
    Initial  : Var_T := Var_T'First;
    Settable : Bool  := True;
  package CVar is
      procedure Set (Val : Var_T);
      function Get return Var_T;
    end; 

  -------------
  -- Command --
  -------------
  --
  -- Comment here !!!
  --

  generic
    Name : Str;
    with procedure Callback (Args : Array_Str_16_Unbound);
    Save : access function return Str := null;
  package Command is end; 

  ------------
  -- Status --
  ------------

  -- A mutex or task-safe flag, however you want to look at it...
  protected type Safe_Status is
      function Occupied return Bool;
      procedure Occupied (Value : Bool);
    private
      Status : Bool := False;
    end;

  -----------
  -- Timer --
  -----------

  -- Simple timer type
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
  -- Color --
  -----------

  -- A three byte color type R8G8B8
  type Color_State is record
      Red   : Int_8_Unsigned;
      Green : Int_8_Unsigned;
      Blue  : Int_8_Unsigned;
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

  -----------------
  -- Conversions --
  -----------------

  -- Unchecked converions
  function To_Ptr_Int_16_Unsigned_C is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_16_Unsigned_C is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr_Int_16_Unsigned_C);
  function To_Ptr_Int_32_Unsigned   is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Int_32_Unsigned);
  function To_Ptr_Char_8_C          is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Char_8_C);
  function To_Ptr_Char_16_C         is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Char_16_C);
  function To_Ptr_Const_Char_8_C    is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Const_Char_8_C);
  function To_Ptr_Const_Char_16_C   is new Ada.Unchecked_Conversion (Ptr,                   Ptr_Const_Char_16_C);
  function To_Ptr_Const_Char_16_C   is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr_Const_Char_16_C);
  function To_Ptr                   is new Ada.Unchecked_Conversion (Ptr_Const_Char_16_C,   Ptr);
  function To_Ptr                   is new Ada.Unchecked_Conversion (Int_Ptr,               Ptr);
  function To_Int_32_Unsigned_C     is new Ada.Unchecked_Conversion (Int_32_Signed_C,       Int_32_Unsigned_C);
  function To_Int_32_Unsigned       is new Ada.Unchecked_Conversion (Int_32_Signed_C,       Int_32_Unsigned);
  function To_Int_32_Unsigned       is new Ada.Unchecked_Conversion (Real_32,               Int_32_Unsigned);
  function To_Int_32_Signed_C       is new Ada.Unchecked_Conversion (Int_32_Unsigned_C,     Int_32_Signed_C);
  function To_Int_32_Signed         is new Ada.Unchecked_Conversion (Int_32_Unsigned,       Int_32_Signed);
  function To_Int_16_Signed         is new Ada.Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed);
  function To_Int_16_Signed_C       is new Ada.Unchecked_Conversion (Int_16_Unsigned,       Int_16_Signed_C);
  function To_Int_16_Unsigned       is new Ada.Unchecked_Conversion (Char_16_C,             Int_16_Unsigned);
  function To_Int_Ptr               is new Ada.Unchecked_Conversion (Ptr_Int_16_Unsigned_C, Int_Ptr);
  function To_Int_Ptr               is new Ada.Unchecked_Conversion (Ptr_Const_Char_16_C,   Int_Ptr);
  function To_Int_Ptr               is new Ada.Unchecked_Conversion (Ptr_Const_Char_8_C,    Int_Ptr);
  function To_Int_Ptr               is new Ada.Unchecked_Conversion (Ptr,                   Int_Ptr);
  function To_Real_32               is new Ada.Unchecked_Conversion (Real_32,               Int_32_Unsigned);

  -- Assorted constants 
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
  NULL_STR_8          : constant Str_8           := "";
  NULL_STR_8_UNBOUND  : constant Str_8_Unbound   := NULL_UNBOUNDED_STRING;
  WORD_SIZE_IMAGE     : constant Str_8           := WORD_SIZE'Img (WORD_SIZE'Img'First + 1..WORD_SIZE'Img'Last);
  EOL_8               : constant Str_8           := CR & LF;
  TAB_8               : constant Str_8           := "" & HT;

  -- String and character conversions and operations (note, the order of declaration matters here due to the presence of expression functions
  generic
    type Num_T is mod <>;
  function Generic_To_Str_16 (Item : Num_T; Base : Int_32_Positive; Do_Pad_Zeros : Bool := True) return Str_16;
  function To_Str_8               (Item : Str_16)              return Str_8;
  function To_Str_8               (Item : Ptr_Const_Char_8_C)  return Str_8;
  function To_Str_8               (Item : Str_8_Unbound)       return Str_8               renames To_String;
  function To_Str_8_Unbound       (Item : Str_8)               return Str_8_Unbound       renames To_Unbounded_String;
  function To_Str_16              (Item : Str_16_Unbound)      return Str_16              renames To_Wide_String;
  function To_Str_16_Unbound      (Item : Str_16)              return Str_16_Unbound      renames To_Unbounded_Wide_String;
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
  function Split                  (Item : Str_16; On : Str_16 := " ") return Array_Str_16_Unbound;

  -- More contants
  EOL_16        : constant Str_16   := To_Char_16 (CR) & To_Char_16 (LF);
  TAB_16        : constant Str_16   := To_Char_16 (HT) & "";
  NULL_STR_16_C : constant Str_16_C := To_Str_16_C (NULL_STR_16);
  NULL_STR_8_C  : constant Str_8_C  := To_Str_8_C  (NULL_STR_8);
  NULL_STR_C    : constant Str_16_C := NULL_STR_16_C;
  PATH_SEP      : constant Str_16   := "/";
  NULL_PTR      : constant Ptr      := NULL_ADDRESS;

  -- ???
  package Vector_Str_16_Unbound is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Str_16_Unbound);

  -------------
  -- Parsing --
  -------------
  --
  -- 
  --

  -- Subprograms to load raw data (say raw binrary data for sharders)
  function Load (Path : Str) return Stream_Element_Array;

  -- A package for catagorizing sets of parsers that load a single data file (e.g. an image format)
  generic
    type Format_T is (<>);
    type T is private;
  package Handler is

      -- 
      function Load  (Path : Str) return T;

      -- 
      generic
        Kind       : Format_T;
        Extensions : Str; -- Separated by commas: "tga,png,tga"
        with function Load (Path : Str) return T;
      package Format is end;
    end;

  -- A simple parser for loading configuration and data files. To parse a file instantiate the package and calls its functions
  generic
    Path            : Str;
    Comment         : Str  := NULL_STR; 
    Comment_Start   : Str  := NULL_STR;
    Comment_End     : Str  := NULL_STR;
    Separator       : Char_16 := ' ';
    Tab_Replacement : Char_16 := ' ';
  package Parser is

      -- Test for the position of the parser currently in the file
      function At_EOL return Bool;
      function At_EOF return Bool;

      -- Fetch the next item in the file
      function Next return Str_16_Unbound;
      function Next return Str is (To_Str (Next));
      function Next return Real_64;
      function Next return Real_64;
      function Next return Real_64;
      function Next return Real_64;

      -- Parse the entire next line 
      function Next_Line return Str_16_Unbound;
      function Next_Line return Str is (To_Str (Next_Line));

      -- A "set" is a bounded item like a string surrounded by quotes or a C-style comment /* */
      function Next_Set           (Ending : Str) return Str_16_Unbound;
      function Next_Set           (Ending : Str) return Str is (To_Str (Next_Set (Ending)));
      function Next_Set (Starting, Ending : Str) return Str_16_Unbound;
      function Next_Set (Starting, Ending : Str) return Str is (To_Str (Next_Set (Starting, Ending)));

      -- Procedures for ignoring data or skiping useless information
      procedure Skip_Line;
      procedure Skip_Set           (Ending : Str);
      procedure Skip_Set (Starting, Ending : Str);
      procedure Skip     (Amount : Int_32_Positive := 1);

      -- Assert that the next item must match the text argument
      procedure Assert (Text               : Str);
      procedure Assert (T1, T2             : Str);
      procedure Assert (T1, T2, T3         : Str);
      procedure Assert (T1, T2, T3, T4     : Str);
      procedure Assert (T1, T2, T3, T4, T5 : Str);
    end;

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

  ----------------
  -- Containers --
  ----------------

  -- Task-safe and normal mutli-way tree type with sensible defaults
  generic
    type Tree_T is private;
  package Trees is
      package Unsafe is new Ada.Containers.Indefinite_Multiway_Trees (Tree_T, "="); 
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
      protected type Safe_Tree is   
          procedure Clear;
          procedure Replace         (Pos    : Cursor; Item : Tree_T);
          procedure Query           (Pos    : Cursor; Process : not null access procedure (Item : Tree_T));
          procedure Update          (Pos    : Cursor; Process : not null access procedure (Item : in out Tree_T));
          procedure Iterate_Subtree (Pos    : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Iterate                          (Process : not null access procedure (Pos : Cursor));
          procedure Iterate         (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Iterate_Back    (Parent : Cursor; Process : not null access procedure (Pos : Cursor));
          procedure Set             (Source : Unsafe.Tree);
          procedure Move            (Source : in out Unsafe.Tree);
          procedure Delete_Leaf     (Pos    : in out Cursor);
          procedure Delete_Subtree  (Pos    : in out Cursor);
          procedure Swap            (I, J   : Cursor);
          procedure Delete          (Parent         : Cursor);
          procedure Prepend         (Parent         : Cursor; Item : Tree_T; Count : Int_32_Positive := 1);
          procedure Append          (Parent         : Cursor; Item : Tree_T; Count : Int_32_Positive := 1);
          procedure Insert          (Parent, Before : Cursor; Item : Tree_T; Count : Int_32_Positive := 1);
          procedure Splice_Subtree  (Parent, Before, Pos           : Cursor);
          procedure Splice          (Parent, Before, Source_Parent : Cursor);
          procedure Copy_Subtree    (Parent, Before, Source        : Cursor);
          procedure Next            (Pos    : in out Cursor);
          procedure Previous        (Pos    : in out Cursor);
          function Subtree_Nodes    (Pos    : Cursor)                return Int_32_Positive;
          function Depth            (Pos    : Cursor)                return Int_32_Positive;
          function "="              (L, R   : Cursor)                return Bool;
          function Is_Root          (Pos    : Cursor)                return Bool;
          function Is_Leaf          (Pos    : Cursor)                return Bool;
          function Has              (Pos    : Cursor)                return Bool;
          function Has              (Item   : Tree_T)                return Bool;
          function Equals           (Item   : Unsafe.Tree)           return Bool;
          function Find             (Item   : Tree_T)                return Cursor;
          function Get              (Pos    : Cursor)                return Tree_T;
          function Find_In_Subtree  (Pos    : Cursor; Item : Tree_T) return Cursor;
          function Ancestor_Find    (Pos    : Cursor; Item : Tree_T) return Cursor;
          function Child_Count      (Parent        : Cursor)         return Int_32_Positive;
          function Child_Depth      (Parent, Child : Cursor)         return Int_32_Positive;
          function Parent           (Pos    : Cursor)                return Cursor;
          function First            (Parent : Cursor)                return Cursor;
          function First            (Parent : Cursor)                return Tree_T;
          function Last             (Parent : Cursor)                return Tree_T;
          function Last             (Parent : Cursor)                return Cursor;
          function Is_Empty                                          return Bool;
          function Node_Count                                        return Int_32_Positive;
          function Root                                              return Cursor;
          function Get                                               return Unsafe.Tree;
        private
          This : Unsafe.Tree;
        end;
    end;

  -- Task-safe and normal vector and array type with sensible defaults
  generic
    type Vec_T is private;
  package Vectors is
      package Unsafe is new Ada.Containers.Indefinite_Vectors (Int_32_Positive, Vec_T);
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
      protected type Safe_Vector is
          procedure Clear;
          procedure Set     (Val : Unsafe.Vector);
          procedure Next    (Pos : in out Cursor);
          procedure Replace (Pos :        Cursor;      Item : Vec_T);
          procedure Append                            (Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Prepend                           (Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Insert  (Before : Int_32_Positive; Item : Vec_T; Count : Int_32_Positive := 1);
          procedure Delete  (Index  : Int_32_Positive;               Count : Int_32_Positive := 1);
          function Has      (Pos    : Cursor)          return Bool;
          function Get      (Pos    : Cursor)          return Vec_T;
          function Get      (Index  : Int_32_Positive) return Vec_T;
          function Get                                 return Unsafe.Vector;
          function First                               return Cursor;
          function Length                              return Int_32_Positive;
        private
          This : Unsafe.Vector;
        end;

      -- Easy conversion to static arrays and back from the vector types
      type Unsafe_Array is array (Int_32_Positive range <>) of Vec_T;
      function To_Safe_Vector   (Item : Unsafe_Array)  return Safe_Vector;
      function To_Unsafe_Vector (Item : Unsafe_Array)  return Unsafe.Vector;
      function To_Unsafe_Array  (Item : Unsafe.Vector) return Unsafe_Array;
      function To_Unsafe_Array  (Item : Safe_Vector)   return Unsafe_Array is (To_Unsafe_Array (Item.Get));
    end;

  -- Task-safe and normal hashed map type with sensible defaults
  generic
    type Map_T is private;
  package Hashed is
      package Unsafe is new Ada.Containers.Indefinite_Hashed_Maps (Str_16_Unbound, Map_T, Wide_Hash, "=");
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
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
          function First                   return Cursor;
        private
          This : Unsafe.Map;
        end;
    end;

  -- Task-safe and normal ordered map type with sensible defaults
  generic
    type Key_T is (<>);
    type Map_T is private;
  package Ordered is
      package Unsafe is new Ada.Containers.Indefinite_Ordered_Maps (Key_T, Map_T, "<", "="); use Unsafe;
      subtype Cursor is Unsafe.Cursor;
      NO_ELEMENT : Cursor := Unsafe.NO_ELEMENT;
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
        private
          This : Unsafe.Map;
        end;
    end;
end;
