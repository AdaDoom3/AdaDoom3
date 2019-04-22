
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

with Ada.Direct_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Directories;       use Ada.Directories;
with Neo.Core.Math;         use Neo.Core.Math;
with Neo.Core.Arrays;       use Neo.Core.Arrays;
with Neo.Core.Strings;      use Neo.Core.Strings;
with Neo.Core.Console;      use Neo.Core.Console;
with Neo.Core.Vectors;
with Neo.Core.Ordered;
with Neo.Core.Hashed;
with Neo.Core.Hashed_Indefinite;
with Neo.Core.Trees;

-- Separator for the "Data" layer consisting of packages for loading and understanding file formats
package Neo.Data is

  ------------
  -- Binary --
  ------------

  -- Load a binary file with optional padding specified in bytes (needed for SPIR-V shaders among other things)
  function Load (Path : Str; Padding : Positive := 1) return Array_Byte;

  -- Seek through a file by a certain number of bytes
  procedure Skip (File : in out Ada.Streams.Stream_IO.File_Type; Bytes : Positive);

  --------------------------
  -- Description_Language --
  --------------------------
  --
  -- Modified ARM BNF for OpenDDL. Added conventions are "to" for range between digits, rounded braces for grouping, quotes for
  -- escaping the various braces (e.g. "{" for {, "[" for [, et cetera), and "* #" for repetative items.
  --
  -- https://web.archive.org/web/20180809181020/http://openddl.org/
  --
  -- Names:
  --
  --   identifier        ::= ascii-letter | {[_] (ascii-letter | digit)}
  --   name              ::= ($ | %) identifier
  --   reference         ::= name | null | $identifier {%identifier}
  --
  -- Digits:
  --
  --   hex-digit         ::= digit | A to F | a to f
  --   octal-digit       ::= 0 to 7
  --   binary-digit      ::= 0 | 1
  --
  -- Characters:
  --
  --   escape-character  ::= \ (" | ' | ? | \ | a | b | f | n | r | t | v | x hex-digit * 2 | u hex-digit * 4 | U hex-digit * 8
  --   character         ::= escape-character | visible-ascii-character
  --
  -- Literals:
  --
  --   boolean-literal   ::= false | true
  --   decimal-literal   ::= digit {[_] digit}
  --   hex-literal       ::= 0 (x | X) hex-digit {[_] hex-digit}
  --   octal-literal     ::= 0 (o | O) octal-digit {[_] octal-digit}
  --   binary-literal    ::= 0 (b | B) binary-digit {[_] binary-digit}
  --   character-literal ::= 'character'
  --   integer-literal   ::= [+ | -] decimal-literal | (hex-literal | octal-literal | binary-literal)
  --   float-literal     ::= [+ | -] decimal-literal [. decimal-literal] [(E | e) integer-literal]
  --   string-literal    ::= "{character}"
  --
  -- Data:
  --
  --   data-type         ::= bool | int8 | int16 | int32 | int64 | unsigned_int8 | unsigned_int16 | unsigned_int32 | unsigned_int64 | half
  --                           | float | double | string | ref | type | float16 | float32 | float64
  --                           | b | i8 | i16 | i32 | i64 | u8 | u16 | u32 | u64 | h | d | s | r | t | f | f16 | f32 | f64
  --   data-list-item    ::= integer-literal | boolean-literal | float-literal | string-literal | reference | data-type
  --   data-list         ::= data-list-item {, data-list-item}
  --   data-array-list   ::= "{" data-list "}" {, "{" data-list "}"}
  --   property          ::= identifier = data-list-item
  --   structure         ::= (data-type [name] "{" [data-list] "}" | "[" integer-listeral "]" [name] "{" data-array-list "}")
  --                           | (indentifier [name] ["(" [property {, property}] ")"] "{" {structure} "}")
  --   file              ::= {structure}
  --

  -- During parsing we encounter arrays of things (mostly floating point), so we deploy multithreading for the extra large ones, which, in
  -- some cases, can reduce parsing time in half. The WORKERS_THRESHOLD controls the length of arrays at which this behavior gets triggered.
  WORKERS_THRESHOLD : Positive := 7_500;
  WORKER_COUNT      : Positive := 8;

  -- Identifier
  IDENTIFIER_MAX : constant Positive := 32;
  function Id (Str : Str_8) return Str_8_Super is (P (Str, IDENTIFIER_MAX));
  NULL_IDENTIFIER : Str_8_Super (IDENTIFIER_MAX) := Id (NULL_STR_8);

  type Component_Amount_Kind is (Zero_Or_One, Zero_To_Two, Zero_To_Three, Zero_To_Four, Zero_Or_More, None,
                                 Exactly_One, One_Or_Two,  One_To_Three,  One_To_Four,  One_Or_More,  Max_One);

  type Primitive_Kind is (Bool_Kind,              Bool_x2_Kind,            Bool_x3_Kind,
                          Bool_x4_Kind,           Bool_x8_Kind,            Bool_x16_Kind,
                          String_Kind,            String_x2_Kind,          String_x3_Kind,
                          String_x4_Kind,         String_x8_Kind,          String_x16_Kind,
                          Ref_Kind,               Ref_x2_Kind,             Ref_x3_Kind,
                          Ref_x4_Kind,            Ref_x8_Kind,             Ref_x16_Kind,
                          Type_Kind,              Type_x2_Kind,            Type_x3_Kind,
                          Type_x4_Kind,           Type_x8_Kind,            Type_x16_Kind,
                          Float16_Kind,           Float32_Kind,            Float64_Kind,
                          Float16_x2_Kind,        Float32_x2_Kind,         Float64_x2_Kind,
                          Float16_x3_Kind,        Float32_x3_Kind,         Float64_x3_Kind,
                          Float16_x4_Kind,        Float32_x4_Kind,         Float64_x4_Kind,
                          Float16_x8_Kind,        Float32_x8_Kind,         Float64_x8_Kind,
                          Float16_x16_Kind,       Float32_x16_Kind,        Float64_x16_Kind,
                          Int8_Kind,              Int16_Kind,              Int32_Kind,              Int64_Kind,
                          Int8_x2_Kind,           Int16_x2_Kind,           Int32_x2_Kind,           Int64_x2_Kind,
                          Int8_x3_Kind,           Int16_x3_Kind,           Int32_x3_Kind,           Int64_x3_Kind,
                          Int8_x4_Kind,           Int16_x4_Kind,           Int32_x4_Kind,           Int64_x4_Kind,
                          Int8_x8_Kind,           Int16_x8_Kind,           Int32_x8_Kind,           Int64_x8_Kind,
                          Int8_x16_Kind,          Int16_x16_Kind,          Int32_x16_Kind,          Int64_x16_Kind,
                          Unsigned_Int8_Kind,     Unsigned_Int16_Kind,     Unsigned_Int32_Kind,     Unsigned_Int64_Kind,
                          Unsigned_Int8_x2_Kind,  Unsigned_Int16_x2_Kind,  Unsigned_Int32_x2_Kind,  Unsigned_Int64_x2_Kind,
                          Unsigned_Int8_x3_Kind,  Unsigned_Int16_x3_Kind,  Unsigned_Int32_x3_Kind,  Unsigned_Int64_x3_Kind,
                          Unsigned_Int8_x4_Kind,  Unsigned_Int16_x4_Kind,  Unsigned_Int32_x4_Kind,  Unsigned_Int64_x4_Kind,
                          Unsigned_Int8_x8_Kind,  Unsigned_Int16_x8_Kind,  Unsigned_Int32_x8_Kind,  Unsigned_Int64_x8_Kind,
                          Unsigned_Int8_x16_Kind, Unsigned_Int16_x16_Kind, Unsigned_Int32_x16_Kind, Unsigned_Int64_x16_Kind,
                          Structure_Kind);

  subtype Any_Float_Kind            is Primitive_Kind range Float16_Kind           .. Float64_Kind;
  subtype Any_Float_x2_Kind         is Primitive_Kind range Float16_x2_Kind        .. Float64_x2_Kind;
  subtype Any_Float_x3_Kind         is Primitive_Kind range Float16_x3_Kind        .. Float64_x3_Kind;
  subtype Any_Float_x4_Kind         is Primitive_Kind range Float16_x4_Kind        .. Float64_x4_Kind;
  subtype Any_Float_x8_Kind         is Primitive_Kind range Float16_x8_Kind        .. Float64_x8_Kind;
  subtype Any_Float_x16_Kind        is Primitive_Kind range Float16_x16_Kind       .. Float64_x16_Kind;
  subtype Any_Int_Kind              is Primitive_Kind range Int8_Kind              .. Int64_Kind;
  subtype Any_Int_x2_Kind           is Primitive_Kind range Int8_x2_Kind           .. Int64_x2_Kind;
  subtype Any_Int_x3_Kind           is Primitive_Kind range Int8_x3_Kind           .. Int64_x3_Kind;
  subtype Any_Int_x4_Kind           is Primitive_Kind range Int8_x4_Kind           .. Int64_x4_Kind;
  subtype Any_Int_x8_Kind           is Primitive_Kind range Int8_x8_Kind           .. Int64_x8_Kind;
  subtype Any_Int_x16_Kind          is Primitive_Kind range Int8_x16_Kind          .. Int64_x16_Kind;
  subtype Any_Unsigned_Int_Kind     is Primitive_Kind range Unsigned_Int8_Kind     .. Unsigned_Int64_Kind;
  subtype Any_Unsigned_Int_x2_Kind  is Primitive_Kind range Unsigned_Int8_x2_Kind  .. Unsigned_Int64_x2_Kind;
  subtype Any_Unsigned_Int_x3_Kind  is Primitive_Kind range Unsigned_Int8_x3_Kind  .. Unsigned_Int64_x3_Kind;
  subtype Any_Unsigned_Int_x4_Kind  is Primitive_Kind range Unsigned_Int8_x4_Kind  .. Unsigned_Int64_x4_Kind;
  subtype Any_Unsigned_Int_x8_Kind  is Primitive_Kind range Unsigned_Int8_x8_Kind  .. Unsigned_Int64_x8_Kind;
  subtype Any_Unsigned_Int_x16_Kind is Primitive_Kind range Unsigned_Int8_x16_Kind .. Unsigned_Int64_x16_Kind;

  type Array_Primitive_Set     is array (Primitive_Kind)                       of Bool;
  type Array_Primitive_Kind    is array (Positive range <>)                    of Primitive_Kind;
  type Array_x2_Primitive_Kind is array (Positive range <>, Positive range <>) of Primitive_Kind;

  -- One discriminated record to rule them all...
  type Primitive_State (Kind : Primitive_Kind; Len : Positive) is record
      case Kind is
        when Bool_Kind               => Bool_Val               : Array_Bool               (1..Len);
        when Bool_x2_Kind            => Bool_x2_Val            : Array_x2_Bool            (1..Len, 1..2);
        when Bool_x3_Kind            => Bool_x3_Val            : Array_x2_Bool            (1..Len, 1..3);
        when Bool_x4_Kind            => Bool_x4_Val            : Array_x2_Bool            (1..Len, 1..4);
        when Bool_x8_Kind            => Bool_x8_Val            : Array_x2_Bool            (1..Len, 1..8);
        when Bool_x16_Kind           => Bool_x16_Val           : Array_x2_Bool            (1..Len, 1..16);
        when String_Kind             => String_Val             : Array_Str_32_Unbound     (1..Len);
        when String_x2_Kind          => String_x2_Val          : Array_x2_Str_32_Unbound  (1..Len, 1..2);
        when String_x3_Kind          => String_x3_Val          : Array_x2_Str_32_Unbound  (1..Len, 1..3);
        when String_x4_Kind          => String_x4_Val          : Array_x2_Str_32_Unbound  (1..Len, 1..4);
        when String_x8_Kind          => String_x8_Val          : Array_x2_Str_32_Unbound  (1..Len, 1..8);
        when String_x16_Kind         => String_x16_Val         : Array_x2_Str_32_Unbound  (1..Len, 1..16);
        when Ref_Kind                => Ref_Val                : Array_Ptr                (1..Len);        -- Ptr_Structure_State
        when Ref_x2_Kind             => Ref_x2_Val             : Array_x2_Ptr             (1..Len, 1..2);  -- Ptr_Structure_State
        when Ref_x3_Kind             => Ref_x3_Val             : Array_x2_Ptr             (1..Len, 1..3);  -- Ptr_Structure_State
        when Ref_x4_Kind             => Ref_x4_Val             : Array_x2_Ptr             (1..Len, 1..4);  -- Ptr_Structure_State
        when Ref_x8_Kind             => Ref_x8_Val             : Array_x2_Ptr             (1..Len, 1..8);  -- Ptr_Structure_State
        when Ref_x16_Kind            => Ref_x16_Val            : Array_x2_Ptr             (1..Len, 1..16); -- Ptr_Structure_State
        when Type_Kind               => Type_Val               : Array_Primitive_Kind     (1..Len);
        when Type_x2_Kind            => Type_x2_Val            : Array_x2_Primitive_Kind  (1..Len, 1..2);
        when Type_x3_Kind            => Type_x3_Val            : Array_x2_Primitive_Kind  (1..Len, 1..3);
        when Type_x4_Kind            => Type_x4_Val            : Array_x2_Primitive_Kind  (1..Len, 1..4);
        when Type_x8_Kind            => Type_x8_Val            : Array_x2_Primitive_Kind  (1..Len, 1..8);
        when Type_x16_Kind           => Type_x16_Val           : Array_x2_Primitive_Kind  (1..Len, 1..16);
        when Float16_Kind            => Float16_Val            : Array_Real_16            (1..Len);
        when Float32_Kind            => Float32_Val            : Array_Real_32            (1..Len);
        when Float64_Kind            => Float64_Val            : Array_Real_64            (1..Len);
        when Float16_x2_Kind         => Float16_x2_Val         : Array_x2_Real_16         (1..Len, 1..2);
        when Float32_x2_Kind         => Float32_x2_Val         : Array_x2_Real_32         (1..Len, 1..2);
        when Float64_x2_Kind         => Float64_x2_Val         : Array_x2_Real_64         (1..Len, 1..2);
        when Float16_x3_Kind         => Float16_x3_Val         : Array_x2_Real_16         (1..Len, 1..3);
        when Float32_x3_Kind         => Float32_x3_Val         : Array_x2_Real_32         (1..Len, 1..3);
        when Float64_x3_Kind         => Float64_x3_Val         : Array_x2_Real_64         (1..Len, 1..3);
        when Float16_x4_Kind         => Float16_x4_Val         : Array_x2_Real_16         (1..Len, 1..4);
        when Float32_x4_Kind         => Float32_x4_Val         : Array_x2_Real_32         (1..Len, 1..4);
        when Float64_x4_Kind         => Float64_x4_Val         : Array_x2_Real_64         (1..Len, 1..4);
        when Float16_x8_Kind         => Float16_x8_Val         : Array_x2_Real_16         (1..Len, 1..8);
        when Float32_x8_Kind         => Float32_x8_Val         : Array_x2_Real_32         (1..Len, 1..8);
        when Float64_x8_Kind         => Float64_x8_Val         : Array_x2_Real_64         (1..Len, 1..8);
        when Float16_x16_Kind        => Float16_x16_Val        : Array_x2_Real_16         (1..Len, 1..16);
        when Float32_x16_Kind        => Float32_x16_Val        : Array_x2_Real_32         (1..Len, 1..16);
        when Float64_x16_Kind        => Float64_x16_Val        : Array_x2_Real_64         (1..Len, 1..16);
        when Int8_Kind               => Int8_Val               : Array_Int_8              (1..Len);
        when Int16_Kind              => Int16_Val              : Array_Int_16             (1..Len);
        when Int32_Kind              => Int32_Val              : Array_Int                (1..Len);
        when Int64_Kind              => Int64_Val              : Array_Int_64             (1..Len);
        when Int8_x2_Kind            => Int8_x2_Val            : Array_x2_Int_8           (1..Len, 1..2);
        when Int16_x2_Kind           => Int16_x2_Val           : Array_x2_Int_16          (1..Len, 1..2);
        when Int32_x2_Kind           => Int32_x2_Val           : Array_x2_Int             (1..Len, 1..2);
        when Int64_x2_Kind           => Int64_x2_Val           : Array_x2_Int_64          (1..Len, 1..2);
        when Int8_x3_Kind            => Int8_x3_Val            : Array_x2_Int_8           (1..Len, 1..3);
        when Int16_x3_Kind           => Int16_x3_Val           : Array_x2_Int_16          (1..Len, 1..3);
        when Int32_x3_Kind           => Int32_x3_Val           : Array_x2_Int             (1..Len, 1..3);
        when Int64_x3_Kind           => Int64_x3_Val           : Array_x2_Int_64          (1..Len, 1..3);
        when Int8_x4_Kind            => Int8_x4_Val            : Array_x2_Int_8           (1..Len, 1..4);
        when Int16_x4_Kind           => Int16_x4_Val           : Array_x2_Int_16          (1..Len, 1..4);
        when Int32_x4_Kind           => Int32_x4_Val           : Array_x2_Int             (1..Len, 1..4);
        when Int64_x4_Kind           => Int64_x4_Val           : Array_x2_Int_64          (1..Len, 1..4);
        when Int8_x8_Kind            => Int8_x8_Val            : Array_x2_Int_8           (1..Len, 1..8);
        when Int16_x8_Kind           => Int16_x8_Val           : Array_x2_Int_16          (1..Len, 1..8);
        when Int32_x8_Kind           => Int32_x8_Val           : Array_x2_Int             (1..Len, 1..8);
        when Int64_x8_Kind           => Int64_x8_Val           : Array_x2_Int_64          (1..Len, 1..8);
        when Int8_x16_Kind           => Int8_x16_Val           : Array_x2_Int_8           (1..Len, 1..16);
        when Int16_x16_Kind          => Int16_x16_Val          : Array_x2_Int_16          (1..Len, 1..16);
        when Int32_x16_Kind          => Int32_x16_Val          : Array_x2_Int             (1..Len, 1..16);
        when Int64_x16_Kind          => Int64_x16_Val          : Array_x2_Int_64          (1..Len, 1..16);
        when Unsigned_Int8_Kind      => Unsigned_Int8_Val      : Array_Int_8_Unsigned     (1..Len);
        when Unsigned_Int16_Kind     => Unsigned_Int16_Val     : Array_Int_16_Unsigned    (1..Len);
        when Unsigned_Int32_Kind     => Unsigned_Int32_Val     : Array_Int_32_Unsigned    (1..Len);
        when Unsigned_Int64_Kind     => Unsigned_Int64_Val     : Array_Int_64_Unsigned    (1..Len);
        when Unsigned_Int8_x2_Kind   => Unsigned_Int8_x2_Val   : Array_x2_Int_8_Unsigned  (1..Len, 1..2);
        when Unsigned_Int16_x2_Kind  => Unsigned_Int16_x2_Val  : Array_x2_Int_16_Unsigned (1..Len, 1..2);
        when Unsigned_Int32_x2_Kind  => Unsigned_Int32_x2_Val  : Array_x2_Int_32_Unsigned (1..Len, 1..2);
        when Unsigned_Int64_x2_Kind  => Unsigned_Int64_x2_Val  : Array_x2_Int_64_Unsigned (1..Len, 1..2);
        when Unsigned_Int8_x3_Kind   => Unsigned_Int8_x3_Val   : Array_x2_Int_8_Unsigned  (1..Len, 1..3);
        when Unsigned_Int16_x3_Kind  => Unsigned_Int16_x3_Val  : Array_x2_Int_16_Unsigned (1..Len, 1..3);
        when Unsigned_Int32_x3_Kind  => Unsigned_Int32_x3_Val  : Array_x2_Int_32_Unsigned (1..Len, 1..3);
        when Unsigned_Int64_x3_Kind  => Unsigned_Int64_x3_Val  : Array_x2_Int_64_Unsigned (1..Len, 1..3);
        when Unsigned_Int8_x4_Kind   => Unsigned_Int8_x4_Val   : Array_x2_Int_8_Unsigned  (1..Len, 1..4);
        when Unsigned_Int16_x4_Kind  => Unsigned_Int16_x4_Val  : Array_x2_Int_16_Unsigned (1..Len, 1..4);
        when Unsigned_Int32_x4_Kind  => Unsigned_Int32_x4_Val  : Array_x2_Int_32_Unsigned (1..Len, 1..4);
        when Unsigned_Int64_x4_Kind  => Unsigned_Int64_x4_Val  : Array_x2_Int_64_Unsigned (1..Len, 1..4);
        when Unsigned_Int8_x8_Kind   => Unsigned_Int8_x8_Val   : Array_x2_Int_8_Unsigned  (1..Len, 1..8);
        when Unsigned_Int16_x8_Kind  => Unsigned_Int16_x8_Val  : Array_x2_Int_16_Unsigned (1..Len, 1..8);
        when Unsigned_Int32_x8_Kind  => Unsigned_Int32_x8_Val  : Array_x2_Int_32_Unsigned (1..Len, 1..8);
        when Unsigned_Int64_x8_Kind  => Unsigned_Int64_x8_Val  : Array_x2_Int_64_Unsigned (1..Len, 1..8);
        when Unsigned_Int8_x16_Kind  => Unsigned_Int8_x16_Val  : Array_x2_Int_8_Unsigned  (1..Len, 1..16);
        when Unsigned_Int16_x16_Kind => Unsigned_Int16_x16_Val : Array_x2_Int_16_Unsigned (1..Len, 1..16);
        when Unsigned_Int32_x16_Kind => Unsigned_Int32_x16_Val : Array_x2_Int_32_Unsigned (1..Len, 1..16);
        when Unsigned_Int64_x16_Kind => Unsigned_Int64_x16_Val : Array_x2_Int_64_Unsigned (1..Len, 1..16);
        when Structure_Kind          => null;
      end case;
    end record;

  -- ???
  type Property_State (Kind : Primitive_Kind := String_Kind; Defaulted : Bool := False) is record
      Identifier : Str_8_Super (IDENTIFIER_MAX);
      case Defaulted is
        when True  => Default            : Primitive_State (Kind, 1);
        when False => Overrides_From_Ref : Bool;
      end case;
    end record;
  NULL_PROPERTY : constant Property_State := (Bool_Kind, False, (IDENTIFIER_MAX, others => <>), False);
  type Array_Property is array (1..6) of Property_State;

  -- ???
  type Primitive_Component_State is record
      Allow_Arrays       : Bool;
      Allowed_Primitives : Array_Primitive_Set;
    end record;

  -- ???
  generic
    type Struct_T is (<>);
    Override_Ref : Struct_T;
    with procedure Parse_Data_Type (Text : Str_8; C : in out Positive; Struct : out Struct_T);
  package Description_Language_Struct_Types is
      type Array_Struct_T            is array (Positive range <>) of Struct_T;
      type Array_Struct_Count        is array (Struct_T) of Natural;
      type Array_Struct_Set          is array (Struct_T) of Bool;
      type Array_Struct_Hierarchy    is array (Struct_T) of Array_Struct_Set; -- We must go deeper
      type Array_Struct_Property     is array (Struct_T) of Array_Property;
      type Array_Component_Amount    is array (Struct_T) of Component_Amount_Kind;
      type Array_Structure_Component is array (Struct_T) of Array_Component_Amount;
      type Array_Primitive_Component is array (Struct_T) of Primitive_Component_State;
    end;

  -- ???
  generic
    with package Struct_Types is new Description_Language_Struct_Types (<>);
    Top_Level_Structures : Struct_Types.Array_Struct_Set;
    Hierarchy            : Struct_Types.Array_Struct_Hierarchy;
    Properties           : Struct_Types.Array_Struct_Property;
    Structure_Components : Struct_Types.Array_Structure_Component;
    Primitive_Components : Struct_Types.Array_Primitive_Component;
  package Description_Language is
      use Struct_Types;

      Component_Mismatch,
      Bad_Reference,
      Syntax_Error,
      Unexpected_Primitive_Array,
      Invalid_Data_Type_Literal,
      Bad_Hierarchy,
      Invalid_Escape_Character,
      Invalid_Hex_In_Valued_Character,
      Invalid_Character,
      Invalid_Float_Literal,
      Invalid_Character_Literal,
      Invalid_Name,
      Int_Overflow,
      Undefined_Property,
      Identifier_Expected,
      Local_Name_In_Global_Scope,
      Local_Structure_At_Top_Level,
      Required_Property_Not_Present,
      Primitive_At_Top_Level,
      Invalid_Characters_In_Identifier,
      Disallowed_Structure_At_Top_Level : Exception;

      -- Recursive structure optimized for external parsing of the OpenDDL tree structure
      type Structure_State;
      type Ptr_Structure_State is access all Structure_State;
      type Array_Ptr_Structure_State is array (Positive range <>) of Ptr_structure_State;
      package Hashed_Ptr_Structure_State is new Neo.Core.Hashed  (Ptr_Structure_State); use Hashed_Ptr_Structure_State;
      package Vector_Ptr_Structure_State is new Neo.Core.Vectors (Ptr_Structure_State); use Vector_Ptr_Structure_State;
      type Structure_State (Kind : Primitive_Kind; Len : Positive) is record
          Is_Global : Bool                := False;
          Name      : Ptr_Str_8           := null;
          Parent    : Ptr_Structure_State := null;
          case Kind is
            when Structure_Kind => Class          : Struct_T;
                                   Counts         : Array_Struct_Count;
                                   Properties     : Hashed_Ptr_Structure_State.Ptr_Unsafe_Map    := null;
                                   Named_Children : Hashed_Ptr_Structure_State.Ptr_Unsafe_Map    := null;
                                   Children       : Vector_Ptr_Structure_State.Ptr_Unsafe_Vector := null;
            when others         => Primitive      : Primitive_State (Kind, Len);
          end case;
        end record;
      procedure Free is new Unchecked_Deallocation (Structure_State, Ptr_Structure_State);

      -- Reference conversions
      function Ref    is new Unchecked_Conversion (Ptr, Ptr_Structure_State);
      function To_Ref is new Unchecked_Conversion (Ptr_Structure_State, Ptr);

      type File_State is new Controlled with record
          Name             : Ptr_Str_16;
          Counts           : Array_Struct_Count;
          Structures       : Vector_Ptr_Structure_State.Unsafe.Vector;
          Named_Structures : Hashed_Ptr_Structure_State.Unsafe.Map;
          Globals          : Hashed_Ptr_Structure_State.Unsafe.Map;
        end record;
      procedure Finalize (File : in out File_State);

      -- IO
      procedure Load (Path : Str; File : out File_State);
      procedure Dump (Path : Str; File :     File_State); -- Debugging

      -- Getters
      function Get (Structure : Ptr_Structure_State; Kind : Struct_T) return Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Primitive : Primitive_Kind) return Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Kind : Struct_T) return Array_Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T) return Array_Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Class : Struct_T; Primitive : Primitive_Kind) return Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T) return Ptr_Structure_State;
      function Get (Structure : Ptr_Structure_State; Path : Array_Struct_T; Primitive : Primitive_Kind) return Ptr_Structure_State;
    end;
end;
























