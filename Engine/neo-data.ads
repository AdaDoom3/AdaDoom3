
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
  -- escaping the various braces (e.g. "{" for {, "[" for [, et cetera), and "* #" for repentative items.
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
  --   integer-literal   ::= [+ | -] decimal-literal | hex-literal | octal-literal | binary-literal
  --   float-literal     ::= decimal-literal [. decimal-literal] [(E | e) integer-literal]
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

  subtype Identifier_Str is Str_8_Super (32);

  type Component_Amount_Kind is (Zero_Or_One, Zero_To_Two, Zero_To_Three, Zero_To_Four, Zero_Or_More, None,
                                 Exactly_One, One_Or_Two,  One_To_Three,  One_To_Four,  One_Or_More,  Max_One);

  type Primitive_Kind is (Bool_Kind,              Bool_x2_Kind,            Bool_x3_Kind,
                          Bool_x4_Kind,           Bool_x8_Kind,            Bool_x16_Kind
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

  type Array_Primitive_Set is array (Primitive_Kind) of Bool;

  -- One discriminated record to rule them all... a consiquences of optimization
  type Primitive_State (Kind : Primitive_Kind; Len : Positive) is record
      case Kind is
        when Bool_Kind               => Bool_Val               : Array_Bool               (1..Len);
        when Bool_x2_Kind            => Bool_x2_Val            : Array_x2_Bool            (1..2,  1..Len);
        when Bool_x3_Kind            => Bool_x3_Val            : Array_x2_Bool            (1..3,  1..Len);
        when Bool_x4_Kind            => Bool_x4_Val            : Array_x2_Bool            (1..4,  1..Len);
        when Bool_x8_Kind            => Bool_x8_Val            : Array_x2_Bool            (1..8,  1..Len);
        when Bool_x16_Kind           => Bool_x16_Val           : Array_x2_Bool            (1..16, 1..Len);
        when String_Kind             => String_Val             : Array_Str_32_Unbound     (1..Len);
        when String_x2_Kind          => String_x2_Val          : Array_x2_Str_32_Unbound  (1..2,  1..Len);
        when String_x3_Kind          => String_x3_Val          : Array_x2_Str_32_Unbound  (1..3,  1..Len);
        when String_x4_Kind          => String_x4_Val          : Array_x2_Str_32_Unbound  (1..4,  1..Len);
        when String_x8_Kind          => String_x8_Val          : Array_x2_Str_32_Unbound  (1..8,  1..Len);
        when String_x16_Kind         => String_x16_Val         : Array_x2_Str_32_Unbound  (1..16, 1..Len);
        when Ref_Kind                => Ref_Val                : Array_Ptr_Kind           (1..Len);        -- Ptr_Structure_State
        when Ref_x2_Kind             => Ref_x2_Val             : Array_x2_Ptr_Kind        (1..2,  1..Len); -- Ptr_Structure_State
        when Ref_x3_Kind             => Ref_x3_Val             : Array_x2_Ptr_Kind        (1..3,  1..Len); -- Ptr_Structure_State
        when Ref_x4_Kind             => Ref_x4_Val             : Array_x2_Ptr_Kind        (1..4,  1..Len); -- Ptr_Structure_State
        when Ref_x8_Kind             => Ref_x8_Val             : Array_x2_Ptr_Kind        (1..8,  1..Len); -- Ptr_Structure_State
        when Ref_x16_Kind            => Ref_x16_Val            : Array_x2_Ptr_Kind        (1..16, 1..Len); -- Ptr_Structure_State
        when Type_Kind               => Type_Val               : Array_Primitive_Kind     (1..Len);
        when Type_x2_Kind            => Type_x2_Val            : Array_x2_Primitive_Kind  (1..2,  1..Len);
        when Type_x3_Kind            => Type_x3_Val            : Array_x2_Primitive_Kind  (1..3,  1..Len);
        when Type_x4_Kind            => Type_x4_Val            : Array_x2_Primitive_Kind  (1..4,  1..Len);
        when Type_x8_Kind            => Type_x8_Val            : Array_x2_Primitive_Kind  (1..8,  1..Len);
        when Type_x16_Kind           => Type_x16_Val           : Array_x2_Primitive_Kind  (1..16, 1..Len);
        when Float16_Kind            => Float16_Val            : Array_Real_16            (1..Len);
        when Float32_Kind            => Float32_Val            : Array_Real_32            (1..Len);
        when Float64_Kind            => Float64_Val            : Array_Real_64            (1..Len);
        when Float16_x2_Kind         => Float16_x2_Val         : Array_x2_Real_16         (1..2,  1..Len);
        when Float32_x2_Kind         => Float32_x2_Val         : Array_x2_Real_32         (1..2,  1..Len);
        when Float64_x2_Kind         => Float64_x2_Val         : Array_x2_Real_64         (1..2,  1..Len);
        when Float16_x3_Kind         => Float16_x3_Val         : Array_x2_Real_16         (1..3,  1..Len);
        when Float32_x3_Kind         => Float32_x3_Val         : Array_x2_Real_32         (1..3,  1..Len);
        when Float64_x3_Kind         => Float64_x3_Val         : Array_x2_Real_64         (1..3,  1..Len);
        when Float16_x4_Kind         => Float16_x4_Val         : Array_x2_Real_16         (1..4,  1..Len);
        when Float32_x4_Kind         => Float32_x4_Val         : Array_x2_Real_32         (1..4,  1..Len);
        when Float64_x4_Kind         => Float64_x4_Val         : Array_x2_Real_64         (1..4,  1..Len);
        when Float16_x8_Kind         => Float16_x8_Val         : Array_x2_Real_16         (1..8,  1..Len);
        when Float32_x8_Kind         => Float32_x8_Val         : Array_x2_Real_32         (1..8,  1..Len);
        when Float64_x8_Kind         => Float64_x8_Val         : Array_x2_Real_64         (1..8,  1..Len);
        when Float16_x16_Kind        => Float16_x16_Val        : Array_x2_Real_64         (1..16, 1..Len);
        when Float32_x16_Kind        => Float32_x32_Val        : Array_x2_Real_16         (1..16, 1..Len);
        when Float64_x16_Kind        => Float64_x64_Val        : Array_x2_Real_64         (1..16, 1..Len);
        when Int8_Kind               => Int8_Val               : Array_Int_8              (1..Len);
        when Int16_Kind              => Int16_Val              : Array_Int_16             (1..Len);
        when Int32_Kind              => Int32_Val              : Array_Int_32             (1..Len);
        when Int64_Kind              => Int64_Val              : Array_Int_64             (1..Len);
        when Int8_x2_Kind            => Int8_x2_Val            : Array_x2_Int_8           (1..2,  1..Len);
        when Int16_x2_Kind           => Int16_x2_Val           : Array_x2_Int_16          (1..2,  1..Len);
        when Int32_x2_Kind           => Int32_x2_Val           : Array_x2_Int_32          (1..2,  1..Len);
        when Int64_x2_Kind           => Int64_x2_Val           : Array_x2_Int_64          (1..2,  1..Len);
        when Int8_x3_Kind            => Int8_x3_Val            : Array_x2_Int_8           (1..3,  1..Len);
        when Int16_x3_Kind           => Int16_x3_Val           : Array_x2_Int_16          (1..3,  1..Len);
        when Int32_x3_Kind           => Int32_x3_Val           : Array_x2_Int_32          (1..3,  1..Len);
        when Int64_x3_Kind           => Int64_x3_Val           : Array_x2_Int_64          (1..3,  1..Len);
        when Int8_x4_Kind            => Int8_x4_Val            : Array_x2_Int_8           (1..4,  1..Len);
        when Int16_x4_Kind           => Int16_x4_Val           : Array_x2_Int_16          (1..4,  1..Len);
        when Int32_x4_Kind           => Int32_x4_Val           : Array_x2_Int_32          (1..4,  1..Len);
        when Int64_x4_Kind           => Int64_x4_Val           : Array_x2_Int_64          (1..4,  1..Len);
        when Int8_x8_Kind            => Int8_x8_Val            : Array_x2_Int_8           (1..8,  1..Len);
        when Int16_x8_Kind           => Int16_x8_Val           : Array_x2_Int_16          (1..8,  1..Len);
        when Int32_x8_Kind           => Int32_x8_Val           : Array_x2_Int_32          (1..8,  1..Len);
        when Int64_x8_Kind           => Int64_x8_Val           : Array_x2_Int_64          (1..8,  1..Len);
        when Int8_x16_Kind           => Int8_x16_Val           : Array_x2_Int_8           (1..16, 1..Len);
        when Int16_x16_Kind          => Int16_x16_Val          : Array_x2_Int_16          (1..16, 1..Len);
        when Int32_x16_Kind          => Int32_x16_Val          : Array_x2_Int_32          (1..16, 1..Len);
        when Int64_x16_Kind          => Int64_x16_Val          : Array_x2_Int_64          (1..16, 1..Len);
        when Unsigned_Int8_Kind      => Unsigned_Int8_Val      : Array_Unsigned_Int_8     (1..Len);
        when Unsigned_Int16_Kind     => Unsigned_Int16_Val     : Array_Unsigned_Int_16    (1..Len);
        when Unsigned_Int32_Kind     => Unsigned_Int32_Val     : Array_Unsigned_Int_32    (1..Len);
        when Unsigned_Int64_Kind     => Unsigned_Int64_Val     : Array_Unsigned_Int_64    (1..Len);
        when Unsigned_Int8_x2_Kind   => Unsigned_Int8_x2_Val   : Array_x2_Unsigned_Int_8  (1..2,  1..Len);
        when Unsigned_Int16_x2_Kind  => Unsigned_Int16_x2_Val  : Array_x2_Unsigned_Int_16 (1..2,  1..Len);
        when Unsigned_Int32_x2_Kind  => Unsigned_Int32_x2_Val  : Array_x2_Unsigned_Int_32 (1..2,  1..Len);
        when Unsigned_Int64_x2_Kind  => Unsigned_Int64_x2_Val  : Array_x2_Unsigned_Int_64 (1..2,  1..Len);
        when Unsigned_Int8_x3_Kind   => Unsigned_Int8_x3_Val   : Array_x2_Unsigned_Int_8  (1..3,  1..Len);
        when Unsigned_Int16_x3_Kind  => Unsigned_Int16_x3_Val  : Array_x2_Unsigned_Int_16 (1..3,  1..Len);
        when Unsigned_Int32_x3_Kind  => Unsigned_Int32_x3_Val  : Array_x2_Unsigned_Int_32 (1..3,  1..Len);
        when Unsigned_Int64_x3_Kind  => Unsigned_Int64_x3_Val  : Array_x2_Unsigned_Int_64 (1..3,  1..Len);
        when Unsigned_Int8_x4_Kind   => Unsigned_Int8_x4_Val   : Array_x2_Unsigned_Int_8  (1..4,  1..Len);
        when Unsigned_Int16_x4_Kind  => Unsigned_Int16_x4_Val  : Array_x2_Unsigned_Int_16 (1..4,  1..Len);
        when Unsigned_Int32_x4_Kind  => Unsigned_Int32_x4_Val  : Array_x2_Unsigned_Int_32 (1..4,  1..Len);
        when Unsigned_Int64_x4_Kind  => Unsigned_Int64_x4_Val  : Array_x2_Unsigned_Int_64 (1..4,  1..Len);
        when Unsigned_Int8_x8_Kind   => Unsigned_Int8_x8_Val   : Array_x2_Unsigned_Int_8  (1..8,  1..Len);
        when Unsigned_Int16_x8_Kind  => Unsigned_Int16_x8_Val  : Array_x2_Unsigned_Int_16 (1..8,  1..Len);
        when Unsigned_Int32_x8_Kind  => Unsigned_Int32_x8_Val  : Array_x2_Unsigned_Int_32 (1..8,  1..Len);
        when Unsigned_Int64_x8_Kind  => Unsigned_Int64_x8_Val  : Array_x2_Unsigned_Int_64 (1..8,  1..Len);
        when Unsigned_Int8_x16_Kind  => Unsigned_Int8_x16_Val  : Array_x2_Unsigned_Int_8  (1..16, 1..Len);
        when Unsigned_Int16_x16_Kind => Unsigned_Int16_x16_Val : Array_x2_Unsigned_Int_16 (1..16, 1..Len);
        when Unsigned_Int32_x16_Kind => Unsigned_Int32_x16_Val : Array_x2_Unsigned_Int_32 (1..16, 1..Len);
        when Unsigned_Int64_x16_Kind => Unsigned_Int64_x16_Val : Array_x2_Unsigned_Int_64 (1..16, 1..Len);
        when Structure_Kind          => null;
      end case;
    end record with Pack;
  package Hashed_Primitive_State is new Indefinite_Hashed (Primitive_State);

  type Property_State (Kind : Primitive_Kind := String_Kind; Defaulted : Bool := False) is record
      Identifier : Identifier_Str;
      case Defaulted is
        when True  => Default            : Primitive_State (Kind, 1);
        when False => Overrides_From_Ref : Bool;
      end case;
    end record;
  NULL_PROPERTY : constant Property_State := (Bool_Type, False, NULL_STR_8_SUPER);
  type Array_Property is array (1..6) of Property_State;

  type Primitive_Component_State is
      Allow_Arrays       : Bool;
      Allowed_Primitives : Array_Primitive_Set;
    end record;

  generic

     -- Structure names used in the engine - should all start with uppercase
    type Struct_T is (<>);
    with procedure Parse_Data_Type (Text : Str_8; C : Positive; Primitive : out Primitive_Kind; Struct : out Struct_T);

    type Array_Struct_Set          is array (Struct_T) of Bool;
    type Array_Struct_Count        is array (Struct_T) of Natural;
    type Array_Struct_Hierarchy    is array (Struct_T) of Array_Struct_Set; -- We must go deeper
    type Array_Struct_Property     is array (Struct_T) of Array_Property;
    type Array_Component_Amount    is array (Struct_T) of Component_Amount_Kind;
    type Array_Primitive_Component is array (Struct_T) of Primitive_Component_State;

    Top_Level_Structures : Array_Struct_Set;
    Hierarchy            : Array_Struct_Hierarchy;
    Properties           : Array_Struct_Property;
    Structure_Components : Array_Component_Amount;
    Primitive_Components : Array_Primitive_Component;
  package Description_Language is

    -- Recursive structure optimized for external parsing of the OpenDDL tree structure
    type Structure_State;
    type Ptr_Structure_State is access all Structure_State;
    type Structure_State (Kind : Primitive_Kind := Structure_Kind; Len : Positive := 1) is Controlled with record
        Parent : Ptr_Struct_State;
        case Kind is
          when Structure_Kind => Structure  : Struct_T;
                                 Properties : Hashed_Primitive_State.Unsafe.Map;
                                 Children   : Ptr := New_Vector_Structure_State_Ptr_Unsafe_Vector;
                                 Locals     : Ptr := New_Hashed_Structure_State.Ptr_Unsafe_Map;
          when others         => Primitive  : Primitive_State (Kind, Len);
        end case;
      end record;
    package Hashed_Structure_State     is new Hashed (Structure_State);
    package Hashed_Ptr_Structure_State is new Hashed (Ptr_Struct_State);
    procedure Initialize (Item : in out Structure_State);
    procedure Finalize   (Item : in out Structure_State);

    type File_State is record
        Name       : Str_Path;
        Structures : Vector_Ptr_Structure_State.Unsafe_Vector;
        Globals    : Hashed_Ptr_Structure_State.Unsafe.Map;
      end record;

    -- IO
    procedure Load (Name : Str; File : out File_State);
  end;
end;
























