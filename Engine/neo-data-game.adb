
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

-- Isolate parsers for different model formats into separate packages and consolidate them here
package body Neo.Data.Game is

  ----------------------------
  -- Engine_Exchange_Format --
  ----------------------------
  --
  -- https://web.archive.org/web/20180903014314/http://opengex.org/OpenGEX.2.0.pdf
  --
  -- Additional structures and fields:
  --
  --   ...
  --

  -- Enumerate all possible structures, its wrapped in its own package due to naming conflicts with "Time" and "Name"
  package OpenGEX is type Structure_Kind is (BoneNode,     CameraNode,     GeometryNode,   LightNode,    Node, -- Should all start with uppercase
                                             Animation,    Transform,      Translation,    Rotation,     Scale,
                                             CameraObject, GeometryObject, LightObject,    ObjectRef,    MaterialRef,
                                             Bone,         BoneCountArray, BoneIndexArray, BoneRefArray, BoneWeightArray,
                                             Atten,        Clip,           Color,          Extension,    IndexArray,
                                             Key,          Material,       Mesh,           Metric,       Morph,
                                             MorphWeight,  Name,           Param,          Skeleton,     Skin,
                                             Texture,      Time,           Value,          VertexArray,  Track); end; use OpenGEX;
  subtype Node_Kind     is OpenGEX.Structure_Kind range BoneNode  .. Node;
  subtype Movement_Kind is OpenGEX.Structure_Kind range Animation .. Scale;

  -- Could have used the Value attribute, but that is a linear search and we want the SPEEEEED
  procedure Parse_Struct_Types (Text : Str_8; C : in out Positive; Struct : out OpenGEX.Structure_Kind) with Inline is
    procedure Assert_Rest (Match : Str_8) with Inline is
      begin
        C := C + 1;
        if Text (C..C + Match'Length - 1) /= Match then raise Program_Error; end if;
        C := C + Match'Length;
      end;
    begin
      case Text (C) is
        when 'B' =>
          Assert_Rest ("one");
          case Text (C) is
            when 'N' => Assert_Rest ("ode");        Struct := BoneNode;
            when 'C' => Assert_Rest ("ountArray");  Struct := BoneCountArray;
            when 'I' => Assert_Rest ("ndexArray");  Struct := BoneIndexArray;
            when 'R' => Assert_Rest ("efArray");    Struct := BoneRefArray;
            when 'W' => Assert_Rest ("eightArray"); Struct := BoneWeightArray;
            when others => raise Program_Error;
          end case;
        when 'C' =>
          C := C + 1;
          case Text (C) is
            when 'l' => Assert_Rest ("ip");  Struct := Clip;
            when 'o' => Assert_Rest ("lor"); Struct := Color;
            when 'a' =>
              Assert_Rest ("mera");
              case Text (C) is
                when 'N' => Assert_Rest ("ode");   Struct := CameraNode;
                when 'O' => Assert_Rest ("bject"); Struct := CameraObject;
                when others => raise Program_Error;
              end case;
            when others => raise Program_Error;
          end case;
        when 'G' =>
          Assert_Rest ("eometry");
          case Text (C) is
            when 'N' => Assert_Rest ("ode");   Struct := GeometryNode;
            when 'O' => Assert_Rest ("bject"); Struct := GeometryObject;
            when others => raise Program_Error;
          end case;
        when 'L' =>
          Assert_Rest ("ight");
          case Text (C) is
            when 'N' => Assert_Rest ("ode");   Struct := LightNode;
            when 'O' => Assert_Rest ("bject"); Struct := LightObject;
            when others => raise Program_Error;
          end case;
        when 'N' =>
          C := C + 1;
          case Text (C) is
            when 'o' => Assert_Rest ("de"); Struct := Node;
            when 'a' => Assert_Rest ("me"); Struct := OpenGEX.Name;
            when others => raise Program_Error;
          end case;
        when 'A' =>
          C := C + 1;
          case Text (C) is
            when 'n' => Assert_Rest ("imation"); Struct := Animation;
            when 't' => Assert_Rest ("ten");     Struct := Atten;
            when others => raise Program_Error;
          end case;
        when 'T' =>
          C := C + 1;
          case Text (C) is
            when 'e' => Assert_Rest ("xture"); Struct := Texture;
            when 'i' => Assert_Rest ("me");    Struct := OpenGEX.Time;
            when 'r' =>
              Assert_Rest ("a");
              case Text (C) is
                when 'c' => Assert_Rest ("k"); Struct := Track;
                when 'n' =>
                  Assert_Rest ("s");
                  case Text (C) is
                    when 'f' => Assert_Rest ("orm");   Struct := Transform;
                    when 'l' => Assert_Rest ("ation"); Struct := Translation;
                    when others => raise Program_Error;
                  end case;
                when others => raise Program_Error;
              end case;
            when others => raise Program_Error;
          end case;
        when 'R' => Assert_Rest ("otation"); Struct := Rotation;
        when 'S' =>
          C := C + 1;
          case Text (C) is
            when 'c' => Assert_Rest ("Scale"); Struct := Scale;
            when 'k' =>
              C := C + 1;
              case Text (C) is
                when 'e' => Assert_Rest ("leton"); Struct := Skeleton;
                when 'i' => Assert_Rest ("n");     Struct := Skin;
                when others => raise Program_Error;
              end case;
            when others => raise Program_Error;
          end case;
        when 'M' =>
          C := C + 1;
          case Text (C) is
            when 'e' =>
              C := C + 1;
              case Text (C) is
                when 't' => Assert_Rest ("ric"); Struct := Metric;
                when 's' => Assert_Rest ("h");   Struct := Mesh;
                when others => raise Program_Error;
              end case;
            when 'a' =>
              Assert_Rest ("terial");
              case Text (C) is
                when 'R' => Assert_Rest ("ef"); Struct := MaterialRef;
                when others =>                  Struct := Material;
              end case;
            when 'o' =>
              Assert_Rest ("rph");
              case Text (C) is
                when 'W'    => Assert_Rest ("eight"); Struct := MorphWeight;
                when others =>                        Struct := Morph;
              end case;
            when others => raise Program_Error;
          end case;
        when 'O' => Assert_Rest ("bjectRef");  Struct := ObjectRef;
        when 'E' => Assert_Rest ("xtension");  Struct := Extension;
        when 'I' => Assert_Rest ("ndexArray"); Struct := IndexArray;
        when 'K' => Assert_Rest ("ey");        Struct := Key;
        when 'P' => Assert_Rest ("aram");      Struct := Param;
        when 'V' =>
          C := C + 1;
          case Text (C) is
            when 'a' => Assert_Rest ("lue");       Struct := Value;
            when 'e' => Assert_Rest ("rtexArray"); Struct := VertexArray;
            when others => raise Program_Error;
          end case;
        when others => raise Program_Error;
      end case;
    end;

  -- Do the actual instantiation
  package Struct_Types is new Description_Language_Struct_Types (OpenGEX.Structure_Kind, ObjectRef, Parse_Struct_Types);
  package Engine_Exchange_Format is new Description_Language (Struct_Types,

  -------------------------
  Top_Level_Structures => (
  -------------------------

  -- Exclusivly top-level
  Clip           |
  GeometryObject |
  LightObject    |
  Material       |
  Metric         |

  -- Either top-level or sub-level
  BoneNode     |
  CameraNode   |
  CameraObject |
  Extension    |
  GeometryNode |
  LightNode    |
  Node         => True, others => False),

  --------------
  Hierarchy => (
  --------------

  Track           => (Animation                                            => True, others => False),
  BoneRefArray    => (Skeleton                                             => True, others => False),
  BoneCountArray  => (Skin                                                 => True, others => False),
  BoneIndexArray  => (Skin                                                 => True, others => False),
  BoneWeightArray => (Skin                                                 => True, others => False),
  Skeleton        => (Skin                                                 => True, others => False),
  IndexArray      => (Mesh                                                 => True, others => False),
  Skin            => (Mesh                                                 => True, others => False),
  VertexArray     => (Mesh                                                 => True, others => False),
  Value           => (Track                                                => True, others => False),
  OpenGEX.Time    => (Track                                                => True, others => False),
  Key             => (OpenGEX.Time | Value                                 => True, others => False),
  Atten           => (LightObject                                          => True, others => False),
  Texture         => (LightObject | Material                               => True, others => False),
  Color           => (LightObject | Material                               => True, others => False),
  Param           => (LightObject | Material | CameraObject | Atten | Clip => True, others => False),
  MaterialRef     => (GeometryNode                                         => True, others => False),
  ObjectRef       => (GeometryNode | CameraNode | LightNode                => True, others => False),
  Mesh            => (GeometryObject                                       => True, others => False),
  Morph           => (GeometryObject                                       => True, others => False),
  MorphWeight     => (GeometryObject                                       => True, others => False),
  CameraNode      => (Node_Kind                                            => True, others => False),
  BoneNode        => (Node_Kind                                            => True, others => False),
  GeometryNode    => (Node_Kind                                            => True, others => False),
  Node            => (Node_Kind                                            => True, others => False),
  LightNode       => (Node_Kind                                            => True, others => False),
  Animation       => (Node_Kind | Texture                                  => True, others => False),
  Rotation        => (Node_Kind | Texture                                  => True, others => False),
  Scale           => (Node_Kind | Texture                                  => True, others => False),
  Translation     => (Node_Kind | Texture                                  => True, others => False),
  Transform       => (Node_Kind | Texture | Skin | Skeleton                => True, others => False),
  OpenGEX.Name    => (Node_Kind | Morph   | Clip | Material                => True, others => False),
  Extension       =>                                                               (others => False), others => (others => False)),

  -------------------------
  Primitive_Components => (
  -------------------------

  BoneRefArray    => (Allow_Arrays => True,  Allowed_Primitives => (Ref_Kind                                                            => True, others => False)),
  BoneCountArray  => (Allow_Arrays => True,  Allowed_Primitives => (Any_Unsigned_Int_Kind                                               => True, others => False)),
  BoneIndexArray  => (Allow_Arrays => True,  Allowed_Primitives => (Any_Unsigned_Int_Kind                                               => True, others => False)),
  BoneWeightArray => (Allow_Arrays => True,  Allowed_Primitives => (Float32_Kind                                                        => True, others => False)),
  Color           => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind          | Float32_x4_Kind                          => True, others => False)),
  IndexArray      => (Allow_Arrays => True,  Allowed_Primitives => (Any_Unsigned_Int_Kind    | Any_Unsigned_Int_x2_Kind |
                                                                    Any_Unsigned_Int_x3_Kind | Any_Unsigned_Int_x4_Kind                 => True, others => False)),
  Key             => (Allow_Arrays => True,  Allowed_Primitives => (Float32_Kind | Float32_x3_Kind | Float32_x4_Kind | Float32_x16_Kind => True, others => False)),
  MaterialRef     => (Allow_Arrays => False, Allowed_Primitives => (Ref_Kind                                                            => True, others => False)),
  Metric          => (Allow_Arrays => False, Allowed_Primitives => (String_Kind  | Float32_Kind                                         => True, others => False)),
  MorphWeight     => (Allow_Arrays => False, Allowed_Primitives => (Float32_Kind                                                        => True, others => False)),
  OpenGEX.Name    => (Allow_Arrays => False, Allowed_Primitives => (String_Kind                                                         => True, others => False)),
  ObjectRef       => (Allow_Arrays => False, Allowed_Primitives => (Ref_Kind                                                            => True, others => False)),
  Param           => (Allow_Arrays => False, Allowed_Primitives => (Float32_Kind                                                        => True, others => False)),
  Rotation        => (Allow_Arrays => False, Allowed_Primitives => (Float32_Kind | Float32_x4_Kind                                      => True, others => False)),
  Scale           => (Allow_Arrays => False, Allowed_Primitives => (Float32_Kind | Float32_x3_Kind                                      => True, others => False)),
  Texture         => (Allow_Arrays => False, Allowed_Primitives => (String_Kind                                                         => True, others => False)),
  Transform       => (Allow_Arrays => True,  Allowed_Primitives => (Float32_x16_Kind                                                    => True, others => False)),
  Translation     => (Allow_Arrays => False, Allowed_Primitives => (Float32_Kind | Float32_x3_Kind                                      => True, others => False)),
  VertexArray     => (Allow_Arrays => True,  Allowed_Primitives => (Any_Float_Kind    | Any_Float_x2_Kind |
                                                                    Any_Float_x3_Kind | Any_Float_x4_Kind                               => True, others => False)), others => (False, (others => False))),

  -------------------------
  Structure_Components => (
  -------------------------

  Morph           => (OpenGEX.Name => Max_One,                                                                                                  others => None),
  BoneNode        => (OpenGEX.Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More,                           others => None),
  Node            => (OpenGEX.Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More,                           others => None),
  CameraNode      => (OpenGEX.Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  LightNode       => (OpenGEX.Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  GeometryNode    => (OpenGEX.Name => Max_One, Node_Kind | Movement_Kind | MaterialRef | MorphWeight => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  Clip            => (OpenGEX.Name => Max_One, Param                   => Zero_Or_More,                                                         others => None),
  Material        => (OpenGEX.Name => Max_One, Param | Color | Texture => Zero_Or_More,                                                         others => None),
  LightObject     => (Atten => Zero_Or_More,   Param | Color | Texture => Max_One,                                                              others => None),
  CameraObject    => (Param => Zero_Or_More,                                                                                                    others => None),
  Atten           => (Param => Zero_Or_More,                                                                                                    others => None),
  OpenGEX.Time    => (Key   => One_To_Three,                                                                                                    others => None),
  Value           => (Key   => One_To_Four,                                                                                                     others => None),
  Skin            => (Transform     => Zero_Or_One, Skeleton | BoneCountArray | BoneIndexArray | BoneWeightArray => Exactly_One,                others => None),
  Extension       => (Extension     => Zero_Or_More,                                                                                            others => None),
  Texture         => (Movement_Kind => Zero_Or_More,                                                                                            others => None),
  Animation       => (Track         => One_Or_More,                                                                                             others => None),
  GeometryObject  => (Mesh          => One_Or_More, Morph      => Zero_Or_More,                                                                 others => None),
  Mesh            => (VertexArray   => One_Or_More, IndexArray => Zero_Or_More, Skin => Max_One,                                                others => None),
  Skeleton        => (Transform | BoneRefArray => Exactly_One,                                                                                  others => None),
  Track           => (OpenGEX.Time | Value     => Exactly_One,                                                                                  others => None), others => (others => None)),

  ---------------
  Properties => (
  ---------------

  Animation      => ((Identifier => Id ("clip"),        Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => Id ("begin"),       Kind => Float32_Kind,        Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => Id ("end"),         Kind => Float32_Kind,        Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Atten          => ((Identifier => Id ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("distance")))),
                     (Identifier => Id ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  Clip           => ((Identifier => Id ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Color          => ((Identifier => Id ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Extension      => ((Identifier => Id ("applic"),      Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => NULL_STR_32_UNBOUND))),
                     (Identifier => Id ("type"),        Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  GeometryNode   => ((Identifier => Id ("visible"),     Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),
                     (Identifier => Id ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),
                     (Identifier => Id ("motion blur"), Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),                                             others => NULL_PROPERTY),
  GeometryObject => ((Identifier => Id ("visible"),     Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),
                     (Identifier => Id ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),
                     (Identifier => Id ("motion blur"), Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  IndexArray     => ((Identifier => Id ("material"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => Id ("restart"),     Kind => Unsigned_Int64_Kind, Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => Id ("front"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("ccw")))),       others => NULL_PROPERTY),
  Key            => ((Identifier => Id ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("value")))),     others => NULL_PROPERTY),
  LightNode      => ((Identifier => Id ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),                                             others => NULL_PROPERTY),
  LightObject    => ((Identifier => Id ("type"),        Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => Id ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  Material       => ((Identifier => Id ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  MaterialRef    => ((Identifier => Id ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Mesh           => ((Identifier => Id ("lod"),         Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => Id ("primitive"),   Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("triangles")))), others => NULL_PROPERTY),
  Metric         => ((Identifier => Id ("key"),         Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Morph          => ((Identifier => Id ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => Id ("base"),        Kind => Unsigned_Int32_Kind, Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  MorphWeight    => ((Identifier => Id ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Param          => ((Identifier => Id ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Rotation       => ((Identifier => Id ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("axis")))),
                     (Identifier => Id ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Scale          => ((Identifier => Id ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("xyz")))),
                     (Identifier => Id ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Texture        => ((Identifier => Id ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => Id ("texcoord"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  OpenGEX.Time   => ((Identifier => Id ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  Track          => ((Identifier => Id ("target"),      Kind => Ref_Kind,            Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Transform      => ((Identifier => Id ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Translation    => ((Identifier => Id ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("xyz")))),
                     (Identifier => Id ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Value          => ((Identifier => Id ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  VertexArray    => ((Identifier => Id ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => Id ("morph"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY), others => (others => NULL_PROPERTY)));

  use Struct_Types;
  use Engine_Exchange_Format;
  use Engine_Exchange_Format.Hashed_Ptr_Structure_State;

  -------------
  -- Getters --
  -------------

  function Get (Possible_Matches : Array_Ptr_Structure_State; Attrib : Str_32) return Ptr_Structure_State is
    Result : Ptr_Structure_State  := null;
    begin
      for Possible_Match of Possible_Matches loop
        if Possible_Match.Properties /= null and then Possible_Match.Properties.Contains (U ("attrib"))
          and then Possible_Match.Properties.Element (U ("attrib")).Primitive.String_Val (1) = To_Str_32_Unbound (Attrib)
        then
          if Result /= null then raise Program_Error; end if;
          Result := Possible_Match;
        end if;
      end loop;
      return Result;
    end;
  function Get (Structure : Ptr_Structure_State; Class : OpenGEX.Structure_Kind; Attrib : Str_32) return Ptr_Structure_State is
    (Get (Get (Structure, Class), Attrib));
  function Get (Structure : Ptr_Structure_State; Class_Path : Array_Struct_T; Attrib : Str_32) return Ptr_Structure_State is
    (Get (Get (Structure, Class_Path), Attrib));
  function Get (Structure : Ptr_Structure_State; Class : OpenGEX.Structure_Kind; Attrib : Str_32; Kind : Primitive_Kind) return Ptr_Structure_State is
    (Get (Get (Get (Structure, Class), Attrib), Kind));
  function Get (Structure : Ptr_Structure_State; Class_Path : Array_Struct_T; Attrib : Str_32; Kind : Primitive_Kind) return Ptr_Structure_State is
    (Get (Get (Get (Structure, Class_Path), Attrib), Kind));

  -----------------
  -- Conversions --
  -----------------

  function To_Geometry_Name (Val : Str_32_Unbound) return Geometry_Name is
    (To_Super_String (To_Str_8 (Val), MAX_GEOMETRY_NAME));
  function To_Geometry_Name (Val : Str) return Geometry_Name is
    (To_Super_String (To_Str_8 (Val), MAX_GEOMETRY_NAME));

  function To_Transform_4D (Val : Array_x2_Real_32; I : Positive := 1) return Transform_4D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 5)), Real_32 (Val (I,  9)), Real_32 (Val (I, 13)),
     Real_32 (Val (I, 2)), Real_32 (Val (I, 6)), Real_32 (Val (I, 10)), Real_32 (Val (I, 14)),
     Real_32 (Val (I, 3)), Real_32 (Val (I, 7)), Real_32 (Val (I, 11)), Real_32 (Val (I, 15)));

  function To_Vector_3D (Val : Array_x2_Real_32; I : Positive := 1) return Vector_3D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 2)), Real_32 (Val (I,  3)));

  function To_Vector_2D (Val : Array_x2_Real_32; I : Positive := 1) return Vector_2D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 2)));

  ----------------
  -- Load_Model --
  ----------------

  procedure Load_Model (Path : Str) is
    Start      : Ada.Calendar.Time := Clock;
    File       : Engine_Exchange_Format.File_State;
    Result     : Ptr_Mesh_State := null;
    Built_Pose : Bool := False;
    Current_Mesh : Positive := 1;
    Current       : Ptr_Structure_State;

    -- ???
    procedure Load_Structure (Structure : Ptr_Structure_State) is
      begin
        case Structure.Class is

          -- ???
          when GeometryObject =>

            -- Sanity checks
            if Result.Skeleton = null then return; end if; -- raise Program_Error; end if;

            -- Load mesh
            declare

            Bone_Mappings : Array_Bone_Index (1..Result.Skeleton.Bone_Count) := (others => BAD_CHILD);
            Mesh          : Ptr_Geometry_Object_State                 := new Geometry_Object_State'
              (Vertex_Count         => Get (Structure, (OpenGEX.Mesh, VertexArray), "position", Float32_x3_Kind).Len,
               Index_Count          => Get (Structure, (OpenGEX.Mesh, IndexArray), Unsigned_Int32_x3_Kind).Len,
               Bone_Influence_Count => Bone_Index (Get (Structure, (OpenGEX.Mesh, Skin, BoneCountArray),  Unsigned_Int16_Kind).Len),
               Bone_Weight_Count    => Bone_Index (Get (Structure, (OpenGEX.Mesh, Skin, BoneWeightArray), Float32_Kind).Len), others => <>);

            begin
              Mesh.Skeleton := Result.Skeleton;

              -- Build the mapping
              Current := Get (Structure, (OpenGEX.Mesh, Skin, OpenGEX.Skeleton, BoneRefArray), Ref_Kind);
              for I in Current.Primitive.Ref_Val'Range loop
                for J in Result.Skeleton.Bones'Range loop
                  if Get (Ref (Current.Primitive.Ref_Val (I)), OpenGEX.Name, String_Kind).Primitive.String_Val (1)
                    = To_Str_32_Unbound (Super_To_String (Result.Skeleton.Bones (J).Name))
                  then
                    if Bone_Mappings (Bone_Index (I)) /= BAD_CHILD then raise Program_Error; end if; -- Duplicate bone
                    Bone_Mappings (Bone_Index (I)) := J;
                  end if;
                end loop;
              end loop;

              -- Verify all bones have been mapped
              for Bone_Mapping of Bone_Mappings loop
                if Bone_Mapping = BAD_CHILD then raise Program_Error; end if;
              end loop;

              -- Build the skeleton pose assuming they are all the same
              if not Built_Pose then
                Current := Get (Structure, (OpenGEX.Mesh, Skin, OpenGEX.Skeleton, Transform), Float32_x16_Kind);
                for I in Result.Skeleton.Pose'Range loop
                  Result.Skeleton.Pose (I) := To_Transform_4D (Current.Primitive.Float32_x16_Val, Int (I));
                end loop;
                Built_Pose := True;
              end if;

              -- Load 3D data
              Current := Get (Structure, (OpenGEX.Mesh, VertexArray), "position", Float32_x3_Kind);
              for I in Mesh.Verticies'Range loop Mesh.Verticies (I).Position  := To_Vector_3D (Current.Primitive.Float32_x3_Val, I); end loop;
              Current := Get (Structure, (OpenGEX.Mesh, VertexArray), "normal",   Float32_x3_Kind);
              for I in Mesh.Verticies'Range loop Mesh.Verticies (I).Normal    := To_Vector_3D (Current.Primitive.Float32_x3_Val, I); end loop;
              Current := Get (Structure, (OpenGEX.Mesh, VertexArray), "texcoord", Float32_x2_Kind);
              for I in Mesh.Verticies'Range loop Mesh.Verticies (I).Texturing := To_Vector_2D (Current.Primitive.Float32_x2_Val, I); end loop;
              Current := Get (Structure, (OpenGEX.Mesh, IndexArray), Unsigned_Int32_x3_Kind);
              Mesh.Indicies := Current.Primitive.Unsigned_Int32_x3_Val;

              -- Load bone stuff
              Current := Get (Structure, (OpenGEX.Mesh, Skin, BoneCountArray),  Unsigned_Int16_Kind);
              for I in Mesh.Bone_Influences'Range loop Mesh.Bone_Influences (I) := Bone_Index (Current.Primitive.Unsigned_Int16_Val (Int (I))); end loop;
              Current := Get (Structure, (OpenGEX.Mesh, Skin, BoneIndexArray),  Unsigned_Int16_Kind);
              for I in Mesh.Bone_Indicies'Range   loop Mesh.Bone_Indicies (I)   := Bone_Mappings (Bone_Index (Current.Primitive.Unsigned_Int16_Val (Int (I))) + 1); end loop;
              Current := Get (Structure, (OpenGEX.Mesh, Skin, BoneWeightArray), Float32_Kind);
              for I in Mesh.Bone_Weights'Range    loop Mesh.Bone_Weights (I)    := Current.Primitive.Float32_Val  (Int (I)); end loop;

              -- Slap in the globals
              Result.Geometries (Current_Mesh) := Mesh;
              Current_Mesh := Current_Mesh + 1;
            end;

          -- ???
          when Node =>

            -- We have found the true skeleton - we assume it has animations associated with it and they are all in this file!
            if Structure.Counts (BoneNode) = 1 then

              -- Sanity checks
              if Result.Skeleton /= null then raise Program_Error with "More than 1 skeleton in file!"; end if;

              -- Load skeleton
              declare
              Max_Children     : Bone_Index := 1;
              Bone_Count       : Bone_Index := 0;
              Animation_Frames : Positive   := 1;
              Root_Bone        : Ptr_Structure_State := Get (Structure, BoneNode);

              -- ???
              procedure Obtain_Descriminants (Bone : Ptr_Structure_State) is
                begin
                  if Bone_Index (Bone.Counts (BoneNode)) > Max_Children then
                    Max_Children := Bone_Index (Bone.Counts (BoneNode));
                  end if;
                  Bone_Count := Bone_Count + 1;
                  if Bone.Named_Children /= null then
                    for I of Bone.Named_Children.all loop
                      if I.Class = BoneNode then Obtain_Descriminants (I); end if;
                    end loop;
                  end if;
                end;

              -- ???
              procedure Load_Bone (Bone : Ptr_Structure_State; Parent : Bone_Index) is
                Current     : Bone_Index                := Bone_Count;
                Child_Count : Bone_Index                := 1;
                Children    : Array_Ptr_Structure_State := Get (Bone, BoneNode);
                Field       : Ptr_Structure_State;
                begin

                  -- Setup current bone
                  Result.Skeleton.Bones (Current).Name      := To_Geometry_Name (Get (Bone, OpenGEX.Name, String_Kind).Primitive.String_Val (1));
                  Result.Skeleton.Bones (Current).Transform := To_Transform_4D  (Get (Bone, Transform,    Float32_x16_Kind).Primitive.Float32_x16_Val, 1);
                  Result.Skeleton.Bones (Current).Parent    := Parent;

                  -- Animation values
                  Field := Get (Bone, (Animation, Track, Value, Key), Float32_x16_Kind);
                  for I in 1..Field.Len loop
                    Result.Skeleton.Animation_Values (Current, I) := To_Transform_4D (Field.Primitive.Float32_x16_Val, I);
                  end loop;

                  -- Animation times
                  Field := Get (Bone, (Animation, Track, OpenGEX.Time, Key), Float32_Kind);
                  for I in 1..Field.Len loop
                    Result.Skeleton.Animation_Times (Current, I) := Field.Primitive.Float32_Val (I);
                  end loop;

                  -- Children
                  for I of Children loop
                    Bone_Count := Bone_Count + 1;
                    Result.Skeleton.Bone_Children (Current, Child_Count) := Bone_Count;
                    Child_Count := Child_Count + 1;
                    Load_Bone (I, Current);
                  end loop;
                end;

              -- Start of processing for BoneNode
              begin

                -- Obtain the max children and bone count, then construct the skeleton
                Obtain_Descriminants (Root_Bone);
                Result.Skeleton := new Skeleton_State'(Bone_Count, Max_Children, 1,
                                                        Get (Root_Bone, (Animation, Track, OpenGEX.Time, Key), Float32_Kind).Len, others => <>);

                -- Load the skeleton by recursivly looking for children
                Bone_Count := 1; -- Repurposed
                Result.Skeleton.Bone_Children := (others => (others => BAD_CHILD));
                Load_Bone (Root_Bone, BAD_CHILD);

                -- Set the animations, right now the plugin doesn't support multiple animations per skeleton
                Result.Skeleton.Animation_Names (1) := To_Geometry_Name ("main");
                Result.Skeleton.Animations (1)      := 1;
              end;
            end if;
          when Material => null;
        when others => null; end case;
      end;

    -- Start of Load
    begin

      -- Bulk of the IO
      Engine_Exchange_Format.Load (S (OS_Info.App_Path) & PATH_MODELS & Path & ".ogex", File);

      -- Prepare result
      Result := new Mesh_State'(File.Counts (GeometryObject), others => <>);
      Result.Name := To_Geometry_Name ("main");

      -- Load structures
      for I of File.Named_Structures loop Load_Structure (I); end loop;
      for I of File.Structures       loop Load_Structure (I); end loop;

      -- Inform the console about the loading of the assets
      Meshes.Insert (U (Path), Result);
      Line ("Loaded " & Path & " in" & Duration'Wide_Image (Clock - Start));
    end;

  ----------
  -- Free --
  ----------

  procedure Free (Path : Str) is
    begin
      null;
    end;
end;
