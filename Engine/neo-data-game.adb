
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

-- Isolate parsers for different model formats into separate packages and consolidate them here
package body Neo.Data.Game is

  use Neo.Data.Identifier_Str;

  ---------------
  -- Utilities --
  ---------------

  function To_Transform_4D (Val : Array_x2_Real_32; I : Positive := 1) return Transform_4D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 5)), Real_32 (Val (I,  9)), Real_32 (Val (I, 13)),
     Real_32 (Val (I, 2)), Real_32 (Val (I, 6)), Real_32 (Val (I, 10)), Real_32 (Val (I, 14)),
     Real_32 (Val (I, 3)), Real_32 (Val (I, 7)), Real_32 (Val (I, 11)), Real_32 (Val (I, 15)));

  function To_Vector_3D (Val : Array_x2_Real_32; I : Positive := 1) return Vector_3D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 2)), Real_32 (Val (I,  3)));

  function To_Vector_2D (Val : Array_x2_Real_32; I : Positive := 1) return Vector_2D is
    (Real_32 (Val (I, 1)), Real_32 (Val (I, 2)));

  function To_Point_3D (Val : Array_x2_Real_32; I, J : Positive := 1) return Point_3D is
    (Real_32 (Val (I, J)), Real_32 (Val (I, J + 1)), Real_32 (Val (I, J + 2)));

  function To_Point_2D (Val : Array_x2_Real_32; I, J : Positive := 1) return Point_2D is
    (Real_32 (Val (I, J)), Real_32 (Val (I, J + 1)));

  ----------------------------
  -- Engine_Exchange_Format --
  ----------------------------
  -- https://web.archive.org/web/20180903014314/http://opengex.org/OpenGEX.2.0.pdf

  -- Enumerate all possible structures, its wrapped in its own package due to naming conflicts with "Time" and "Name"
  package OpenGEX is type Structure_Kind is (BoneNode,     CameraNode,     GeometryNode,   LightNode,    Node,
                                             Animation,    Transform,      Translation,    Rotation,     Scale,
                                             CameraObject, GeometryObject, LightObject,    ObjectRef,    MaterialRef,
                                             Bone,         BoneCountArray, BoneIndexArray, BoneRefArray, BoneWeightArray,
                                             Atten,        Clip,           Color,          Extension,    IndexArray,
                                             Key,          Material,       Mesh,           Metric,       Morph,
                                             MorphWeight,  Name,           Param,          Skeleton,     Skin,
                                             Texture,      Time,           Value,          VertexArray,  Track); end; use OpenGEX;
  subtype Node_Kind     is OpenGEX.Structure_Kind range BoneNode  .. Node;
  subtype Movement_Kind is OpenGEX.Structure_Kind range Animation .. Scale;

  package OpenGEX_Struct_Types is new Description_Language_Struct_Types (OpenGEX.Structure_Kind, ObjectRef); use OpenGEX_Struct_Types;
  package Engine_Exchange_Format is new Description_Language (OpenGEX_Struct_Types,

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
  Extension    |
  Node_Kind    |
  CameraObject => True, others => False),

  ----------------------
  Primitive_Components => (
  ----------------------

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

  ----------------------
  Structure_Components => (
  ----------------------

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
  Track           => (OpenGEX.Time | Value     => Exactly_One,                                                                                  others => None),
  Skeleton        => (Transform | BoneRefArray => Exactly_One,                                                                                  others => None),
  Skin            => (Transform     => Zero_Or_One, Skeleton | BoneCountArray | BoneIndexArray | BoneWeightArray => Exactly_One,                others => None),
  Extension       => (Extension     => Zero_Or_More,                                                                                            others => None),
  Texture         => (Movement_Kind => Zero_Or_More,                                                                                            others => None),
  Animation       => (Track         => One_Or_More,                                                                                             others => None),
  GeometryObject  => (Mesh          => One_Or_More, Morph      => Zero_Or_More,                                                                 others => None),
  Mesh            => (VertexArray   => One_Or_More, IndexArray => Zero_Or_More, Skin => Max_One,                                                others => None), others => (others => None)),

  -------------
  Properties => (
  -------------

  Animation      => ((Identifier => S ("clip"),        Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => S ("begin"),       Kind => Float32_Kind,        Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => S ("end"),         Kind => Float32_Kind,        Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Atten          => ((Identifier => S ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("distance")))),
                     (Identifier => S ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  Clip           => ((Identifier => S ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Color          => ((Identifier => S ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Extension      => ((Identifier => S ("applic"),      Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => NULL_STR_32_UNBOUND))),
                     (Identifier => S ("type"),        Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  GeometryNode   => ((Identifier => S ("visible"),     Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),
                     (Identifier => S ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),
                     (Identifier => S ("motion blur"), Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),                                             others => NULL_PROPERTY),
  GeometryObject => ((Identifier => S ("visible"),     Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),
                     (Identifier => S ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),
                     (Identifier => S ("motion blur"), Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  IndexArray     => ((Identifier => S ("material"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => S ("restart"),     Kind => Unsigned_Int64_Kind, Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => S ("front"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("ccw")))),       others => NULL_PROPERTY),
  Key            => ((Identifier => S ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("value")))),     others => NULL_PROPERTY),
  LightNode      => ((Identifier => S ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Overrides_From_Ref => True),                                             others => NULL_PROPERTY),
  LightObject    => ((Identifier => S ("type"),        Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => S ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  Material       => ((Identifier => S ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => True))),            others => NULL_PROPERTY),
  MaterialRef    => ((Identifier => S ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Mesh           => ((Identifier => S ("lod"),         Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => S ("primitive"),   Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("triangles")))), others => NULL_PROPERTY),
  Metric         => ((Identifier => S ("key"),         Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Morph          => ((Identifier => S ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),
                     (Identifier => S ("base"),        Kind => Unsigned_Int32_Kind, Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  MorphWeight    => ((Identifier => S ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  Param          => ((Identifier => S ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Rotation       => ((Identifier => S ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("axis")))),
                     (Identifier => S ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Scale          => ((Identifier => S ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("xyz")))),
                     (Identifier => S ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Texture        => ((Identifier => S ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => S ("texcoord"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY),
  OpenGEX.Time   => ((Identifier => S ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  Track          => ((Identifier => S ("target"),      Kind => Ref_Kind,            Defaulted => False,  Overrides_From_Ref => False),                                            others => NULL_PROPERTY),
  Transform      => ((Identifier => S ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Translation    => ((Identifier => S ("kind"),        Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("xyz")))),
                     (Identifier => S ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default            => (Bool_Kind,           1, (1 => False))),           others => NULL_PROPERTY),
  Value          => ((Identifier => S ("curve"),       Kind => String_Kind,         Defaulted => True,   Default            => (String_Kind,         1, (1 => W ("linear")))),    others => NULL_PROPERTY),
  VertexArray    => ((Identifier => S ("attrib"),      Kind => String_Kind,         Defaulted => False,  Overrides_From_Ref => False),
                     (Identifier => S ("morph"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default            => (Unsigned_Int32_Kind, 1, (1 => 0))),               others => NULL_PROPERTY), others => (others => NULL_PROPERTY)));

  use Engine_Exchange_Format.Hashed_Ptr_Structure_State;

  ---------------
  -- Load_Mesh --
  ---------------

  procedure Load_Mesh (Path : Str; Result : in out Ptr_Mesh_State) is
    use OpenGEX_Struct_Types;
    use Engine_Exchange_Format;
    File            : Engine_Exchange_Format.File_State;
    Current, GeoObj : Ptr_Structure_State;
    Current_Mesh    : Positive := 1;
    Current_Segment : Positive := 1;
    Segment_Count   : Natural := 0;
    Vertex_Count    : Natural := 0;
    Index_Count     : Natural := 0;
    Last_Vertex     : Natural := 0;
    Last_Index      : Natural := 1;
    Start           : Ada.Calendar.Time := Clock;
    begin

      -- Bulk of the IO
      Engine_Exchange_Format.Load (Path & ".ogex", File);

      -- Count verticies and indicies
      for Structure of File.Structures loop
        if Structure.Class = GeometryObject then
          for Child of Structure.Children.all loop
            if Child.Class = OpenGEX.Mesh then
              Vertex_Count := Vertex_Count + Get (Child, VertexArray, "attrib", "position", Float32_x3_Kind).Len;
              for Index_Array of Get (Child, IndexArray) loop
                Segment_Count := Segment_Count + 1;
                Index_Count   := Index_Count   + Index_Array.Children.Element (1).Primitive.Len;
              end loop;
            end if;
          end loop;
        end if;
      end loop;

      -- Create the mesh
      Result      := new Mesh_State'(Vertex_Count, Index_Count, Segment_Count, 0, 0, others => <>);
      Result.Name := Geometry_Name_Str.S (To_Str_16 (Base_Name (To_Str_8 (Path))));

      -- Load structures
      for Structure of File.Structures loop
        case Structure.Class is

          -- 3D data
          when GeometryNode =>
            GeoObj := Ref (Get (Structure, ObjectRef, Ref_Kind).Primitive.Ref_Val (1));
            for Child of Get (GeoObj, OpenGEX.Mesh) loop

              -- Verticies
              Current := Get (GeoObj, (OpenGEX.Mesh, VertexArray), "attrib", "position", Float32_x3_Kind);
              for I in Result.Vertices'Range loop
                Result.Vertices (I).Position := To_Point_3D (Current.Primitive.Float32_x3_Val, I);
              end loop;
              Current := Get (GeoObj, (OpenGEX.Mesh, VertexArray), "attrib", "normal", Float32_x3_Kind);
              for I in Result.Vertices'Range loop
                Result.Vertices (I).Normal := To_Point_3D (Current.Primitive.Float32_x3_Val, I);
              end loop;
              Current := Get (GeoObj, (OpenGEX.Mesh, VertexArray), "attrib", "texcoord", Float32_x2_Kind);
              for I in Result.Vertices'Range loop
                Result.Vertices (I).Texture_Coordinate := To_Point_2D (Current.Primitive.Float32_x2_Val, I);
              end loop;

              -- Segments
              for Grand_Child of Get (Child, IndexArray) loop
                Current := Grand_Child.Children.Element (1);
                declare
                Mesh_Indicies : Array_x2_Int_32_Unsigned (1..Current.Primitive.Len, 1..3);
                for Mesh_Indicies'Address use Result.Indices (Last_Index, 1)'Address;


                Material_Index : Int_32_Unsigned := Grand_Child.Properties.Element
                                                      (U ("material")).Primitive.Unsigned_Int32_Val (1);
                A : Ptr_Structure_State;
                begin
                  Mesh_Indicies := Current.Primitive.Unsigned_Int32_x3_Val;

                  -- Get the material something
                  for Possible_Match of Get (Structure, MaterialRef) loop
                    if Possible_Match.Properties /= null and then Possible_Match.Properties.Contains (U ("index"))
                      and then Possible_Match.Properties.Element (U ("index")).Primitive.Unsigned_Int32_Val (1) = Material_Index
                    then
                      A := Ref (Possible_Match.Children.Element (1).Primitive.Ref_Val (1));
                      exit;
                    end if;
                  end loop;

                  -- Material
                  Result.Segments (Current_Segment) := (Index    => Last_Index + 1,
                                                        Count    => Current.Primitive.Len,
                                                        Name     => Geometry_Name_Str.S (To_Str (GeoObj.Name.all)
                                                                      & Current_Mesh'Wide_Image),
                                                        Material => S
                                                                      (Replace
                                                                         (To_Str_16
                                                                            (Get (Class     => OpenGEX.Name,
                                                                                  Primitive => String_Kind,
                                                                                  Structure => A).Primitive.String_Val (1)),
                                                                          "/", "_")));
                  Last_Index      := Last_Index + Current.Primitive.Len;
                  Current_Segment := Current_Segment + 1;
                end;
              end loop;
            end loop;
        when others => null; end case;
      end loop;

      -- Dump timing
      Line ("Loaded " & To_Str_16 (Base_Name (To_Str_8 (Path))) & " in" & Duration'Wide_Image (Clock - Start) & "s");
      Engine_Exchange_Format.Finalize (File);
    end;

  --------------------
  -- Load_Materials --
  --------------------

  procedure Load_Materials (Path : Str; Result : in out Hashed_Material.Unsafe.Map) is
    use OpenGEX_Struct_Types;
    use Engine_Exchange_Format;
    Material : Material_State;
    File     : Engine_Exchange_Format.File_State;
    Struct   : Ptr_Structure_State;
    Start    : Ada.Calendar.Time := Clock;
    begin

      -- Bulk of the IO
      Engine_Exchange_Format.Load (Path, File);

      -- Load structures
      for I of File.Structures loop
        if I.Class = OpenGEX.Material then
          Struct := Get (I, Texture, "attrib", "normal", String_Kind);
          if Struct /= null then
            Material.Normal := S (To_Str_16 (Struct.Primitive.String_Val (1)));
          end if;
          Struct := Get (I, Texture, "attrib", "specular", String_Kind);
          if Struct /= null then
            Material.Specular := S (To_Str_16 (Get (I, Texture, "attrib", "specular", String_Kind).Primitive.String_Val (1)));
          end if;
          Struct := Get (I, Texture, "attrib", "diffuse", String_Kind);
          if Struct /= null then
            Material.Base_Color := S (To_Str_16 (Get (I, Texture, "attrib", "diffuse",  String_Kind).Primitive.String_Val (1)));
          else
            Struct := Get (I, Texture, "attrib", "editor", String_Kind);
            if Struct /= null then
              Material.Base_Color := S (To_Str_16 (Get (I, Texture, "attrib", "editor",  String_Kind).Primitive.String_Val (1)));
            end if;
          end if;
          Struct := Get (I, OpenGEX.Name, String_Kind);
          if Struct /= null then
            Material.Name := S (To_Str_16 (Get (I, OpenGEX.Name, String_Kind).Primitive.String_Val (1)));
          end if;
          if not Result.Contains (U (To_Lower (To_Str_16 (Get (I, OpenGEX.Name, String_Kind).Primitive.String_Val (1))))) then
            Result.Insert (U (To_Lower (To_Str_16 (Get (I, OpenGEX.Name, String_Kind).Primitive.String_Val (1)))), Material);
          else
            null;
            --  Line ("Duplicate material definition for " & To_Str_16 (Get (I, OpenGEX.Name, String_Kind).Primitive.String_Val (1)));
          end if;
        end if;
      end loop;
      Line ("Loaded " & To_Str_16 (Base_Name (To_Str_8 (Path))) & " in" & Duration'Wide_Image (Clock - Start) & "s");
      Engine_Exchange_Format.Finalize (File);
    end;

  -------------------
  -- Id_Map_Format --
  -------------------
  --
  -- Entity $entity12
  -- {
  --   Class {string {"light"}}
  --   Name {string {"light_5538"}}
  --   Origin {float[3] {{-832, 560, 104}}}
  --   // Uknown attribute: "noshadows" "0"
  --   LightRadius {float[3] {{46, 22, 71}}}
  --   LightCenter {float[3] {{68, 78, 44}}}
  -- }
  -- ...
  -- GeometryObject $light_5487 // 2
  -- {
  --    Mesh
  --    {
  --       MaterialName {string {"textures_mcity_mcitya12_2_break"}}
  --       VertexArray
  --       {
  --          float[8] // x y z u v nx ny nz
  --          {
  --             {-8, 21, 21, 0, 0, 0, 0.4705882668, -0.8823529482}, {8, -24, -3, -1, 1, 0, 0.4705882668, -0.8823529482},
  --             {-8, -24, -3, -1, 0, 0, 0.4705882668, -0.8823529482}
  --          }
  --       }
  --       IndexArray
  --       {
  --          unsigned_int32
  --          {
  --             0, 1, 2, 0, 3, 1
  --          }
  --       }
  --    }
  --    ...
  -- }
  -- ...
  -- PortalArray
  -- {
  --    Portal $portal0
  --    {
  --       unsigned_int[3] {{4, 2, 1}}
  --       VertexArray
  --       {
  --          float[3] {{1412, 604, 56}, {1412, 740, 56}, {1412, 740, 196}, {1412, 604, 196}}
  --       }
  --    }
  --    ...
  -- }
  -- NodeArray // 6580
  -- {
  --    Node $node0
  --    {
  --       Plane {float[4] {{1, 0, 0, -1024}}}
  --       ChildAreas {int32[2] {{1, 677}}}
  --    }
  --    ...
  -- }
  -- ShadowObject $_prelight_light_5520
  -- {
  --    VertexCount {int32 {492}}
  --    NoCaps {int32 {312}}
  --    NoFrontCaps {int32 {528}}
  --    IndexCount {int32 {744}}
  --    PlaneBits {unsigned_int32 {63}}
  --    VertexArray
  --    {
  --       float[3]
  --       {
  --          {1800, 362, 34}, {1800, 362, 34}, {1800, 362, 20}, {1800, 362, 20}, {1794.7169189453, 362, 33.0754737854},
  --          {1800, 360.1738891602, 20}, {1800, 362, 34}, {1800, 362, 34}, {1800, 362, 20}, {1800, 362, 20},
  --          {1794.3433837891, 362, 34}, {1800, 360.0234985352, 20}, {1800, 362, 34}, {1800, 362, 34}, {1794.7169189453, 362},
  --          {1800, 360.1738891602, 20}, {1800, 348, 34}, {1800, 348, 34}, {1800, 348, 34}, {1800, 348, 34},
  --       }
  --    }
  --    IndexArray
  --    {
  --       unsigned_int32 {
  --          0, 2, 3, 0, 3, 1, 16, 0, 17, 0, 1, 17, 26, 28, 29, 26, 29, 27
  --       }
  --    }
  -- }
  -- ...
  --

  package IdMap is type Structure_Kind is (GeometryObject, Mesh,        MaterialName, VertexArray,  IndexArray,  PortalArray,
                                           Portal,         NodeArray,   Node,         ShadowObject, VertexCount, NoCaps,
                                           NoFrontCaps,    IndexCount,  PlaneBits,    Entity,       Class,       Name,
                                           Origin,         LightCenter, LightRadius,  LastCamPos,   LastCamAng,  ObjectRef,
                                           Plane,          ChildAreas); end; use IdMap;
  subtype Entity_Key_Value_Kind is IdMap.Structure_Kind range Class..LastCamAng;

  package IdMap_Struct_Types is new Description_Language_Struct_Types (IdMap.Structure_Kind, ObjectRef);
  package Id_Map_Format      is new Description_Language (IdMap_Struct_Types,

  -------------------------
  Top_Level_Structures => (
  -------------------------

  IdMap.Entity   |
  ShadowObject   |
  GeometryObject |
  PortalArray    |
  NodeArray      => True, others => False),

  -------------------------
  Primitive_Components => (
  -------------------------

  Portal       => (Allow_Arrays => False, Allowed_Primitives => (Unsigned_Int32_x3_Kind            => True, others => False)),
  MaterialName => (Allow_Arrays => False, Allowed_Primitives => (String_Kind                       => True, others => False)),
  Class        => (Allow_Arrays => False, Allowed_Primitives => (String_Kind                       => True, others => False)),
  LastCamPos   => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind                   => True, others => False)),
  LastCamAng   => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind                   => True, others => False)),
  IdMap.Name   => (Allow_Arrays => False, Allowed_Primitives => (String_Kind                       => True, others => False)),
  IdMap.Origin => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind                   => True, others => False)),
  LightCenter  => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind                   => True, others => False)),
  LightRadius  => (Allow_Arrays => False, Allowed_Primitives => (Float32_x3_Kind                   => True, others => False)),
  VertexArray  => (Allow_Arrays => True,  Allowed_Primitives => (Float32_x3_Kind | Float32_x8_Kind => True, others => False)),
  IndexArray   => (Allow_Arrays => True,  Allowed_Primitives => (Unsigned_Int32_Kind               => True, others => False)),
  Plane        => (Allow_Arrays => False, Allowed_Primitives => (Float32_x4_Kind                   => True, others => False)),
  ChildAreas   => (Allow_Arrays => False, Allowed_Primitives => (Int32_x2_Kind                     => True, others => False)),
  VertexCount  => (Allow_Arrays => False, Allowed_Primitives => (Int32_Kind                        => True, others => False)),
  NoCaps       => (Allow_Arrays => False, Allowed_Primitives => (Int32_Kind                        => True, others => False)),
  NoFrontCaps  => (Allow_Arrays => False, Allowed_Primitives => (Int32_Kind                        => True, others => False)),
  IndexCount   => (Allow_Arrays => False, Allowed_Primitives => (Int32_Kind                        => True, others => False)),
  PlaneBits    => (Allow_Arrays => False, Allowed_Primitives => (Unsigned_Int32_Kind               => True, others => False)),
    others => (False, (others => False))),

  -------------------------
  Structure_Components => (
  -------------------------

  NodeArray      => (Node                                    => Zero_Or_More, others => None),
  PortalArray    => (Portal                                  => Zero_Or_More, others => None),
  GeometryObject => (Mesh                                    => Zero_Or_More, others => None),
  IdMap.Entity   => (Entity_Key_Value_Kind                   => Zero_Or_More, others => None),
  Node           => (Plane | ChildAreas                      => Exactly_One,  others => None),
  Portal         => (VertexArray                             => Exactly_One,  others => None),
  Mesh           => (VertexArray | IndexArray | MaterialName => Exactly_One,  others => None),
  ShadowObject   => (VertexCount | IndexArray | NoFrontCaps | NoCaps | IndexCount | PlaneBits | VertexArray => Exactly_One,
    others => None), others => (others => None)));

  --------------
  -- Load_Map --
  --------------

  procedure Load_Map (Path : Str; Result : in out Ptr_Map_State) is
    use Id_Map_Format;
    File             : Id_Map_Format.File_State;
    Current          : Id_Map_Format.Ptr_Structure_State;
    Current_Mesh     : Positive := 1;
    Current_Segment  : Positive := 1;
    Vertex_Count     : Natural;
    Index_Count      : Natural;
    Last_Vertex      : Natural;
    Last_Index       : Natural;
    Kind             : Entity_Kind;
    Entity           : Ptr_Entity_State;
    Mesh             : Ptr_Mesh_State;
    Start            : Ada.Calendar.Time := Clock;
    begin

      -- Bulk of the IO
      Id_Map_Format.Load (Path & ".idmap", File);

      -- Prepare result
      Result := new Map_State'(File.Counts (GeometryObject), others => <>);

      -- Load structures
      for Structure of File.Structures loop
        case Structure.Class is

          -- Level entities like lights and ammo boxes
          when IdMap.Entity =>

            -- Attempt to determine the kind
            Kind := Moveable_Entity;

            -- Add the mesh
            Construct_Entity (Entity, Kind);

            -- We found the level starting position
            if Structure.Counts (Class) = 1
              and then Get (Structure, Class, String_Kind).Primitive.String_Val (1) = "info_player_start"
            then
            --    Entity.Transform.WX := ;
            --    Entity.Transform.WY := ;
            --    Entity.Transform.WZ := ;
              Entity.Origin := To_Point_3D (Get (Structure, Origin, Float32_x3_Kind).Primitive.Float32_x3_Val, 1, 1);
              Set_Translate (Entity.Transform, To_Point_3D (Get (Structure, Origin, Float32_x3_Kind).Primitive.Float32_x3_Val, 1, 1));
            end if;
            if Structure.Counts (IdMap.Name) = 1 then
              Entity.Name := To_Str_16_Unbound (Get (Structure, IdMap.Name, String_Kind).Primitive.String_Val (1));
            end if;
            --if Structure.Counts (MeshName) = 1 then
            --  Entity.Mesh_Name := To_Str_16_Unbound (Get (Structure, MeshName, String_Kind).Primitive.String_Val (1));
            --end if;

            --  -- Other location attributes
            --  if Structure.Counts (Angle) = 1 then
            --    Entity.Transform.XX := ;
            --    Entity.Transform.XY := ;
            --    Entity.Transform.XZ := ;
            --  end if;
            --
            --
            --  if Structure.Counts (Rotation) = 1 then
            --    Entity.Transform.XX := ;
            --    Entity.Transform.XY := ;
            --    Entity.Transform.XZ := ;
            --    Entity.Transform.YX := ;
            --    Entity.Transform.YY := ;
            --    Entity.Transform.YZ := ;
            --    Entity.Transform.ZX := ;
            --    Entity.Transform.ZY := ;
            --    Entity.Transform.ZZ := ;
            --  end if;
            --
            --
            --  if Structure.Counts (Origin) = 1 then
            --    Entity.Transform.WX := ;
            --    Entity.Transform.WY := ;
            --    Entity.Transform.WZ := ;
            --  end if;
            if not Result.Entities.Contains (Entity.Name) then
              Result.Entities.Insert (Entity.Name, Entity);
            end if;

          -- Precomputed shadow volumes
          when ShadowObject =>
            null;

          -- Map geometry - brush, patches, areas, etc.
          when GeometryObject =>
            if Structure.Counts (IdMap.Mesh) > 0 then

              -- Count the Indicies and Verticies
              Vertex_Count := 0;
              Index_Count  := 0;
              for Child of Structure.Children.all loop
                Index_Count  := Index_Count  + Get (Child, IndexArray,  Unsigned_Int32_Kind).Len / 3;
                Vertex_Count := Vertex_Count + Get (Child, VertexArray, Float32_x8_Kind).Len;
              end loop;

              -- Create the mesh
              Result.Geometry (Current_Mesh) := new Mesh_State'(Vertex_Count         => Vertex_Count,
                                                                Index_Count          => Index_Count,
                                                                Segment_Count        => Structure.Counts (IdMap.Mesh),
                                                                Bone_Influence_Count => 0,
                                                              Bone_Weight_Count    => 0, others => <>);
              Mesh := Result.Geometry (Current_Mesh);
              Current_Mesh := Current_Mesh + 1;
              Mesh.Name := Geometry_Name_Str.S (To_Str (Structure.Name.all));

              -- Load the meshes
              Last_Index      := 0;
              Last_Vertex     := 0;
              Current_Segment := 1;
              for Child of Structure.Children.all loop

                -- Mesh segment
                Mesh.Segments (Current_Segment) := (Name     => Geometry_Name_Str.S (To_Str (Structure.Name.all) & Current_Mesh'Wide_Image),
                                                    Material => S (To_Str_16 (Get (Child, MaterialName, String_Kind).Primitive.String_Val (1))),
                                                    Index    => Last_Index + 1,
                                                    Count    => Get (Child, IndexArray, Unsigned_Int32_Kind).Len / 3);
                Current_Segment := Current_Segment + 1;

                -- Mesh indicies
                Current := Get (Child, IndexArray, Unsigned_Int32_Kind);
                for I in Last_Index + 1..Last_Index + (Current.Primitive.Unsigned_Int32_Val'Last / 3) loop
                  Mesh.Indices (I, 1) := Current.Primitive.Unsigned_Int32_Val ((I - Last_Index) * 3 - 2) + Int_32_Unsigned (Last_Vertex);
                  Mesh.Indices (I, 2) := Current.Primitive.Unsigned_Int32_Val ((I - Last_Index) * 3 - 1) + Int_32_Unsigned (Last_Vertex);
                  Mesh.Indices (I, 3) := Current.Primitive.Unsigned_Int32_Val ((I - Last_Index) * 3)     + Int_32_Unsigned (Last_Vertex);
                end loop;
                Last_Index := Last_Index + Current.Primitive.Unsigned_Int32_Val'Length / 3;

                -- Mesh verticies
                Current := Get (Child, VertexArray, Float32_x8_Kind);
                for I in Last_Vertex + 1..Last_Vertex + Current.Primitive.Float32_x8_Val'Last loop
                  Mesh.Vertices (I).Position           := To_Point_3D (Current.Primitive.Float32_x8_Val, I - Last_Vertex, 1);
                  Mesh.Vertices (I).Texture_Coordinate := To_Point_2D (Current.Primitive.Float32_x8_Val, I - Last_Vertex, 4);
                  Mesh.Vertices (I).Normal             := To_Point_3D (Current.Primitive.Float32_x8_Val, I - Last_Vertex, 6);
                end loop;
                Last_Vertex := Last_Vertex + Current.Primitive.Float32_x8_Val'Length;
              end loop;
            end if;

          -- Portals between areas
          when PortalArray =>
            null;

          -- Tree of areas
          when NodeArray =>
            null;
        when others => null; end case;
      end loop;

      -- Dump timing
      Line ("Loaded " & To_Str_16 (Base_Name (To_Str_8 (Path))) & " in" & Duration'Wide_Image (Clock - Start) & "s");
      Id_Map_Format.Finalize (File);
    end;

  ----------------------
  -- Construct_Entity --
  ----------------------

  procedure Construct_Entity (Item : in out Ptr_Entity_State; Kind : Entity_Kind)  is
  begin
    case Kind is
      when Character_Entity   => Item := new Entity_State'(Character_Entity,   others => <>);
      when Camera_Entity      => Item := new Entity_State'(Camera_Entity,      others => <>);
      when AI_Entity          => Item := new Entity_State'(AI_Entity,          others => <>);
      when Moveable_Entity    => Item := new Entity_State'(Moveable_Entity,    others => <>);
      when Item_Entity        => Item := new Entity_State'(Item_Entity,        others => <>);
      when Hurt_Entity        => Item := new Entity_State'(Hurt_Entity,        others => <>);
      when Information_Entity => Item := new Entity_State'(Information_Entity, others => <>);
      when Speaker_Entity     => Item := new Entity_State'(Speaker_Entity,     others => <>);
      when Activator_Entity   => Item := new Entity_State'(Activator_Entity,   others => <>);
      when Animated_Entity    => Item := new Entity_State'(Animated_Entity,    others => <>);
      when Light_Entity       => Item := new Entity_State'(Light_Entity,       others => <>);
      when Camera_View_Entity => Item := new Entity_State'(Camera_View_Entity, others => <>);
      when Clip_Model_Entity  => Item := new Entity_State'(Clip_Model_Entity,  others => <>);
      when Door_Entity        => Item := new Entity_State'(Door_Entity,        others => <>);
      when Earthquake_Entity  => Item := new Entity_State'(Earthquake_Entity,  others => <>);
      when Elevator_Entity    => Item := new Entity_State'(Elevator_Entity,    others => <>);
      when Emitter_Entity     => Item := new Entity_State'(Emitter_Entity,     others => <>);
      when Explosion_Entity   => Item := new Entity_State'(Explosion_Entity,   others => <>);
      when Forcefield_Entity  => Item := new Entity_State'(Forcefield_Entity,  others => <>);
      when Fracture_Entity    => Item := new Entity_State'(Fracture_Entity,    others => <>);
      when Effects_Entity     => Item := new Entity_State'(Effects_Entity,     others => <>);
      when Portal_Entity      => Item := new Entity_State'(Portal_Entity,      others => <>);
      when Remove_Entity      => Item := new Entity_State'(Remove_Entity,      others => <>);
    end case;
  end;
end;



















































