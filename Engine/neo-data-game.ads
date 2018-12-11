
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

with Neo.Core.Maps; use Neo.Core.Maps;

-- Unified 3D model, shader, and surface type definitions
package Neo.Data.Game is

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

  type Structure_Kind is (BoneNode,     CameraNode,     GeometryNode,   LightNode,    Node, -- Should all start with uppercase
                          Animation,    Transform,      Translation,    Rotation,     Scale,
                          CameraObject, GeometryObject, LightObject,    ObjectRef,    MaterialRef,
                          Bone,         BoneCountArray, BoneIndexArray, BoneRefArray, BoneWeightArray,
                          Atten,        Clip,           Color,          Extension,    IndexArray,
                          Key,          Material,       Mesh,           Metric,       Morph,
                          MorphWeight,  Name,           Param,          Skeleton,     Skin,
                          Texture,      Time,           Value,          VertexArray,  Track);

  subtype Node_Kind     is Structure_Kind range BoneNode  .. Node;
  subtype Movement_Kind is Structure_Kind range Animation .. Scale;

  function Get_Structure_Value (Text : Identifier_Str) return Structure_Kind;

  package Engine_Exchange_Format is new Description_Language (Structure_Kind, Get_Structure_Value,

  -------------------------
  Top_Level_Structures => (
  -------------------------

  BoneNode       |
  CameraNode     |
  Clip           | -- Only top-level
  Extension      |
  GeometryNode   |
  GeometryObject | -- Only top-level
  LightNode      |
  LightObject    | -- Only top-level
  Material       | -- Only top-level
  Metric         | -- Only top-level
  Node           => True, others => False),

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
  Time            => (Track                                                => True, others => False),
  Key             => (Time | Value                                         => True, others => False),
  Atten           => (LightObject                                          => True, others => False),
  Texture         => (LightObject | Material                               => True, others => False),
  Color           => (LightObject | Material                               => True, others => False),
  Param           => (LightObject | Material | CameraObject | Atten | Clip => True, others => False),
  MaterialRef     => (GeometryNode                                         => True, others => False),
  ObjectRef       => (GeometryNode | CameraNode | LightNode                => True, others => False),
  Mesh            => (GeometryObject                                       => True, others => False),
  Morph           => (GeometryObject                                       => True, others => False),
  MorphWeight     => (GeometryObject                                       => True, others => False),
  CameraNode      => (Node_Struct_Kind                                     => True, others => False),
  BoneNode        => (Node_Struct_Kind                                     => True, others => False),
  GeometryNode    => (Node_Struct_Kind                                     => True, others => False),
  Node            => (Node_Struct_Kind                                     => True, others => False),
  LightNode       => (Node_Struct_Kind                                     => True, others => False),
  Animation       => (Node_Struct_Kind | Texture                           => True, others => False),
  Rotation        => (Node_Struct_Kind | Texture                           => True, others => False),
  Scale           => (Node_Struct_Kind | Texture                           => True, others => False),
  Translation     => (Node_Struct_Kind | Texture                           => True, others => False),
  Transform       => (Node_Struct_Kind | Texture | Skin | Skeleton         => True, others => False),
  Name            => (Node_Struct_Kind | Morph   | Clip | Material         => True, others => False),
  Extension       => (others => False), others => (others => False)),

  type Node_State is record
      case Kind is
        when Track | Animation =>
           :
        when Skin | Skeleton =>
          BoneWeightArray :
          BoneIndexArray  :
          BoneCountArray  :


  -------------------
  Primitive_Data => (
  -------------------

  BoneCountArray  => (Allow_Arrays => True,  (Any_Unsigned_Int_Kind                                               => True, others => False)),
  BoneIndexArray  => (Allow_Arrays => True,  (Any_Unsigned_Int_Kind                                               => True, others => False)),
  BoneWeightArray => (Allow_Arrays => True,  (Float32_Kind                                                        => True, others => False)),
  Color           => (Allow_Arrays => False, (Float32_x3_Kind          | Float32_x4_Kind                          => True, others => False)),
  IndexArray      => (Allow_Arrays => True,  (Any_Unsigned_Int_Kind    | Any_Unsigned_Int_x2_Kind |
                                              Any_Unsigned_Int_x3_Kind | Any_Unsigned_Int_x4_Kind                 => True, others => False)),
  Key             => (Allow_Arrays => True,  (Float32_Kind | Float32_x3_Kind | Float32_x4_Kind | Float32_x16_Kind => True, others => False)),
  MaterialRef     => (Allow_Arrays => False, (Ref_Kind                                                            => True, others => False)),
  Metric          => (Allow_Arrays => False, (String_Kind  | Float32_Kind                                         => True, others => False)),
  MorphWeight     => (Allow_Arrays => False, (Float32_Kind                                                        => True, others => False)),
  Name            => (Allow_Arrays => False, (String_Kind                                                         => True, others => False)),
  ObjectRef       => (Allow_Arrays => False, (Ref_Kind                                                            => True, others => False)),
  Param           => (Allow_Arrays => False, (Float32_Kind                                                        => True, others => False)),
  Rotation        => (Allow_Arrays => False, (Float32_Kind | Float32_x4_Kind                                      => True, others => False)),
  Scale           => (Allow_Arrays => False, (Float32_Kind | Float32_x3_Kind                                      => True, others => False)),
  Texture         => (Allow_Arrays => False, (String_Kind                                                         => True, others => False)),
  Transform       => (Allow_Arrays => False, (Float32_x16_Kind                                                    => True, others => False)),
  Translation     => (Allow_Arrays => False, (Float32_Kind | Float32_x3_Kind                                      => True, others => False)),
  VertexArray     => (Allow_Arrays => True,  (Any_Float_Kind    | Any_Float_x2_Kind |
                                              Any_Float_x3_Kind | Any_Float_x4_Kind                               => True, others => False)), others => (Allow_Arrays => False, (others => False))),

  -------------------
  Structure_Data => (
  -------------------

  Morph           => (Name => Max_One,                                                                                                  others => None),
  BoneNode        => (Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More,                           others => None),
  Node            => (Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More,                           others => None),
  BoneRefArray    => (Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  LightNode       => (Name => Max_One, Node_Kind | Movement_Kind                             => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  GeometryNode    => (Name => Max_One. Node_Kind | Movement_Kind | MaterialRef | MorphWeight => Zero_Or_More, ObjectRef => Exactly_One, others => None),
  Clip            => (Name => Max_One,       Param                   => Zero_Or_More,                                                   others => None),
  Material        => (Name => Max_One,       Param | Color | Texture => Zero_Or_More,                                                   others => None),
  LightObject     => (Atten => Zero_Or_More, Param | Color | Texture => Max_One,                                                        others => None),
  CameraObject    => (Param => Zero_Or_More,                                                                                            others => None),
  Atten           => (Param => Zero_Or_More,                                                                                            others => None),
  Time            => (Key   => One_To_Three,                                                                                            others => None),
  Value           => (Key   => One_To_Four,                                                                                             others => None),
  Skeleton        => (Transform | BoneRefArray => Exactly_One,                                                                          others => None),
  Skin            => (Transform     => Zero_Or_One, Skeleton | BoneCountArray | BoneIndexArray | BoneWeightArray => Exactly_One,        others => None),
  Extension       => (Extension     => Zero_Or_More,                                                                                    others => None),
  Texture         => (Movement_Kind => Zero_Or_More,                                                                                    others => None),
  Animation       => (Track         => One_Or_More,                                                                                     others => None),
  Track           => (Time | Value  => Exactly_One,                                                                                     others => None),
  GeometryObject  => (Mesh          => One_Or_More, Morph      => Zero_Or_More,                                                         others => None),
  Mesh            => (VertexArray   => One_Or_More, IndexArray => Zero_Or_More, Skin => Max_One,                                        others => None), others => (others => None)),

  ---------------
  Properties => (
  ---------------

  Animation      => ((Name => P ("clip"),        Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),
                     (Name => P ("begin"),       Kind => Float32_Kind,        Defaulted => False,  Ref_Override => False),
                     (Name => P ("end"),         Kind => Float32_Kind,        Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  Atten          => ((Name => P ("kind"),        Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("distance"))),
                     (Name => P ("curve"),       Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("linear"))),     others => NULL_PROPERTY),
  Clip           => ((Name => P ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY),
  Color          => ((Name => P ("attrib"),      Kind => String_Kind,         Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  Extension      => ((Name => P ("applic"),      Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => NULL_STR_UNBOUND)),
                     (Name => P ("type"),        Kind => String_Kind,         Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  GeometryNode   => ((Name => P ("visible"),     Kind => Bool_Kind,           Defaulted => False,  Ref_Override => True),
                     (Name => P ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Ref_Override => True),
                     (Name => P ("motion blur"), Kind => Bool_Kind,           Defaulted => False,  Ref_Override => True),                                     others => NULL_PROPERTY),
  GeometryObject => ((Name => P ("visible"),     Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => True)),
                     (Name => P ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => True)),
                     (Name => P ("motion blur"), Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => True)),             others => NULL_PROPERTY),
  IndexArray     => ((Name => P ("material"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),
                     (Name => P ("restart"),     Kind => Unsigned_Int64_Kind, Defaulted => False,  Ref_Override => False),
                     (Name => P ("front"),       Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("ccw"))),        others => NULL_PROPERTY),
  Key            => ((Name => P ("kind"),        Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("value"))),      others => NULL_PROPERTY),
  LightNode      => ((Name => P ("shadow"),      Kind => Bool_Kind,           Defaulted => False,  Ref_Override => True),                                     others => NULL_PROPERTY),
  LightObject    => ((Name => P ("type"),        Kind => String_Kind,         Defaulted => False,  Ref_Override => False),
                     (Name => P ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => True)),             others => NULL_PROPERTY),
  Material       => ((Name => P ("shadow"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => True)),             others => NULL_PROPERTY),
  MaterialRef    => ((Name => P ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY),
  Mesh           => ((Name => P ("lod"),         Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY),
                     (Name => P ("primitive"),   Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("triangles"))),  others => NULL_PROPERTY),
  Metric         => ((Name => P ("key"),         Kind => String_Kind,         Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  Morph          => ((Name => P ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),
                     (Name => P ("base"),        Kind => Unsigned_Int32_Kind, Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  MorphWeight    => ((Name => P ("index"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY),
  Param          => ((Name => P ("attrib"),      Kind => String_Kind,         Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  Rotation       => ((Name => P ("kind"),        Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("axis"))),
                     (Name => P ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => False)),            others => NULL_PROPERTY),
  Scale          => ((Name => P ("kind"),        Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("xyz"))),
                     (Name => P ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => False)),            others => NULL_PROPERTY),
  Texture        => ((Name => P ("attrib"),      Kind => String_Kind,         Defaulted => False,  Ref_Override => False),
                     (Name => P ("texcoord"),    Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY),
  Time           => ((Name => P ("curve"),       Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("linear"))),     others => NULL_PROPERTY),
  Track          => ((Name => P ("target")),     Kind => Ref_Kind,            Defaulted => False,  Ref_Override => False),                                    others => NULL_PROPERTY),
  Transform      => ((Name => P ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (String_Val         => U ("object"))),     others => NULL_PROPERTY),
  Translation    => ((Name => P ("kind"),        Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("xyz"))),
                     (Name => P ("object"),      Kind => Bool_Kind,           Defaulted => True,   Default      => (Bool_Val           => False)),            others => NULL_PROPERTY),
  Value          => ((Name => P ("curve"),       Kind => String_Kind,         Defaulted => True,   Default      => (String_Val         => U ("linear"))),     others => NULL_PROPERTY),
  VertexArray    => ((Name => P ("attrib"),      Kind => String_Kind,         Defaulted => False,  Ref_Override => False),
                     (Name => P ("morph"),       Kind => Unsigned_Int32_Kind, Defaulted => True,   Default      => (Unsigned_Int32_Val => 0)),                others => NULL_PROPERTY), others => (others => NULL_PROPERTY)),

  -------------------------------
  -- Top-Level Data Structures --
  -------------------------------

  type Metrics_State is record
    end record;

  type Camera_State is record
    end record;

  type Skeleton_State is record -- Synthetic
    end record;

  type Animation_Clip_State is record
--       Clip     : Int_Unsigned := 0;
--       Starting : Duration     := 0.0;
--       Ending   : Duration     := 0.0;
    end record;

  type Geometry_Object_State (Mesh_Count : Positive := 1; Morph_Count : Natural := 0) is record
--       Visible : Bool := True;
--       Shadow  : Bool := True;
--       Blur    : Bool := True;
--       Meshes  : Vector_Mesh_State.Unsafe_Array (1..Mesh_Count);
--       Morphs  : Vector_Morph_State.Unsafe_Array (1..Morph_Count);
    end record;

  type Light_Object_State is record
    end record;

  type Material_State is record
    end record;

  -----------
  -- Scene --
  -----------

  type Node_State is record

--       Name        : Str_8 (1..128) := (others => ' ');
--       Transform   : Transform_4D := ZERO_TRANSFORM_4D;
--       Translation :
--       Rotation    :
--       Scale       :
--       Animation   :
      case Kind is
        when Light_Node =>

--   --     @"Name""Name"
--   --     @"ObjectRef""ObjectRef"
--   --     @"Transform""Transform"
--   --     ""point"""point"
--   --     ""infinite"""infinite"

    end record;

--   ----------
--   --  --
--   ----------

--   type Geometry_State (Mesh_Count : Positive := 1; Morph_Count : Natural := 0) is record
--       Visible : Bool := True;
--       Shadow  : Bool := True;
--       Blur    : Bool := True;
--       Meshes  : Vector_Mesh_State.Unsafe_Array (1..Mesh_Count);
--       Morphs  : Vector_Morph_State.Unsafe_Array (1..Morph_Count);
--     end record;

--   type Material is record
--     end record;

--   type Camera is record
--     end record;

--   type Clip is record
--     end record;

--   type Light is record
--     end record;

--   ----------
--   --  --
--   ----------

--   type Track is record
--       At_Time : Duration := 0.0;
--       Value   :
--     end record;

--   type Animation is record
--     end record;

--   type Clip is record

--   type Atten

--   type Bone is record
--     end record;

--   type Camera is
--   --     @"Name""Name"
--   --     @"ObjectRef""ObjectRef"
--   --     @"Transform""Transform"
--     end record

--   type Geometry is record
--   --     @"Name""Name"
--   --     @"ObjectRef""ObjectRef"
--   --     @"MaterialRef""MaterialRef"
--   --     @"Transform""Transform"
--     end record;

--   type Light_State (Kind : Light_Kind := Point_Light; Casts_Shadows : Bool := False) is record
--       Color : Color_State := COLOR_WHITE;
--       Intensity : Real_Percent := 100.0;

--   --     @"Name""Name"
--   --     @"ObjectRef""ObjectRef"
--   --     @"Transform""Transform"
--   --     ""point"""point"
--   --     ""infinite"""infinite"
--     end record;

--   type Mesh is record

--   type Skeleton is record

--   type Skin is record

--   type Texture is record

--   type Node is record
--       Name        : Str_8 (1..128) := (others => ' ');
--       Transform   : Transform_4D := ZERO_TRANSFORM_4D;
--       Translation :
--       Rotation    :
--       Scale       :
--       Animation   :
--       case Kind is
--         when Bode_Node =>
--         when Geometry_Node =>

--     end record;

--   --------
--   -- IO --
--   --------

--   procedure Load (Path : Str; Data : in out Engine_Exchange_Format.Tree);
--   procedure Load (Path : Str;
--   procedure Load (Data : Engine_Exchange_Format.Tree;
--   procedure Load_Map       (Path : Str);
--   procedure Load_Camera    (Path : Str);
--   procedure Load_Animation (Path : Str);
--   procedure Load_Mesh      (Path : Str);
--   procedure Load_Material  (Path : Str);
-- end;












--   -------------
--   -- Formats --
--   -------------

--   type Format_Kind is (Doom3_Format,      -- web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling
--                        Wavefront_Format); -- web.archive.org/web/20160810123453/https://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf

--   ------------
--   -- Camera --
--   ------------

--   type Camera_Frame_State is record
--       FOV         : Real          := 0.0;
--       Point       : Point_3D      := ZERO_POINT_3D;
--       Orientation : Quaternion_4D := ZERO_QUATERNION_4D;
--     end record;
--   package Vector_Camera_Frame is new Neo.Core.Vectors (Camera_Frame_State);

--   type Camera_State is record
--       Frame_Rate : Positive := 1;
--       Frames     : Vector_Camera_Frame.Unsafe.Vector;
--       Cuts       : Vector_Positive.Unsafe.Vector;
--     end record;
--   package Hashed_Camera is new Neo.Core.Hashed (Camera_State);

--   ---------------
--   -- Animation --
--   ---------------

--   type Bounding_State is record
--       A, B : Point_3D := ZERO_POINT_3D;
--     end record;

--   type Joint_State is record
--       Name         : Str_8 (1..32) := (others => NULL_CHAR_8);
--       Parent_Index : Int_64        := -2;
--       Point        : Point_3D      := ZERO_POINT_3D;
--       Orientation  : Quaternion_4D := ZERO_QUATERNION_4D;
--     end record;
--   package Treed_Joint  is new Neo.Core.Trees   (Joint_State);
--   package Vector_Joint is new Neo.Core.Vectors (Joint_State);

--   type Animation_Frame_State is record
--       Bounding : Bounding_State := (others => <>);
--       Joints   : Vector_Joint.Unsafe.Vector;
--     end record;
--   package Vector_Animation_Frame is new Neo.Core.Vectors (Animation_Frame_State);

--   type Animation_State is record
--       Frame_Rate : Positive := 1;
--       Frames     : Vector_Animation_Frame.Unsafe.Vector;
--     end record;
--   package Hashed_Animation is new Neo.Core.Hashed (Animation_State);

--   ----------
--   -- Mesh --
--   ----------

--   type Weight_State is record
--       Joint_Index : Natural      := 0; -- Could have used a tree cursor instead of an joint index but that would be ineffecient
--       Amount      : Real_Percent := 0.0;
--       Point       : Point_3D     := ZERO_POINT_3D;
--     end record;
--   package Vector_Weight is new Neo.Core.Vectors (Weight_State);

--   type Animated_Vertex_State is record
--       Texture      : Point_2D        := ZERO_POINT_2D;
--       Weight_Count : Int_64_Unsigned := 1;
--       Start_Weight : Int_64_Unsigned := 1;
--       Velocity     : Real_64         := 0.0;
--     end record with Convention => C;
--   package Vector_Animated_Vertex is new Neo.Core.Vectors (Animated_Vertex_State);

--   type Static_Vertex_State is record
--       Texture  : Point_2D := ZERO_POINT_2D;
--       Point    : Point_3D := ZERO_POINT_3D;
--       Normal   : Point_3D := ZERO_POINT_3D;
--       Velocity : Real_64  := 0.0;
--     end record with Convention => C;
--   package Vector_Static_Vertex is new Neo.Core.Vectors (Static_Vertex_State);

--   type Animated_Surface_State is record
--       Material : Str_Unbound := NULL_STR_UNBOUND;
--       Vertices : Vector_Animated_Vertex.Unsafe.Vector; -- Starts at 0
--       Indicies : Vector_Int_32_Unsigned.Unsafe.Vector;
--       Weights  : Vector_Weight.Unsafe.Vector; -- Seporated out of animated vertices for GPU skinning
--     end record;
--   package Vector_Animated_Surface is new Neo.Core.Vectors (Animated_Surface_State);

--   type Static_Surface_State is record
--       Material : Str_Unbound := NULL_STR_UNBOUND;
--       Vertices : Vector_Static_Vertex.Unsafe.Vector; -- Starts at 0
--       Indicies : Vector_Int_32_Unsigned.Unsafe.Vector;
--       Bounding : Bounding_State;
--     end record;
--   package Vector_Static_Surface is new Neo.Core.Vectors (Static_Surface_State);

--   type Mesh_State (Is_Animated : Bool := False) is record
--       case Is_Animated is
--         when False => Static_Surfaces : Vector_Static_Surface.Unsafe.Vector;
--         when True =>
--           Animated_Surfaces : Vector_Animated_Surface.Unsafe.Vector;
--           Animations        : Vector_Str_Unbound.Unsafe.Vector;
--           Base_Frame        : Animation_Frame_State := (others => <>);
--       end case;
--     end record;
--   package Vector_Mesh is new Neo.Core.Vectors (Mesh_State);
--   package Hashed_Mesh is new Neo.Core.Hashed (Mesh_State);

--   ---------
--   -- Map --
--   ---------

--   type Partition_Kind is (Normal_Partition, Area_Partition, Opaque_Partition);

--   type Partition_Node_State (Kind : Partition_Kind := Normal_Partition) is record
--       Is_Positive : Bool := True;
--       case Kind is
--         when Area_Partition   => Area_Id : Int      := 0;
--         when Normal_Partition => Plane   : Plane_4D := (others => <>);
--       when others => null; end case;
--     end record;
--   package Treed_Partition_Node is new Neo.Core.Trees (Partition_Node_State);

--   type Brush_Side_State is record
--       Material : Str_Unbound  := NULL_STR_UNBOUND;
--       Texture  : Transform_3D := (others => <>);
--       Origin   : Point_3D     := (others => <>);
--       Plane    : Plane_4D     := (others => <>);
--     end record;
--   package Vector_Brush_Side is new Neo.Core.Vectors (Brush_Side_State);
--   package Vector_Brush      is new Neo.Core.Vectors (Vector_Brush_Side.Unsafe.Vector);

--   ---------------------
--   -- Collision Model --
--   ---------------------

--   type Clip_State is record
--       Sides         : Vector_Plane_4D.Unsafe.Vector;
--       Bounding      : Bounding_State := (others => <>);
--       Is_Player     : Bool           := False;
--       Is_Opaque     : Bool           := False;
--       Is_Water      : Bool           := False;
--       Is_Solid      : Bool           := False;
--       Is_Monster    : Bool           := False;
--       Is_Moveable   : Bool           := False;
--       Is_Bot        : Bool           := False;
--       Is_Blood      : Bool           := False;
--       Is_Trigger    : Bool           := False;
--       Is_Body       : Bool           := False;
--       Is_Flashlight : Bool           := False;
--       Is_Corpse     : Bool           := False;
--       Is_Animation  : Bool           := False;
--       Is_Obstacle   : Bool           := False;
--     end record;
--   package Vector_Clip is new Neo.Core.Vectors (Clip_State);

--   type Edge_State is record
--       A, B      : Point_3D := (others => <>);
--       Internal  : Natural  := 0; -- ???
--       Num_Users : Natural  := 0;
--     end record;
--   package Vector_Edge is new Neo.Core.Vectors (Edge_State);

--   type Polygon_State is record
--       Material : Str_Unbound    := NULL_STR_UNBOUND;
--       Bounding : Bounding_State := (others => <>);
--       Plane    : Plane_4D       := (others => <>);
--       Edges    : Vector_Int.Unsafe.Vector;
--     end record;
--   package Vector_Polygon is new Neo.Core.Vectors (Polygon_State);

--   type Collision_Node_State is record
--       Is_Back   : Bool           := False;
--       Distance  : Real           := 0.0;
--       Dimension : Dimension_Kind := X_Dimension;
--     end record;
--   package Treed_Collision_Node is new Neo.Core.Trees (Collision_Node_State);

--   type Collision_State is record
--       Name     : Str_Unbound := NULL_STR_UNBOUND;
--       Polygons : Vector_Polygon.Unsafe.Vector;
--       Clipping : Vector_Clip.Unsafe.Vector;
--       Edges    : Vector_Edge.Unsafe.Vector;
--       Nodes    : Treed_Collision_Node.Unsafe.Tree;
--     end record;
--   package Vector_Collision is new Neo.Core.Vectors (Collision_State);

--   --------------------
--   -- Area Awareness --
--   --------------------

--   -- type Area_Kind is (Ladder_Area, Floor_Area, Liquid_Area);
--   --
--   -- type Area_Side_State is record
--   --     Kind  : Area_Kind;
--   --     Plane : Plane_3D;
--   --   end record;
--   -- package Vector_Area is new Neo.Core.Vectors (Area_Side_State);
--   --
--   -- type Area_State is record
--   --     Bounding          : Bounding_State;
--   --     Sides             : Vector_Area.Unsafe_Vector;
--   --     Center            : Point_3D;
--   --     Is_Ledge          : Bool;
--   --     Is_Liquid         : Bool;
--   --     Is_Walk_Reachable : Bool;
--   --     Is_Fly_Reachable  : Bool;
--   --     Contains_Water    : Bool;
--   --     Contains_Portal   : Bool;
--   --     Contains_Obstacle : Bool;
--   --     Can_Walk          : Bool;
--   --     Can_Crouch        : Bool;
--   --     Can_Ledge_Walk    : Bool;
--   --     Can_Barrier_Jump  : Bool;
--   --     Can_Jump          : Bool;
--   --     Can_Fly           : Bool;
--   --     Can_Go_By_Special : Bool;
--   --     Can_Go_By_Water   : Bool;
--   --     Can_Go_By_Air     : Bool;
--   --   end record;
--   -- package Vector_Area is new Neo.Core.Vectors (Area_State);
--   --
--   -- type Area_Cluster_State is record
--   --   end record;
--   --
--   -- type AI_State is record
--   --     Bounding        : Bounding_State;
--   --     Gravity         : Vector_3D;
--   --     Max_Step_Height : Real;
--   --     Min_Floor_Cos   : Real;
--   --     Clusters        : Vector_Cluster.Unsafe.Vector;
--   --   end record;

--   type Map_Entity_State is record
--       Point       : Point_3D      := (others => <>);
--       Orientation : Quaternion_4D := (others => <>);
--       Key_Values  : Hashed_Str_Unbound.Unsafe.Map;
--       Patches     : Vector_Static_Surface.Unsafe.Vector;
--       Targets     : Vector_Str_Unbound.Unsafe.Vector;
--       Brushes     : Vector_Brush.Unsafe.Vector;
--     end record;
--   package Hashed_Entity is new Neo.Core.Hashed (Map_Entity_State);

--   ---------
--   -- Map --
--   ---------

--   type Map_State is record
--       Geometry_CRC : Int_64_Unsigned := 0;
--       Collisions   : Vector_Collision.Unsafe.Vector;
--       Entities     : Hashed_Entity.Unsafe.Map;
--       Partitions   : Treed_Partition_Node.Unsafe.Tree;
--       -- AI : AI_State;
--     end record;
--   package Hashed_Map is new Neo.Core.Hashed (Map_State); --  LOL "Hashed map"

--   -------------
--   -- Surface --
--   -------------
--   --
--   -- A Surface defines how materials interact with all of the other portions of a game engine including footsteps and grenade and bullet
--   -- damage - what decals are applied at points of impact and what sounds are generated. In addition, physical properties like friction
--   -- and sound reverb are included.
--   --
--   -- Each property of a surface can take any number of variations (thus the use of a vector instead of an array).
--   --

--   type Surface_Kind is (Flesh_Surface, Nasty_Flesh_Surface, Thin_Wood_Surface,    Dense_Wood_Surface,    Liquid_Surface, Slime_Surface,
--                         Paper_Surface, Cardboard_Surface,   Thin_Rock_Surface,    Dense_Rock_Surface,    Gravel_Surface, Asphalt_Surface,
--                         Brick_Surface, Dirt_Surface,        Grass_Surface,        Foliage_Surface,       Sand_Surface,   Snow_Surface,
--                         Ice_Surface,   Mud_Surface,         Thin_Metal_Surface,   Thick_Metal_Surface,   Chain_Surface,  Grenade_Surface,
--                         Can_Surface,   Weapon_Surface,      Thin_Plastic_Surface, Thick_Plastic_Surface, Glass_Surface,  Rubber_Surface,
--                         Tile_Surface,  Carpet_Surface);

--   type Surface_State is record
--       Mass_Per_Cubic_Unit   : Real_Percent := 0.0;
--       Friction              : Real_Percent := 0.0;
--       Restitution           : Real_Percent := 0.0;
--       Noise_Reduction       : Real_Percent := 0.0; -- www.archtoolbox.com/representation/architectural-concepts/acoustics.html
--       Physics_Hit_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
--       Bullet_Hole_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
--       Knife_Slash_Emitter   : Str_Unbound  := NULL_STR_UNBOUND;
--       Walk_Light_Emitter    : Str_Unbound  := NULL_STR_UNBOUND;
--       Explosion_Emitter     : Str_Unbound  := NULL_STR_UNBOUND;
--       Walk_Hard_Emitter     : Str_Unbound  := NULL_STR_UNBOUND;
--       Explosion_Materials   : Vector_Str_Unbound.Unsafe.Vector;
--       Bullet_Hole_Materials : Vector_Str_Unbound.Unsafe.Vector;
--       Knife_Slash_Materials : Vector_Str_Unbound.Unsafe.Vector;
--       Land_Light_Materials  : Vector_Str_Unbound.Unsafe.Vector;
--       Land_Hard_Materials   : Vector_Str_Unbound.Unsafe.Vector;
--       Footstep_Materials    : Vector_Str_Unbound.Unsafe.Vector; -- Scale to feet of model ???
--     end record;

--   -- Returns constant Surface_State for a given Surface_Kind. All surfaces are defined in the body of this package (neo-model.adb).
--   -- function Get_Surface (Kind : Surface_Kind) return Surface_State;

--   --------------
--   -- Material --
--   --------------
--   --
--   -- A Material or "shader" is a grouping of texture maps and settings that define what a given entity, decal, or map element will look
--   -- like and how it might be affected by light or camera position.
--   --
--   -- This implementation of materials offloads all of the work into separate shader programs so no logic has to be contained within the
--   -- actual material record.
--   --
--   -- References.
--   --   web.archive.org/web/20150303182930/https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/
--   --   web.archive.org/web/20160305174701/https://www.iddevnet.com/doom3/materials.php
--   --

--   type Filter_Kind  is (Linear_Filter,  Nearest_Filter);
--   type Clamp_Kind   is (No_Clamp,       Zero_Clamp,      Normal_Clamp,          Zero_Alpha_Clamp);
--   type Cube_Kind    is (No_Cube,        Image_Cube,      Camera_Cube,           Mirror_Cube,       Skybox_Cube);
--   type Domain_Kind  is (Surface_Domain, Decal_Domain,    Post_Process_Domain,   Light_Domain,      Menu_Domain,   No_Domain);
--   type Blend_Kind   is (Opaque_Blend,   Masked_Blend,    Additive_Blend,        Modulate_Blend,    Mirror_Blend,  Remote_Blend);
--   type Shading_Kind is (Unlit_Shading,  Lit_Shading,     Subsurface_Shading,    Profile_Shading,   Skin_Shading,  Clear_Coat_Shading);
--   type Deform_Kind  is (No_Deform,      Sprite_Deform,   Tube_Deform,           Flare_Deform,      Expand_Deform, Move_Deform,
--                         Eye_Deform,     Particle_Deform, Small_Particle_Deform, Turbulent_Deform);
--   type Sort_Kind    is (Subview_Sort,   GUI_Sort,        Opaque_Sort,           Sky_Sort,          Decal_Sort,    Far_Sort,
--                         Medium_Sort,    Close_Sort,      Almost_Near_Sort,      Nearest_Sort,      Post_Sort);

--   type Material_State (Domain : Domain_Kind := Surface_Domain) is record
--       Has_Smoothed_Tan : Bool            := True;
--       Is_Two_Sided     : Bool            := False;
--       Is_Ambient       : Bool            := False;
--       Is_Opaque        : Bool            := False;
--       Sort             : Sort_Kind       := Opaque_Sort;
--       Clamp            : Clamp_Kind      := No_Clamp;
--       Filter           : Filter_Kind     := Linear_Filter;
--       Surface          : Surface_kind    := Thin_Rock_Surface;
--       Deform           : Deform_Kind     := No_Deform;
--       Color_Mod        : Color_State     := NULL_COLOR;
--       Transform        : Transform_4D    := (1.0, 0.0, 0.0, 0.0,
--                                              0.0, 1.0, 0.0, 0.0,
--                                              0.0, 0.0, 0.0, 0.0); -- ???
--       case Domain is
--         when No_Domain => null;
--         when Menu_Domain => Menu_Id : Str_Unbound := NULL_STR_UNBOUND;
--         when others =>
--           Base_Color : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
--           case Domain is
--             when Light_Domain => null;
--             when others =>
--               -- Blend                : Blend_Kind    := Opaque_Blend;
--               Cube                 : Cube_Kind   := No_Cube; -- Cube map textures assume _px, _py, _pz, _nx, _ny, _nz name convention
--               Prefilter            : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
--               Irradiance           : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
--               Specular             : Str_Unbound := NULL_STR_UNBOUND; -- RGBA
--               Normal               : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
--               Displacement         : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
--               Metallic             : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
--               Roughness            : Str_Unbound := NULL_STR_UNBOUND; -- Single channel
--               -- brdfLUT     : Str_Unbound   := NULL_STR_UNBOUND; -- RGB
--               -- albedoMap           : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
--               -- Ambient_Occlusion    : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
--               -- Refraction           : Str_Unbound   := NULL_STR_UNBOUND; -- Single channel
--           end case;
--       end case;
--     end record;
--   package Hashed_Material is new Neo.Core.Hashed (Material_State);

--   -----------
--   -- Cache --
--   -----------

--   Maps       : Hashed_Map.Safe_Map;
--   Meshes     : Hashed_Mesh.Safe_Map;
--   Cameras    : Hashed_Camera.Safe_Map;
--   Materials  : Hashed_Material.Safe_Map;
--   Animations : Hashed_Animation.Safe_Map;

--   --------
--   -- IO --
--   --------

--   procedure Load_Map       (Path : Str);
--   procedure Load_Camera    (Path : Str);
--   procedure Load_Animation (Path : Str);
--   procedure Load_Mesh      (Path : Str);
--   procedure Load_Material  (Path : Str);

--   --procedure Save_Map       (Path : Str);
--   --procedure Save_Camera    (Path : Str);
--   --procedure Save_Animation (Path : Str);
--   --procedure Save_Mesh      (Path : Str);
--   --procedure Save_Material  (Path : Str);
-- end;
