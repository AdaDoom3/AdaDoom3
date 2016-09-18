
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

with Neo.Math; use Neo.Math;
package Neo.Model is

  ------------
  -- Format --
  ------------

  type Format_Kind is(
    Wavefront_Format,
    -- 1988 Wavefront Technologies
    -- http://web.archive.org/web/20160810123453/https://www.cs.utah.edu/~boulos/cs3505/obj_spec.pdf
    Id_Technology_Format);
    -- 1991 Id Software
    -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling

  ------------
  -- Camera --
  ------------

  type Frame_State is record
      Coordinate    : Coordinate_3D_State;
      Orientation   : Vector_Quaternion.Unsafe.Vector;
      Field_Of_View : Real_32;
    end record;
  package Vector_Frame is new Vectors (Frame_State);
  type Camera_State is record
      Frame_Rate : Int_32_Positive;
      Frames     : Vector_Frame.Unsafe.Vector;
      Cuts       : Vector_Int_32_Positive.Unsafe.Vector;
    end record;
  package Hashed_Camera is new Hashed_Maps (Camera_State);

  ---------------
  -- Animation --
  ---------------

  type Bounds_Array is array (1..2) of Coordinate_3D_State;
  package Vector_Quaternion is new Vectors (Quaternion_State);
  type Joint_State is record
      Name        : Str_16_Unbounded;
      Coordinate  : Coordinate_3D_State;
      Orientation : Vector_Quaternion.Unsafe.Vector;
    end record;
  package Treed_Joint is new Trees (Joint_State);
  type Frame_State is record
      Skeleton : Treed_Joint.Unsafe.Tree;
      Bounds   : Bounds_Array;
    end record;
  package Vector_Treed_Joint is new Vectors (Treed_Joint.Unsafe.Tree);
  type Animation_State is record
      Frame_Rate : Int_32_Positive;
      Frames     : Vector_Treed_Joint.Unsafe.Vector;
    end record;
  package Hashed_Animation is new Hashed_Maps (Animation_State);
  type Weight_State is record
      Joint    : Str_2_Unbound;
      Position : Int_32_Natural; 
      Amount   : Real_32_Percent;
    end record;
  package Vector_Weight is new Vectors (Weight_State);

  ----------
  -- Mesh --
  ----------

  type Vertex_State (Animated : Bool := False) is record
      Texture  : Coordinate_2D_State;
      Position : Coordinate_3D_State;
      --case Is_Animated is
        --when True  =>
        --Weights : Vector_Record_Mesh_Weight.Unsafe.Vector;
        --when False => Position : 
      --end case;
    end record;
  package Vector_Vertex         is new Vectors (Vertex_State);
  package Ordered_Vertex        is new Ordered_Maps (Int_32_Natural, Vector_Vertex.Unsafe.Vector);
  package Vector_Ordered_Vertex is new Vectors (Ordered_Vertex.Unsafe.Map);
  type Group_State is record
      Material : Str_16_Unbound;
      Shapes   : Vector_Ordered_Vertex.Unsafe.Vector;      
    end record;
  package Hashed_Group is new Hashed_Maps (Group_State);
  type Mesh_State (Animated : Bool := True; Bounded : Bool := True) is record
      Groups : Hashed_Group.Unsafe.Hashed_Map;
      case Animated is
        when True =>
          Skeleton   : Treed_Joint.Unsafe.Tree;
          Animations : Vector_Str_16_Unbound.Unsafe.Vector;
      when False => null; end case;
      case Bounded is
        when True =>
          Bounds : Bounds_Array;
      when False => null; end case;
    end record;
  package Hashed_Mesh is new Hashed_Maps (Mesh_State);

  -----------
  -- Level --
  -----------

  type Partition_Kind is (X_Partition,   Y_Partition,    Z_Partition);
  type Clip_Kind      is (No_Clip,       Player_Clip,    Opaque_Clip, Water_Clip,   Solid_Clip, Monster_Clip,
                          Moveable_Clip, Bot_Clip,       Blood_Clip,  Trigger_Clip, Body_Clip,  Flashlight_Clip,
                          Corpse_Clip,   Animation_Clip, Obstacle_Clip);
  type Edge_State is record
      Start : Int_32_Natural;
      End   : Int_32_Natural;
    end record;
  package Vector_Edge is new Vectors (Edge_State);
  type Side_State is record
      Clip  : Clip_Kind;
      Plane : Real_Plane_3D;
    end record;
  package Vector_Side is new Vectors (Side_State);
  type Brush_State is record
      Bounds : Brush_State;
      Sides  : Vector_Side.Unsafe.Vector;
    end record;
  package Vector_Brush        is Vectors (Brush_State);
  package Vector_Vector_Brush is Vectors (Vector_Brush.Unsafe.Vector);
  type Surface_State is record
      Material : Str_16_Unbound;
      Bounds   : Bounds_Array;
      Plane    : Plane_3D_State;
      Edges    : Vector_Int_32_Natural.Unsafe.Vector;
    end record;
  package Vector_Surface        is new Vectors (Surface_State);
  package Vector_Vector_Surface is new Vectors (Vector_Surface.Unsafe.Vector);
  type Patch_State is record
      Edge_Verticies : Edge_State;
      Edge_Triangles : Edge_State;
      Mesh           : Mesh_State (Animated => False, Bounded => False);
      Subdivisions_X : Int_32_Natural;
      Subdivisions_Y : Int_32_Natural;
      Width          : Int_32_Natural;
      Height         : Int_32_Natural;
      Maximum_Width  : Int_32_Natural;
      Maximum_Height : Int_32_Natural;
    end record;
  package Vector_Patch is new Vectors (Patch_State);
  type Brush_State is record
      Material : Str_16_Unbound;
      Texture  : Array_2_Record_Coordinate_3D; -- !!!
      Origin   : Coordinate_3D_State;
      Plane    : Plane_3D_State;
    end record;
  package Vector_Brush        is new Vectors (Brush_State);
  package Vector_Vector_Brush is new Vectors (Vector_Brush.Unsafe.Vector);
  type Collision_Section_State is record
      Partition : Partition_Kind;
      Distance  : Int_32_Natural;
    end record;
  package Treed_Collision_Section is new Trees (Collision_Section_State);
  package Treed_Plane_3D          is new Trees (Plane_3D_State);
  type Level_State is record
      Geometry_CRC       : Int_32_Unsigned := 0;
      Edge_Verticies     : Vector_Edge.Unsafe.Vector;
      Patches            : Vector_Patch.Unsafe.Vector;
      Vertices           : Vector_Record_Coordinate_3D.Unsafe.Vector;
      Brushes            : Vector_Vector_Brush.Unsafe.Vector;
      Entities           : Vector_Hashed_Str_16_Unbound.Unsafe.Vector;
      Brush_Blocks       : Vector_Vector_Brush.Unsafe.Vector;
      Surface_Blocks     : Vector_Vector_Surface.Unsafe.Vector;
      Surface_Sections   : Treed_Plane.Unsafe.Vector;
      Collision_Sections : Treed_Collision_Section.Unsafe.Vector;
    end record;
  package Hashed_Level_State is new Hashed_Maps (Level_State);

  --------
  -- IO --
  --------

  function Load  (Name : Str_16) return Animation_State;
  function Load  (Name : Str_16) return Camera_State;
  function Load  (Name : Str_16) return Mesh_State;
  function Load  (Name : Str_16) return Level_State;
  procedure Save (Name : Str_16; Item : Animation_State);
  procedure Save (Name : Str_16; Item : Camera_State);
  procedure Save (Name : Str_16; Item : Mesh_State);
  procedure Save (Name : Str_16; Item : Level_State);
end;
