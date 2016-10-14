
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
    Id_Technology_Format);
    -- 1991 Id Software
    -- http://web.archive.org/web/20121018035741/http://www.modwiki.net/wiki/Modelling

  ------------
  -- Camera --
  ------------

  -- Camera animation frame
  type Frame_State is record
      Point         : Point_3D;
      Orientation   : Quaternion_4D;
      Field_Of_View : Real;
    end record;
  package Vector_Frame is new Vectors (Frame_State);

  -- Camera animation sequence
  type Camera_State is record
      Frame_Rate : Int_32_Positive;
      Frames     : Vector_Frame.Unsafe.Vector;
      Cuts       : Vector_Int_32_Positive.Unsafe.Vector;
    end record;
  package Hashed_Camera is new Hashed_Maps (Camera_State);

  ---------------
  -- Animation --
  ---------------

  type Bounds_Array is array (1..2) of Point_3D;

  -- Skeleton joint
  type Joint_State is record
      Name        : Str;
      Point       : Point_3D;
      Orientation : Quaternion_4D;
    end record;
  package Treed_Joint is new Trees (Joint_State);

  -- Skeleton with pre-computed bounds
  type Frame_State is record
      Skeleton : Treed_Joint.Unsafe.Tree;
      Bounds   : Bounds_Array;
    end record;
  package Vector_Frame is new Vectors (Treed_Joint.Unsafe.Tree);
 
  -- Animation
  type Animation_State is record
      Frame_Rate : Int_32_Positive;
      Frames     : Vector_Treed_Joint.Unsafe.Vector;
    end record;
  package Hashed_Animation is new Hashed_Maps (Animation_State);

  ----------
  -- Mesh --
  ----------

  -- Vertex joint weight
  type Weight_State is record
      Joint  : Str;
      Point  : Point_3D; 
      Amount : Real_Percent;
    end record;
  package Vector_Weight is new Vectors (Weight_State);

  -- Vertex
  type Vertex_State (Animated : Bool := True) is record
      Texture : Point_2D;
      Point   : Point_3D;
      case Is_Animated is
        when True => Weights : Vector_Weight.Unsafe.Vector;
      when False => null; end case;
    end record;
  package Vector_Vertex        is new Vectors (Vector_State);
  package Vector_Vector_Vertex is new Vectors (Vector_Vertex.Unsafe.Vector);

  -- Vertex group
  type Group_State is record
      Material  : Str_Unbound;
      Triangles : Vector_Vector_Vertex.Unsafe_Vector;      
    end record;
  package Vector_Group is new Vectors (Group_State);

  -- Mesh
  type Mesh_State (Animated, Bounded : Bool := True) is record
      Groups : Vector_Group.Unsafe.Vector;
      case Animated is
        when True =>
          Skeleton   : Treed_Joint.Unsafe.Tree;
          Animations : Vector_Str.Unsafe.Vector;
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

  -- ???
  type Edge_State is record
      Start, End : Int_32_Natural;
    end record;
  package Vector_Edge is new Vectors (Edge_State);

  -- ???
  type Side_State is record
      Clip  : Clip_Kind;
      Plane : Real_Plane_3D;
    end record;
  package Vector_Side is new Vectors (Side_State);

  -- ???
  type Brush_State is record
      Bounds : Brush_State;
      Sides  : Vector_Side.Unsafe.Vector;
    end record;
  package Vector_Brush        is Vectors (Brush_State);
  package Vector_Vector_Brush is Vectors (Vector_Brush.Unsafe.Vector);

  -- ???
  type Surface_State is record
      Material : Str_Unbound;
      Bounds   : Bounds_Array;
      Plane    : Plane;
      Edges    : Vector_Int_32_Natural.Unsafe.Vector;
    end record;
  package Vector_Surface        is new Vectors (Surface_State);
  package Vector_Vector_Surface is new Vectors (Vector_Surface.Unsafe.Vector);

  -- Stucture representing
  type Patch_State is record
      Edge_Verticies : Vector_Edge.Unsafe.Vector;
      Edge_Triangles : Edge_State;
      Mesh           : Mesh_State (Animated => False);
      Subdivision_Y  : Int_32_Natural;
      Subdivision_X  : Int_32_Natural;
      Width          : Int_32_Natural;
      Height         : Int_32_Natural;
    end record;
  package Vector_Patch is new Vectors (Patch_State);

  -- ???
  type Brush_State is record
      Material : Str_Unbound;
      Texture  : Point_3D; -- !!!
      Origin   : Point_3D;
      Plane    : Plane;
    end record;
  package Vector_Brush        is new Vectors (Brush_State);
  package Vector_Vector_Brush is new Vectors (Vector_Brush.Unsafe.Vector);

  -- ???
  type Collision_Section_State is record
      Partition : Partition_Kind;
      Distance  : Int_32_Natural;
    end record;
  package Treed_Collision_Section is new Trees (Collision_Section_State);
  package Treed_Plane_3D          is new Trees (Plane_3D_State);

  -- ???
  type Level_State is record
      Geometry_CRC       : Int_32_Unsigned := 0;
      Edge_Verticies     : Vector_Edge.Unsafe.Vector;
      Patches            : Vector_Patch.Unsafe.Vector;
      Brushes            : Vector_Vector_Brush.Unsafe.Vector;
      Brush_Blocks       : Vector_Vector_Brush.Unsafe.Vector;
      Entities           : Vector_Hashed_Str_Unbound.Unsafe.Vector;
      Collision_Sections : Treed_Collision_Section.Unsafe.Vector;
      Surface_Blocks     : Vector_Vector_Surface.Unsafe.Vector;
      Surface_Sections   : Treed_Plane.Unsafe.Vector;
    end record;
  package Hashed_Level_State is new Hashed_Maps (Level_State);

  --------
  -- IO --
  --------

  function Load (Path : Str) return Mesh_State;
  function Load (Path : Str) return Level_State;
  function Load (Path : Str) return Camera_State;
  function Load (Path : Str) return Animation_State;
end;
