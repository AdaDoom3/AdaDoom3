
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

-- Load Id Tech modling formats - they are almost all exclusvily plain text - very slow loading...
separate (Neo.Model) package body Id_Tech is

  ------------
  -- Common --
  ------------

  -- Internal structure that reflects how joints are layed-out by the md5mesh and md5anim data files
  type Internal_Joint_State is record
      External : Joint_State;
      Parent   : Int := -2;
    end record;
  package Vector_Internal_Joint is new Vectors (Internal_Joint_State);

  -- Bulding a skeleton from a vector of internal joint structures
  function Build_Skeleton (Joints : Vector_Internal_Joint.Unsafe.Vector) return Treed_Joint.Unsafe.Tree is
    Skeleton : Treed_Joint.Unsafe.Tree;
    begin
      for Joint of Joints loop
        if Joint.Parent = 0 then Skeleton.Append_Child (Skeleton.Root, Joint.External);
        else Skeleton.Append_Child (Skeleton.Find (Joints.Element (Joint.Parent).External), Joint.External); end if;
      end loop;
      return Skeleton;
    end;

  -- Helper for calculating a bounding box 
  function Build_Bounds (Triangles : Vector_Triangle.Unsafe_Vector) return Bounds_Array is
    begin
      return ;
    end;

  ------------
  -- Camera --
  ------------

  function Load (Path : Str) return Mesh_State is

    -- Load an md5camera: https://modwiki.xnet.fi/MD5CAMERA_%28file_format%29
    package Camera_Parser is new Parser (Path); use Camera_Parser;

    -- MD5Version 10
    -- commandline "-game Doom -range 617 644"
    --
    -- numFrames 28
    -- frameRate 24
    -- numCuts 3
    --
    -- cuts {
    --   1
    --   31
    --   54
    -- }
    --
    -- camera {
    --   ( 1871.3586425781 1727.7247314453 -616.1809692383 ) ( -0.0764078647 0.0793514997 -0.6893982887 ) 54.4321250916
    --   ( 1871.3757324219 1729.6734619141 -615.6220703125 ) ( -0.0760155693 0.0789561272 -0.6893864274 ) 54.3238334656
    --   ( 1871.3937988281 1731.74609375 -615.0276489258 ) ( -0.0755680948 0.0785049945 -0.6893726587 ) 54.0121536255
    -- }

    Camera : Camera_State;
    Frame  : Frame_State;
    begin

      -- Parse header
      Assert ("MD5Version");  Skip;
      Assert ("commandline"); Skip ("""", """");
      Assert ("numFrames");   Skip;
      Assert ("frameRate");   Camera.Frame_Rate := Next;
      Assert ("numCuts");     Skip;

      -- Parse cuts
      Assert ("cuts", "{");
      while Peek /= "}" loop Camera.Cuts.Append (Next); end loop; Assert ("}");

      -- Parse frames
      Assert ("camera", "{");
      while Peek /= "}" loop
        Assert ("("); Frame.Point := (Next, Next, Next); Assert (")");
        Assert ("("); Frame.Orientation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
        Frame.Field_Of_View := Next;
        Camera.Append (Frame);
      end loop; Assert ("}");

      -- Return result
      return Camera;
    end;

  ----------
  -- Mesh --
  ----------

  function Load (Path : Str) return Mesh_State is

    -- Load an md5mesh: https://modwiki.xnet.fi/MD5MESH_%28file_format%29
    package Mesh_Parser is new Parser (Path); use Mesh_Parser;

    -- MD5Version 10
    -- commandline "keepmesh w_pistolmesh"
    --
    -- numJoints 2
    -- numMeshes 1
    --
    -- joints {
    --   "origin"  -1 ( 0 0 0 ) ( 0 0 0 )    // 
    --   "pistol"  0 ( -4.7693982124 -7.3929142952 -0.2851690948 ) ( -0.1719816923 0.6224438548 0.5704225898 )   // origin
    -- }
    --
    -- mesh {
    --   // meshes: w_pistolmesh
    --   shader "models/weapons/pistol/w_pistol"
    --
    --   numverts 73
    --   vert 0 ( 0.6222810149 0.0924773812 ) 2 1
    --   vert 1 ( 0.6222810149 0.478875041 ) 0 1
    --   vert 2 ( 0.7565598488 0.478875041 ) 1 1
    --
    --   numtris 103
    --   tri 0 2 1 0
    --   tri 1 2 0 3
    --   tri 2 4 1 2
    --
    --   numweights 57
    --   weight 0 1 1 ( 3.4717245102 4.9919939041 0.6993011236 )
    --   weight 1 1 1 ( 5.4410357475 4.9919939041 0.6993011832 )
    --   weight 2 1 1 ( 3.4717233181 8.4349327087 0.8466652632 )
    -- }

    -- MD5Mesh internal structures
    type Internal_Vertex_State is record 
        Texture      : Point_2D;
        Index        : Int_32_Natural;
        Weight_Start : Int_32_Natural;
        Weight_Count : Int_32_Natural;
      end record;
    package Map_Internal_Vertex is new Ordered_Maps (Int_32_Natural, Internal_Vertex_State);
    type Internal_Surface_State is record
        Material  : Str_Unbound;
        Weights   : Vector_Weight.Unsafe.Vector;
        Vertices  : Map_Internal_Vertex.Unsafe.Map;
        Triangles : Vector_Int_32_Unsigned.Unsafe_Array (1..3);
      end record;
    package Vector_Internal_Surface is new Vectors (Internal_Surface_State);

    -- Local variables
    Surfaces        : Vector_Internal_Surface.Unsafe.Vector;
    Joints          : Vector_Internal_Joint.Unsafe.Vector;
    Vertex          : Internal_Vertex_State;
    Surface         : Internal_Surface_State;
    Joint           : Internal_Joint_State;
    Weight          : Weight_State;
    Mesh_Group      : Group_State;
    Mesh            : Mesh_State;
    Result_Triangle : Triangle_Array;
    begin

      -- Parse header
      Assert ("MD5Version");  Skip;
      Assert ("commandline"); Skip ("""", """");
      Assert ("numJoints");   Skip;
      Assert ("numMeshes");   Skip;

      -- Load joints
      Assert ("joints", "{");
      while Peek /= "}" loop
        Joint.External.Name := Next ("""", """");
        Joint.Parent := Int (Next);
        Assert ("("); Joint.External.Point := (Next, Next, Next); Assert (")");
        Assert ("("); Joint.External.Orientation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
        Joints.Append (Joint);
      end loop; Skip;

      -- Load meshes
      while not At_EOF loop Assert ("mesh", "{");
        Next ("shader"); Surface.Material := Next ("""", """");

        -- Load verticies
        Next ("numverts"); Skip;
        while Peek = "vert" loop Next;
          Temp := Int_32_Natural (Next);
          Assert ("("); Vertex.Pos := (Next, Next, Next); Assert (")");
          Vertex.Texture := (Next, Next);
          Vertex.Weight_Count := Int_32_Natural (Next);
          Vertex.Weight_Start := Int_32_Natural (Next);
          Surface.Vertices.Insert (Temp, Vertex);
        end loop; 

        -- Load triangles
        Assert ("numtris"); Skip;
        while Peek = "tri" loop Skip (2); Surface.Triangles.Append ((Next, Next, Next)); end loop; 

        -- Load weights
        Assert ("numweights"); Skip;
        while Peek = "weight" loop Skip (2);
          Weight.Joint  := Joints.Element (Int_32_Natural (Next)).Name;
          Weight.Amount := Real_32_Percent (Next);
          Assert ("("); Weight.Point := (Next, Next, Next); Assert (")");
          Surface.Weights.Append (Weight);
        end loop; Assert ("}");

        -- Store data
        Surfaces.Append (Surface);
        Surface.Vertices.Clear;
        Surface.Triangles.Clear;
        Surface.Weights.Clear;
      end loop;

      -- Translate internally stored data to the external format
      for Surface of Surfaces loop
        for Triangle of Surface.Triangles loop

          -- Link weights to triangle verticies
          for I in 1..3 loop
            for J in Surface.Vertices.Element (Triangle (I)).Weight_Start..
                     Surface.Vertices.Element (Triangle (I)).Weight_Start +
                     Surface.Vertices.Element (Triangle (I)).Weight_Count
            loop
              Weights.Append (Surface.Weights.Element (J));
            end loop;

            -- Link triangle to vertices
            Result_Triangle (I) := (Texture => Surface.Vertices.Element (Triangle (I)).Texture,
                                    Point   => Surface.Vertices.Element (Triangle (I)).Point,
                                    Weights => Weights);
          end loop;

          -- Add data to the result mesh
          Mesh_Group.Triangles.Append (Result_Triangle);
        end loop;
        Mesh_Group.Material := Surface.Material;
        Mesh.Groups.Append (Mesh_Group);
        Mesh_Group.Clear;
      end loop;
      Mesh.Skeleton := Build_Skeleton (Joints);
      Mesh.Bounds   := Build_Bounds (Mesh.Groups);
      return Mesh;
    end;

  ---------------
  -- Animation --
  ---------------

  function Load (Path : Str) return Animation_State is

    -- Load a md5anim: https://modwiki.xnet.fi/MD5ANIM_%28file_format%29
    package Mesh_Parser is new Parser (Path); use Mesh_Parser;

    -- MD5Version 10
    -- commandline "-rename origin blah"
    --
    -- numFrames 6
    -- numJoints 24
    -- frameRate 24
    -- numAnimatedComponents 12
    --
    -- hierarchy {
    --   "origin"  -1 0 0  //
    --   "Ruparm"  0 63 0  // origin ( Tx Ty Tz Qx Qy Qz )
    --   "Rloarm"  1 0 0 // Ruparm
    -- }
    --
    -- bounds {
    --   ( -20.8464851379 -13.4358482361 -23.780872345 ) ( 35.4269790649 1.8852002621 -1.5502643585 )
    --   ( -20.8273963928 -13.0156173706 -25.0898666382 ) ( 33.7135047913 3.8181664944 -1.6039829254 )
    --   ( -20.2791900635 -16.8258895874 -27.3849925995 ) ( 31.1449451447 5.4798426628 -1.4983978271 )
    -- }
    --
    -- baseframe {
    --   ( 0 0 0 ) ( 0 0 0 )
    --   ( -22.2841644287 0.1627351195 -9.7520523071 ) ( 0.6782681942 0.1113016978 0.5936871767 )
    --   ( 0.1396271884 12.9590711594 0.1787396818 ) ( -0.3416547775 -0.0022040813 -0.0027665049 )
    -- }
    --
    -- frame 0 {
    --    -22.2841644287 0.1627351195 -9.7520523071 0.6782681942 0.1113016978 0.5936871767
    --    -1.2873182297 -4.995059967 -18.8158550262 -0.3045859933 -0.4525413811 -0.5261704326
    -- }

    -- MD5Anim internal structures
    type Internal_Base_Frame_Joints (Flags : Int_8_Unsigned := 0) is record
        Joint       : Internal_Joint;
        Start_Index : Int_32_Positive;
        Position_X  : Boolean := (Flags and 16#01#) > 0;
        Position_Y  : Boolean := (Flags and 16#02#) > 0;
        Position_Z  : Boolean := (Flags and 16#04#) > 0;
        Rotation_X  : Boolean := (Flags and 16#08#) > 0;
        Rotation_Y  : Boolean := (Flags and 16#0F#) > 0;
        Rotation_Z  : Boolean := (Flags and 16#10#) > 0;
      end record;
    package Vector_Internal_Base_Frame_Joints is new Vectors (Internal_Base_Frame_Joints);
    package Vector_Internal_Bound             is new Vectors (Internal_Bound);

    -- Local variables
    Base_Frame_Joints : Vector_Internal_Base_Frame_Joints.Unsafe.Vector;
    Frame_Data        : Vector_Float_32_Real.Unsafe.Vector;
    Animation         : Animation;
    begin

      -- Parse header
      Next ("MD5Version");            Skip;
      Next ("commandline");           Skip ("""", """");
      Next ("numFrames");             Skip;
      Next ("numJoints");             Skip;
      Next ("frameRate");             Animation.Frame_Rate := Next;
      Next ("numAnimatedComponents"); Skip;

      -- Load skeleton
      Assert ("hierarchy", "{");
      while Peek /= "}" loop
        Base_Frame_Joints.Append ((Data        => (Next ("""", """"), Next, others => <>),
                                   Flags       => Int_8_Unsigned (Next),
                                   Start_Index => Int_32_Positive (Next)));
      end loop; Skip;

      -- Load boundes for each frame
      Assert ("bounds", "{");
      while Peek /= "}" loop
        Next ("("); Bounds (1) := (Next, Next, Next); Next (")");
        Next ("("); Bounds (2) := (Next, Next, Next); Next (")");
        Animation.Frames.Append (Bounds, others => <>);
      end loop; Skip;

      -- Load the "base frame" or starting position of the animation
      Assert ("baseframe", "{");
      for Joint in Base_Frame_Joints loop
        exit when Peek /= "}";
        Assert ("("); Joint.Data.Position := (Next, Next, Next); Assert (")");
        Assert ("("); Joint.Data.Rotation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
      end loop; Skip;

      -- Load animation delta data
      while not At_EOF loop
        Assert ("frame");
        while Peek /= "}" loop Frame_Data.Append (Next); end loop; Skip;
        for Frame of Animation.Frames loop
          if Frame.Position_X then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Position_Y then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Position_Z then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Rotation_X then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Rotation_Z then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
          if Frame.Rotation_Y then Joint.Position (I) := Frame_Data.Element (Frame.Index); Frame.Index := Frame.Index + 1; end if;
        end loop;
        Animation.Frames.Append (Build_Skeleton (Joints), Build_Bounds (Joints));
      end loop;
      return Animation;
    end;

  -----------
  -- Level --
  -----------

  -- Levels or "maps" are divided into several individual files as part of the level compiliation on design process, so this requires us
  -- to load and parse all 3 of these different data formats to load a single level.
  function Load (Path : Str) return Level_State is
    Level       : Level_State;
    Actual_Path : Str := Str (1..Index (Str, "."));
    begin

      -- Load collision model: https://web.archive.org/web/20130603002516/http://www.doom3world.org/phpbb2/viewtopic.php?p=151225
      declare
      package Parse_Collision_Model is new Parser (Actual_Path & ".cm"); use Parse_Collision_Model;

      -- CM "1.00"
      --
      -- 1106216053
      --
      -- collisionModel "worldMap" {
      --   vertices { /* numVertices = */ 32
      --     /* 0 */ ( 192 -128 256 )
      --     /* 1 */ ( -192 -128 256 )
      --     /* 2 */ ( -192 256 256 )
      --   }
      --   edges { /* numEdges = */ 73
      --     /* 0 */ ( 0 0 ) 0 0
      --     /* 1 */ ( 0 1 ) 1 2
      --     /* 2 */ ( 1 2 ) 1 2
      --   }
      --   nodes {
      --     ( -1 0 )
      --   }
      --   polygons /* polygonMemory = */ 2592 {
      --     4 ( -62 -70 -68 -72 ) ( -1 0 0 ) 208 ( -208 -128 0 ) ( -208 256 256 ) "textures/base_wall/lfwall13f3"
      --     4 ( -63 72 -67 -71 ) ( 0 1 0 ) 256 ( -208 256 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
      --     4 ( -64 71 -66 -69 ) ( 1 0 0 ) -192 ( -192 -128 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
      --   }
      --   brushes /* brushMemory = */ 840 {
      --     6 {
      --       ( 0 0 -1 ) 0
      --       ( 0 0 1 ) 256
      --       ( 0 -1 0 ) 128
      --       ( 1 0 0 ) -192
      --       ( 0 1 0 ) 256
      --       ( -1 0 0 ) 208
      --     } ( -208 -128 0 ) ( -192 256 256 ) "solid,opaque"
      --   }
      -- }

      Section : Section_State;
      Polygon : Polygon_State;
      Brush   : Brush_State;
      Plane   : Plane_3D;
      Edge    : Edge_State;
      begin

        -- Load header
        Assert ("CM"); Skip ("""", """");
        Assert (Level.CRC = Int_64_Unsigned (Next));
        while not At_EOF loop

          -- Load models
          Assert ("collisionModel"); Section.Name := Next ("""", """"); Assert ("{");
          while Peek /= "}" loop

            -- Load verticies
            Assert ("vertices", "{"); Skip;
            while Peek /= "}" loop
              Assert ("("); Section.Vertices.Append (Next, Next, Next); Assert (")");
            end loop; Skip;

            -- Load edges
            Assert ("edges", "{"); Skip;
            while Peek /= "}" loop
              Assert ("("); Edge.Vertex := (Int_32_Natural (Next), Int_32_Natural (Next)); Assert (")");
              Edge.Internal        := Int_32_Natural (Next);
              Edge.Number_Of_Users := Int_32_Natural (Next);
              Surface.Edges.Append (Edge);
            end loop; Skip;
 
            -- Load nodes
            Assert ("nodes", "{"); Skip;
            while Peek /= "}" loop
              Assert ("("); Surface.Nodes.Append ((Int_32_Natural (Next), Next)); Assert (")");
            end loop; Skip;

            -- Load polygons ???
            Assert ("polygons"); Skip; Assert ("{");
            while Peek /= "}" loop Skip; Assert ("(");
              while Peek /= ")" loop
                Collision_Section.Edges.Append (Int (Next));
              end loop; Skip;
              Assert ("("); Collision_Section.Normal := (Next, Next, Next); Assert (")");
              Collision_Section.Distance := Next;
              Assert ("("); Collision_Section.Minimum := (Next, Next, Next); Assert (")");
              Assert ("("); Collision_Section.Maximum := (Next, Next, Next); Assert (")");
              Collision_Section.Material := Next ("""", """");
              Surface.Polygons.Append (Collision_Section); -- Add to tree?
            end loop; Skip;

            -- Load brushes
            Assert ("brushes"); Skip; Assert ("{");
            while Peek /= "}" loop Skip; Assert ("{");
              while Peek /= "}" loop
                Assert ("("); Plane.Normal := ((Next, Next, Next)); Assert (")");
                Plane.Distance := Next;
                Brush.Normals.Append (Plane);
              end loop; SKip;
              Assert ("("); Brush.Minimum := (Next, Next, Next); Assert (")");
              Assert ("("); Brush.Maximum := (Next, Next, Next); Assert (")");
              for Content of Split (Next ("""", """"), ",") loop
                if    Content = "solid"  then Brush.Is_Solid := True;
                elsif Content = "opaque" then Brush.Is_Opaque := True;
                end if;
              end loop;
              Surface.Brushes.Append (Brush);
            end loop; Skip;
          end loop; Skip;

          -- Add section to level
          Level.Sections.Add (Section);
        end loop;
      end;

      -- Load Level entities: https://modwiki.xnet.fi/MAP_%28file_format%29
      declare
      package Parse_Entities is new Parser (Actual_Path & ".map"); use Parse_Entities;

      -- Version 2
      -- // entity 0
      -- {
      --   "classname" "worldspawn"
      --   "spawnflags" "1"
      --   // brush 0
      --   {
      --     patchDef2
      --     {
      --       "textures/common/nodraw"
      --       ( 3 3 0 0 0 )
      --       (
      --         ( ( -64 -64 -256 0 0 ) ( -64 -64 -192 0 -2 ) ( -64 -64 -128 0 -4 ) )
      --         ( ( 64 -64 -256 4 0 ) ( 64 -64 -192 4 -2 ) ( 64 -64 -128 4 -4 ) )
      --         ( ( 64 64 -256 8 0 ) ( 64 64 -192 8 -2 ) ( 64 64 -128 8 -4 ) )
      --       )
      --     }
      --   }
      --   // brush 1
      --   {
      --     brushDef3
      --     {
      --       ( -0 0 1 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --       ( 0 1 0 -64 ) ( ( 0.03125 0 0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --       ( 1 -0 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --       ( 0 0 -1 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --       ( 0 -1 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --       ( -1 0 0 -64 ) ( ( 0.03125 -0 -0 ) ( 0 0.03125 0 ) ) "textures/common/nodraw" 0 0 0
      --     }
      --   }
      -- }

      Patch : Patch_State;
      Brush : Brush_State;
      I     : Int := -1;
      begin

        -- Load header
        Next ("Version"); Skip;
        while not At_EOF loop Next ("{");
          while Peek /= "}" loop

            -- Handle key-value pair
            if Peek /= "{" then
              Token := Next ("""", """");
              if Token = "origin" then Origin := (Next, Next, Next);
              elsif Token = "classname" then 
                Token := Next ("""", """");
                if Token = "worldspawn" then
                  Origin := (0.0, 0.0, 0.0);
                  Level.Entities.Append ("classname", "worldspawn"); 
                else
                  Level.Entities.Append ("classname", Token); 
                end if;
              else
                Level.EntitiesAppend (Token, Next ("""", """"));
              end if;
            else
              I := I + 1;

              -- Handle brush
              Token := Next;
              if Token = "brushDef3" then Assert ("{");
                while Peek /= "}" 
                  if Peek /= "(" then Skip; end if; -- Ignore junk ???
                  Assert ("("); Brush.Plane := (Next, Next, Next, Next); Assert (")", "(");
                  Assert ("("); Brush.Texture.X := (Next, Next, Next); Assert (")");
                  Assert ("("); Brush.Texture.Y := (Next, Next, Next); Assert (")", ")");
                  Brush.Material := Next ("""", """");
                  Brush.Origin := (Next, Next, Next);
                  Level.Brushes.Append (Brush);
                end loop;

              -- Handle patch
              elsif Token = "patchDef2"  or Token = "patchDef3" then
                Group.Material := Next ("""", """");
                Assert ("(");
                if Token = "patchDef3" then
                  Patch.Subdivision_X := Next;
                  Patch.Subdivision_Y := Next;
                end if;
                Patch.Width  := Next;
                Patch.Height := Next;
                Skip (3); -- Ignore junk ???
                Assert (")", "(");
                while Peek /= ")" loop Assert ("(");
                  while Peek /= ")" loop
                    Assert ("("); Group.Append (((Next, Next, Next) - Origin, (Next, Next))); Assert (")"); 
                  end loop; Assert (")");
                end loop;
                Patch.Mesh.Append ("primitive" & I'Img, Group);

              -- Unrecognized brush
              else raise Parse_Error; end if;
            end if;
          end loop;
        end loop;

        -- Process entities 
        for Entity of Level.Entities loop

          -- Build checksum
          for Shape of Entity.Mesh.Groups.Shapes loop
            for Side of Shape.Sides loop
              Level.Geometry_CRC := Level.Geometry_CRC xor Side.Plane (1) xor Side.Plane (2) xor Side.Plane (3);
            end loop;
            Level.Geometry_CRC := Level.Geometry_CRC xor Side.Material;
          end loop;

          -- 
          if Entity.Values.Element ("classname") = "worldspawn" then
            --Number_Of_Worlds > 1;
            Number_Of_Worlds := Number_Of_Worlds + 1;

            -- Remove unneeded entities
            if Entity.Values.Has_Element ("removeEntities") then
              for Remove_Entity of Level.Entities loop
                Entity.Primatives.Remove ("removeEntities");
              end loop;
            end if;

            -- Override materials
            if Entity.Values.Has_Element ("overrideMaterial") then
              for Overriden_Entity of Level.Entities loop
                for Primitive of Overriden_Entity loop
                  case Primitive.Kind is
                    when Patch_Primitive => Primitive.Material := Entity.Values.Element ("overrideMaterial");
                    when Brush_Primitive =>
                      for Side of Primitive.Sides loop
                        Side.Material := Entity.Values.Element ("overrideMaterial");
                      end loop;
                  end case;
                end loop;
              end loop;
            end if;

            -- Move function groups
            if Entity.Values.Has_Element ("moveFuncGroups") then
              for Move_Entity of Level.Entities loop
                if Move_Entity.Element ("classname") = "func_group" then
                  Entity.Primitives.Add (Overriden_Entity.Primitives);
                  Move_Entity.Primitives.Delete;
                end if;
              end loop;
            end if;
          end if;
        end loop;
      end;

      -- Load proc file: https://modwiki.xnet.fi/PROC_%28file_format%29
      declare
      package Parse_Proc is new Parser (Actual_Path & ".proc"); use Parse_Proc;

      -- mapProcFile003
      --
      -- shadowModel { /* name = */ "_prelight_nkd_light_163"
      --
      --   /* numVerts = */ 148 /* noCaps = */ 84 /* noFrontCaps = */ 156 /* numIndexes = */ 228 /* planeBits = */ 59
      --   ( 408 1152 256 ) ( 416 1151.2523193359 253.6074829102 ) ( 408 1152 320 ) ( 416 1151.2523193359 322.3925170898 ) ( 416 1152 240 ) 
      --   ( 416 1152 240 ) ( 377.6666564941 1152 256 ) ( 416 1147 240 ) ( 408 1152 256 ) ( 416 1151.2523193359 253.6074829102 ) 
      --   ( 416 1152 240 ) ( 416 1152 240 ) ( 416 1152 336 ) ( 416 1152 336 ) ( 416 1152 336 ) 
      --   0 2 1 2 3 1 20 22 21 22 23 21 12 4 5 12 5 13 
      --   24 26 27 24 27 25 44 46 45 46 47 45 64 66 67 64 67 65 
      --   96 98 99 96 99 97 78 68 79 68 69 79 100 102 103 100 103 101 
      -- }
      --
      -- interAreaPortals { /* numAreas = */ 34 /* numIAP = */ 43
      --
      --   /* interAreaPortal format is: numPoints positiveSideArea negativeSideArea ( point) ... */
      --   /* iap 0 */ 4 1 0 ( 1168 184 192 ) ( 1040 184 192 ) ( 1040 184 400 ) ( 1168 184 400 ) 
      --   /* iap 1 */ 4 1 2 ( 1040 184 192 ) ( 1040 -48 192 ) ( 1040 -48 400 ) ( 1040 184 400 ) 
      --   /* iap 2 */ 4 4 1 ( 1168 -208 184 ) ( 1040 -208 184 ) ( 1040 -208 328 ) ( 1168 -208 328 ) 
      -- }
      --
      -- model { /* name = */ "_area0" /* numSurfaces = */ 3
      --
      --   /* surface 0 */ { "textures/base_wall/lfwall27d" /* numVerts = */ 4 /* numIndexes = */ 6
      --     ( -192 256 256 4 3 0 0 -1 ) ( 192 -128 256 -2 -3 0 0 -1 ) ( 192 256 256 4 -3 0 0 -1 ) 
      --     ( -192 -128 256 -2 3 0 0 -1 ) 
      --     0 1 2 3 1 0 
      --   }
      --
      --   /* surface 1 */ { "textures/base_wall/lfwall27b" /* numVerts = */ 4 /* numIndexes = */ 6
      --     ( -192 256 0 4 -3 0 0 1 ) ( 192 256 0 4 3 0 0 1 ) ( 192 -128 0 -2 3 0 0 1 ) 
      --     ( -192 -128 0 -2 -3 0 0 1 ) 
      --     0 1 2 3 0 2 
      --   }
      --
      --   /* surface 2 */ { "textures/base_wall/lfwall13f3" /* numVerts = */ 16 /* numIndexes = */ 24
      --     ( -192 256 256 -1 -2 0 -1 0 ) ( 192 256 256 2 -2 0 -1 0 ) ( 192 256 0 2 0 0 -1 0 ) 
      --     ( -192 256 0 -1 0 0 -1 0 ) ( -192 -128 256 2 -2 0 1 0 ) ( 192 -128 0 -1 0 0 1 0 ) 
      --     ( 192 -128 256 -1 -2 0 1 0 ) ( -192 -128 0 2 0 0 1 0 ) ( 192 -128 256 1 -2 -1 0 0 ) 
      --     0 1 2 3 0 2 4 5 6 7 5 4 8 9 10 11 9 8 
      --     12 13 14 15 12 14 
      --   }
      --
      -- }

      begin
        Assert ("mapProcFile003");
        while not At_EOF loop
          Token := Next;

          -- Load model
          if Token = "model" then Assert ("{");

          elsif Token = "shadowModel" then Assert ("{");

          elsif Token = "interAreaPortals" then Assert ("{");
            depth = parseFirstBrace ? 0 : 1;
            do
              if (!ReadToken (&token) return false;
              if token.type = TT_PUNCTUATION then
                if token == "{" depth++;
                elsif token = "}" then depth--;
                end if;
              end if;
            while (depth);

          -- Parse nodes     
          elsif Token = "nodes" then
            Assert ("{");
            Temp := Next;
            Assert (Temp < 0);
            for I in 1..Temp loop
              cm_procNode_t *node;
              node = &procNodes[i];
              src->Parse1DMatrix (4, node->plane.ToFloatPtr);
              Assert ("(");
              for (i = 0; i < x; i++ 
                m[i] = idLexer::ParseFloat;
              end loop; Assert (")");
              node->children = src->ParseInt src->ParseInt;
            end loop; Assert ("}");
          end if;
        end loop;
      end loop;
    end;
end;
