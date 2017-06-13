
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

-- Id Tech skeletal mesh, animation, shader, and level data format loaders
separate (Neo.Data.Model) package body Id_Tech is

  --------------
  -- Skeleton --
  --------------

  -- Internal structure that reflects how joints are layed-out by the md5mesh and md5anim data files
  type Internal_Joint_State is record
      External : Joint_State := (others => <>);
      Parent   : Int         := -2;
    end record;
  package Vector_Internal_Joint is new Neo.Core.Vectors (Internal_Joint_State);

  -- Bulding a skeleton from a vector of internal joint structures
  function Build_Skeleton (Joints : Vector_Internal_Joint.Unsafe.Vector) return Treed_Joint.Unsafe.Tree is
    Skeleton : Treed_Joint.Unsafe.Tree;
    begin
      for Joint of Joints loop
        if Joint.Parent = -1 then Skeleton.Append_Child (Skeleton.Root, Joint.External);
        else Skeleton.Append_Child (Skeleton.Find (Joints.Element (Joint.Parent + 1).External), Joint.External); end if;
      end loop;
      return Skeleton;
    end;
  function Build_Bounding (Joints : Vector_Internal_Joint.Unsafe.Vector) return Bounding_State is
    Bounding : Bounding_State;
    begin
      for Joint of Joints loop Adjust_Bounding (Joint.External.Point, Bounding); end loop;
      return Bounding;
    end;

  --------------
  -- Material --
  --------------

  -- Basically an unsightly hack...
  function Load (Path : Str) return Hashed_Material.Unsafe.Map is 

    -- Load a material file: https://www.iddevnet.com/doom3/materials.php
    package Parse_Materials is new Parser (Path); use Parse_Materials;

    -- Id Tech MTR files are loaded with "features" so the example below is basic...

    -- textures/alphalabs/a_lfwall12b2
    -- {
    --   qer_editorimage textures/alphalabs/a_lfwall12b2
    --   {Old_Player
    --     blend bumpmap     
    --     map   textures/base_wall/lfwall12b_local
    --     
    --   }
    --   diffusemap  textures/alphalabs/a_lfwall12b2
    --   specularmap textures/alphalabs/a_lfwall12b_s
    -- }

    Name     : Str_Unbound;
    Material : Material_State;
    Result   : Hashed_Material.Unsafe.Map;
    begin
      while not At_EOF loop
        Name := Next; Assert ("{");
        while Peek /= "}" loop declare Token : Str := Next; begin -- To avoid pesky unbounded string conversions

            -- Handle sharder properties (only a couple are supported ATM). Add support for more ???
            if    Token = "noShadows"          then Material.Cast_Shadow  := False;
            elsif Token = "noSelfShadow"       then Material.Shadow_Self  := False;
            elsif Token = "unsmoothedTangents" then Material.Smoothed_Tan := False;
            elsif Token = "twoSided"           then Material.Two_Sided    := True;
            elsif Token = "specularmap"        then Material.Specular     := Next;
            elsif Token = "bumpmap"            then Material.Normal       := Next;
            elsif Token = "diffusemap"         then Material.Base_Color   := Next;
                                               end if; --     Material.Opacity      := Material.Base_Color; end if;
        end; end loop; Skip;
        Result.Insert (Name, Material);
      end loop;
      return Result;
    end;

  ------------
  -- Camera --
  ------------

  function Load (Path : Str) return Camera_State is

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
    Frame  : Camera_Frame_State;
    begin

      -- Parse header
      Assert ("MD5Version"); Skip;
      Assert ("commandline"); Skip_Set ("""", """");
      Assert ("numFrames"); Skip;
      Assert ("frameRate"); Camera.Frame_Rate := Next;
      Assert ("numCuts"); Skip;

      -- Parse cuts
      Assert ("cuts", "{");
      while Peek /= "}" loop Camera.Cuts.Append (Next); end loop; Assert ("}");

      -- Parse frames
      Assert ("camera", "{");
      while Peek /= "}" loop Assert ("(");
        Frame.Point       := (Next, Next, Next); Assert (")", "(");
        Frame.Orientation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
        Frame.FOV         := Next;
        Camera.Frames.Append (Frame);
      end loop; Skip;

      -- Return result
      return Camera;
    end;

  -------------------
  -- Skeletal Mesh --
  -------------------

  function Load (Path : Str) return Skeletal_Mesh_State is

    -- Load an md5mesh: https://modwiki.xnet.fi/MD5MESH_%28file_format%29
    package Skeletal_Mesh_Parser is new Parser (Path); use Skeletal_Mesh_Parser;

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

    -- Internal structures
    type Internal_Weight_State is record
        External    : Weight_State;
        Joint_Index : Natural;
      end record;
    package Vector_Internal_Weight is new Neo.Core.Vectors (Internal_Weight_State);
    type Internal_Vertex_State is record 
        External     : Vertex_State (Has_Weights => True);
        Index        : Natural;
        Weight_Start : Natural;
        Weight_Count : Natural;
      end record;
    package Vector_Internal_Vertex is new Neo.Core.Vectors (Internal_Vertex_State);
    type Internal_Surface_State is record
        Material  : Str_Unbound;
        Weights   : Vector_Internal_Weight.Unsafe.Vector;
        Vertices  : Vector_Internal_Vertex.Unsafe.Vector;
        Triangles : Vector_Triangle.Unsafe.Vector;
      end record;
    package Vector_Internal_Surface is new Neo.Core.Vectors (Internal_Surface_State);

    -- Local variables
    Surfaces : Vector_Internal_Surface.Unsafe.Vector;
    Joints   : Vector_Internal_Joint.Unsafe.Vector;
    Vertex   : Internal_Vertex_State;
    Surface  : Internal_Surface_State;
    Joint    : Internal_Joint_State;
    Weight   : Internal_Weight_State;
    Weights  : Vector_Weight.Unsafe.Vector;
    Group    : Mesh_State;
    Mesh     : Skeletal_Mesh_State;
    begin

      -- Parse header
      Assert ("MD5Version");Skip;
      Assert ("commandline"); Skip_Set ("""", """");
      Assert ("numJoints"); Skip;
      Assert ("numMeshes"); Skip;

      -- Load joints
      Assert ("joints", "{");
      while Peek /= "}" loop
        Joint.External.Name        := Next_Set ("""", """");
        Joint.Parent               := Next; Assert ("(");
        Joint.External.Point       := (Next, Next, Next); Assert (")", "(");
        Joint.External.Orientation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
        Joints.Append (Joint);
      end loop; Skip;

      -- Load meshes
      while not At_EOF loop Assert ("mesh", "{", "shader");
        Surface.Material := Next_Set ("""", """");

        -- Load verticies
        Assert ("numverts"); Skip;
        while Peek = "vert" loop Skip (2); Assert ("(");
          Vertex.External.Texture := (Next, Next); Assert (")");
          Vertex.Weight_Count     := Next;
          Vertex.Weight_Start     := Next;
          Surface.Vertices.Append (Vertex);
        end loop; 

        -- Load triangles
        Assert ("numtris"); Skip;
        while Peek = "tri" loop Skip (2); Surface.Triangles.Append ((Next, Next, Next)); end loop; 

        -- Load weights
        Assert ("numweights"); Skip;
        while Peek = "weight" loop Skip (2);
          Weight.Joint_Index     := Next;
          Weight.External.Amount := Next; Assert ("(");
          Weight.External.Point  := (Next, Next, Next); Assert (")");
          Surface.Weights.Append (Weight);
        end loop; Assert ("}");

        -- Store data
        Surfaces.Append (Surface);
        Surface.Triangles.Clear;
        Surface.Vertices.Clear;
        Surface.Weights.Clear;
      end loop;

      -- Build the skeleton and link the weights to joints
      Mesh.Skeleton := Build_Skeleton (Joints);
      for S of Surfaces loop
        for W of S.Weights loop
          W := ((Mesh.Skeleton.Find (Joints.Element (W.Joint_Index).External), W.External.Point, W.External.Amount), 0);
        end loop;
      end loop;

      -- Translate internally stored data to the external format
      for S of Surfaces loop
        for V of S.Vertices loop

          -- Link weights to triangle verticies
          for I in V.Weight_Start..V.Weight_Start + V.Weight_Count loop
            V.External.Weights.Append (S.Weights.Element (I).External);
          end loop;
        end loop;

        -- Add data to the result mesh
        Group.Material := S.Material;
        Group.Indicies := S.Triangles;
        Build_Bounding (Group);
        Mesh.Groups.Append (Group);
        Group := (others => <>);
      end loop;
      return Mesh;
    end;

  ---------------
  -- Animation --
  ---------------

  function Load (Path : Str) return Animation_State is

    -- Load a md5anim: https://modwiki.xnet.fi/MD5ANIM_%28file_format%29
    package Mesh_Parser is new Parser (Path); use Mesh_Parser;

    -- MD5Version 10
    -- commandline "-rename origin ..."
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

    -- Internal structure
    type Internal_Joint_Base_State is record
        Data  : Internal_Joint_State := (others => <>);
        Flags : Byte                 := 0;
        Start : Int_Unsigned      := 0;
      end record;
    package Vector_Internal_Joint_Base is new Neo.Core.Vectors (Internal_Joint_Base_State);

    -- Local variables
    Joint      : Internal_Joint_State;
    Joints     : Vector_Internal_Joint.Unsafe.Vector;
    Frame_Base : Vector_Internal_Joint_Base.Unsafe.Vector;
    Frame_Data : Vector_Real.Unsafe.Vector;
    Animation  : Animation_State;
    Bounding   : Bounding_State;
    I          : Positive := 1;
    begin

      -- Parse header
      Assert ("MD5Version"); Skip;
      Assert ("commandline"); Skip_Set ("""", """");
      Assert ("numFrames"); Skip;
      Assert ("numJoints"); Skip;
      Assert ("frameRate"); Animation.Frame_Rate := Next;
      Assert ("numAnimatedComponents"); Skip;

      -- Load skeleton
      Assert ("hierarchy", "{");
      while Peek /= "}" loop Frame_Base.Append ((((Next_Set ("""", """"), others => <>), Next), Next, Next)); end loop; Skip;

      -- Load boundes for each frame
      Assert ("bounds", "{");
      while Peek /= "}" loop Assert ("(");
        Bounding.A := (Next, Next, Next); Assert (")", "(");
        Bounding.B := (Next, Next, Next); Assert (")");
        Animation.Frames.Append ((Bounding, others => <>));
      end loop; Skip;

      -- Load the "base frame" or starting position of the animation
      Assert ("baseframe", "{");
      for Joint of Frame_Base loop
        exit when Peek = "}"; Assert ("(");
        Joint.Data.External.Point       := (Next, Next, Next); Assert (")", "(");
        Joint.Data.External.Orientation := To_Quaternion_4D ((Next, Next, Next)); Assert (")");
      end loop; Skip;

      -- Load animation delta data
      while not At_EOF loop
        Assert ("frame"); Skip; Assert ("{"); 
        while Peek /= "}" loop Frame_Data.Append (Next); end loop; Skip;
        for Base of Frame_Base loop
          Joint := Base.Data;
          if (Base.Flags and 16#01#) > 0 then Joint.External.Point.X       := Frame_Data.Element (I); I := I + 1; end if;
          if (Base.Flags and 16#02#) > 0 then Joint.External.Point.Y       := Frame_Data.Element (I); I := I + 1; end if;
          if (Base.Flags and 16#04#) > 0 then Joint.External.Point.Z       := Frame_Data.Element (I); I := I + 1; end if;
          if (Base.Flags and 16#08#) > 0 then Joint.External.Orientation.X := Frame_Data.Element (I); I := I + 1; end if;
          if (Base.Flags and 16#0F#) > 0 then Joint.External.Orientation.Y := Frame_Data.Element (I); I := I + 1; end if;
          if (Base.Flags and 16#10#) > 0 then Joint.External.Orientation.Z := Frame_Data.Element (I); I := I + 1; end if;
          Joint.External.Orientation := To_Quaternion_4D (To_Vector_3D (Joint.External.Orientation));
          Joints.Append (Joint);
        end loop;
        Animation.Frames.Append ((Build_Bounding (Joints), Build_Skeleton (Joints)));
        Joints.Clear;
      end loop;
      return Animation;
    end;

  -----------
  -- Level --
  -----------
  --
  -- Levels or "maps" are divided into several individual files as part of the level compiliation process, so this requires us to load 
  -- and parse all 3 of these different data formats to load a single level.
  --

  function Load (Path : Str) return Level_State is
    Level       : Level_State;
    Actual_Path : Str := Path (Path'First..Index (Path, "."));
    begin

      -- Load the area awareness system data: https://modwiki.xnet.fi/The_Doom_3_AAS_system
      -- declare package Parse_AI is new Parser (Actual_Path & ".aas48"); use Parse_AI;

      -- DewmAAS "1.07"
      --
      -- 3358620753
      --
      -- settings
      -- {
      --   bboxes
      --   {
      --     (-24 -24 0)-(24 24 82)
      --   }
      --   usePatches = 0
      --   writeBrushMap = 0
      --   playerFlood = 0
      --   allowSwimReachabilities = 0
      --   allowFlyReachabilities = 1
      --   fileExtension = "aas48"
      --   gravity = (0 0 -1050)
      --   maxStepHeight = 18
      --   maxBarrierHeight = 32
      --   maxWaterJumpHeight = 20
      --   maxFallHeight = 64
      --   minFloorCos = 0.6999999881
      --   tt_barrierJump = 100
      --   tt_startCrouching = 100
      --   tt_waterJump = 100
      --   tt_startWalkOffLedge = 100
      -- }
      --
      -- planes 1684 {
      --   0 ( 1 0 0 2048 )
      --   1 ( -1 0 0 -2048 )
      --   2 ( 1 0 0 3584 )
      -- }
      --
      -- vertices 1046 {
      --   0 ( 3288 124 -704 )
      --   1 ( 3368 124 -704 )
      --   2 ( 3288 196 -704 )
      -- }
      --
      -- edges 1402 {
      --   0 ( 0 0 )
      --   1 ( 0 1 )
      --   2 ( 0 2 )
      -- }
      --
      -- edgeIndex 1905 {
      --   0 ( -1 )
      --   1 ( 2 )
      --   2 ( -3 )
      -- }
      --
      -- faces 3529 {
      --   0 ( 0 0 0 0 0 0 )
      --   1 ( 115 1 1 0 0 0 )
      --   2 ( 111 1 1 0 0 0 )
      -- }
      --
      -- faceIndex 4832 {
      --   0 ( 1 )
      --   1 ( 2 )
      --   2 ( 3 )
      -- }
      --
      -- areas 583 {
      --   0 ( 0 0 0 0 0 0 ) 0 {
      --   }
      --   1 ( 193 0 0 7 1 0 ) 1 {
      --     2 4 (3288 160 -704) (3287.5 160 -704) 2 1
      --   }
      --   2 ( 193 0 7 8 2 0 ) 2 {
      --     2 3 (3128 160 -704) (3127.5 160 -704) 6 1
      --     2 4 (3232 160 -704) (3232.5 160 -704) 9 1
      --   }
      -- }
      --
      -- nodes 2130 {
      --   0 ( 0 0 0 )
      --   1 ( 0 2 464 )
      --   2 ( 2 0 3 )
      -- }
      --
      -- portals ?? {
      --   0 ( 0 0 0 0 0 )
      -- }
      --
      -- portalIndex 88 {
      --   0 ( 2 )
      --   1 ( 1 )
      --   2 ( 2 )
      -- }
      --
      -- clusters 32 {
      --   0 ( 0 0 49737368 0 )
      --   1 ( 2 2 0 1 )
      --   2 ( 3 3 1 2 )
      -- }
      -- 
      -- begin
      -- 
      --   -- Load header
      --   Assert ("DewmAAS"); Skip_Set ("""", """");
      -- 
      --   -- Load settings
      --   Assert ("settings", "{");
      --   while Peek /= "}" loop
      --     Token := Next;
      --     if Token = "bboxes" then Assert ("{", "(");
      --       AI.Bounding.A := (Next, Next, Next); Assert (")", "-", "(");
      --       AI.Bounding.B := (Next, Next, Next); Assert (")", "}");
      --     elsif Token = "gravity" then Assert ("=", "(");
      --       AI.Gravity := (Next, Next, Next); Assert (")");
      --     elsif Token = "maxStepHeight" then Assert ("=");
      --       AI.Max_Step_Height := Next;
      --     elsif Token = "minFloorCos" then Assert ("=");
      --       AI.Min_Floor_Cos := Next;
      --     end if;
      --   end loop;
      -- 
      --   -- Load planes
      --   Assert ("planes"); Skip; Assert ("{");
      --   while Peek /= "}" loop Skip; Assert ("(");
      --     Planes.Append (Next, Next, Next, Next); Assert (")");
      --   end loop; 
      -- 
      --   -- Load verticies
      --   Assert ("vertices"); Skip; Assert ("{");
      --   while Peek /= "}" loop Skip; Assert ("(");
      --     Verticies.Append (Next, Next, Next); Assert (")");
      --   end loop;
      -- 
      --   -- Handle edges
      --   Assert ("edges"); Skip; Assert ("{");
      --   while Peek /= "}" loop Skip; Assert ("(");
      -- 
      --   -- Handle faces
      --   -- Handle areas
      --   -- Handle partitions
      --   -- Handle portals
      --   -- Clusters
      -- exception when Invalid_File => Line ("Continuting without AAS file for " & Path); end;

      -- Load entities: https://modwiki.xnet.fi/MAP_%28file_format%29
      declare package Parse_Entities is new Parser (Actual_Path & ".map"); use Parse_Entities;

      -- Version 2
      -- // entity 0
      -- {
      --   "classname" "worldspawn"
      --   "name" "info_locationseparator_4"
      --   "origin" "372 704 204"
      --
      --   // brush 0
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
      --
      --   // primitive 0
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
      --
      --   // primitive 1
      --   {
      --     patchDef3
      --     {
      --       "textures/decals/cam_base"
      --       ( 3 3 1 1 0 0 0 )
      --       (
      --         ( ( -1010 576.25 101 0 0 ) ( -1012 578.25 101 0 0.5 ) ( -1014 576.25 101 0 1 ) )
      --         ( ( -1010 576.25 99 0.5 0 ) ( -1012 578.25 99 0.5 0.5 ) ( -1014 576.25 99 0.5 1 ) )
      --         ( ( -1010 576.25 97 1 0 ) ( -1012 578.25 97 1 0.5 ) ( -1014 576.25 97 1 1 ) )
      --       )
      --     }
      --   }
      -- }

      Patch_Height : Positive;
      Patch_Width  : Positive;
      Patch        : Mesh_State;
      Brush        : Vector_Brush_Side.Unsafe.Vector;
      Side         : Brush_Side_State;
      Entity       : Entity_State;
      Token        : Str_Unbound;
      Triangle     : Triangle_Array;
      begin

        -- Load header
        Assert ("Version"); Skip;
        while not At_EOF loop Assert ("{");
          while Peek /= "}" loop

            -- Parse key-value pair
            if Peek /= "{" then
              Token := Next_Set ("""", """");
              if Token = "origin" then Assert (""""); Entity.Point := (Next, Next, Next); Assert (""""); 
              elsif Token /= "classname" then Entity.Key_Values.Insert (Token, Next_Set ("""", """")); 
              else Entity.Key_Values.Insert (U ("classname"), Next_Set ("""", """")); end if;

            -- Handle geometry
            else Skip; Token := Next;

              -- Load brush
              if Token = "brushDef3" then
                Assert ("{");
                while Peek /= "}" loop
                  Assert ("(");
                  Side.Plane      := (Next, Next, Next, Next); Assert (")", "(", "(");
                  Side.Texture.XX := Next;
                  Side.Texture.YX := Next;
                  Side.Texture.ZX := Next; Assert (")", "(");
                  Side.Texture.YY := Next;
                  Side.Texture.ZY := Next; Assert (")", ")");
                  Side.Material   := Next_Set ("""", """");
                  Side.Origin     := (Next, Next, Next);
                  Brush.Append (Side);
                end loop; Skip;
                Entity.Brushes.Append (Brush);
                Brush.Clear;

              -- Load patch
              elsif Token = "patchDef2" or Token = "patchDef3" then

                -- Parse header
                Patch.Material := Next_Set ("""", """"); Assert ("(");
                if Token = "patchDef3" then Skip (2); end if;
                Patch_Width  := Next;
                Patch_Height := Next; Skip (3); Assert (")", "(");

                -- Parse vertices
                while Peek /= ")" loop Assert ("(");
                  while Peek /= ")" loop Assert ("(");
                    Patch.Vertices.Append ((False, (Next, Next), (Next, Next, Next) - Entity.Point, others => <>)); Assert (")"); 
                  end loop; Assert (")");
                end loop; Skip;

                -- Build indicies
                for I in 0..Patch_Height loop
                  for J in 1..Patch_Width loop
                    Triangle := (if J mod 2 = 0 then (I * Patch_Width + J - 1, I * Patch_Width + J, (I + 1) * Patch_Width + J - 1)
                                 else (I * Patch_Width + J - 1, (I + 1) * Patch_Width + J - 2, (I + 1) * Patch_Width + J - 1));
                  end loop;
                end loop;
                Entity.Patches.Append (Patch);
                Patch.Vertices.Clear;
              end if;
            end if;
          end loop; Skip;

          -- Append entity
          Level.Entities.Insert (Entity.Key_Values.Element (U ("classname")), Entity);
          Entity.Key_Values.Clear;
          Entity.Brushes.Clear;
          Entity.Patches.Clear;
        end loop;

        -- Build checksum 
        for Entity of Level.Entities loop          
          for Brush of Entity.Brushes loop
            for Side of Brush loop
              Level.Geometry_CRC := Level.Geometry_CRC xor To_Int_64_Unsigned (Side.Plane.X) xor
                                                           To_Int_64_Unsigned (Side.Plane.Y) xor
                                                           To_Int_64_Unsigned (Side.Plane.Z);
              for Text of S (Side.Material) loop -- crc ^= str[i] << (i & 3) ???
                Level.Geometry_CRC := Level.Geometry_CRC xor Char_16'Pos (Text);
              end loop;
            end loop;
          end loop;
        end loop;
      end;

      -- Load collision model: https://web.archive.org/web/20130603002516/http://www.doom3world.org/phpbb2/viewtopic.php?p=151225
      declare package Parse_Collision_Model is new Parser (Actual_Path & ".cm"); use Parse_Collision_Model;

      -- CM "1.00"
      --
      -- 1106216053
      --
      -- collisionModel "worldMap" {
      --
      --   vertices { /* numVertices = */ 32
      --     /* 0 */ ( 192 -128 256 )
      --     /* 1 */ ( -192 -128 256 )
      --     /* 2 */ ( -192 256 256 )
      --   }
      --
      --   edges { /* numEdges = */ 73
      --     /* 0 */ ( 0 0 ) 0 0
      --     /* 1 */ ( 0 1 ) 1 2
      --     /* 2 */ ( 1 2 ) 1 2
      --   }
      --
      --   nodes {
      --     ( 0 128 ) 
      --     ( 1 160 ) 
      --     ( -1 0 ) 
      --     ( -1 0 ) 
      --   }
      --
      --   polygons /* polygonMemory = */ 2592 {
      --     4 ( -62 -70 -68 -72 ) ( -1 0 0 ) 208 ( -208 -128 0 ) ( -208 256 256 ) "textures/base_wall/lfwall13f3"
      --     4 ( -63 72 -67 -71 ) ( 0 1 0 ) 256 ( -208 256 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
      --     4 ( -64 71 -66 -69 ) ( 1 0 0 ) -192 ( -192 -128 0 ) ( -192 256 256 ) "textures/base_wall/lfwall13f3"
      --   }
      --
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

      Points    : Vector_Point_3D.Unsafe.Vector;
      Polygon   : Polygon_State;
      Edge      : Edge_State;
      Plane     : Plane_4D;
      Clip      : Clip_State;
      Collision : Collision_State;
      begin

        -- Load header and verify the checksum
        Assert ("CM"); Skip_Set ("""", """");
        Assert (Level.Geometry_CRC = Next);

        -- Handle collisions
        while not At_EOF loop
          Assert ("collisionModel"); Collision.Name := Next_Set ("""", """"); Assert ("{");

          -- Load points
          Assert ("vertices", "{"); Skip;
          while Peek /= "}" loop Assert ("(");
            Points.Append ((Next, Next, Next)); Assert (")");
          end loop; Skip;

          -- Load edges
          Assert ("edges", "{"); Skip;
          while Peek /= "}" loop Assert ("(");
            Edge.A         := Points.Element (Next);
            Edge.B         := Points.Element (Next); Assert (")");
            Edge.Internal  := Next;
            Edge.Num_Users := Next;
            Collision.Edges.Append (Edge);
          end loop; Skip;

          -- Load nodes
          declare use Treed_Collision_Node.Unsafe;
          Node_Kind : Int    := 0;
          Is_Back   : Bool   := False;
          I         : Cursor := Collision.Nodes.Root;
          begin
            Assert ("nodes", "{"); Skip;
            while Peek /= "}" loop Assert ("(");
              Node_Kind := Next;
              if Node_Kind = -1 then -- Its a leaf node - eat it, switch sides, and move up
                Skip;
                I := Parent (I);
                Is_Back := True;
              else
                Collision.Nodes.Append_Child (I, (Is_Back, Next, Dimension_Kind'Val (Node_Kind + 1)), 1);
                I := First_Child (I);
                if Is_Back then Is_Back := False; end if;
              end if; Assert (")");
            end loop; Skip;
          end;

          -- Load polygons
          Assert ("polygons"); Skip; Assert ("{");
          while Peek /= "}" loop Skip; Assert ("(");
            while Peek /= ")" loop Polygon.Edges.Append (Next); end loop; Skip; Assert ("(");
            Polygon.Plane      := (Next, Next, Next, 0.0); Assert (")");
            Polygon.Plane.W    := Next; Assert ("(");
            Polygon.Bounding.A := (Next, Next, Next); Assert (")", "(");
            Polygon.Bounding.B := (Next, Next, Next); Assert (")");
            Polygon.Material   := Next_Set ("""", """");
            Collision.Polygons.Append (Polygon);
            Polygon.Edges.Clear;
          end loop; Skip;

          -- Load clipping
          Assert ("brushes"); Skip; Assert ("{");
          while Peek /= "}" loop Skip; Assert ("{");
            while Peek /= "}" loop Assert ("(");
              Plane   := (Next, Next, Next, 0.0); Assert (")"); 
              Plane.W := Next;
              Clip.Sides.Append (Plane);
            end loop; SKip;
            Clip.Bounding.A := (Next, Next, Next); Assert (")", "(");
            Clip.Bounding.B := (Next, Next, Next); Assert (")");
            for Content of Split (Next_Set ("""", """"), ",") loop
              if    Content = "playerclip"         then Clip.Is_Player     := True;
              elsif Content = "opaque"             then Clip.Is_Opaque     := True;
              elsif Content = "water"              then Clip.Is_Water      := True;
              elsif Content = "solid"              then Clip.Is_Solid      := True;
              elsif Content = "monsterclip"        then Clip.Is_Monster    := True;
              elsif Content = "moveableclip"       then Clip.Is_Moveable   := True;
              elsif Content = "aas_solid"          then Clip.Is_Bot        := True;
              elsif Content = "blood"              then Clip.Is_Blood      := True;
              elsif Content = "trigger"            then Clip.Is_Trigger    := True;
              elsif Content = "body"               then Clip.Is_Body       := True;
              elsif Content = "flashlight_trigger" then Clip.Is_Flashlight := True;
              elsif Content = "corpse"             then Clip.Is_Corpse     := True;
              elsif Content = "ikclip"             then Clip.Is_Animation  := True;
              elsif Content = "aas_obstacle"       then Clip.Is_Obstacle   := True; end if;
            end loop;
            Collision.Clipping.Append (Clip);
            Clip.Sides.Clear;
          end loop; Skip (2);

          -- Add collision data to result
          Level.Collisions.Append (Collision);
          Collision.Polygons.Clear;
          Collision.Clipping.Clear;
          Collision.Edges.Clear;
        end loop;
      end;

      -- Load proc file: https://modwiki.xnet.fi/PROC_%28file_format%29
      declare package Parse_Proc is new Parser (Actual_Path & ".proc"); use Parse_Proc;

      -- mapProcFile003
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
      --   }
      --
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
      -- nodes { /* numNodes = */ 463
      -- 
      --   /* node format is: ( planeVector ) positiveChild negativeChild */
      --   /* a child number of 0 is an opaque, solid area */
      --   /* negative child numbers are areas: (-1-child) */
      --   /* node 0 */ ( 1 0 0 0 ) 1 140
      --   /* node 1 */ ( 0 1 0 0 ) 2 34
      --   /* node 2 */ ( -0.7071069479 0.7071066499 0 -45.2549667358 ) 6 8
      -- }
      --
      -- shadowModel { /* name = */ "_prelight_nkd_light_163"
      --
      --   /* numVerts = */ 148 /* noCaps = */ 84 /* noFrontCaps = */ 156 /* numIndexes = */ 228 /* planeBits = */ 59
      --   ( 408 1152 256 ) ( 416 1151 253 ) ( 408 1152 320 ) ( 416 1151 322 ) ( 416 1152 240 ) 
      --   ( 416 1152 240 ) ( 377 1152 256 ) ( 416 1147 240 ) ( 408 1152 256 ) ( 416 1151 253 ) 
      --   ( 416 1152 240 ) ( 416 1152 240 ) ( 416 1152 336 ) ( 416 1152 336 ) ( 416 1152 336 ) 
      --   0 2 1 2 3 1 20 22 21 22 23 21 12 4 5 12 5 13 
      -- }

      -- Internal node structure and vector for convience
      type Internal_Partition_Node_State is record
          Plane    : Plane_4D              := (others => <>);
          Children : Array_Positive (1..2) := (others => 1);
        end record;
      package Vector_Internal_Partition_Node is new Neo.Core.Vectors (Internal_Partition_Node_State);
      use Treed_Partition_Node.Unsafe;
      Nodes : Vector_Internal_Partition_Node.Unsafe.Vector;

      -- Recusive function for building the parition tree
      function Build_Partitions (I : Natural := 1) return Tree is
        A, B : Tree;
        Pos  : Cursor;
        begin

          -- Positive child, then negative child
          for Child of Nodes.Element (I).Children loop 
            if    Child < 0 then A.Append_Child (A.Root, (Area_Partition, True, -(Child + 1)));
            elsif Child = 0 then A.Append_Child (A.Root, (Opaque_Partition, True));
            else
              B   := Build_Partitions (Child + 1);
              Pos := B.Root;
              A.Splice_Subtree (A.Root, NO_ELEMENT, B, Pos);
            end if;
          end loop;
          return A;
        end;

      -- Load the data
      begin
        Assert ("mapProcFile003");

        -- Parse nodes (we dont care about interAreaPortals, shadowModels, or models)...
        while not At_EOF loop
          if Peek /= "nodes" then Skip; Skip_Set ("{", "}");
          else Skip; Assert ("{"); 
            while Peek /= "}" loop Assert ("(");
              Nodes.Append (((Next, Next, Next, Next), (Next, Next))); Assert (")");
            end loop;
          end if;
        end loop;

        -- Build node tree
        Level.Partitions := Build_Partitions;
      end;

      -- Return result which includes all geometry, the BSP tree, and the collision model
      return Level;
    end;
end;
