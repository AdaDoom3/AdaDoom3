
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

-- Doom III style renderer
separate (Neo.Engine.Renderer) package body Doom3 is

  -------------
  -- Shaders --
  -------------
  
  --package Depth_Pass  is new Shader ("depth"); 
  --package Light_Pass  is new Shader ("light");
  --package Fog_Pass    is new Shader ("fog"); 
  --package Sky_Pass    is new Shader ("sky");
  --package UI_Pass     is new Shader ("ui"); 
  --package Post_Pass   is new Shader ("post");
  --package Shadow_Pass is new Shader ("shadow");

  -- Static samplers
  --package Default_Image.Set   ("default");
  --package White_Image.Set     ("white");
  --package Black_Image.Set     ("black");
  --package Fog_Image.Set       ("fog");
  --package Fog_Enter_Image.Set ("fog_enter");
  
  -- Feature switches
  package Enable_Bloom       is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool, Name_ID => "enableBloom"); 
  package Enable_HDR         is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool, Name_ID => "enableHDR"); 
  package Enable_Blur        is new Uniform (Layout_Set => 2, Binding => 4, Uniform_T => Bool, Name_ID => "enableBlur"); 
  package Enable_Haze        is new Uniform (Layout_Set => 2, Binding => 5, Uniform_T => Bool, Name_ID => "enableHaze");  
                          
  -- Uniforms
  package Light_Projection   is new Uniform (Layout_Set => 2, Binding => 14, Uniform_T => Matrix_4D, Name_ID => "lightProjection"); -- S, T, Q
  package Local_Light_Origin is new Uniform (Layout_Set => 2, Binding => 15, Uniform_T => Vector_4D, Name_ID => "localLightOrigin");
  package Local_View_Origin  is new Uniform (Layout_Set => 2, Binding => 16, Uniform_T => Vector_4D, Name_ID => "localViewOrigin");
  package Color_Value        is new Uniform (Layout_Set => 2, Binding => 24, Uniform_T => Vector_4D, Name_ID => "colorValue");
  package Test_Alpha         is new Uniform (Layout_Set => 2, Binding => 17, Uniform_T => Vector_4D, Name_ID => "testAlpha");
  package Light_Falloff      is new Uniform (Layout_Set => 2, Binding => 18, Uniform_T => Vector_4D, Name_ID => "lightFalloff");
  package Light_Scale        is new Uniform (Layout_Set => 2, Binding => 19, Uniform_T => Vector_4D, Name_ID => "lightScale");
  package Screen_Factor      is new Uniform (Layout_Set => 2, Binding => 20, Uniform_T => Vector_4D, Name_ID => "screenFactor");
  package UI_Coord           is new Uniform (Layout_Set => 2, Binding => 21, Uniform_T => Vector_4D, Name_ID => "uiCoord");
  package Diffuse_Modifier   is new Uniform (Layout_Set => 2, Binding => 22, Uniform_T => Vector_4D, Name_ID => "diffuseModifier");
  package Specular_Modifier  is new Uniform (Layout_Set => 2, Binding => 23, Uniform_T => Vector_4D, Name_ID => "specularModifier");
  package Vertex_Color_Mod   is new Uniform (Layout_Set => 2, Binding => 27, Uniform_T => Vector_4D, Name_ID => "vertexColorMod");
  package Vertex_Color_Add   is new Uniform (Layout_Set => 2, Binding => 28, Uniform_T => Vector_4D, Name_ID => "vertexColorAdd");
  package Overbright         is new Uniform (Layout_Set => 2, Binding => 29, Uniform_T => Vector_4D, Name_ID => "overbright");
   
  -----------------
  -- Build_Frame --
  -----------------
 
  procedure Build_Frame (Frame : in out Framebuffer_State; View : View_State) is
 
    -----------
    -- Clear --
    -----------
  
    -- Globals used by clear functions
    --Attachment : aliased VkClearAttachment := (colorAttachment => 0,
    --                                           clearValue      => (depthStencil => (depth   => 1.0,
    --                                                                                stencil => 128,
    --                                                                                others  => <>), others => <>), others => <>);
    --Clear_Rectange : aliased VkClearRect := (baseArrayLayer => 0,
    --                                         layerCount     => 1,
    --                                         rect           => (extent => Swapchain, others => <>), others => <>);
                                             
    -- Various clearning subprograms for convience
    procedure Clear_Color (Value : Color_State) is
      begin null;
        --Attachment.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
        --Attachment.clearValue.color := (Value.Red   / Byte'Last,
        --                                Value.Green / Byte'Last,
        --                                Value.Blue  / Byte'Last,
        --                                Value.Alpha / Byte'Last);
        --vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
      end;
    procedure Clear_Stencil is
      begin null;
        --Attachment.aspectMask := VK_IMAGE_ASPECT_STENCIL_BIT;
        --vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
      end;
    procedure Clear_Depth is
      begin null;
        --Attachment.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
        --vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
      end;
   
    -----------------------
    -- Prepare_Transform --
    -----------------------
   
    function Prepare_Transform (Transform : Transform_4D) return Matrix_4D is
      Result : Matrix_4D := ZERO_MATRIX_4D;
      begin
        return Result;
      end;
  
    --------------------
    -- Update_Scissor --
    --------------------
  
    --procedure Update_Scissor (Scissor : Rectangle) is
    --  begin null;
    --    --if Current_Scissor /= Light.Scissor then
        --  null; -- Update
        --end if;
    --  end;
  
    --------------
    -- Draw_Fog --
    --------------
  
    procedure Draw_Fog (Surfaces : Vector_Bottom_Level_State.Unsafe.Vector) is --, fogPlanes, &Light.inverseBaseLightProject) is
      begin null;
        for Surface of Surfaces loop null;
          --if Surface.Space /= Current_Space then
  
            -- ???
          --  if inverseBaseLightProject = NULL_PTR then
          --    null;
  
            -- ???
          --  else
          --    null;
          --  end if;
          --end if;
          --Draw (Surface);
        end loop;
      end;   
   
    -- Local variables
    Processed         : Int         := 0;
    Light_Depth_Test  : Bool        := False;
    Current_Space     : Space_State := (others => <>);
    Fog_Plane         : Plane_4D    := (others => <>);
    Parameter         : Vector_4D   := ZERO_VECTOR_4D;
    Color             : Vector_4D   := ZERO_VECTOR_4D;
    Projection        : Matrix_4D   := ZERO_MATRIX_4D;
    Light_To_Texture  : Matrix_4D   := ZERO_MATRIX_4D;
    
    -- Start of Draw_Frame
    begin
    
      -----------
      -- Setup --
      -----------
      
      
      ----------------
      -- Fill Depth --
      ----------------
      --
      -- Fill the depth buffer arranged according to visibility
      --
      
--        for Visibility in Visibility_Kind'Range loop
--          for Light of View.Lights (Point_Light) loop
--   
--            -- Translucent interactions fail the mirror clip plane operation - shadow surfaces are ignored for obvious reasons...
--            for Kind in Interaction_Kind loop
--              for Surface of Light.Interactions (Kind) loop
--     
--                -- Draw the surface based on material blending kind
--                case Visibility is
--                
--                  -- Fast path
--                  when Opaque_Visibility => null;
--                  
--                  -- Normal path
--                  when others => null;
--                end case;
--              end loop;
--            end loop;
--          end loop;
--        end loop;
      
      --------------------
      -- Point Lighting --
      --------------------
 
      -- Light the world
      for Lights of View.Lights (Point_Light) loop  
  
        -- Only need to clear the stencil buffer and perform stencil testing if there are shadows
        if Lights.Interactions (Shadow_Interaction).Length /= 0 and not View.Is_Mirror then
          null;
        else
          null;
        end if;
 
        -- Move through the current light's entities in sorted order
        ------------
        -- Shadow --
        ------------
            
        for Light of Lights.Interactions (Shadow_Interaction) loop null;
        end loop;
        
        -----------
        -- Light --
        -----------
        
        for Light of Lights.Interactions (Direct_Interaction) loop null;
        end loop;
      end loop;
      
      ----------------------
      -- Ambient Lighting --
      ----------------------
  
      --for Surface of View.Surfaces loop
      --  if Surface.Is
      --  for Mesh_Surface of 
      --  if Meshes.Get (S (Surface.Mesh)).Material.Is_Ambient and not Surface.From_Menu then
      --    null;
      --  end if;
      --end loop;
  
      ------------------
      -- Fog Lighting --
      ------------------
 
      for Light of View.Lights (Fog_Light) loop
        null;
      end loop;
          
      --------------------
      -- Blend Lighting --
      --------------------
 
      for Light of View.Lights (Blend_Light) loop
        null;
      end loop;
      
      ---------------------
      -- Post Processing --
      ---------------------
      
      
      -----------
      -- Menus --
      -----------
      
      for Surface of View.Surfaces loop
        if Surface.From_Menu then
          null;
        end if;
      end loop;
    end;
    
  ----------------
  -- Build_View --
  ----------------
  
  function Build_View return View_State is
  
    -----------------------
    -- Light check tasks --
    -----------------------
    
    procedure Run_Light_Check is
      begin
        null;
      end;
       
    -------------------
    -- Flood_Portals --
    -------------------
    --
    -- Recursivly find lights and entities by flowing from an origin through visible portals that the view origin can see into
    --
   
    procedure Flood_Portals (Origin : Vector_3D; Planes : in out Vector_Plane_4D.Unsafe.Vector) is
      begin null;
      
        -- If we are outside the world, mark everything
  --        if View.Area < 0 then
  --          for I in 1..Length (Planes) loop
  --            null;
  --          end loop;
  --        else
  --          for Portal of Portals loop
  --   
  --            -- Check the portal isn't a closed door and that it is facing the view
  --            if not Portal.Double_Portal.Is_View_Block and Distance < -0.1 then
  --              Flood_Portals (Origin, New_Portals);
  --            else
  --            
  --              -- Clip the portal winding to all of the planes
  --              for Plane of Portal.Planes loop exit when not Is_Clip_In_Place (Portal.Winding, Plane);
  --   
  --                -- Continue only if the portal is visible and not fogged out
  --                if Portal.Winding.Points.Length /= 0 and Is_Fogged (Portal) then
  --  
  --                  -- Go through the portal
  --                  New_Portals.Append (Portal);
  --    
  --                  for Point of Portal.Winding.Points loop
  --                    null; --R_LocalPointToGlobal (space.modelMatrix, (*w)[i].ToVec3(), v);
  --                    --R_GlobalToNormalizedDeviceCoordinates (v, ndc);
  --    
  --                    -- Generate a set of clipping planes to restrict the view visibility beyond the scissor
  --                    for Point of Winding.Points loop
  --                      null;
  --                    end loop;
  --                  end loop;
  --                end if;
  --              end loop;
  --            end if;
  --          end loop;
  --        end if; 
      end;
      
    Result : View_State := (others => <>);
      
    -- Start of Run_Frontend
    begin
      null;
    
      -- Setup the view matrix
--        Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
--                   Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
--                   Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);
--  
--        const float zNear = ( viewDef->renderView.cramZNear ) ? ( r_znear.GetFloat() * 0.25f ) : r_znear.GetFloat();
--        float ymax = zNear * idMath::Tan( DEG2RAD( viewDef->renderView.fov_y ) * 0.5f );
--        float ymin = -ymax;
--        float xmax = zNear * idMath::Tan( DEG2RAD( viewDef->renderView.fov_x ) * 0.5f );
--        float xmin = -xmax;
--        const float width = xmax - xmin;
--        const float height = ymax - ymin;
--        const int viewWidth = viewDef->viewport.x2 - viewDef->viewport.x1 + 1;
--        const int viewHeight = viewDef->viewport.y2 - viewDef->viewport.y1 + 1;
--  
--        viewDef->projectionMatrix[0*4+0] = 2.0f * zNear / width;
--        viewDef->projectionMatrix[1*4+0] = 0.0f;
--        viewDef->projectionMatrix[2*4+0] = 0.0f;
--        viewDef->projectionMatrix[3*4+0] = 0.0f;
--        viewDef->projectionMatrix[0*4+1] = 0.0f;
--        viewDef->projectionMatrix[1*4+1] = -2.0f * zNear / height;
--        viewDef->projectionMatrix[2*4+1] = 0.0f;
--        viewDef->projectionMatrix[3*4+1] = 0.0f;
--  
--        -- this is the far-plane-at-infinity formulation, and
--        -- crunches the Z range slightly so w=0 vertexes do not
--        -- rasterize right at the wraparound point
--  
--        viewDef->projectionMatrix[0*4+2] = 0.0f;
--        viewDef->projectionMatrix[1*4+2] = 0.0f;
--        viewDef->projectionMatrix[2*4+2] = -0.999f; // adjust value to prevent imprecision issues
--        viewDef->projectionMatrix[3*4+2] = -1.0f * zNear;
--        viewDef->projectionMatrix[0*4+3] = 0.0f;
--        viewDef->projectionMatrix[1*4+3] = 0.0f;
--        viewDef->projectionMatrix[2*4+3] = -1.0f;
--        viewDef->projectionMatrix[3*4+3] = 0.0f;
--        if ( viewDef->renderView.flipProjection ) {
--        	viewDef->projectionMatrix[1*4+1] = -viewDef->projectionMatrix[1*4+1];
--        }
      
      -- Walk the BSP tree to find the area where the view origin is
--        BSP := View.Level.Partitions.Get;
--        Pos := BSP.Root;
--        loop
--          Node := Get (Pos);
--          case Node.Kind is
--            when Area_Partition   => Area := Node.Area_Id; exit;
--            when Opaque_Partition => raise In_Solid;
--            when Normal_Partition => Pos := (if View.Origin * Normal (Node.Plane) + Node.Plane.W > 0 then First (Pos) else Last (Pos));
--          end case;
--        end loop;
--  
--        -- Flood through portals
--        Flood_Portals (Origin, Area, View.Planes);
--  
--        -- Reset the amount of light jobs if changed (this will need tuning)
--        if    Length (View.Lights) < Length (Light_Jobs)                       then Light_Jobs.Append (new Vector_Lights.Unsafe.Vector (Length (View.Lights) - Length (Light_Jobs)));
--        elsif Length (View.Lights) + MAX_IDLE_LIGHT_JOBS > Length (Light_Jobs) then Light_Jobs.Remove (1..Length (View.Lights) + MAX_IDLE_LIGHT_JOBS - Length (Light_Jobs)); end if;
--  
--        -- Kick-off light projection and searching for lit models then wait for them to finish
--        for I in 1..Length (View.Lights) loop
--          Light_Jobs.Element (I).Do_Pass (View.Lights.Element (I));
--        end loop;
--        for Light_Job of Light_Jobs loop
--          Light_Job.Confirm_Completion;
--        end loop;
--  
--  --         -- Determine all possible connected areas for light-behind-door culling
--  --         if Area := OUTSIDE_AREA then View.Connected_Areas := (others => True)
--  --         else View.Level.Portals.Iterate_Subtree (Flood_Portals); end if;
--  
--        -- R_SortDrawSurfs            -- A simple C qsort call. C++ sort would have been faster thanks to inlining.
--        -- R_Generate1024 ** 2s
--        -- R_AddDrawViewCmd
      return Result;   
    end;
 end;
 
 
 
 
 
 
 
 
 


































--  
--    -- Amount of idle frontend light job tasks that are allowed to sit idle until tasks start being destroyed
--    MAX_IDLE_LIGHT_JOBS : constant Positive := 10;
--    FOG_SCALE           : constant Float    := 0.001;
--  
--    -- ???
--    Area : ;
--    BSP  : ;
--    Pos  : ;
--  
--    --
--    --
--    --
--  
--      begin
--        loop
--          View.View_Entities.Append (Render_Lights.Shadow_Only_View_Entities);
--  
--          -- Evaluate light shader registers
--  
--          -- Additve lights with no stage in the light shader is positive, remove the light
--          if Light.Kind := Fog_Light or Light.Kind := Blend_Light then
--            Outter: for Stage of Light.Stages loop
--              if Light.Stage.Registers.Condition_Register then
--  
--                -- Snap tiny values to zero
--                for Register of Light.Stage.Registers loop
--                  if Register < 0.001 then Register := 0.0; end if;
--                end loop;
--  
--                -- We found a light that add something
--                for Register of Light.Stage.Registers loop
--                  exit Outter when Register > 0.0;
--                end loop;
--              end if;
--              if Stage := Light.Stages.Last then
--  
--  -- We didn't find any addition to the scene
--  goto Skip_Light;
--            end if;
--          end loop Outter;
--        end if;
--  
--        -- Copy data to backend
--  
--        -- Create a "fog plane" to represent light far clip plane
--  
--        -- Calculate the matrix that projects the zero-to-one cube to exactly cover the light frustum in clip space
--  
--        -- Calculate the project bounds, either not clipped at all, near clipped, or fully clipped
--  
--        -- Build the scissor
--  
--        -- Create interactions with all entities the light may touch and add entities that may cast shadows
--        for Reference of Light.References loop
--  
--          -- Some lights have their center of projection outside of the world so ignore areas not connected to the light center
--          if not Light.World.Areas_Are_Connected (Light.Area, Reference.Area, PS_BLOCK_VIEW) then
--  
--            -- Check the models in this area
--            for Model of Reference.Entities loop
--              if not (vLight.entityInteractionState[ edef.index ] /= viewLight_t::INTERACTION_UNCHECKED or else
--  
--                -- A large fraction of static entity / light pairs will still have no interactions even though they are both present in the same area(s)
--                eModel /= NULL && !eModel.IsDynamicModel && inter = INTERACTION_EMPTY or else
--  
--                  -- We don't want the lights on weapons to illuminate anything else. There are two assumptions here - that
--                  -- allowLightInViewID is only used for weapon lights, and that all weapons will have weaponDepthHack. A more general
--                  -- solution would be to have an allowLightOnEntityID field.
--                  light.parms.allowLightInViewID && light.parms.pointLight && !eParms.weaponDepthHack or else
--  
--                  --
--                  (eParms.noShadow or ( eModel && !eModel.ModelHasShadowCastingSurfaces ) ) && !edef.IsDirectlyVisible or else
--                  eModel && !eModel.ModelHasInteractingSurfaces && !eModel.ModelHasShadowCastingSurfaces or else
--                  inter = NULL and (eParms.noDynamicInteractions or R_CullModelBoundsToLight( light, edef.localReferenceBounds, edef.modelRenderMatrix )) or else
--                  edef.IsDirectlyVisible
--                  !lightCastsShadows
--              then
--  
--              end if;
--            end loop;
--          end if;
--        end loop;
--      end;
--  
--    -------------------
--    -- Flood_Portals --
--    -------------------
--    --
--    -- Recursivly find lights and entities by flowing from an origin through visible portals that the view origin can see into
--    --
--  
--    procedure Flood_Portals (Origin : Vector_3D; Planes : in out Vector_Plane_4D.Unsafe.Vector) is
--      portalStack_t ps;
--      ps.next := NULL;
--      ps.p := NULL;
--      idScreenRect r;
--      idVec3 v;
--      idVec3 ndc;
--      begin
--        Portals.Planes := Planes;
--        Portals.Rectangle := View.Scissor;
--  
--        -- If we are outside the world, mark everything
--        if View.Area < 0 then
--          for I in 1..Length (Planes) loop
--            Area_Screen_Rect (I) := View.Scissor;
--            Portals.Add_Area_To_View (Portal)
--          end loop;
--        else
--          portalArea_t * area := &portalAreas[ areaNum ];
--  
--          -- Cull models and lights to the current collection of planes
--          AddAreaToView ( areaNum, ps);
--          Area_Screen_Rect (Area) := (if Is_Empty (Area_Screen_Rect (Area)) then Portals.Rect;
--                                      else Area_Screen_Rect (Area) := Join (Portals.Rect, Area_Screen_Rect (Area)));
--  
--          -- Go through all the portals
--          for Portal of Portals loop
--  
--            -- Check the portal isn't a closed door and that it is facing the view
--            Distance := Portal.Plane.Distance (Origin);
--            if not Portal.Double_Portal.Is_View_Block and Distance < -0.1 then
--  
--              -- If we are very close to the portal surface, avoid clipping that may cause areas to vanish
--              if Distance < 1.0 then
--                New_Portals := Portals;
--                New_Portals.Append (Portal);
--                Flood_View_Through_Portals (Origin, New_Portals);
--              else
--  
--                -- Clip the portal winding to all of the planes
--                for Plane of Portal.Planes loop exit when not Is_Clip_In_Place (Portal.Winding, Plane); end loop;
--  
--                -- Continue only if the portal is visible and not fogged out
--                if Portal.Winding.Points.Length /= 0 and Is_Fogged (Portal) then
--  
--                  -- Go through the portal
--                  New_Portals.Append (Portal);
--  
--                  -- Scissor things outside the screen pixel bounding box
--                  New_Portals.Rect := Screen_Rect_From_Winding (Portal.Winding, Identity_Space);
--                  View_Width  := View.Port.X2 - View.Port.X1;
--                  View_Height := View.Port.Y2 - View.Port.Y1;
--                  for Point of Portal.Winding.Points loop
--                    R_LocalPointToGlobal (space.modelMatrix, (*w)[i].ToVec3(), v);
--                    R_GlobalToNormalizedDeviceCoordinates (v, ndc);
--                    Screen_Rect.Points.Append (X => (ndc[0] * 0.5 + 0.5) * viewWidth,
--                                               Y => (ndc[1] * 0.5 + 0.5) * viewHeight);
--                  end loop;
--                  r.Expand;
--  
--                  -- Slop might have spread it a pixel outside, so trim it back
--                  New_Portals.Rect.Intersect (Portals.Rect);
--  
--                  -- Generate a set of clipping planes to restrict the view visibility beyond the scissor
--                  for Point of Winding.Points loop
--                    newStack.portalPlanes[newStack.numPortalPlanes].Normal().Cross( origin - w[j].ToVec3(), origin - w[i].ToVec3());
--  
--                    -- Skip degenerates
--                    if newStack.portalPlanes[newStack.numPortalPlanes].Normalize() >= 0.01f then
--                      newStack.portalPlanes[newStack.numPortalPlanes].FitThroughPoint( origin);
--                    end if;
--                  end loop;
--                end if;
--              end if;
--            end if;
--  
--            --
--            newStack.portalPlanes[newStack.numPortalPlanes] := p.plane;
--            Flood_View_Through_Portals (origin, Portal.Into_Area, New_Portals);
--          end loop;
--        end if;
--      end;
--  
--    -- Start of Frontend
--    begin
--      for View of Views loop
--  
--        -- Setup the view matrix
--        Viewer := (Axis.XX, Axis.YX, Axis.ZX, -Origin.X * Axis.XX - Origin.Y * YX - Origin.Z * Axis.ZX,
--                   Axis.XY, Axis.YY, Axis.ZY, -Origin.X * Axis.XY - Origin.Y * YY - Origin.Z * Axis.ZY,
--                   Axis.XZ, Axis.YZ, Axis.ZZ, -Origin.X * Axis.XZ - Origin.Y * YZ - Origin.Z * Axis.ZZ);
--  
--        const float zNear = ( viewDef->renderView.cramZNear ) ? ( r_znear.GetFloat() * 0.25f ) : r_znear.GetFloat();
--        float ymax = zNear * idMath::Tan( DEG2RAD( viewDef->renderView.fov_y ) * 0.5f );
--        float ymin = -ymax;
--        float xmax = zNear * idMath::Tan( DEG2RAD( viewDef->renderView.fov_x ) * 0.5f );
--        float xmin = -xmax;
--        const float width = xmax - xmin;
--        const float height = ymax - ymin;
--        const int viewWidth = viewDef->viewport.x2 - viewDef->viewport.x1 + 1;
--        const int viewHeight = viewDef->viewport.y2 - viewDef->viewport.y1 + 1;
--  
--        viewDef->projectionMatrix[0*4+0] = 2.0f * zNear / width;
--        viewDef->projectionMatrix[1*4+0] = 0.0f;
--        viewDef->projectionMatrix[2*4+0] = 0.0f;
--        viewDef->projectionMatrix[3*4+0] = 0.0f;
--        viewDef->projectionMatrix[0*4+1] = 0.0f;
--        viewDef->projectionMatrix[1*4+1] = -2.0f * zNear / height;
--        viewDef->projectionMatrix[2*4+1] = 0.0f;
--        viewDef->projectionMatrix[3*4+1] = 0.0f;
--  
--        -- this is the far-plane-at-infinity formulation, and
--        -- crunches the Z range slightly so w=0 vertexes do not
--        -- rasterize right at the wraparound point
--  
--        viewDef->projectionMatrix[0*4+2] = 0.0f;
--        viewDef->projectionMatrix[1*4+2] = 0.0f;
--        viewDef->projectionMatrix[2*4+2] = -0.999f; // adjust value to prevent imprecision issues
--        viewDef->projectionMatrix[3*4+2] = -1.0f * zNear;
--        viewDef->projectionMatrix[0*4+3] = 0.0f;
--        viewDef->projectionMatrix[1*4+3] = 0.0f;
--        viewDef->projectionMatrix[2*4+3] = -1.0f;
--        viewDef->projectionMatrix[3*4+3] = 0.0f;
--        if ( viewDef->renderView.flipProjection ) {
--        	viewDef->projectionMatrix[1*4+1] = -viewDef->projectionMatrix[1*4+1];
--        }
--  }
--        -- Setup render matricies for faster culliong
--        View.Render_Projection := Transpose (View.Projection);
--        View.World_Space.MVP := View.Render_Projection * Transpose (View.World_Space.Model_View);
--        View.Frustum := ((4)(3) => Z_Near.Get, others => -Get_Frustum_Planes (View.World_Space.MVP));
--  
--        -- Walk the BSP tree to find the area where the view origin is
--        BSP := View.Level.Partitions.Get;
--        Pos := BSP.Root;
--        loop
--          Node := Get (Pos);
--          case Node.Kind is
--            when Area_Partition   => Area := Node.Area_Id; exit;
--            when Opaque_Partition => raise In_Solid;
--            when Normal_Partition => Pos := (if View.Origin * Normal (Node.Plane) + Node.Plane.W > 0 then First (Pos) else Last (Pos));
--          end case;
--        end loop;
--  
--        -- Flood through portals
--        Flood_Portals (Origin, Area, View.Planes);
--  
--        -- Reset the amount of light jobs if changed (this will need tuning)
--        if    Length (View.Lights) < Length (Light_Jobs)                       then Light_Jobs.Append (new Vector_Lights.Unsafe.Vector (Length (View.Lights) - Length (Light_Jobs)));
--        elsif Length (View.Lights) + MAX_IDLE_LIGHT_JOBS > Length (Light_Jobs) then Light_Jobs.Remove (1..Length (View.Lights) + MAX_IDLE_LIGHT_JOBS - Length (Light_Jobs)); end if;
--  
--        -- Kick-off light projection and searching for lit models then wait for them to finish
--        for I in 1..Length (View.Lights) loop
--          Light_Jobs.Element (I).Do_Pass (View.Lights.Element (I));
--        end loop;
--        for Light_Job of Light_Jobs loop
--          Light_Job.Confirm_Completion;
--        end loop;
--  
--  --         -- Determine all possible connected areas for light-behind-door culling
--  --         if Area := OUTSIDE_AREA then View.Connected_Areas := (others => True)
--  --         else View.Level.Portals.Iterate_Subtree (Flood_Portals); end if;
--  
--        -- R_SortDrawSurfs            -- A simple C qsort call. C++ sort would have been faster thanks to inlining.
--        -- R_Generate1024 ** 2s
--        -- R_AddDrawViewCmd
--      end loop;
--    end;
--  
--  
--  -- Shadow stencil shading with prenumbra: https://web.archive.org/web/20160417154820/http://www.terathon.com/gdc05_lengyel.pdf
--  separate (Neo.Engine.Renderer) procedure Doom3 is
--  
--    -----------------------
--    -- Prepare_Transform --
--    -----------------------
--  
--    function Prepare_Transform (Transform : Transform_4D) return Matrix_4D is
--      Result : Matrix_4D := ZERO_MATRIX_4D;
--      begin
--  
--        -- ???
--        Set_Matrix_4D_X (Result, (Transform.XX, Transform.XY, 0.0, Transform.XZ));
--        Set_Matrix_4D_Y (Result, (Transform.YX, Transform.YY, 0.0, Transform.YZ));
--  
--        -- Keep scrolls from making large values, but handle center rotations and scales which make offsets > 1
--        if Result.XW < -40.0 or Result.XW > 40.0 then Result.XW := Result.XW - Real_64 (Int (Result.XW)); end if;
--        if Result.YW < -40.0 or Result.YW > 40.0 then Result.YW := Result.YW - Real_64 (Int (Result.YW)); end if;
--  
--        return Result;
--      end;
--  
--    --------------------
--    -- Update_Scissor --
--    --------------------
--  
--    procedure Update_Scissor (Scissor : Rectangle) is
--      begin
--        if Current_Scissor /= Light.Scissor then
--          vkCmdSetScissor (Commands, 0, 1, (Extent => (Width  => View.Port.X1 + View.Scissor.X1,
--                                                       Height => View.Port.Y1 + View.Scissor.Y1),
--                                            Offset => (X => View.scissor.X2 + 1 - View.Scissor.X1,
--                                                       Y => View.scissor.Y2 + 1 - View.Scissor.Y1)));
--          Current_Scissor := Light.Scissor;
--        end if;
--      end;
--  
--    --------------
--    -- Draw_Fog --
--    --------------
--  
--    procedure Draw_Fog (Surfaces : Vector_Surface.Unsafe.Vector, fogPlanes, &Light.inverseBaseLightProject) is
--      idPlane localFogPlanes[4];
--      begin
--        Current_Space := (others => <>);
--        for Surface of Surfaces loop
--          Update_Scissor (Surface.Scissor);
--          if Surface.Space /= Current_Space then
--  
--            -- ???
--            if inverseBaseLightProject = NULL_PTR then
--              MVP.Set (Surface.space.mvp);
--              Global_Plane_To_Local (Surface.space.Model, fogPlanes, localFogPlanes);
--  
--            -- ???
--            else
--              idRenderMatrix invProjectMVPMatrix;
--              MVP.Set (inverseBaseLightProject * View.worldSpace.mvp);
--              inverseBaseLightProject.InverseTransformPlane( fogPlanes, localFogPlanes, false );;
--            end if;
--            TEXGEN_0_S.Set (localFogPlanes[0].ToFloatPtr() );
--            TEXGEN_0_T.Set (localFogPlanes[1].ToFloatPtr() );
--            TEXGEN_1_T.Set (localFogPlanes[2].ToFloatPtr() );
--            TEXGEN_1_S.Set (localFogPlanes[3].ToFloatPtr() );
--            Current_Space := (if inverseBaseLightProject = NULL then Surface.space else NULL);
--          end if;
--          Draw (Surface);
--        end loop;
--      end;
--  
--    -----------
--    -- Clear --
--    -----------
--  
--    Attachment : aliased VkClearAttachment := (colorAttachment => 0,
--                                               clearValue      => (depthStencil => (depth   => 1.0,
--                                                                                    stencil => 128,
--                                                                                    others  => <>), others => <>), others => <>);
--    Clear_Rectange : aliased VkClearRect := (baseArrayLayer => 0,
--                                             layerCount     => 1,
--                                             rect           => (extent => Swapchain, others => <>), others => <>);
--    procedure Clear_Color (Value : Color_State) is
--      begin
--        Attachment.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
--        Attachment.clearValue.color := (Value.Red   / Byte'Last,
--                                        Value.Green / Byte'Last,
--                                        Value.Blue  / Byte'Last,
--                                        Value.Alpha / Byte'Last);
--        vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
--      end;
--    procedure Clear_Stencil is
--      begin
--        Attachment.aspectMask := VK_IMAGE_ASPECT_STENCIL_BIT;
--        vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
--      end;
--    procedure Clear_Depth is
--      begin
--        Attachment.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
--        vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
--      end;
--  
--    -- Local variables
--    Processed         : Int         := 0;
--    GUI_Screen_Offset :  := ;
--    Light_Depth_Test  : Bool        := False;
--    Current_Space     : Space_State := (others => <>);
--    Fog_Plane         : Plane_4D    := (others => <>);
--    Parameter         : Vector_4D   := ZERO_VECTOR_4D;
--    Color             : Vector_4D   := ZERO_VECTOR_4D;
--    Current_Space     : Matrix_4D   := ZERO_MATRIX_4D;
--    Projection        : Matrix_4D   := ZERO_MATRIX_4D;
--    Light_To_Textur   : Matrix_4D   := ZERO_MATRIX_4D;
--    Joint_Buffer      : Vector_Mesh.Unsafe.Vector;
--    
--  -- Start of backend
--  begin
--  
--  
--    -- Main loop
--    loop
--  
--      -----------
--      -- Setup --
--      -----------
--  
--      -- Wait for the frontend to give us something new
--      - (Clock - Last_Time); Last_Time := Clock;
--      while Last_View_Update.Get < Last_View_Render.Get loop delay FRONTEND_WAIT_DURATION; end loop;
--      View := Common_View.Get;
--  
--      -- Update dynamic viewport state and window clipping
--      vkCmdSetViewport (Commands, 0, 1, (Height    => View.Port.y2 + 1 - View.Port.y1,
--                                         Width     => View.Port.x2 + 1 - View.Port.x1,
--                                         Min_Depth => 0.0,
--                                         Max_Depth => 1.0));
--  
--      -- Update dynamic scissor which may be smaller than the viewport for 1024 ** 2s
--      Update_Scissor (View.Scissor);
--  
--      -- Uniforms
--      Overbright.Set      (lightScale.GetFloat * 0.5);
--      Projection.Set      (Transpose (View.Projection), 4); -- X ???
--      Local_To_Global.Set (View.Origin.X, View.Origin.Y, View.Origin.Z, 1.0));
--  
--      -----------
--      -- Depth --
--      -----------
--      --
--      -- ???
--      --
--  
--      -- Setup pass
--      Clear_Depth;
--      Clear_Stencil;
--      Depth_Pass.Commit;
--      Pipeline := (others => <>);
--  
--      -- Fill the depth buffer
--      for Visibility in _Visibility..Perforated_Visibility loop
--        for Light of View.Lights (Point_Light) then
--  
--          -- Translucent interactions fail the mirror clip plane operation - shadow surfaces are ignored for obvious reasons...
--          for Surface of Light.Interactions (Direct_Interaction) loop
--  
--              -- Change the MVP matrix if needed
--              if Surface.Space /= Current_Space then
--                MVP.Set (Surface.Space.MVP);
--                Current_Space := Surface.Space;
--              end if;
--  
--              -- Draw the surface based on material blending kind
--              case Visibility is
--  
--                -- Solid geometry fast path
--                when Opaque_Visibility => 
--                  if Surface.Material.Domain = Surface_Domain then Clear_Color (COLOR_WHITE);
--                  else
--                    Pipeline.Depth_Pass_Kind := ;
--                    Pipeline.Alpha_Mask      := VK_COLOR_COMPONENT_A_BIT;
--                  end if;
--                  Draw (Surface);
--  
--                -- Other perforated or 1024 ** 2 surfaces
--                when others =>
--  
--                  -- 1024 ** 2s may down-modulate the color buffer, otherwise black is drawn
--                  if Visibility = 1024 ** 2_Visibility then
--                    Pipeline.Source_Blend_Factor      := VK_BLEND_FACTOR_DST_COLOR;
--                    Pipeline.Destination_Blend_Factor := VK_BLEND_FACTOR_ZERO;
--                    Pipeline.Depth_Compare            := VK_COMPARE_OP_LESS_OR_EQUAL;
--                    --Color                             := To_Vulkan (COLOR_BLACK);
--                  end if;
--  
--                  -- Set privatePolygonOffset if necessary and set the alpha modulate
--                  Pipeline.Test_Alpha := Has_Alpha_Channel (Surface.Material.Base_Color);
--                  Color_Mod.Set (zero);
--                  Color_Add.Set (one);
--                  Clear_Color (Surface.Material.Color_Mod);
--  
--                  -- Set texture matrix and texture generators
--                  shaderRegisters; RB_LoadShaderTextureMatrix (Surface.shaderRegisters, &pStage.texture);
--                  case Stage.Material.Cube_Map is
--                    when Mirror_Cube_Map => Normal_Image.Set (Surface.Material.Normal);
--                    when Skybox_Cube_Map => SkyBox_Pass.Commit;
--                  when others => null; end case;
--  
--                  -- Push the surface
--                  Draw (Surface);
--  
--                  -- Cleanup
--                  if Surface.Material.Polygon_Offset /= 0.0 then
--                    vkCmdSetDepthBias (Commands, r_offsetUnits.GetFloat * shader.GetPolygonOffset, 0.0, r_offsetFactor.GetFloat);
--                  end if;
--                end case;
--              end case;
--            end if;
--          end loop;
--  
--          -- ???
--          Test_Alpha.Set (ZERO_VECTOR_4D);
--        end loop;
--      end loop;
--  
--      --------------------
--      -- Stencil Shadow --
--      --------------------
--      --
--      -- ???
--      --
--  
--      -- Setup pass
--      Pipeline := (others => <>);
--  
--      -- Light the world
--      for Light of View.Lights loop
--  
--        -- Skip fog and blend lights
--        if Light.Shader.Kind /= Fog_Kind and Light.Shader.Kind /= Blend_Kind then
--  
--  
--          -- Only need to clear the stencil buffer and perform stencil testing if there are shadows
--          if Light.Global_Shadows.Length /= 0 and Light.Local_Shadows.Length /= 0 and not View.Is_Mirror then
--  
--            -- Clear to zero for stencil select and set the depth bounds for the whole light
--            Clear_Stencil;
--            Update_Scissor (Light.Scissor);
--            vkCmdSetDepthBounds (Commands, Light.Scissor.Z_Min, Light.Scissor.Z_Max);
--  
--            -- Setup shader
--            .Set (View.World_Space.MVP * Light.Inverse_Projection);
--            Shadow_Pass.Commit;
--  
--            -- Setup pipeline
--            Pipeline.Stencil_Test_Enable    := False;
--            Pipeline.Alpha_Blend            := VK_COLOR_COMPONENT_A_BIT;
--            Pipeline.Depth_Write_Enable     := True;
--            Pipeline.Depth_Compare          := VK_COMPARE_OP_LESS_OR_EQUAL;
--            Pipeline.Stencil_Compare        := VK_COMPARE_OP_ALWAYS;
--            Pipeline. := GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE;
--            Pipeline. := GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE;
--            Pipeline. := qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_REPLACE, GL_ZERO;
--            Pipeline.Cull                   := VK_CULL_MODE_NONE;
--            Pipeline.Back_Stencil_Operation := qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_ZERO, GL_REPLACE;     
--  
--            -- Draw the deformed Zero_One_Cube_Model into the frustum to exactly cover the light volume
--            Draw (ZERO_ONE_CUBE_MESH);
--  
--            -- Cleanup     
--            vkCmdSetDepthBounds (Commands, 0.0, 0.0); 
--  
--          -- ???
--          else
--  
--            -- Set the depth bounds for the whole light
--            Clear_Stencil (STENCIL_SHADOW_TEST_VALUE);
--            if Light.Scissor.Z_Min /= 0.0 and Light.Scissor.Z_Max /= 0.0 then
--               vkCmdSetDepthBounds (Commands, Light.Scissor.Z_Min, Light.Scissor.Z_Max);
--            end if;
--  
--            -- Always clear whole Cull tiles
--            Update_Scissor ((x1 => (Light.Scissor.X1 +  0) and not 15, y1 => (Light.Scissor.Y1 +  0) and not 15,
--                             x2 => (Light.Scissor.X2 + 15) and not 15, y2 => (Light.Scissor.Y2 + 15) and not 15););
--  
--            -- Make sure stencil mask passes for the clear
--            Pipeline := (others => <>);
--          end if;
--  
--          -- Begin moving through the Light's entities in sorted order
--          for Interaction of Light.Interactions loop
--            case Pass_Sort is
--  
--              -----------------
--              -- Shadow Pass --
--              -----------------
--              --
--              -- ???
--              --
--  
--              when Shadow_Sort'Range => 
--  
--                -- Setup pipeline
--                Clear_Texture (0);
--                Shadow_Pass.Commit;            
--                vkCmdSetDepthBias (Commands, -shadowPolygonOffset, 0.0, shadowPolygonFactor);
--                Pipeline.Depth_Write_Enable := True; -- Only write to the stencil buffer ignoring the color or depth buffer
--                Pipeline.Alpha_Mask         := VK_COLOR_COMPONENT_A_BIT 
--                Pipeline.Depth_Compare      := VK_COMPARE_OP_LESS_OR_EQUAL;
--                Pipeline.Cull               := VK_CULL_MODE_NONE; -- Two Sided Stencil reduces two draw calls to only one
--                Pipeline.Stencil_Fail       := VK_STENCIL_OP_KEEP;
--                Pipeline.Stencil_Z_Fail     := VK_STENCIL_OP_KEEP;
--                Pipeline.Stencil_Pass       := VK_STENCIL_OP_INCREMENT_AND_CLAMP; 
--                Pipeline. := GLS_STENCIL_MAKE_REF STENCIL_SHADOW_TEST_VALUE;
--                Pipeline. := GLS_STENCIL_MAKE_MASK STENCIL_SHADOW_MASK_VALUE;          
--  
--                -- Process the chain of shadows with the current rendering state
--                Current_Space := (others => <>);
--                for Surface of View.Surfaces loop
--  
--                  -- Update the scissor
--                  Update_Scissor (Surface.Scissor);
--  
--                  -- change the matrix and set the local light position to allow the vertex program to project the shadow volume end cap to infinity
--                  if Current_Space /= Surface.Space then
--                    MVP.Set (Surface.space.mvp);
--                    Local_Light_Origin.Set (Global_To_Local (Surface.space.Model, Light.Origin));
--                    Current_Space := Surface.space;
--                  end if;
--  
--                  -- Preload plus Z-pass
--                  GL_SeparateStencil( STENCIL_FACE_FRONT, GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_DECR | GLS_STENCIL_OP_PASS_DECR );
--                  GL_SeparateStencil( STENCIL_FACE_BACK, GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_INCR | GLS_STENCIL_OP_PASS_INCR );
--                  qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : Surface.numIndexes, GL_INDEX_TYPE,
--                    (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof( idShadowVertSkinned ));
--  
--                  -- Render again with Z-pass
--                  qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
--                  qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
--                  qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : Surface.numIndexes, GL_INDEX_TYPE,
--                    (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof ( idShadowVert ));
--                end case;
--  
--                -- Cleanup the shadow specific rendering state and reset depth bounds
--                Pipeline. := CT_FRONT_SIDED; -- Two Sided Stencil reduces two draw calls to only one
--                GL_DepthBoundsTest( Light.Scissor.zmin, Light.Scissor.zmax);
--  

--              ----------------
--              -- Light Pass --
--              ----------------
--              --
--              --
--              --
--  
--              when Light_Sort'Range =>
--                for Interaction of Light.Interactions loop if Light.Interactions (Kind).Length > 0 then
--  
--                  -- Load the pipeline state
--                  if Interaction.Kind = Translucent_Interaction then
--                    Pipeline.Depth_Compare := VK_COMPARE_OP_LESS_OR_EQUAL;
--                    Pipeline.Test_Depth    := False; -- Disable the depth bounds test on translucent surfaces as they don't write depth
--                    Pipeline. := false;
--                    Pipeline. := false;                  
--                  else
--                    Pipeline.Depth_Compare := VK_COMPARE_OP_EQUAL;
--                    Pipeline.Test_Stencil  := performStencilTest;
--                    Pipeline. := r_useLightDepthBounds.Get;
--                  end if;
--                  Pipeline.Source_Blend      := VK_BLEND_FACTOR_ONE;
--                  Pipeline.Destination_Blend := VK_BLEND_FACTOR_ONE;
--                  Pipeline.Test_Depth        := False;
--                  Pipeline.Stencil_Compare   := VK_COMPARE_OP_EQUAL;
--                  Pipeline. := GLS_STENCIL_MAKE_REFSTENCIL_SHADOW_TEST_VALUE;
--                  Pipeline. := GLS_STENCIL_MAKE_MASKSTENCIL_SHADOW_MASK_VALUE;
--                  Update_Scissor (Light.Scissor);
--  
--                  -- Load the light material transform
--                  Light_Texture := Prepare_Map_Transform (Light.Material.Transform);
--                  Set_Matrix_4D_Z      (Light_Texture, (0.0, 0.0, 1.0, 0.0));
--                  Set_Matrix_4D_W      (Light_Texture, (0.0, 0.0, 1.0, 0.0));
--                  Fall_Off_Image.Set   (INTERACTION_TEXUNIT_FALLOFF);
--                  Projection_Image.Set (Light.Material.texture); -- light projection texture
--                  Vertex_Color_Mod.Set (zero);
--                  Vertex_Color_Add.Set (one);
--  
--                  -- Even if the space does not change between light stages, each light stage may need a different lightTextureMatrix baked in
--                  Current_Space := NULL;
--  
--                  -- For all surfaces on this light list, generate an interaction for this light stage
--                  for Surface of Sorted_Surfaces loop
--  
--                    -- Select the render program
--                    Light_Pass.Commit;
--  
--                    -- Change the MVP matrix, view/light origin and light projection vectors if needed
--                    if Surface.Space /= Current_Space then
--                      Current_Space := Surface.Space;
--  
--                      -- turn off the light depth bounds test if this model is rendered with a depth hack
--                      if Do_Bound_Light_Depth then
--                        if not Surface.Space.weaponDepthHack and Surface.Space.modelDepthHack = 0.0 and lightDepthBoundsDisabled then
--                          vkCmdSetDepthBounds (Commands, Light.Scissor.zmin, Light.Scissor.zmax);
--                          Pipeline.Test_Depth      := False;
--                          lightDepthBoundsDisabled := false;
--                        elsif not lightDepthBoundsDisabled then
--                          Pipeline.Test_Depth      := False;
--                          lightDepthBoundsDisabled := true;
--                        end if;
--                      end if;
--  
--                      -- tranform the light/view origin into model local space set the local light/view origin
--                      Local_Light_Origin := (others => 0.0);
--                      Local_View_Origin  := (others => 1.0);
--                      Global_Point_To_Local (Surface.Space.Model, Light.Global_Light_Origin, Local_Light_Origin);
--                      Global_Point_To_Local (Surface.Space.Model, View.Render.Origin,        Local_View_Origin);
--  
--                      -- Multiply the local light projection by the light texture matrix
--                      Set_Matrix_4D_X (Final, Get_Matrix_4D_X (Light_Projection));
--                      Set_Matrix_4D_Y (Final, Get_Matrix_4D_Y (Light_Projection));
--                      Set_Matrix_4D_Z (Final, (others => 0.0));
--                      Set_Matrix_4D_W (Final, Get_Matrix_4D_Z (Light_Projection));
--                      Final := Light_Texture * Final;
--  
--                      -- Transform the light project into model local space
--                      Projection := Global_To_Local (Surface.Space.Model, Light.Projection);
--  
--                      Light_Projection(0)(0) := final(0*4+0); Light_Projection(0)(1) = final(1*4+0); Light_Projection(0)(2) = final(2*4+0); Light_Projection(0)(3) = final(3*4+0);
--                      Light_Projection(1)(0) := final(0*4+1); Light_Projection(1)(1) = final(1*4+1); Light_Projection(1)(2) = final(2*4+1); Light_Projection(1)(3) = final(3*4+1);
--  
--                      -- Load uniforms Set the light projection
--                      Light_Projection.Set   (To_Matrix_3D (Light_Projection)); -- Light_Projection
--                      Light_Falloff.Set      (Light_Projection); -- LIGHTFALLOFF_S
--                      Light_Scale.Set        (Light_Scale.Get);
--                      Vertex_Color.Set       (Surface.Material.Vertex_Color);
--                      Image_Transform.Set    (Prepare_Map_Transform (Surface.Material.Transform));
--                      MVP.Set                (Surface.Space.MVP);
--                      Projection_Image.Set   (Surface.Material.);
--                      Base_Color_Image.Set   (Surface.Material.Base_Color);
--                      Irradiance_Image.Set   (Surface.Material.Irradiance);
--                      Prefilter_Image.Set    (Surface.Material.Prefilter);
--                      Specular_Image.Set     (Surface.Material.Specular);
--                      Normal_Image.Set       (Surface.Material.Normal);
--                      Metallic_Image.Set     (Surface.Material.Metallic);
--                      Roughness_Image.Set    (Surface.Material.Roughness);
--                      Displacement_Image.Set (Surface.Material.Displacement);
--                      case vertexColor is
--                        when SVC_IGNORE =>
--                          RENDERPARM_VERTEXCOLOR_MODULATE, zero );
--                          RENDERPARM_VERTEXCOLOR_ADD, one );
--                        when SVC_MODULATE =>
--                          RENDERPARM_VERTEXCOLOR_MODULATE, one );
--                          RENDERPARM_VERTEXCOLOR_ADD, zero );
--                        when SVC_INVERSE_MODULATE =>
--                          RENDERPARM_VERTEXCOLOR_MODULATE, negOne );
--                          RENDERPARM_VERTEXCOLOR_ADD, one );
--                      end case;
--  
--                      -- Draw the interaction
--                      Draw (Surface);
--                    end if;
--  
--                    -- ???
--                    vkCmdSetDepthBounds (Commands, Light.Scissor.zmin, Light.Scissor.zmax);
--                  end loop;
--            end case;
--          end loop;
--      end loop;
--  
--      -- Reset depth bounds
--      Pipeline := (others => <>);
--  

--      ------------------
--      -- Ambient Pass --
--      ------------------
--      --
--      -- ???
--      --
--  
--      -- Setup pass
--      Pipeline := (others => <>);
--      GUI_Screen_Offset := (if View.Entities /= NULL then 0.0 else stereoEye * View.renderView.stereoScreenSeparation);
--      Ambient_Pass.Commit;
--  
--      -- Add light glare and GUIs
--      for Surface of Surfaces loop
--  
--        -- Some deforms may disable themselves by setting numIndexes = 0
--        if Surface.Material.Is_Ambient then
--  
--          -- We need to draw the post process shaders after we have drawn the fog lights
--          exit when Surface.Material.Kind = Post_Process_Domain and not Current_Render_Copied;
--  
--          -- if we are rendering a 3D view and the surface's eye index doesn't match the current view's eye index then we skip the surface
--          -- if the stereoEye value of a surface is 0 then we need to draw it for both eyes.
--          if not >> stereoEye /= 0 and Surface.material.GetStereoEye /= 0 and (stereoRender_swapEyes.GetBool ? ( Surface.material.GetStereoEye == stereoEye ) : ( Surface.material.GetStereoEye != stereoEye)) then
--  
--            -- Determine the stereoDepth offset guiStereoScreenOffset will always be zero for 3D views, so the != check will never force an update due to the current sort value.
--            Current_GUI_Stereo_Offset := GUI_Stereo_Offset * Surface.Sort;
--  
--            -- Change the matrix and other space related vars if needed
--            if Surface.space /= Current_Space or Current_GUI_Stereo_Offset /= currentGuiStereoOffset then
--              Current_Space := Surface.space; 
--              Offset        := MVP;
--              Offset (0, 3) := Offset (0, 3) + Stereo_Offset;
--  
--              -- Set eye position in local space
--              MVP.Set             (Offset[0], 4); -- X???
--              MVP.Set             (Current_Space.mvp, guiStereoScreenOffset != 0.0 (Current_GUI_Stereo_Offset));
--              LOCALVIEWORIGIN.Set (GlobalPointToLocal (Current_Space.Model, View.renderView.viewor)(1.0);
--              Local_To_Global.Set (Transpose (Current_Space.Local_To_Global), 4);
--              Local_To_Eye.Set    (Transpose (Current_Space.Local_To_Eye), 4);
--            end if;
--  
--            -- Update pipeline
--            Pipeline.Cull := (if Surface.Current_Space.isGuiSurface then VK_CULL_MODE_NONE else shader.GetCullType);
--            Update_Scissor (Surface.Scissor);
--            if shader.TestMaterialFlag (MF_POLYGONOFFSET) then
--              vkCmdSetDepthBias (Commands, r_offsetUnits.GetFloat * shader.GetPolygonOffset, 0.0, r_offsetFactor);
--              Pipeline.Offset_Polygon := True;
--            end if;
--  
--            -- Draw when the stage is not zero or one which are used for some alpha masks
--            if Pipeline.Source_Blend_Factor = GLS_SRCBLEND_ZERO or Pipeline.Destination_Blend_Factor = GLS_DSTBLEND_ONE then
--              enableSkinning.Set (Surface.Is_Animated);
--              Draw (Surface);
--            end if;
--          end if;
--        end if;
--      end loop;
--  
--      -- ???
--  
--      ---------------------
--      -- Distortion Pass --
--      ---------------------
--      --
--      -- ??? 
--      --
--  
--      -- force fog plane to recalculate
--      Clear_Color (WHITE_COLOR);
--      Current_Space := NULL_PTR;
--      Pipeline      := (others => <>);
--  
--      -- Handle special lights
--      for Light of View.Lights loop
--        case Light.Kind is
--  
--          --------------
--          -- Fog Pass --
--          --------------
--          --
--          -- Fog and blend lights, drawn after emissive surfaces so they are properly dimmed down
--          --
--  
--          when Fog_Light =>
--            Fog_Pass.Commit;
--  
--            -- Find the current color and density of the fog
--            Clear_Color (Light.Material.Color_Modifer);
--  
--            -- Calculate the falloff planes 
--            Fog_Planes := (1 => Get_Matrix_4D_Y (View.World_Space.Local_To_Eye) -- Set a fog distance of 500 otherwise, distance = alpha color
--                                   * (if Light.color <= 1.0 then -0.5 / FOG_DISTANCE else -0.5 / Light.color),
--                           2 => (0.0, 0.0, 0.0, 0.5),
--                           3 => FOG_SCALE * Light.Fog_Plane, -- Texture generation for the fade plane, which is always "top" on unrotated lights
--                           4 => (0.0, 0.0, 0.0, FOG_SCALE * Light.Fog_Plane.Distance (View.Render.Origin) + FOG_ENTER));
--            Fog_Planes (3).W := Fog_Planes (3).W + FOG_ENTER;
--  
--            -- Draw direct light interactions
--            Pipeline.Source_Blend      := VK_BLEND_FACTOR_SRC_ALPHA;
--            Pipeline.Destination_Blend := VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
--            Pipeline.Depth_Compare     := VK_COMPARE_OP_EQUAL;
--            Draw_Fog (Light.Interactions (Direct_Interaction), Fog_Planes);
--  
--            -- Draw light frustum bounding planes aren't in the depth buffer, so use depthfunc_less instead of depthfunc_equal
--            Pipeline.Depth_Compare        := VK_COMPARE_OP_LESS_OR_EQUAL;
--            Pipeline.Cull                 := VK_CULL_MODE_BACK_BIT;
--            Zero_One_Cube_Surface.Space   := View.World_Space;
--            Zero_One_Cube_Surface.Scissor := View.Scissor;          
--            Draw_Fog (Zero_One_Cube_Surface, Fog_Plane, Light.Inverse_Projection); -- S is based on the view origin
--  

--          ----------------
--          -- Blend Pass --
--          ----------------
--          --
--          -- ???
--          --
--  
--          when Blend_Light =>
--  
--            -- Set the the falloff and the projected texture
--            Set_Matrix_4D_Z         (Light_Texture, (0.0, 0.0, 1.0, 0.0));
--            Set_Matrix_4D_W         (Light_Texture, (0.0, 0.0, 0.0, 1.0)));
--            Base_Color_Image.Set    (Light.Material.Base_Color);
--            Light_Falloff_Image.Set (Light.Falloff);
--            Pipeline.Depth_Compare  := VK_COMPARE_OP_EQUAL;
--            Light_Texture           := Prepare_Map_Transform (Light.Material.Transform);
--  
--            -- Get the modulate values from the light, including alpha, unlike normal lights
--            Clear_Color (Light.Material.Color_Mod);
--  
--            -- ???
--            Current_Space := (others => <>);
--            for Surface of Surfaces loop
--  
--              -- Set shader parameters
--              Update_Scissor (Surface.Scissor);
--              if Surface.Space /= Current_Space then
--                MVP.Set (Surface.Space.MVP);
--                lightProjectInCurrentSpace.Set (GlobalPlaneToLocal (Surfaces.Space.Local_To_Eye, Light.Projection))
--                Current_Space := Surface.Space;
--              end if;
--  
--              -- Draw it
--              Draw (Surface);
--            end loop;
--        end case;
--  
--        -- Reset the pipeline
--        Pipeline := (others => <>);
--      end loop;
--  

--      ---------------
--      -- Post Pass --
--      ---------------
--      --
--      -- Capture the depth for the motion blur before rendering any post process surfaces that may contribute to the depth
--      --
--  
--      const idScreenRect & viewport = View.viewport;
--      globalImages.currentDepthImage.CopyDepthbuffer( viewport.x1, viewport.y1, viewport.GetWidth, viewport.GetHeight);
--    end loop;
--  end;

