=
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

-- The "backend" procedure exists to separate all Vulkan command generation so it may execute in an auxiliary task.
separate (Neo.Engine.Renderer) procedure Backend is
                          
  -- ???
  package Model              is new Uniform (Layout_Set => 2, Binding =>  9, Uniform_T => Matrix_4D);
  package Projection         is new Uniform (Layout_Set => 2, Binding => 10, Uniform_T => Matrix_4D);
  package MVP                is new Uniform (Layout_Set => 2, Binding => 11, Uniform_T => Matrix_4D);
  package View               is new Uniform (Layout_Set => 2, Binding => 12, Uniform_T => Matrix_4D);
  package Transform          is new Uniform (Layout_Set => 2, Binding => 13, Uniform_T => Matrix_4D); -- S, T, Q, Enabled
  package Light_Projection   is new Uniform (Layout_Set => 2, Binding => 14, Uniform_T => Matrix_4D); -- S, T, Q
  package Local_Light_Origin is new Uniform (Layout_Set => 2, Binding => 15, Uniform_T => Vector_4D);
  package Local_View_Origin  is new Uniform (Layout_Set => 2, Binding => 16, Uniform_T => Vector_4D);
  package Color_Value        is new Uniform (Layout_Set => 2, Binding => 24, Uniform_T => Vector_4D);
  package Local_To_Global    is new Uniform (Layout_Set => 2, Binding => 25, Uniform_T => Vector_4D);
  package Local_To_Eye       is new Uniform (Layout_Set => 2, Binding => 26, Uniform_T => Vector_4D);
  --package Tesselation_Max    is new Uniform (Layout_Set => 2, Binding =>  6, Uniform_T => Real_64_C);
  --package Tesselation_Amount is new Uniform (Layout_Set => 2, Binding =>  7, Uniform_T => Real_64_C);
  --package Delta_MS           is new Uniform (Layout_Set => 2, Binding =>  8, Uniform_T => Int_64_Unsigned_C);
  --package Test_Alpha         is new Uniform (Layout_Set => 2, Binding => 17, Uniform_T => Vector_4D);
  --package Light_Falloff      is new Uniform (Layout_Set => 2, Binding => 18, Uniform_T => Vector_4D);
  --package Light_Scale        is new Uniform (Layout_Set => 2, Binding => 19, Uniform_T => Vector_4D);
  --package Screen_Factor      is new Uniform (Layout_Set => 2, Binding => 20, Uniform_T => Vector_4D);
  --package UI_Coord           is new Uniform (Layout_Set => 2, Binding => 21, Uniform_T => Vector_4D);
  --package Diffuse_Modifier   is new Uniform (Layout_Set => 2, Binding => 22, Uniform_T => Vector_4D);
  --package Specular_Modifier  is new Uniform (Layout_Set => 2, Binding => 23, Uniform_T => Vector_4D);
  --package Vertex_Color_Mod   is new Uniform (Layout_Set => 2, Binding => 27, Uniform_T => Vector_4D);
  --package Vertex_Color_Add   is new Uniform (Layout_Set => 2, Binding => 28, Uniform_T => Vector_4D);
  --package Overbright         is new Uniform (Layout_Set => 2, Binding => 29, Uniform_T => Vector_4D);
          
  -- Shader feature flags
  --package Enable_Tesselation is new Uniform (Layout_Set => 2, Binding => 1, Uniform_T => Bool);
  --package Enable_Bloom       is new Uniform (Layout_Set => 2, Binding => 2, Uniform_T => Bool); 
  --package Enable_HDR         is new Uniform (Layout_Set => 2, Binding => 3, Uniform_T => Bool); 
  --package Enable_Blur        is new Uniform (Layout_Set => 2, Binding => 4, Uniform_T => Bool); 
  --package Enable_Haze        is new Uniform (Layout_Set => 2, Binding => 5, Uniform_T => Bool);
  
    Vertex_Bindings : Array_VkDescriptorSetLayoutBinding := (1 => (binding         => 0, -- UBO
                                                                   descriptorCount => 1,
                                                                   descriptorType  => VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                                                   stageFlags      => VK_SHADER_STAGE_VERTEX_BIT, others => <>));
    Fragment_Bindings : Array_VkDescriptorSetLayoutBinding := (1 => (binding         => 1, -- Sampler
                                                                     descriptorCount => 1,
                                                                     descriptorType  => VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                                                     stageFlags      => VK_SHADER_STAGE_FRAGMENT_BIT, others => <>));
    Animated_Vertex_Input : Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Position
                                                                         location => 0,
                                                                         format   => VK_FORMAT_R32G32B32A32_SFLOAT,
                                                                         offset   => Static_Vertex_Obj.Point'Position / 8),
                                                                        (binding  => 0, -- Texture Coordinate
                                                                         location => 1,
                                                                         format   => VK_FORMAT_R16G16_SFLOAT,
                                                                         offset   => Static_Vertex_Obj.Texture'Position / 8),
                                                                        (binding  => 0, -- Normal
                                                                         location => 2,
                                                                         format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                         offset   => Static_Vertex_Obj.Normal'Position / 8),
                                                                        (binding  => 0, -- Velocity
                                                                         location => 3,
                                                                         format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                         offset   => Static_Vertex_Obj.Velocity'Position / 8));
    Static_Vertex_Input : Array_VkVertexInputAttributeDescription := ((binding  => 0, -- Texture Coordinate
                                                                       location => 1,
                                                                       format   => VK_FORMAT_R16G16_SFLOAT,
                                                                       offset   => Animated_Vertex_Obj.Texture'Position / 8),
                                                                      (binding  => 0, -- Start weight
                                                                       location => 2,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Start_Weight'Position / 8),
                                                                      (binding  => 0, -- Weight count
                                                                       location => 3,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Weight_Count'Position / 8),
                                                                      (binding  => 0, -- Velocity
                                                                       location => 4,
                                                                       format   => VK_FORMAT_R8G8B8A8_UNORM,
                                                                       offset   => Animated_Vertex_Obj.Velocity'Position / 8));
  package Depth_Pass  is new Shader ("depth"); 
  package Light_Pass  is new Shader ("light");
  --package Fog_Pass    is new Shader ("fog"); 
  --package Sky_Pass    is new Shader ("sky");
  --package UI_Pass     is new Shader ("ui"); 
  --package Post_Pass   is new Shader ("post");
  --package Shadow_Pass is new Shader (Path                  => "shadow",
  --                                   Static_Vertex_Input   => ((binding  => 0, -- Position
  --                                                              location => 0,
  --                                                              format   => VK_FORMAT_R32G32B32A32_SFLOAT,
  --                                                              offset   => Static_Vertex_Obj.Point'Position / Byte'Size),
  --                                                             (binding  => 0, -- Velocity
  --                                                              location => 4,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Static_Vertex_Obj.Texture'Position / Byte'Size)),                                     
  --                                   Animated_Vertex_Input => ((binding  => 0, -- Start weight
  --                                                              location => 0,
  --                                                              format   => VK_FORMAT_R32G32B32A32_SFLOAT,
  --                                                              offset   => Animated_Vertex_Obj.Start_Weight'Position / Byte'Size),
  --                                                             (binding  => 0, -- Weight count
  --                                                              location => 4,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Animated_Vertex_Obj.Weight_Count'Position / Byte'Size),
  --                                                             (binding  => 0, -- Velocity
  --                                                              location => 5,
  --                                                              format   => VK_FORMAT_R8G8B8A8_UNORM,
  --                                                              offset   => Animated_Vertex_Obj.Velocity'Position / Byte'Size)));

  ----------------------------
  -- Prepare_Texture_Matrix --
  ----------------------------

  function Prepare_Texture_Matrix (Transform : Transform_4D) return Matrix_4D is
    Result : Matrix_4D := ZERO_MATRIX_4D;
    begin

      -- ???
      Set_Matrix_4D_X (Result, (Transform.XX, Transform.XY, 0.0, Transform.XZ));
      Set_Matrix_4D_Y (Result, (Transform.YX, Transform.YY, 0.0, Transform.YZ));

      -- Keep scrolls from making large values, but handle center rotations and scales which make offsets > 1
      if Result.XW < -40.0 or Result.XW > 40.0 then Result.XW := Result.XW - Real_64 (Int (Result.XW)); end if;
      if Result.YW < -40.0 or Result.YW > 40.0 then Result.YW := Result.YW - Real_64 (Int (Result.YW)); end if;

      return Result;
    end;

  --------------------
  -- Update_Scissor --
  --------------------

  procedure Update_Scissor (Scissor : Rectangle) is
    begin
      if Current_Scissor /= Light.Scissor then
        vkCmdSetScissor (Commands, 0, 1, (Extent => (Width  => View.Port.X1 + View.Scissor.X1,
                                                     Height => View.Port.Y1 + View.Scissor.Y1),
                                          Offset => (X => View.scissor.X2 + 1 - View.Scissor.X1,
                                                     Y => View.scissor.Y2 + 1 - View.Scissor.Y1)));
        Current_Scissor := Light.Scissor;
      end if;
    end;

  --------------
  -- Draw_Fog --
  --------------

  procedure Draw_Fog (Surfaces : Vector_Surface.Unsafe.Vector, fogPlanes, &Light.inverseBaseLightProject) is
    idPlane localFogPlanes[4];
    begin
      Current_Space := (others => <>);
      for Surface of Surfaces loop
        Update_Scissor (Surface.Scissor);
        if Surface.Space /= Current_Space then

          -- ???
          if inverseBaseLightProject = NULL_PTR then
            MVP.Set (Surface.space.mvp);
            Global_Plane_To_Local (Surface.space.Model, fogPlanes, localFogPlanes);

          -- ???
          else
            idRenderMatrix invProjectMVPMatrix;
            MVP.Set (inverseBaseLightProject * View.worldSpace.mvp);
            inverseBaseLightProject.InverseTransformPlane( fogPlanes, localFogPlanes, false );;
          end if;
          TEXGEN_0_S.Set (localFogPlanes[0].ToFloatPtr() );
          TEXGEN_0_T.Set (localFogPlanes[1].ToFloatPtr() );
          TEXGEN_1_T.Set (localFogPlanes[2].ToFloatPtr() );
          TEXGEN_1_S.Set (localFogPlanes[3].ToFloatPtr() );
          Current_Space := (if inverseBaseLightProject = NULL then Surface.space else NULL);
        end if;
        Draw (Surface);
      end loop;
    end;

  -----------
  -- Clear --
  -----------

  Attachment : aliased VkClearAttachment := (colorAttachment => 0,
                                             clearValue      => (depthStencil => (depth   => 1.0,
                                                                                  stencil => 128,
                                                                                  others  => <>), others => <>), others => <>);
  Clear_Rectange : aliased VkClearRect := (baseArrayLayer => 0,
                                           layerCount     => 1,
                                           rect           => (extent => Swapchain, others => <>), others => <>);
  procedure Clear_Color (Value : Color_State) is
    begin
      Attachment.aspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
      Attachment.clearValue.color := (Value.Red   / Byte'Last,
                                      Value.Green / Byte'Last,
                                      Value.Blue  / Byte'Last,
                                      Value.Alpha / Byte'Last);
      vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
    end;
  procedure Clear_Stencil is
    begin
      Attachment.aspectMask := VK_IMAGE_ASPECT_STENCIL_BIT;
      vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
    end;
  procedure Clear_Depth is
    begin
      Attachment.aspectMask := VK_IMAGE_ASPECT_DEPTH_BIT;
      vkCmdClearAttachments (Commands, 1, Attachment'Unchecked_Access, 1, Clear_Rectange'Unchecked_Access);
    end;

  -- Local variables
  Processed         : Int         := 0;
  GUI_Screen_Offset :  := ;
  Light_Depth_Test  : Bool        := False;
  Current_Space     : Space_State := (others => <>);
  Fog_Plane         : Plane_4D    := (others => <>);
  Parameter         : Vector_4D   := ZERO_VECTOR_4D;
  Color             : Vector_4D   := ZERO_VECTOR_4D;
  Current_Space     : Matrix_4D   := ZERO_MATRIX_4D;
  Projection        : Matrix_4D   := ZERO_MATRIX_4D;
  Light_To_Textur   : Matrix_4D   := ZERO_MATRIX_4D;
  Joint_Buffer      : Vector_Mesh.Unsafe.Vector;
begin

  ---------------------
  -- Global Textures --
  ---------------------

  Default_Image.Set   ("default");
  White_Image.Set     ("white");
  Black_Image.Set     ("black");
  Fog_Image.Set       ("fog");
  Fog_Enter_Image.Set ("fog_enter");

  -- Main loop
  loop

    -----------------
    -- Frame Setup --
    -----------------

    -- Prepare globals and empty garbage
    Free_Sampler_Garbage;
    Free_Memory_Garbage;
    Free_Image_Garbage;
    Current_Frame          := (Current_Frame + 1) mod NUM_FRAME_DATA;
    Current_Descriptor_Set := 0;
    vkResetDescriptorPool (Device, Framebuffer.Element (Current_Frame).Descriptor_Pool, 0);

    -- Wait for the frontend to give us something new
    - (Clock - Last_Time); Last_Time := Clock;
    while Last_View_Update.Get < Last_View_Render.Get loop delay FRONTEND_WAIT_DURATION; end loop;
    View := Common_View.Get;

    -- Update dynamic viewport state and window clipping
    vkCmdSetViewport (Commands, 0, 1, (Height    => View.Port.y2 + 1 - View.Port.y1,
                                       Width     => View.Port.x2 + 1 - View.Port.x1,
                                       Min_Depth => 0.0,
                                       Max_Depth => 1.0));

    -- Update dynamic scissor which may be smaller than the viewport for subviews
    Update_Scissor (View.Scissor);

    -- Uniforms
    Overbright.Set      (lightScale.GetFloat * 0.5);
    Projection.Set      (Transpose (View.Projection), 4); -- X ???
    Local_To_Global.Set (View.Origin.X, View.Origin.Y, View.Origin.Z, 1.0));

    ----------------
    -- Depth Pass --
    ----------------
    --
    -- ???
    --

    -- Setup pass
    Clear_Depth;
    Clear_Stencil;
    Depth_Pass.Commit;
    Pipeline := (others => <>);

    -- Fill the depth buffer
    for Visibility in Subview_Visibility..Perforated_Visibility loop
      for Light of View.Lights (Point_Light) then

        -- Translucent interactions fail the mirror clip plane operation - shadow surfaces are ignored for obvious reasons...
        for Surface of Light.Interactions (Direct_Interaction) loop

            -- Change the MVP matrix if needed
            if Surface.Space /= Current_Space then
              MVP.Set (Surface.Space.MVP);
              Current_Space := Surface.Space;
            end if;

            -- Draw the surface based on material blending kind
            case Visibility is

              -- Solid geometry fast path
              when Opaque_Visibility => 
                if Surface.Material.Domain = Surface_Domain then Clear_Color (COLOR_WHITE);
                else
                  Pipeline.Depth_Pass_Kind := ;
                  Pipeline.Alpha_Mask      := VK_COLOR_COMPONENT_A_BIT;
                end if;
                Draw (Surface);

              -- Other perforated or subview surfaces
              when others =>

                -- Subviews may down-modulate the color buffer, otherwise black is drawn
                if Visibility = Subview_Visibility then
                  Pipeline.Source_Blend_Factor      := VK_BLEND_FACTOR_DST_COLOR;
                  Pipeline.Destination_Blend_Factor := VK_BLEND_FACTOR_ZERO;
                  Pipeline.Depth_Compare            := VK_COMPARE_OP_LESS_OR_EQUAL;
                  --Color                             := To_Vulkan (COLOR_BLACK);
                end if;

                -- Set privatePolygonOffset if necessary and set the alpha modulate
                Pipeline.Test_Alpha := Has_Alpha_Channel (Surface.Material.Base_Color);
                Color_Mod.Set (zero);
                Color_Add.Set (one);
                Clear_Color (Surface.Material.Color_Mod);

                -- Set texture matrix and texture generators
                shaderRegisters; RB_LoadShaderTextureMatrix (Surface.shaderRegisters, &pStage.texture);
                case Stage.Material.Cube_Map is
                  when Mirror_Cube_Map => Normal_Image.Set (Surface.Material.Normal);
                  when Skybox_Cube_Map => SkyBox_Pass.Commit;
                when others => null; end case;

                -- Push the surface
                Draw (Surface);

                -- Cleanup
                if Surface.Material.Polygon_Offset /= 0.0 then
                  vkCmdSetDepthBias (Commands, r_offsetUnits.GetFloat * shader.GetPolygonOffset, 0.0, r_offsetFactor.GetFloat);
                end if;
              end case;
            end case;
          end if;
        end loop;

        -- ???
        Test_Alpha.Set (ZERO_VECTOR_4D);
      end loop;
    end loop;

    --------------------------
    -- Stencil Shadows Pass --
    --------------------------
    --
    -- ???
    --

    -- Setup pass
    Pipeline := (others => <>);

    -- Light the world
    for Light of View.Lights loop

      -- Skip fog and blend lights
      if Light.Shader.Kind /= Fog_Kind and Light.Shader.Kind /= Blend_Kind then


        -- Only need to clear the stencil buffer and perform stencil testing if there are shadows
        if Light.Global_Shadows.Length /= 0 and Light.Local_Shadows.Length /= 0 and not View.Is_Mirror then

          -- Clear to zero for stencil select and set the depth bounds for the whole light
          Clear_Stencil;
          Update_Scissor (Light.Scissor);
          vkCmdSetDepthBounds (Commands, Light.Scissor.Z_Min, Light.Scissor.Z_Max);

          -- Setup shader
          .Set (View.World_Space.MVP * Light.Inverse_Projection);
          Shadow_Pass.Commit;

          -- Setup pipeline
          Pipeline.Stencil_Test_Enable    := False;
          Pipeline.Alpha_Blend            := VK_COLOR_COMPONENT_A_BIT;
          Pipeline.Depth_Write_Enable     := True;
          Pipeline.Depth_Compare          := VK_COMPARE_OP_LESS_OR_EQUAL;
          Pipeline.Stencil_Compare        := VK_COMPARE_OP_ALWAYS;
          Pipeline. := GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE;
          Pipeline. := GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE;
          Pipeline. := qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_REPLACE, GL_ZERO;
          Pipeline.Cull                   := VK_CULL_MODE_NONE;
          Pipeline.Back_Stencil_Operation := qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_ZERO, GL_REPLACE;     

          -- Draw the deformed Zero_One_Cube_Model into the frustum to exactly cover the light volume
          Draw (ZERO_ONE_CUBE_MESH);

          -- Cleanup     
          vkCmdSetDepthBounds (Commands, 0.0, 0.0); 

        -- ???
        else

          -- Set the depth bounds for the whole light
          Clear_Stencil (STENCIL_SHADOW_TEST_VALUE);
          if Light.Scissor.Z_Min /= 0.0 and Light.Scissor.Z_Max /= 0.0 then
             vkCmdSetDepthBounds (Commands, Light.Scissor.Z_Min, Light.Scissor.Z_Max);
          end if;

          -- Always clear whole Cull tiles
          Update_Scissor ((x1 => (Light.Scissor.X1 +  0) and not 15, y1 => (Light.Scissor.Y1 +  0) and not 15,
                           x2 => (Light.Scissor.X2 + 15) and not 15, y2 => (Light.Scissor.Y2 + 15) and not 15););

          -- Make sure stencil mask passes for the clear
          Pipeline := (others => <>);
        end if;

        -- Begin moving through the Light's entities in sorted order
        for Interaction of Light.Interactions loop
          case Pass_Sort is

            -----------------
            -- Shadow Pass --
            -----------------
            --
            -- ???
            --

            when Shadow_Sort'Range => 

              -- Setup pipeline
              Clear_Texture (0);
              Shadow_Pass.Commit;            
              vkCmdSetDepthBias (Commands, -shadowPolygonOffset, 0.0, shadowPolygonFactor);
              Pipeline.Depth_Write_Enable := True; -- Only write to the stencil buffer ignoring the color or depth buffer
              Pipeline.Alpha_Mask         := VK_COLOR_COMPONENT_A_BIT 
              Pipeline.Depth_Compare      := VK_COMPARE_OP_LESS_OR_EQUAL;
              Pipeline.Cull               := VK_CULL_MODE_NONE; -- Two Sided Stencil reduces two draw calls to only one
              Pipeline.Stencil_Fail       := VK_STENCIL_OP_KEEP;
              Pipeline.Stencil_Z_Fail     := VK_STENCIL_OP_KEEP;
              Pipeline.Stencil_Pass       := VK_STENCIL_OP_INCREMENT_AND_CLAMP; 
              Pipeline. := GLS_STENCIL_MAKE_REF STENCIL_SHADOW_TEST_VALUE;
              Pipeline. := GLS_STENCIL_MAKE_MASK STENCIL_SHADOW_MASK_VALUE;          

              -- Process the chain of shadows with the current rendering state
              Current_Space := (others => <>);
              for Surface of View.Surfaces loop

                -- Update the scissor
                Update_Scissor (Surface.Scissor);

                -- change the matrix and set the local light position to allow the vertex program to project the shadow volume end cap to infinity
                if Current_Space /= Surface.Space then
                  MVP.Set (Surface.space.mvp);
                  Local_Light_Origin.Set (Global_To_Local (Surface.space.Model, Light.Origin));
                  Current_Space := Surface.space;
                end if;

                -- Preload plus Z-pass
                GL_SeparateStencil( STENCIL_FACE_FRONT, GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_DECR | GLS_STENCIL_OP_PASS_DECR );
                GL_SeparateStencil( STENCIL_FACE_BACK, GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_INCR | GLS_STENCIL_OP_PASS_INCR );
                qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : Surface.numIndexes, GL_INDEX_TYPE,
                  (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof( idShadowVertSkinned ));

                -- Render again with Z-pass
                qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
                qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : Surface.numIndexes, GL_INDEX_TYPE,
                  (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof ( idShadowVert ));
              end case;

              -- Cleanup the shadow specific rendering state and reset depth bounds
              Pipeline. := CT_FRONT_SIDED; -- Two Sided Stencil reduces two draw calls to only one
              GL_DepthBoundsTest( Light.Scissor.zmin, Light.Scissor.zmax);

            ----------------
            -- Light Pass --
            ----------------
            --
            --
            --

            when Light_Sort'Range =>
              for Interaction of Light.Interactions loop if Light.Interactions (Kind).Length > 0 then

                -- Load the pipeline state
                if Interaction.Kind = Translucent_Interaction then
                  Pipeline.Depth_Compare := VK_COMPARE_OP_LESS_OR_EQUAL;
                  Pipeline.Test_Depth    := False; -- Disable the depth bounds test on translucent surfaces as they don't write depth
                  Pipeline. := false;
                  Pipeline. := false;                  
                else
                  Pipeline.Depth_Compare := VK_COMPARE_OP_EQUAL;
                  Pipeline.Test_Stencil  := performStencilTest;
                  Pipeline. := r_useLightDepthBounds.Get;
                end if;
                Pipeline.Source_Blend      := VK_BLEND_FACTOR_ONE;
                Pipeline.Destination_Blend := VK_BLEND_FACTOR_ONE;
                Pipeline.Test_Depth        := False;
                Pipeline.Stencil_Compare   := VK_COMPARE_OP_EQUAL;
                Pipeline. := GLS_STENCIL_MAKE_REFSTENCIL_SHADOW_TEST_VALUE;
                Pipeline. := GLS_STENCIL_MAKE_MASKSTENCIL_SHADOW_MASK_VALUE;
                Update_Scissor (Light.Scissor);

                -- Load the light material transform
                Light_Texture := Prepare_Map_Transform (Light.Material.Transform);
                Set_Matrix_4D_Z      (Light_Texture, (0.0, 0.0, 1.0, 0.0));
                Set_Matrix_4D_W      (Light_Texture, (0.0, 0.0, 1.0, 0.0));
                Fall_Off_Image.Set   (INTERACTION_TEXUNIT_FALLOFF);
                Projection_Image.Set (Light.Material.texture); -- light projection texture
                Vertex_Color_Mod.Set (zero);
                Vertex_Color_Add.Set (one);

                -- Even if the space does not change between light stages, each light stage may need a different lightTextureMatrix baked in
                Current_Space := NULL;

                -- For all surfaces on this light list, generate an interaction for this light stage
                for Surface of Sorted_Surfaces loop

                  -- Select the render program
                  Light_Pass.Commit;

                  -- Change the MVP matrix, view/light origin and light projection vectors if needed
                  if Surface.Space /= Current_Space then
                    Current_Space := Surface.Space;

                    -- turn off the light depth bounds test if this model is rendered with a depth hack
                    if Do_Bound_Light_Depth then
                      if not Surface.Space.weaponDepthHack and Surface.Space.modelDepthHack = 0.0 and lightDepthBoundsDisabled then
                        vkCmdSetDepthBounds (Commands, Light.Scissor.zmin, Light.Scissor.zmax);
                        Pipeline.Test_Depth      := False;
                        lightDepthBoundsDisabled := false;
                      elsif not lightDepthBoundsDisabled then
                        Pipeline.Test_Depth      := False;
                        lightDepthBoundsDisabled := true;
                      end if;
                    end if;

                    -- tranform the light/view origin into model local space set the local light/view origin
                    Local_Light_Origin := (others => 0.0);
                    Local_View_Origin  := (others => 1.0);
                    Global_Point_To_Local (Surface.Space.Model, Light.Global_Light_Origin, Local_Light_Origin);
                    Global_Point_To_Local (Surface.Space.Model, View.Render.Origin,        Local_View_Origin);

                    -- Multiply the local light projection by the light texture matrix
                    Set_Matrix_4D_X (Final, Get_Matrix_4D_X (Light_Projection));
                    Set_Matrix_4D_Y (Final, Get_Matrix_4D_Y (Light_Projection));
                    Set_Matrix_4D_Z (Final, (others => 0.0));
                    Set_Matrix_4D_W (Final, Get_Matrix_4D_Z (Light_Projection));
                    Final := Light_Texture * Final;

                    -- Transform the light project into model local space
                    Projection := Global_To_Local (Surface.Space.Model, Light.Projection);

                    Light_Projection(0)(0) := final(0*4+0); Light_Projection(0)(1) = final(1*4+0); Light_Projection(0)(2) = final(2*4+0); Light_Projection(0)(3) = final(3*4+0);
                    Light_Projection(1)(0) := final(0*4+1); Light_Projection(1)(1) = final(1*4+1); Light_Projection(1)(2) = final(2*4+1); Light_Projection(1)(3) = final(3*4+1);

                    -- Load uniforms Set the light projection
                    Light_Projection.Set   (To_Matrix_3D (Light_Projection)); -- Light_Projection
                    Light_Falloff.Set      (Light_Projection); -- LIGHTFALLOFF_S
                    Light_Scale.Set        (Light_Scale.Get);
                    Vertex_Color.Set       (Surface.Material.Vertex_Color);
                    Image_Transform.Set    (Prepare_Map_Transform (Surface.Material.Transform));
                    MVP.Set                (Surface.Space.MVP);
                    Projection_Image.Set   (Surface.Material.);
                    Base_Color_Image.Set   (Surface.Material.Base_Color);
                    Irradiance_Image.Set   (Surface.Material.Irradiance);
                    Prefilter_Image.Set    (Surface.Material.Prefilter);
                    Specular_Image.Set     (Surface.Material.Specular);
                    Normal_Image.Set       (Surface.Material.Normal);
                    Metallic_Image.Set     (Surface.Material.Metallic);
                    Roughness_Image.Set    (Surface.Material.Roughness);
                    Displacement_Image.Set (Surface.Material.Displacement);
                    case vertexColor is
                      when SVC_IGNORE =>
                        RENDERPARM_VERTEXCOLOR_MODULATE, zero );
                        RENDERPARM_VERTEXCOLOR_ADD, one );
                      when SVC_MODULATE =>
                        RENDERPARM_VERTEXCOLOR_MODULATE, one );
                        RENDERPARM_VERTEXCOLOR_ADD, zero );
                      when SVC_INVERSE_MODULATE =>
                        RENDERPARM_VERTEXCOLOR_MODULATE, negOne );
                        RENDERPARM_VERTEXCOLOR_ADD, one );
                    end case;

                    -- Draw the interaction
                    Draw (Surface);
                  end if;

                  -- ???
                  vkCmdSetDepthBounds (Commands, Light.Scissor.zmin, Light.Scissor.zmax);
                end loop;
          end case;
        end loop;
    end loop;

    -- Reset depth bounds
    Pipeline := (others => <>);

    ------------------
    -- Ambient Pass --
    ------------------
    --
    -- ???
    --

    -- Setup pass
    Pipeline := (others => <>);
    GUI_Screen_Offset := (if View.Entities /= NULL then 0.0 else stereoEye * View.renderView.stereoScreenSeparation);
    Ambient_Pass.Commit;

    -- Add light glare and GUIs
    for Surface of Surfaces loop

      -- Some deforms may disable themselves by setting numIndexes = 0
      if Surface.Material.Is_Ambient then

        -- We need to draw the post process shaders after we have drawn the fog lights
        exit when Surface.Material.Kind = Post_Process_Domain and not Current_Render_Copied;

        -- if we are rendering a 3D view and the surface's eye index doesn't match the current view's eye index then we skip the surface
        -- if the stereoEye value of a surface is 0 then we need to draw it for both eyes.
        if not >> stereoEye /= 0 and Surface.material.GetStereoEye /= 0 and (stereoRender_swapEyes.GetBool ? ( Surface.material.GetStereoEye == stereoEye ) : ( Surface.material.GetStereoEye != stereoEye)) then

          -- Determine the stereoDepth offset guiStereoScreenOffset will always be zero for 3D views, so the != check will never force an update due to the current sort value.
          Current_GUI_Stereo_Offset := GUI_Stereo_Offset * Surface.Sort;

          -- Change the matrix and other space related vars if needed
          if Surface.space /= Current_Space or Current_GUI_Stereo_Offset /= currentGuiStereoOffset then
            Current_Space := Surface.space; 
            Offset        := MVP;
            Offset (0, 3) := Offset (0, 3) + Stereo_Offset;

            -- Set eye position in local space
            MVP.Set             (Offset[0], 4); -- X???
            MVP.Set             (Current_Space.mvp, guiStereoScreenOffset != 0.0 (Current_GUI_Stereo_Offset));
            LOCALVIEWORIGIN.Set (GlobalPointToLocal (Current_Space.Model, View.renderView.viewor)(1.0);
            Local_To_Global.Set (Transpose (Current_Space.Local_To_Global), 4);
            Local_To_Eye.Set    (Transpose (Current_Space.Local_To_Eye), 4);
          end if;

          -- Update pipeline
          Pipeline.Cull := (if Surface.Current_Space.isGuiSurface then VK_CULL_MODE_NONE else shader.GetCullType);
          Update_Scissor (Surface.Scissor);
          if shader.TestMaterialFlag (MF_POLYGONOFFSET) then
            vkCmdSetDepthBias (Commands, r_offsetUnits.GetFloat * shader.GetPolygonOffset, 0.0, r_offsetFactor);
            Pipeline.Offset_Polygon := True;
          end if;

          -- Draw when the stage is not zero or one which are used for some alpha masks
          if Pipeline.Source_Blend_Factor = GLS_SRCBLEND_ZERO or Pipeline.Destination_Blend_Factor = GLS_DSTBLEND_ONE then
            enableSkinning.Set (Surface.Is_Animated);
            Draw (Surface);
          end if;
        end if;
      end if;
    end loop;

    -- ???

    ---------------------
    -- Distortion Pass --
    ---------------------
    --
    -- ??? 
    --

    -- force fog plane to recalculate
    Clear_Color (WHITE_COLOR);
    Current_Space := NULL_PTR;
    Pipeline      := (others => <>);

    -- Handle special lights
    for Light of View.Lights loop
      case Light.Kind is

        --------------
        -- Fog Pass --
        --------------
        --
        -- Fog and blend lights, drawn after emissive surfaces so they are properly dimmed down
        --

        when Fog_Light =>
          Fog_Pass.Commit;

          -- Find the current color and density of the fog
          Clear_Color (Light.Material.Color_Modifer);

          -- Calculate the falloff planes 
          Fog_Planes := (1 => Get_Matrix_4D_Y (View.World_Space.Local_To_Eye) -- Set a fog distance of 500 otherwise, distance = alpha color
                                 * (if Light.color <= 1.0 then -0.5 / FOG_DISTANCE else -0.5 / Light.color),
                         2 => (0.0, 0.0, 0.0, 0.5),
                         3 => FOG_SCALE * Light.Fog_Plane, -- Texture generation for the fade plane, which is always "top" on unrotated lights
                         4 => (0.0, 0.0, 0.0, FOG_SCALE * Light.Fog_Plane.Distance (View.Render.Origin) + FOG_ENTER));
          Fog_Planes (3).W := Fog_Planes (3).W + FOG_ENTER;

          -- Draw direct light interactions
          Pipeline.Source_Blend      := VK_BLEND_FACTOR_SRC_ALPHA;
          Pipeline.Destination_Blend := VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
          Pipeline.Depth_Compare     := VK_COMPARE_OP_EQUAL;
          Draw_Fog (Light.Interactions (Direct_Interaction), Fog_Planes);

          -- Draw light frustum bounding planes aren't in the depth buffer, so use depthfunc_less instead of depthfunc_equal
          Pipeline.Depth_Compare        := VK_COMPARE_OP_LESS_OR_EQUAL;
          Pipeline.Cull                 := VK_CULL_MODE_BACK_BIT;
          Zero_One_Cube_Surface.Space   := View.World_Space;
          Zero_One_Cube_Surface.Scissor := View.Scissor;          
          Draw_Fog (Zero_One_Cube_Surface, Fog_Plane, Light.Inverse_Projection); -- S is based on the view origin

        ----------------
        -- Blend Pass --
        ----------------
        --
        -- ???
        --

        when Blend_Light =>

          -- Set the the falloff and the projected texture
          Set_Matrix_4D_Z         (Light_Texture, (0.0, 0.0, 1.0, 0.0));
          Set_Matrix_4D_W         (Light_Texture, (0.0, 0.0, 0.0, 1.0)));
          Base_Color_Image.Set    (Light.Material.Base_Color);
          Light_Falloff_Image.Set (Light.Falloff);
          Pipeline.Depth_Compare  := VK_COMPARE_OP_EQUAL;
          Light_Texture           := Prepare_Map_Transform (Light.Material.Transform);

          -- Get the modulate values from the light, including alpha, unlike normal lights
          Clear_Color (Light.Material.Color_Mod);

          -- ???
          Current_Space := (others => <>);
          for Surface of Surfaces loop

            -- Set shader parameters
            Update_Scissor (Surface.Scissor);
            if Surface.Space /= Current_Space then
              MVP.Set (Surface.Space.MVP);
              lightProjectInCurrentSpace.Set (GlobalPlaneToLocal (Surfaces.Space.Local_To_Eye, Light.Projection))
              Current_Space := Surface.Space;
            end if;

            -- Draw it
            Draw (Surface);
          end loop;
      end case;

      -- Reset the pipeline
      Pipeline := (others => <>);
    end loop;

    ---------------
    -- Post Pass --
    ---------------
    --
    -- Capture the depth for the motion blur before rendering any post process surfaces that may contribute to the depth
    --

    const idScreenRect & viewport = View.viewport;
    globalImages.currentDepthImage.CopyDepthbuffer( viewport.x1, viewport.y1, viewport.GetWidth, viewport.GetHeight);
  end loop;
end;
