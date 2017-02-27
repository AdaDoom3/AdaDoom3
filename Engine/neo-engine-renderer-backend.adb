
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

-- Soft stencil shadow shading using prenumbra:
--   https://web.archive.org/web/20160417154820/http://www.terathon.com/gdc05_lengyel.pdf
--   https://web.archive.org/web/20161115053356/http://fabiensanglard.net/doom3/renderer.php
--   
separate (Neo.Engine.Renderer) procedure Backend is   

  -- 
  float parm(4);

  -- 
  bool drawSolid = false;

  -- 
  float color(4);

  -- 
  idPlane fogPlanes(4);

  -- 
  uint64 surfGLState = 0;

  -- 
  idScreenRect rect
  uint64 glState := 0;
  backEnd.currentSpace
  backEnd.viewDef.projectionMatrix
  begin
    loop

      -- Wait for the frontend to give us something new
      while Last_View_Update.Get < Last_View_Render.Get loop delay FRONTEND_WAIT_DURATION; end loop;
      View := Common_View.Get;

      -----------
      -- Setup --
      -----------
      --
      --
      --

      -- Update dynamic viewport state and window clipping
      vkCmdSetViewport (Commands, 0, 1, (Height    => View.Port.y2 + 1 - View.Port.y1,
                                         Width     => View.Port.x2 + 1 - View.Port.x1,
                                         Min_Depth => 0.0,
                                         Max_Depth => 1.0));

      -- Update dynamic scissor which may be smaller than the viewport for subviews
      vkCmdSetScissor (Commands, 0, 1, (Extent => (Width  => backEnd.View.Port.x1 + viewDef.scissor.x1,
                                                   Height => backEnd.View.Port.y1 + viewDef.scissor.y1),
                                        Offset => (X => viewDef.scissor.x2 + 1 - viewDef.scissor.x1,
                                                   Y => viewDef.scissor.y2 + 1 - viewDef.scissor.y1)));

      -- Ensure depth writes are enabled for the depth clear
      GL_State (GLS_DEFAULT);

      -- Clear the depth buffer and set the stencil to 128 for shadowing
      GL_Clear (false, true, true, STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0);

      -- Normal face culling Force face culling to set next time
      GL_Cull (CT_FRONT_SIDED);
      backEnd.glState.faceCulling := -1;  

      -- Bind one global Vertex Array Object (VAO)
      qglBindVertexArray (glConfig.global_vao);

      -- Set eye position shader parameter
      SetFragmentParm (RENDERPARM_OVERBRIGHT,   (others => r_lightScale.GetFloat * 0.5));
      Set_Vertex_Parameter   (RENDERPARM_GLOBALEYEPOS, (View.Origin.X, View.Origin.Y, View.Origin.Z, 1.0));
      Set_Vertex_Parameters  (RENDERPARM_PROJMATRIX_X, R_MatrixTranspose(backEnd.viewDef.projectionMatrix), 4);

      ----------------
      -- Fill Depth --
      ----------------
      --
      -- 
      --

      -- Start with subview surfaces, then opaque surfaces, and finally perforated surfaces
      for Group in Surface_Group_Kind'Range loop
        for Surfaces of View.Surface_Groups.Element (Group) loop
          for Surface of Surfaces loop

            -- Translucent fail the mirror clip plane operation
            if Surface.Material.Blend /= Translucent_Blend then

              -- Change the MVP matrix if needed
              if Surface.Space /= Current_Space then
                Set_Vertex_Parameters (RENDERPARM_MVPMATRIX_X, Surface.space.mvp, 4);
                Current_Space := Surface.Space;
              end if;

              -- Set the polygon offset
              surfGLState = 0;
              if Surface.Material.TestMaterialFlag (MF_POLYGONOFFSET) then
                surfGLState := surfGLState or GLS_POLYGON_OFFSET;
                GL_PolygonOffset (r_offsetFactor.GetFloat, r_offsetUnits.GetFloat * Surface.Material.GetPolygonOffset);
              end if;

              -- Subviews may down-modulate the color buffer, otherwise black is drawn
              if Group = Subview_Group then
                surfGLState := surfGLState or GLS_SRCBLEND_DST_COLOR or GLS_DSTBLEND_ZERO or GLS_DEPTHFUNC_LESS;
                color := (others => 1.0);
              else color := (0.0, 0.0, 0.0, 1.0); end if;

              -- Draw the surface based on material blending kind
              case Group is

                -- Solid geometry fast path
                when Opaque_Group => 
                  if Surface.Material.Domain = Surface_Domain then
                    GLSL.BindShader_Color;
                    GL_Color (color);
                  else
                    GLSL.BindShader_Depth;
                    surfGLState := (surfGLState or GLS_ALPHAMASK);
                  end if;
                  GL_State (surfGLState);
                  Draw (Surface);

                -- Other perforated or subview surfaces
                when others =>
                  if pStage.hasAlphaTest and Surface.shaderRegisters( pStage.conditionRegister ) > 0 then

                    -- Set privatePolygonOffset if necessary and set the alpha modulate
                    if not pStage.privatePolygonOffset then
                      GL_PolygonOffset (r_offsetFactor.GetFloat, r_offsetUnits.GetFloat * pStage.privatePolygonOffset);
                      GL_Color (Surface.shaderRegisters (pStage.color.registers(3)));
                      surfGLState := surfGLState or GLS_POLYGON_OFFSET;
                    else GL_Color (Surface.shaderRegisters (pStage.color.registers(3))); end if;
                    GL_State(surfGLState);
                    SetFragmentParm( RENDERPARM_ALPHA_TEST, alphaTestValue( Surface.shaderRegisters( pStage.alphaTestRegister )).ToFloatPtr);
                    GLSL.BindShader_TextureVertexColor;
                    RB_SetVertexColorParms( SVC_IGNORE);

                    -- Bind the texture
                    GL_SelectTexture (0);
                    pStage.texture.image.Bind;

                    -- Set texture matrix and texGens
                    RB_LoadShaderTextureMatrix (surf.shaderRegisters, &pStage.texture);
                    case Stage.Material.Cube_Map is

                      -- Per-pixel reflection
                      when Mirror_Cube_Map =>
                        GL_SelectTexture (1);
                        Bind_Image (Surface.Material.Normal);
                        GL_SelectTexture (0);
                        Bind_Shader (Bumpy_Environment);
                        Set_Vertex_Parameter (RENDERPARM_TEXGEN_0_ENABLED, (others => 0.0));  

                      -- 
                      when TG_SKYBOX_CUBE =>
                        GLSL.BindShader_SkyBox;
                        Set_Vertex_Parameter (RENDERPARM_TEXGEN_0_ENABLED, (others => 0.0));  

                      -- 
                      when pStage.texture.texgen = TG_SCREEN or pStage.texture.texgen = TG_SCREEN2 =>
                        Set_Vertex_Parameter (RENDERPARM_TEXGEN_0_S, RENDERPARM_TEXGEN_0_T, RENDERPARM_TEXGEN_0_Q, Surface.Space.Model_View * View.Projection);
                        Set_Vertex_Parameter (RENDERPARM_TEXGEN_0_ENABLED, (others => 1.0));  
                    end case;
                    Draw (Surface);

                    -- Unbind the extra bink textures
                    if pStage.texture.cinematic
                      GL_SelectTexture (1);
                      globalImages.BindNull;
                      GL_SelectTexture (2);
                      globalImages.BindNull;
                      GL_SelectTexture (0);
                    end if;

                    -- see if there is also a bump map specified
                    if pStage.texture.texgen == TG_REFLECT_CUBE
                      const shaderStage_t *bumpStage = surf.material.GetBumpStage;
                      GL_SelectTexture (1);
                      globalImages.BindNull;
                      GL_SelectTexture (0);
                      GLSL.Unbind;
                    end

                    -- unset privatePolygonOffset if necessary
                    if pStage.privatePolygonOffset
                      GL_PolygonOffset( r_offsetFactor.GetFloat, r_offsetUnits.GetFloat * shader.GetPolygonOffset);
                    end if;
                  end if;
                end loop;
              end case;
            end if;
          end loop;

          -- 
          SetFragmentParm (RENDERPARM_ALPHA_TEST, vec4_zero.ToFloatPtr);
        end loop;
      end loop;

      -----------
      -- Light --
      -----------
      --
      --
      --

      -- 
      GL_SelectTexture (0);
      for Light of View.Lights loop

        -- Skip fog and lights, check interactions exist?
        if Light.Shader.Kind /= Fog_Kind and Light.Shader.Kind /= Blend_Kind then

          -- Set the depth bounds for the whole light
         GL_DepthBoundsTest (vLight.scissorRect.zmin, vLight.scissorRect.zmax);

          -- Only need to clear the stencil buffer and perform stencil testing if there are shadows performStencilTest
          if vLight.globalShadows /= NULL or vLight.localShadows /= NULL then

            -- mirror flips the sense of the stencil select, so disable the stencil select in the mirror case useLightStencilSelect
            if r_useLightStencilSelect.GetBool && backEnd.viewDef.isMirror = false then

              -- Write a stencil mask for the visible light bounds to hi-stencil

              -- enable the light scissor
              if !backEnd.currentScissor.Equals( vLight.scissorRect ) && r_useScissor.GetBool
                GL_Scissor( backEnd.viewDef.viewport.x1 + vLight.scissorRect.x1, 
                      backEnd.viewDef.viewport.y1 + vLight.scissorRect.y1,
                      vLight.scissorRect.x2 + 1 - vLight.scissorRect.x1,
                      vLight.scissorRect.y2 + 1 - vLight.scissorRect.y1);
                backEnd.currentScissor = vLight.scissorRect;
              end

              -- clear stencil buffer to 0 (not drawable)
              uint64 glStateMinusStencil = GL_GetCurrentStateMinusStencil;
              GL_State( glStateMinusStencil | GLS_STENCIL_FUNC_ALWAYS | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE )); -- make sure stencil mask passes for the clear
              GL_Clear( false, false, true, 0, 0.0, 0.0, 0.0, 0.0);  -- clear to 0 for stencil select

              -- set the depthbounds
              GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);
              GL_State( GLS_COLORMASK | GLS_ALPHAMASK | GLS_DEPTHMASK | GLS_DEPTHFUNC_LESS | GLS_STENCIL_FUNC_ALWAYS | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ));
              GL_Cull( CT_TWO_SIDED);
              GLSL.BindShader_Depth;

              -- set the matrix for deforming the 'zeroOneCubeModel' into the frustum to exactly cover the light volume
              RB_SetMVP( backEnd.viewDef.worldSpace.mvp * vLight.inverseBaseLightProject);

              -- two-sided stencil test
              qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_REPLACE, GL_ZERO);
              qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_ZERO, GL_REPLACE);
              Draw (&backEnd.zeroOneCubeSurface);

              -- Reset stencil state
              GL_Cull (CT_FRONT_SIDED);
              GLSL.Unbind;

              -- Unset the depth bounds test
              GL_DepthBoundsTest (0.0, 0.0);
            else

              -- Always clear whole S-Cull tiles
              rect := (x1 => (vLight.scissorRect.x1 +  0) and not 15, y1 => (vLight.scissorRect.y1 +  0) and not 15,
                       x2 => (vLight.scissorRect.x2 + 15) and not 15, y2 => (vLight.scissorRect.y2 + 15) and not 15);
              if not backEnd.currentScissor = rect then
                GL_Scissor (backEnd.viewDef.viewport.x1 + rect.x1,
                            backEnd.viewDef.viewport.y1 + rect.y1,
                            rect.x2 + 1 - rect.x1,
                            rect.y2 + 1 - rect.y1);
                backEnd.currentScissor := rect;
              end if;

              -- Make sure stencil mask passes for the clear
              GL_State (GLS_DEFAULT);  
              GL_Clear (false, false, true, STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0);
            end if;
          end if;

          for Pass_Sort in Pass_Sort_Kind'Range loop -- Global_Shadow_Sort Local_Light_Sort Local_Shadow_Sort Global_Light_Sort Translucent_Light_Sort
            case Pass_Sort is

              -----------------
              -- Shadow Pass --
              -----------------
              --
              --
              --

              when Shadow_Sort'Range =>
                GLSL.BindShader_Shadow;
                GL_SelectTexture( 0);
                globalImages.BindNull;
                backEnd.currentSpace := NULL;

                -- don't write to the color or depth buffer, just the stencil buffer
                glState := GLS_DEPTHMASK | GLS_COLORMASK | GLS_ALPHAMASK | GLS_DEPTHFUNC_LESS;
                GL_PolygonOffset( r_shadowPolygonFactor.GetFloat, -r_shadowPolygonOffset.GetFloat);

                -- the actual stencil func will be set in the draw code, but we need to make sure it isn't
                -- disabled here, and that the value will get reset for the interactions without looking like a no-change-required
                GL_State( glState | GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_KEEP | GLS_STENCIL_OP_PASS_INCR | 
                  GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ) | GLS_POLYGON_OFFSET);

                -- Two Sided Stencil reduces two draw calls to one for slightly faster shadows
                GL_Cull( CT_TWO_SIDED);

                -- process the chain of shadows with the current rendering state
                for Surface of View.Surfaces loop

                  if !backEnd.currentScissor.Equals( drawSurf.scissorRect ) && r_useScissor.GetBool ) 

                    -- change the scissor
                    GL_Scissor( backEnd.viewDef.viewport.x1 + drawSurf.scissorRect.x1,
                          backEnd.viewDef.viewport.y1 + drawSurf.scissorRect.y1,
                          drawSurf.scissorRect.x2 + 1 - drawSurf.scissorRect.x1,
                          drawSurf.scissorRect.y2 + 1 - drawSurf.scissorRect.y1);
                    backEnd.currentScissor := drawSurf.scissorRect;
                  end
                  if drawSurf.space /= backEnd.currentSpace then

                    -- change the matrix
                    RB_SetMVP( drawSurf.space.mvp);

                    -- set the local light position to allow the vertex program to project the shadow volume end cap to infinity
                    idVec4 localLight( 0.0);
                    R_GlobalPointToLocal( drawSurf.space.modelMatrix, vLight.globalLightOrigin, localLight.ToVec3);
                    Set_Vertex_Parameter( RENDERPARM_LOCALLIGHTORIGIN, localLight.ToFloatPtr);
                    backEnd.currentSpace := drawSurf.space;
                  end if;
                  GLSL.BindShader_Shadow;

                  -- set depth bounds per shadow
                  if r_useShadowDepthBounds.GetBool ) GL_DepthBoundsTest( drawSurf.scissorRect.zmin, drawSurf.scissorRect.zmax); end

                  -- Determine whether or not the shadow volume needs to be rendered with Z-pass or
                  -- Z-fail. It is worthwhile to spend significant resources to reduce the number of
                  -- cases where shadow volumes need to be rendered with Z-fail because Z-fail
                  -- rendering can be significantly slower even on today's hardware. For instance,
                  -- on NVIDIA hardware Z-fail rendering causes the Z-Cull to be used in reverse:
                  -- Z-near becomes Z-far (trivial accept becomes trivial reject). Using the Z-Cull
                  -- in reverse is far less efficient because the Z-Cull only stores Z-near per 16x16
                  -- pixels while the Z-far is stored per 4x2 pixels. (The Z-near coallesce buffer
                  -- which has 4x4 granularity is only used when updating the depth which is not the
                  -- case for shadow volumes.) Note that it is also important to NOT use a Z-Cull
                  -- reconstruct because that would clear the Z-near of the Z-Cull which results in
                  -- no trivial rejection for Z-fail stencil shadow rendering.

                  -- Z-pass
                  if drawSurf.renderZFail = 0 ) or r_forceZPassStencilShadows.GetBool ) 
                    qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                    qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);

                  -- preload + Z-pass
                  elsif r_useStencilShadowPreload.GetBool ) 
                    qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_DECR, GL_DECR);
                    qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_INCR, GL_INCR);
                  end if;

                  -- get vertex buffer
                  const vertCacheHandle_t vbHandle := drawSurf.shadowCache;
                  idVertexBuffer * vertexBuffer;
                  if vertexCache.CacheIsStatic( vbHandle ) ) 
                    vertexBuffer := &vertexCache.staticData.vertexBuffer;
                  else 
                    const uint64 frameNum := (int)( vbHandle >> VERTCACHE_FRAME_SHIFT ) & VERTCACHE_FRAME_MASK;
                    if frameNum /= ( ( vertexCache.currentFrame - 1 ) & VERTCACHE_FRAME_MASK ) ) 
                      continue;
                    end if;
                    vertexBuffer := &vertexCache.frameData(vertexCache.drawListNum).vertexBuffer;
                  end if;

                  -- get index buffer
                  idIndexBuffer * indexBuffer;
                  if vertexCache.CacheIsStatic( drawSurf.indexCache ) ) 
                    indexBuffer := &vertexCache.staticData.indexBuffer;
                  else 
                    const uint64 frameNum := (int)( drawSurf.indexCache >> VERTCACHE_FRAME_SHIFT ) & VERTCACHE_FRAME_MASK;
                    if frameNum /= ( ( vertexCache.currentFrame - 1 ) & VERTCACHE_FRAME_MASK ) ) 
                      continue;
                    end if;
                    indexBuffer := &vertexCache.frameData(vertexCache.drawListNum).indexBuffer;
                  end if;

                  if backEnd.glState.currentIndexBuffer /= (GLuint)indexBuffer.GetAPIObject or !r_useStateCaching.GetBool ) 
                    qglBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer.GetAPIObject);
                    backEnd.glState.currentIndexBuffer := (GLuint)indexBuffer.GetAPIObject;
                  end if;

                  if drawSurf.jointCache then
                    assert( GLSL.ShaderUsesJoints);
                    idJointBuffer jointBuffer;
                    if vertexCache.GetJointBuffer( drawSurf.jointCache, &jointBuffer ) )  then
                    assert( ( jointBuffer.GetOffset & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) = 0);
                    const GLuint ubo := reinterpret_cast< GLuint >( jointBuffer.GetAPIObject);
                    qglBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset, jointBuffer.GetNumJoints * sizeof( idJointMat ));
                    if ( backEnd.glState.vertexLayout /= LAYOUT_DRAW_SHADOW_VERT_SKINNED) or ( backEnd.glState.currentVertexBuffer /= (GLuint)vertexBuffer.GetAPIObject ) or !r_useStateCaching.GetBool ) 
                      qglBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer.GetAPIObject);
                      backEnd.glState.currentVertexBuffer := (GLuint)vertexBuffer.GetAPIObject;
                      qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX)
                      qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL);
                      qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR);
                      qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2);
                      qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST);
                      qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT);
                      qglVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_XYZW_OFFSET ));
                      qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_COLOR_OFFSET ));
                      qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_COLOR2_OFFSET ));
                      backEnd.glState.vertexLayout := LAYOUT_DRAW_SHADOW_VERT_SKINNED;
                    end if;
                  end loop;
                  const uint64 indexOffset := (int)( ibHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK;
                  GLSL.CommitUniforms;
                  qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE,
                    (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof( idShadowVertSkinned ));

                  -- render again with Z-pass
                  if !renderZPass && r_useStencilShadowPreload.GetBool ) 
                    qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                    qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
                    qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE,
                      (triIndex_t *)indexOffset, (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof ( idShadowVert ));
                  end if;
                end case;

                -- cleanup the shadow specific rendering state and reset depth bounds
                GL_Cull( CT_FRONT_SIDED);
                GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);

              ----------------
              -- Light Pass --
              ----------------
              --
              --
              --

              when Light_Sort'Range =>
                if vLight.translucentInteractions /= NULL and not r_skipTranslucent.GetBool then

                  -- Disable the depth bounds test because translucent surfaces don't work with
                  -- the depth bounds tests since they did not write depth during the depth pass.
                  if r_useLightDepthBounds.Get then GL_DepthBoundsTest (0.0, 0.0); end if;

                  -- The depth buffer wasn't filled in for translucent surfaces, so they
                  -- can never be constrained to perforated surfaces with the depthfunc equal.
                  -- Translucent surfaces do not receive shadows. This is a case where a
                  -- shadow buffer solution would work but stencil shadows do not because
                  -- stencil shadows only affect surfaces that contribute to the view depth
                  -- buffer and translucent surfaces do not contribute to the view depth buffer.
                  RB_RenderInteractions (vLight.translucentInteractions, vLight, GLS_DEPTHFUNC_LESS, false, false);
                end if;
                RB_RenderInteractions (vLight.localInteractions, vLight, GLS_DEPTHFUNC_EQUAL, performStencilTest, r_useLightDepthBounds.Get);

                -- change the scissor if needed, it will be constant across all the surfaces lit by the light
                if not backEnd.currentScissor = vLight.scissorRect then
                  GL_Scissor (backEnd.viewDef.viewport.x1 + vLight.scissorRect.x1, 
                              backEnd.viewDef.viewport.y1 + vLight.scissorRect.y1,
                              vLight.scissorRect.x2 + 1 - vLight.scissorRect.x1,
                              vLight.scissorRect.y2 + 1 - vLight.scissorRect.y1);
                  backEnd.currentScissor := vLight.scissorRect;
                end

                -- perform setup here that will be constant for all interactions
                GL_State( GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHMASK | depthFunc | GLS_STENCIL_FUNC_EQUAL | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ));

                -- some rare lights have multiple animating stages, loop over them outside the surface list
                const idMaterial * lightShader := vLight.lightShader;
                const float * lightRegs := vLight.shaderRegisters;

                drawInteraction_t inter := end;
                inter.ambientLight := lightShader.IsAmbientLight;

                -- Split out the complex surfaces from the fast-path surfaces
                -- so we can do the fast path ones all in a row.
                -- The surfaces should already be sorted by space because they
                -- are added single-threaded, and there is only a negligable amount
                -- of benefit to trying to sort by materials.
                static const int MAX_INTERACTIONS_PER_LIGHT := 1024;
                static const int MAX_COMPLEX_INTERACTIONS_PER_LIGHT := 128;
                idStaticList< const drawSurf_t *, MAX_INTERACTIONS_PER_LIGHT > allSurfaces;
                idStaticList< const drawSurf_t *, MAX_COMPLEX_INTERACTIONS_PER_LIGHT > complexSurfaces;
                for ( const drawSurf_t * walk := surfList; walk /= NULL; walk := walk.nextOnLight ) 

                  const idMaterial * surf.material := walk.material;
                  if surf.material.GetFastPathBumpImage ) 
                    allSurfaces.Append( walk);
                  end else 
                    complexSurfaces.Append( walk);
                  end
                end
                for ( int i := 0; i < complexSurfaces.Num; i++ ) 
                  allSurfaces.Append( complexSurfaces(i));
                end
                bool lightDepthBoundsDisabled := false;
                for Light_Stage of Light_Stages loop

                  -- ignore stages that fail the condition
                  if not lightRegs( lightShader.GetStage( lightStageNum).conditionRegister then 

                    -- apply the world-global overbright and the 2x factor for specular
                    const idVec4 diffuseColor := lightColor(
                      r_lightScale.GetFloat * lightRegs( lightShader.GetStage( lightStageNum).color.registers(0) ),
                      r_lightScale.GetFloat * lightRegs( lightShader.GetStage( lightStageNum).color.registers(1) ),
                      r_lightScale.GetFloat * lightRegs( lightShader.GetStage( lightStageNum).color.registers(2) ),
                      lightRegs( lightShader.GetStage( lightStageNum).color.registers(3) ));;
                    const idVec4 specularColor := diffuseColor * 2.0f;

                    float lightTextureMatrix(16);
                    if lightShader.GetStage( lightStageNum).texture.hasMatrix ) 
                      RB_GetShaderTextureMatrix( lightRegs, &lightShader.GetStage( lightStageNum).texture, lightTextureMatrix);
                    end

                    -- texture 1 will be the light falloff texture
                    GL_SelectTexture( INTERACTION_TEXUNIT_FALLOFF);
                    vLight.falloffImage.Bind;

                    -- texture 2 will be the light projection texture
                    GL_SelectTexture( INTERACTION_TEXUNIT_PROJECTION);
                    lightShader.GetStage( lightStageNum).texture.image.Bind;

                    -- force the light textures to not use anisotropic filtering, which is wasted on them
                    -- all of the texture sampler parms should be constant for all interactions, only the actual texture image bindings will change

                    -- For all surfaces on this light list, generate an interaction for this light stage setup renderparms assuming we will be drawing trivial surfaces first
                    RB_SetupForFastPathInteractions( diffuseColor, specularColor);

                    -- even if the space does not change between light stages, each light stage may need a different lightTextureMatrix baked in
                    backEnd.currentSpace := NULL;

                    for Surface of Sorted_Surfaces loop

                      -- select the render prog
                      if lightShader.IsAmbientLight then GLSL.BindShader_InteractionAmbient;
                      else GLSL.BindShader_Interaction; end if;

                      inter.surf := surf;

                      -- change the MVP matrix, view/light origin and light projection vectors if needed
                      if surf.space /= backEnd.currentSpace ) 
                        backEnd.currentSpace := surf.space;

                        -- turn off the light depth bounds test if this model is rendered with a depth hack
                        if useLightDepthBounds ) 
                          if !surf.space.weaponDepthHack && surf.space.modelDepthHack = 0.0 ) 
                            if lightDepthBoundsDisabled ) 
                              GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);
                              lightDepthBoundsDisabled := false;
                            end
                          end else 
                            if !lightDepthBoundsDisabled ) 
                              GL_DepthBoundsTest( 0.0, 0.0);
                              lightDepthBoundsDisabled := true;
                            end
                          end
                        end

                        -- model-view-projection
                        RB_SetMVP( surf.space.mvp);

                        -- tranform the light/view origin into model local space
                        idVec4 localLightOrigin( 0.0);
                        idVec4 localViewOrigin( 1.0f);
                        R_GlobalPointToLocal( surf.space.modelMatrix, vLight.globalLightOrigin, localLightOrigin.ToVec3);
                        R_GlobalPointToLocal( surf.space.modelMatrix, backEnd.viewDef.renderView.vieworg, localViewOrigin.ToVec3);

                        -- set the local light/view origin
                        Set_Vertex_Parameter( RENDERPARM_LOCALLIGHTORIGIN, localLightOrigin.ToFloatPtr);
                        Set_Vertex_Parameter( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr);

                        -- transform the light project into model local space
                        idPlane lightProjection(4);
                        for ( int i := 0; i < 4; i++ ) 
                          R_GlobalPlaneToLocal( surf.space.modelMatrix, vLight.lightProject(i), lightProjection(i));
                        end

                        -- optionally multiply the local light projection by the light texture matrix
                        if lightStage.texture.hasMatrix then
                          RB_BakeTextureMatrixIntoTexgen( lightProjection, lightTextureMatrix);
                          float genMatrix(16);
                          float final(16);
                          genMatrix(0*4+0) = lightProject(0)(0); genMatrix(1*4+0) = lightProject(0)(1); genMatrix(2*4+0) = lightProject(0)(2); genMatrix(3*4+0) = lightProject(0)(3);
                          genMatrix(0*4+1) = lightProject(1)(0); genMatrix(1*4+1) = lightProject(1)(1); genMatrix(2*4+1) = lightProject(1)(2); genMatrix(3*4+1) = lightProject(1)(3);
                          genMatrix(0*4+2) = 0.0; genMatrix(1*4+2) = 0.0; genMatrix(2*4+2) = 0.0; genMatrix(3*4+2) = 0.0;
                          genMatrix(0*4+3) = lightProject(2)(0); genMatrix(1*4+3) = lightProject(2)(1); genMatrix(2*4+3) = lightProject(2)(2); genMatrix(3*4+3) = lightProject(2)(3);
                          R_MatrixMultiply( genMatrix, textureMatrix, final);
                          lightProject(0)(0) = final(0*4+0); lightProject(0)(1) = final(1*4+0); lightProject(0)(2) = final(2*4+0); lightProject(0)(3) = final(3*4+0);
                          lightProject(1)(0) = final(0*4+1); lightProject(1)(1) = final(1*4+1); lightProject(1)(2) = final(2*4+1); lightProject(1)(3) = final(3*4+1);
                        end

                        -- set the light projection
                        Set_Vertex_Parameter( RENDERPARM_LIGHTPROJECTION_S, lightProjection(0).ToFloatPtr);
                        Set_Vertex_Parameter( RENDERPARM_LIGHTPROJECTION_T, lightProjection(1).ToFloatPtr);
                        Set_Vertex_Parameter( RENDERPARM_LIGHTPROJECTION_Q, lightProjection(2).ToFloatPtr);
                        Set_Vertex_Parameter( RENDERPARM_LIGHTFALLOFF_S, lightProjection(3).ToFloatPtr);
                      end

                      -- check for the fast path
                      if surf.material.GetFastPathBumpImage && !r_skipInteractionFastPath.GetBool ) 

                        -- texture 0 will be the per-surface bump map
                        GL_SelectTexture( INTERACTION_TEXUNIT_BUMP);
                        surf.material.GetFastPathBumpImage.Bind;

                        -- texture 3 is the per-surface diffuse map
                        GL_SelectTexture( INTERACTION_TEXUNIT_DIFFUSE);
                        surf.material.GetFastPathDiffuseImage.Bind;

                        -- texture 4 is the per-surface specular map
                        GL_SelectTexture( INTERACTION_TEXUNIT_SPECULAR);
                        surf.material.GetFastPathSpecularImage.Bind;

                        Draw (surf);
                      else
                        inter.diffuseColor := 0;
                        inter.specularColor := 0;

                        -- go through the individual surface stages
                        -- This is somewhat arcane because of the old support for video cards that had to render interactions in multiple passes.
                        -- We also have the very rare case of some materials that have conditional interactions for the "hell writing" that can be shined on them.
                        for ( int surfaceStageNum := 0; surfaceStageNum < surf.material.GetNumStages; surfaceStageNum++ ) 
                          const shaderStage_t *surfaceStage := surf.material.GetStage( surfaceStageNum);

                          if surf.shaderRegisters( surfaceStage.conditionRegister ) ) 
                            case surfaceStage.lighting is
                              when SL_BUMP =>
                                if inter.bumpImage /= NULL ) 
                                  RB_DrawSingleInteraction( &inter);
                                end
                                inter.bumpImage := surfaceStage.texture.image;
                                inter.diffuseImage := NULL;
                                inter.specularImage := NULL;
                                RB_SetupInteractionStage( surfaceStage, surf.shaderRegisters, NULL,
                                            inter.bumpMatrix, NULL);
                              when SL_DIFFUSE => 
                                if inter.diffuseImage /= NULL ) 
                                  RB_DrawSingleInteraction( &inter);
                                end
                                inter.diffuseImage := surfaceStage.texture.image;
                                inter.vertexColor := surfaceStage.vertexColor;
                                RB_SetupInteractionStage( surfaceStage, surf.shaderRegisters, diffuseColor.ToFloatPtr,
                                                         inter.diffuseMatrix, inter.diffuseColor.ToFloatPtr);
                              when SL_SPECULAR: 
                                if inter.specularImage /= NULL ) 
                                  RB_DrawSingleInteraction( &inter);
                                end
                                inter.specularImage := surfaceStage.texture.image;
                                inter.vertexColor := surfaceStage.vertexColor;
                                RB_SetupInteractionStage( surfaceStage, surf.shaderRegisters, specularColor.ToFloatPtr,
                                            inter.specularMatrix, inter.specularColor.ToFloatPtr);
                          end
                        end
                      end if;

                      -- draw the final interaction
                      RB_DrawSingleInteraction( &inter);
                    end
                  end if;
                end loop;
                if useLightDepthBounds && lightDepthBoundsDisabled ) 
                  GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);
                end
                GLSL.Unbind;
            end case;
          end loop;
      end loop;

      -- Disable stencil shadow test
      GL_State (GLS_DEFAULT);

      -- Unbind texture units
      for I in 1..5 loop
        GL_SelectTexture (i);
        globalImages.BindNull;
      end loop;
      GL_SelectTexture (0);

      -- Reset depth bounds
      if r_useLightDepthBounds.Get then GL_DepthBoundsTest (0.0, 0.0); end if;

      -----------------
      -- Add Ambient --
      -----------------
      --
      --
      --

      int processed = 0;
      float guiScreenOffset;
      if viewDef.viewEntitys /= NULL then
        -- guiScreenOffset will be 0 in non-gui views
        guiScreenOffset = 0.0;
      else
        guiScreenOffset = stereoEye * viewDef.renderView.stereoScreenSeparation;
      end if;
      --processed = RB_DrawShaderPasses( drawSurfs, numDrawSurfs, guiScreenOffset, stereoEye);
      -- only obey skipAmbient if we are rendering a view
      if backEnd.viewDef.viewEntitys && r_skipAmbient.GetBool
        return numDrawSurfs;
      end

      GL_SelectTexture( 1);
      globalImages.BindNull;
      GL_SelectTexture( 0);
      backEnd.currentSpace = (const viewEntity_t *)1; -- using NULL makes /analyze think surf.space needs to be checked...
      float currentGuiStereoOffset = 0.0;

      for Surface of Surfaces loop

        -- some deforms may disable themselves by setting numIndexes = 0
        if surf.material.HasAmbient and not surf.material.IsPortalSky and surf.numIndexes /= 0 and not surf.material.SuppressInSubview then

          if backEnd.viewDef.isXraySubview && surf.space.entityDef
            if surf.space.entityDef.parms.xrayIndex != 2
              continue;
            end
          end

          -- we need to draw the post process shaders after we have drawn the fog lights
          exit when surf.material.GetSort >= SS_POST_PROCESS && !backEnd.currentRenderCopied

          -- if we are rendering a 3D view and the surface's eye index doesn't match 
          -- the current view's eye index then we skip the surface
          -- if the stereoEye value of a surface is 0 then we need to draw it for both eyes.
          if not >>> ( stereoEye != 0 ) && ( surf.material.GetStereoEye != 0 ) && ( stereoRender_swapEyes.GetBool ? ( surf.material.GetStereoEye == stereoEye ) : ( surf.material.GetStereoEye != stereoEye); )

            -- determine the stereoDepth offset guiStereoScreenOffset will always be zero for 3D views, so the != check will never force an update due to the current sort value.
            const float thisGuiStereoOffset = guiStereoScreenOffset * surf.sort;

            -- change the matrix and other space related vars if needed
            if surf.space != backEnd.currentSpace || thisGuiStereoOffset != currentGuiStereoOffset
              backEnd.currentSpace = surf.space;
              RB_SetMVPWithStereoOffset( backEnd.currentSpace.mvp, guiStereoScreenOffset != 0.0 (thisGuiStereoOffset));

              -- set eye position in local space
              idVec4 localViewOrigin( 1.0f);
              R_GlobalPointToLocal( backEnd.currentSpace.modelMatrix, backEnd.viewDef.renderView.vieworg, localViewOrigin.ToVec3);
              Set_Vertex_Parameter( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr);

              -- set model Matrix
              float modelMatrixTranspose(16);
              R_MatrixTranspose( backEnd.currentSpace.modelMatrix, modelMatrixTranspose);
              Set_Vertex_Parameters( RENDERPARM_MODELMATRIX_X, modelMatrixTranspose, 4);

              -- Set ModelView Matrix
              float modelViewMatrixTranspose(16);
              R_MatrixTranspose( backEnd.currentSpace.modelViewMatrix, modelViewMatrixTranspose);
              Set_Vertex_Parameters( RENDERPARM_MODELVIEWMATRIX_X, modelViewMatrixTranspose, 4);
            end if;

            -- change the scissor if needed
            if !backEnd.currentScissor.Equals( surf.scissorRect ) && r_useScissor.GetBool
              GL_Scissor( backEnd.viewDef.viewport.x1 + surf.scissorRect.x1, 
                    backEnd.viewDef.viewport.y1 + surf.scissorRect.y1,
                    surf.scissorRect.x2 + 1 - surf.scissorRect.x1,
                    surf.scissorRect.y2 + 1 - surf.scissorRect.y1);
              backEnd.currentScissor = surf.scissorRect;
            end if;

            -- set face culling appropriately
            GL_Cull ((if surf.backEnd.currentSpace.isGuiSurface then CT_TWO_SIDED else shader.GetCullType));
            uint64 surfGLState = surf.extraGLState;

            -- set polygon offset if necessary
            if shader.TestMaterialFlag(MF_POLYGONOFFSET)
              GL_PolygonOffset( r_offsetFactor.GetFloat, r_offsetUnits.GetFloat * shader.GetPolygonOffset);
              surfGLState = GLS_POLYGON_OFFSET;
            end

            -- Ambient lighting
            for ( int stage = 0; stage < shader.GetNumStages; stage++    

              -- check the enable condition and skip the stages involved in lighting
              if surf.shaderRegisters( shader.GetStage(stage).conditionRegister ) /= 0 and shader.GetStage(stage).lighting = SL_AMBIENT then

                uint64 stageGLState = surfGLState;
                if ( surfGLState & GLS_OVERRIDE ) == 0
                  stageGLState |= shader.GetStage(stage).drawStateBits;
                end if;

                -- skip if the stage is ( GL_ZERO, GL_ONE ), which is used for some alpha masks
                if not >>> ( stageGLState & ( GLS_SRCBLEND_BITS | GLS_DSTBLEND_BITS ) ) == ( GLS_SRCBLEND_ZERO | GLS_DSTBLEND_ONE )

                  -- stages
                  newShaderStage_t *newStage = pStage.newStage;
                  GL_State( stageGLState);
                
                  GLSL.BindShader( newStage.glslProgram, newStage.glslProgram);

                  for ( int j = 0; j < newStage.numVertexParms; j++ -- get the expressions for conditionals / color / texcoords
                    Set_Vertex_Parameter( (renderParm_t)( RENDERPARM_USER + j ), surf.shaderRegisters( newStage.vertexParms(j));
                  end

                  -- set rpEnableSkinning if the shader has optional support for skinning
                  if surf.jointCache && GLSL.ShaderHasOptionalSkinning
                    const idVec4 skinningParm( 1.0f);
                    Set_Vertex_Parameter( RENDERPARM_ENABLE_SKINNING, skinningParm.ToFloatPtr);
                  end

                  -- bind texture units
                  for ( int j = 0; j < newStage.numFragmentProgramImages; j++
                    idImage * image = newStage.fragmentProgramImages(j);
                    if image != NULL
                      GL_SelectTexture( j);
                      image.Bind;
                    end
                  end

                  -- draw it
                  Draw (surf);

                  -- unbind texture units
                  for ( int j = 0; j < newStage.numFragmentProgramImages; j++
                    idImage * image = newStage.fragmentProgramImages(j);
                    if image != NULL
                      GL_SelectTexture( j);
                      globalImages.BindNull;
                    end
                  end

                  -- clear rpEnableSkinning if it was set
                  if surf.jointCache && GLSL.ShaderHasOptionalSkinning
                    const idVec4 skinningParm( 0.0);
                    Set_Vertex_Parameter( RENDERPARM_ENABLE_SKINNING, skinningParm.ToFloatPtr);
                  end

                  GL_SelectTexture( 0);
                  GLSL.Unbind;
                end if;
              end if;
            end if;
          end loop;
        end if;
      end loop;

      -- 
      GL_Cull( CT_FRONT_SIDED);
      GL_Color( 1.0, 1.0, 1.0);

      -- Fog and blend lights, drawn after emissive surfaces so they are properly dimmed down
      Assert (r_skipFogLights.GetBool || r_showOverDraw.GetInteger != 0 || backEnd.viewDef.isXraySubview) -- don't fog in xray mode*/

      -- force fog plane to recalculate
      backEnd.currentSpace = NULL;
      for Light of backEnd.viewDef.viewLights loop
        case Light.Kind is

          ---------------
          -- Fog Light --
          ---------------
          --
          --
          --

          when Fog_Light =>

            -- find the current color and density of the fog
            GL_Color( vLight.shaderRegisters( vLight.lightShader.GetStage( 0).color.registers));

            -- calculate the falloff planes -- if they left the default value on, set a fog distance of 500 otherwise, distance = alpha color
            a := (if vLight.shaderRegisters( vLight.lightShader.GetStage( 0).color.registers)(3) <= 1.0 then -0.5 / DEFAULT_FOG_DISTANCE
                  else -0.5 / vLight.shaderRegisters( vLight.lightShader.GetStage( 0).color.registers)(3));

            -- texture 0 is the falloff image
            GL_SelectTexture( 0);
            globalImages.fogImage.Bind;

            -- texture 1 is the entering plane fade correction
            GL_SelectTexture( 1);
            globalImages.fogEnterImage.Bind;

            -- Set the fog plane
            fogPlanes(0)(0) = a * backEnd.viewDef.worldSpace.modelViewMatrix(0*4+2); fogPlanes(0)(1) = a * backEnd.viewDef.worldSpace.modelViewMatrix(1*4+2); fogPlanes(0)(2) = a * backEnd.viewDef.worldSpace.modelViewMatrix(2*4+2); fogPlanes(0)(3) = a * backEnd.viewDef.worldSpace.modelViewMatrix(3*4+2) + 0.5f;-- S-0
            fogPlanes(1)(0) = 0.0;--a * backEnd.viewDef.worldSpace.modelViewMatrix(0*4+0); fogPlanes(1)(1) = 0.0;--a * backEnd.viewDef.worldSpace.modelViewMatrix(1*4+0);  fogPlanes(1)(2) = 0.0;--a * backEnd.viewDef.worldSpace.modelViewMatrix(2*4+0);
            fogPlanes(1)(3) = 0.5f;--a * backEnd.viewDef.worldSpace.modelViewMatrix(3*4+0) + 0.5f; -- T-0 fogPlanes(2)(0) = FOG_SCALE * vLight.fogPlane(0); fogPlanes(2)(1) = FOG_SCALE * vLight.fogPlane(1); fogPlanes(2)(2) = FOG_SCALE * vLight.fogPlane(2); fogPlanes(2)(3) = FOG_SCALE * vLight.fogPlane(3) + FOG_ENTER;-- T-1 will get a texgen for the fade plane, which is always the "top" plane on unrotated lights
            fogPlanes(3)(0) = 0.0; fogPlanes(3)(1) = 0.0; fogPlanes(3)(2) = 0.0; fogPlanes(3)(3) = FOG_SCALE * vLight.fogPlane.Distance( backEnd.viewDef.renderView.vieworg) + FOG_ENTER; -- S-1

            -- Draw it
            GL_State( GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_EQUAL);
            RB_T_BasicFog( drawSurfs, fogPlanes, NULL);
            RB_T_BasicFog( drawSurfs2, fogPlanes, NULL);

            -- Light frustum bounding planes aren't in the depth buffer, so use depthfunc_less instead of depthfunc_equal
            GL_State( GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_LESS);
            GL_Cull( CT_BACK_SIDED);

            backEnd.zeroOneCubeSurface.space = &backEnd.viewDef.worldSpace;
            backEnd.zeroOneCubeSurface.scissorRect = backEnd.viewDef.scissor;

            -- S is based on the view origin
            RB_T_BasicFog( &backEnd.zeroOneCubeSurface, fogPlanes, &vLight.inverseBaseLightProject);

            -- 
            GL_Cull( CT_FRONT_SIDED);
            GL_SelectTexture( 1);
            globalImages.BindNull;
            GL_SelectTexture( 0);
            GLSL.Unbind;

          -----------------
          -- Blend Light --
          -----------------
          --
          --
          --

          when Blend_Light =>

            -- texture 1 will get the falloff texture
            GL_SelectTexture( 1);
            vLight.falloffImage.Bind;

            -- texture 0 will get the projected texture
            GL_SelectTexture( 0);
            GLSL.BindShader_BlendLight;
            for ( int i = 0; i < vLight.lightShader.GetNumStages; i++
              if vLight.shaderRegisters( vLight.lightShader.GetStage(i).conditionRegister )

                -- ...
                GL_State( GLS_DEPTHMASK | vLight.lightShader.GetStage(i).drawStateBits | GLS_DEPTHFUNC_EQUAL);
                GL_SelectTexture( 0);
                vLight.lightShader.GetStage(i).texture.image.Bind;
                if vLight.lightShader.GetStage(i).texture.hasMatrix
                  RB_LoadShaderTextureMatrix( vLight.shaderRegisters, &vLight.lightShader.GetStage(i).texture);
                end

                -- get the modulate values from the light, including alpha, unlike normal lights
                GL_Color( vLight.shaderRegisters( vLight.lightShader.GetStage(i).color.registers();
                RB_T_BlendLight( drawSurfs, vLight);
                RB_T_BlendLight( drawSurfs2, vLight);
              end if;
            end loop;

            -- 
            GL_SelectTexture( 1);
            globalImages.BindNull;
            GL_SelectTexture( 0);
            GLSL.Unbind;
        end case;
      end loop;

      ------------------
      -- Post Process --
      ------------------
      --
      -- capture the depth for the motion blur before rendering any post process surfaces that may contribute to the depth
      --

      const idScreenRect & viewport = backEnd.viewDef.viewport;
      globalImages.currentDepthImage.CopyDepthbuffer( viewport.x1, viewport.y1, viewport.GetWidth, viewport.GetHeight);
    end loop;
  end;