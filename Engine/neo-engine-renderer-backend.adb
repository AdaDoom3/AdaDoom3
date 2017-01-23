
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

-- 
separate (Neo.Engine.Renderer) package body Backend is

  -----------------
  -- Run_Backend --
  -----------------
  -- 
  --
  --

  procedure Run_Backend is

    --------------------
    -- Load_Cube_Maps --
    --------------------
    --
    --
    --

    procedure Load_Cube_Maps (Surface : Surface_State) is 
      begin

        -- set the texture matrix if needed
        RB_LoadShaderTextureMatrix (surf.shaderRegisters, &pStage.texture);

        case Stage.Material.Cube_Map is

          -- Per-pixel reflection
          when Mirror_Cube_Map =>
            GL_SelectTexture (1);
            Bind_Image (Surface.Material.Normal);
            GL_SelectTexture (0);
            Bind_Shader (Bumpy_Environment);
            SetVertexParm (RENDERPARM_TEXGEN_0_ENABLED, (others => 0.0));  

          -- 
        elsif pStage.texture.texgen = TG_SKYBOX_CUBE then

          renderProgManager.BindShader_SkyBox;
          SetVertexParm( RENDERPARM_TEXGEN_0_ENABLED, (others => 0.0));  


        elsif pStage.texture.texgen = TG_SCREEN or pStage.texture.texgen = TG_SCREEN2 then


          float mat[16] := R_MatrixMultiply( surf.space.modelViewMatrix, backEnd.viewDef.projectionMatrix, mat);
         
          SetVertexParm( RENDERPARM_TEXGEN_0_S, mat[0*4+0]; mat[1*4+0]; mat[2*4+0]; mat[3*4+0];;
          SetVertexParm( RENDERPARM_TEXGEN_0_T, mat[0*4+1] mat[1*4+1] mat[2*4+1] mat[3*4+1]);
          SetVertexParm( RENDERPARM_TEXGEN_0_Q, mat[0*4+3] mat[1*4+3] mat[2*4+3]  mat[3*4+3);
          SetVertexParm( RENDERPARM_TEXGEN_0_ENABLED, (others => 1.0));  
        end case;
      end;

    -------------------
    -- End_Texturing --
    -------------------
    --
    --
    --

    procedure End_Texturing is
      begin
      --static void RB_FinishStageTexturing( const shaderStage_t *pStage, const drawSurf_t *surf

        if pStage.texture.cinematic
          -- unbind the extra bink textures
          GL_SelectTexture( 1);
          globalImages.BindNull;
          GL_SelectTexture( 2);
          globalImages.BindNull;
          GL_SelectTexture( 0);
        end

        if pStage.texture.texgen == TG_REFLECT_CUBE
          -- see if there is also a bump map specified
          const shaderStage_t *bumpStage = surf.material.GetBumpStage;
          if bumpStage != NULL
            -- per-pixel reflection mapping with bump mapping
            GL_SelectTexture( 1);
            globalImages.BindNull;
            GL_SelectTexture( 0);
          else
            -- per-pixel reflection mapping without bump mapping
          end
          renderProgManager.Unbind;
        end
      end;

    ----------
    -- Draw --
    ----------
    --
    --
    --

    procedure Draw () is
      idIndexBuffer * indexBuffer := (if vertexCache.CacheIsStatic( surf.indexCache ) &vertexCache.staticData.indexBuffer
                                       else &vertexCache.frameData[vertexCache.drawListNum].indexBuffer);
      idVertexBuffer * vertexBuffer  := (if vertexCache.CacheIsStatic( surf.ambientCache ) &vertexCache.staticData.vertexBuffer
                                          else vertexBuffer = &vertexCache.frameData[vertexCache.drawListNum].vertexBuffer);
      begin
        if surf.jointCache then
          idJointBuffer jointBuffer;
          Assert (!vertexCache.GetJointBuffer( surf.jointCache, &jointBuffer )
          assert( ( jointBuffer.GetOffset() & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) == 0);
          const GLuint ubo = reinterpret_cast< GLuint >( jointBuffer.GetAPIObject());
          qglBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset(), jointBuffer.GetNumJoints() * sizeof( idJointMat ));
        end

        renderProgManager.CommitUniforms;

        if backEnd.glState.currentIndexBuffer /= (GLuint)indexBuffer.GetAPIObject() or not r_useStateCaching.Get then
          qglBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer.GetAPIObject());
          backEnd.glState.currentIndexBuffer = (GLuint)indexBuffer.GetAPIObject;
        end if;

        if backEnd.glState.vertexLayout != LAYOUT_DRAW_VERT or backEnd.glState.currentVertexBuffer /= (GLuint)vertexBuffer.GetAPIObject or not r_useStateCaching.Get then
          qglBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer.GetAPIObject());
          backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer.GetAPIObject;

          -- 
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX);
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL);
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR);
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2);
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST);
          qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT);

          -- 
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 3, GL_FLOAT, GL_FALSE, sizeof( idDrawVert ), (void *)( DRAWVERT_XYZ_OFFSET ));
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_NORMAL, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_NORMAL_OFFSET ));
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR_OFFSET ));
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR2_OFFSET ));
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_ST, 2, GL_HALF_FLOAT, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_ST_OFFSET ));
          qglVertexAttribPointerARB( PC_ATTRIB_INDEX_TANGENT, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_TANGENT_OFFSET ));

          backEnd.glState.vertexLayout = LAYOUT_DRAW_VERT;
        end
        
        qglDrawElementsBaseVertex( GL_TRIANGLES, 
                      r_singleTriangle.GetBool() ? 3 : surf.numIndexes,
                      GL_INDEX_TYPE,
                      (triIndex_t *)(int)( ibHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK,
                      (int)( surf.ambientCache >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK / sizeof ( idDrawVert ));
      end;

    ----------
    -- Main --
    ----------
    --
    -- 
    --
                
    VkRect2D scissor := end;
    VkViewport viewport := end;
    float parm[4];
    bool drawSolid = false;
    float color[4];
    idPlane fogPlanes[4];
    int stage = 0;
    const drawSurf_t * drawSurf = drawSurfs[i];
    const idMaterial * shader = drawSurf.material;
    uint64 surfGLState = 0;
    begin
      loop

        -- Wait for the frontend to give us something new
        while Common.Last_Update.Get < Common.Last_Render.Get loop delay FRONTEND_WAIT_DURATION; end loop;
        View := Common.View.Get;

        -- Update dynamic viewport state and window clipping
        vkCmdSetViewport (drawCmdBuffers[i], 0, 1, (height   => View.Port.y2 + 1 - View.Port.y1,
                                                     width    => View.Port.x2 + 1 - View.Port.x1,
                                                     minDepth => 0.0,
                                                     maxDepth => 1.0););

        -- Update dynamic scissor which may be smaller than the viewport for subviews
        vkCmdSetScissor (drawCmdBuffers[i], 0, 1, &scissor.extent.width :=  backEnd.View.Port.x1 + viewDef.scissor.x1;);
                                                  scissor.extent.height := backEnd.View.Port.y1 + viewDef.scissor.y1;
                                                  scissor.offset.x := viewDef.scissor.x2 + 1 - viewDef.scissor.x1;
                                                  scissor.offset.y := viewDef.scissor.y2 + 1 - viewDef.scissor.y1;

        -- Ensure depth writes are enabled for the depth clear
        GL_State (GLS_DEFAULT);

        -- Clear the depth buffer and set the stencil to 128 for shadowing
        GL_Clear (false, true, true, STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0);

        -- Normal face culling Force face culling to set next time
        GL_Cull (CT_FRONT_SIDED); backEnd.glState.faceCulling := -1;  

        -- Bind one global Vertex Array Object (VAO)
        qglBindVertexArray (glConfig.global_vao);

        -- Set eye position shader parameter
        SetVertexParm (RENDERPARM_GLOBALEYEPOS, (View.Origin.X, View.Origin.Y, View.Origin.Z, 1.0));
        SetFragmentParm (RENDERPARM_OVERBRIGHT, (others => r_lightScale.GetFloat() * 0.5));
        SetVertexParms (RENDERPARM_PROJMATRIX_X, R_MatrixTranspose(backEnd.viewDef.projectionMatrix), 4);

        -----------------------
        -- Fill Depth Buffer --
        -----------------------
        --
        -- Start with subview surfaces, then opaque surfaces, and finally perforated surfaces
        --

        for Surface_Sort in Surface_Sort_Kind'Range loop
          for Surfaces of View.Surfaces.Element (Surface_Sort) loop
            for Surface of Surfaces loop

              -- translucent surfaces don't put anything in the depth buffer and don't
              -- test against it, which makes them fail the mirror clip plane operation
              if shader.Coverage /= MC_TRANSLUCENT then

                -- if all stages of a material have been conditioned off, don't do anything
                for ( ; stage < shader.GetNumStages; stage++   
                  const shaderStage_t * pStage = shader.GetStage( stage);

                  -- check the stage enable condition
                  exit when drawSurf.shaderRegisters[ pStage.conditionRegister ] != 0
                end
                if stage == shader.GetNumStages()
                  continue;
                end

                -- change the matrix if needed
                if drawSurf.space != backEnd.currentSpace
                  RB_SetMVP( drawSurf.space.mvp);
                  backEnd.currentSpace = drawSurf.space;
                end

                -- set polygon offset if necessary subviews will just down-modulate the color buffer, others just draw black
                surfGLState = 0;
                if shader.TestMaterialFlag( MF_POLYGONOFFSET )
                  surfGLState |= GLS_POLYGON_OFFSET;
                  GL_PolygonOffset( r_offsetFactor.GetFloat(), r_offsetUnits.GetFloat() * shader.GetPolygonOffset());
                end if;
                if shader.GetSort() == SS_SUBVIEW
                  surfGLState |= GLS_SRCBLEND_DST_COLOR | GLS_DSTBLEND_ZERO | GLS_DEPTHFUNC_LESS;
                  color[0] := (others => 1.0);
                else color[0] = 0.0f; color[1] = 0.0f; color[2] = 0.0f; color[3] = 1.0f; end if;

                if shader.Coverage = MC_OPAQUE then drawSolid = true;
                elsif shader.Coverage = MC_PERFORATED then

                  -- we may have multiple alpha tested stages
                  -- if the only alpha tested stages are condition register omitted,
                  -- draw a normal opaque surface
                  bool didDraw = false;

                  -- perforated surfaces may have multiple alpha tested stages
                  for Stage of Surface.Material.Stage loop

                    -- check the stage enable condition
                    if pStage.hasAlphaTest and drawSurf.shaderRegisters[ pStage.conditionRegister ] > 0 then

                      -- if we at least tried to draw an alpha tested stage, we won't draw the opaque surface
                      didDraw = true;

                      -- set the alpha modulate
                      color[3] = drawSurf.shaderRegisters[ pStage.color.registers[3] ];

                      -- set privatePolygonOffset if necessary
                      if pStage.privatePolygonOffset
                        GL_PolygonOffset( r_offsetFactor.GetFloat(), r_offsetUnits.GetFloat() * pStage.privatePolygonOffset);
                        stageGLState ;
                        GL_Color( color);
                        GL_State(surfGLState | GLS_POLYGON_OFFSET);
                      else
                        GL_Color( color);
                        GL_State(surfGLState);
                      end if;

                      SetFragmentParm( RENDERPARM_ALPHA_TEST, alphaTestValue( drawSurf.shaderRegisters[ pStage.alphaTestRegister ]).ToFloatPtr());
                      renderProgManager.BindShader_TextureVertexColor;
                      RB_SetVertexColorParms( SVC_IGNORE);

                      -- bind the texture
                      GL_SelectTexture( 0);
                      pStage.texture.image.Bind;

                      -- set texture matrix and texGens
                      RB_PrepareStageTexturing( pStage, drawSurf);

                      -- must render with less-equal for Z-Cull to work properly
                      assert( ( GL_GetCurrentState() & GLS_DEPTHFUNC_BITS ) == GLS_DEPTHFUNC_LESS);

                      -- draw it
                      RB_DrawElementsWithCounters( drawSurf);

                      -- clean up
                      RB_FinishStageTexturing( pStage, drawSurf);

                      -- unset privatePolygonOffset if necessary
                      if pStage.privatePolygonOffset
                        GL_PolygonOffset( r_offsetFactor.GetFloat(), r_offsetUnits.GetFloat() * shader.GetPolygonOffset());
                      end if;
                    end if;
                  end loop;
                  if not didDraw then drawSolid = true; end if;
                end

                -- draw the entire surface solid
                if drawSolid then
                  if shader.GetSort = SS_SUBVIEW then
                    renderProgManager.BindShader_Color;
                    GL_Color( color);
                    GL_State( surfGLState);
                  else
                    renderProgManager.BindShader_Depth;
                    GL_State( surfGLState | GLS_ALPHAMASK);
                  end if;

                  -- must render with less-equal for Z-Cull to work properly
                  assert ((GL_GetCurrentState() & GLS_DEPTHFUNC_BITS ) == GLS_DEPTHFUNC_LESS);

                  -- draw it
                  RB_DrawElementsWithCounters( drawSurf);
                end if;
              end if;
            end loop;
            SetFragmentParm( RENDERPARM_ALPHA_TEST, vec4_zero.ToFloatPtr());
          end loop;
        end loop;

        --------------------------------
        -- Shadowing and Interactions --
        --------------------------------
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
              if r_useLightStencilSelect.GetBool() && backEnd.viewDef.isMirror = false then

                -- Write a stencil mask for the visible light bounds to hi-stencil
                -- RB_StencilSelectLight (vLight);

                -- enable the light scissor
                if !backEnd.currentScissor.Equals( vLight.scissorRect ) && r_useScissor.GetBool()
                  GL_Scissor( backEnd.viewDef.viewport.x1 + vLight.scissorRect.x1, 
                        backEnd.viewDef.viewport.y1 + vLight.scissorRect.y1,
                        vLight.scissorRect.x2 + 1 - vLight.scissorRect.x1,
                        vLight.scissorRect.y2 + 1 - vLight.scissorRect.y1);
                  backEnd.currentScissor = vLight.scissorRect;
                end

                -- clear stencil buffer to 0 (not drawable)
                uint64 glStateMinusStencil = GL_GetCurrentStateMinusStencil;
                GL_State( glStateMinusStencil | GLS_STENCIL_FUNC_ALWAYS | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE )); -- make sure stencil mask passes for the clear
                GL_Clear( false, false, true, 0, 0.0f, 0.0f, 0.0f, 0.0f);  -- clear to 0 for stencil select

                -- set the depthbounds
                GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);


                GL_State( GLS_COLORMASK | GLS_ALPHAMASK | GLS_DEPTHMASK | GLS_DEPTHFUNC_LESS | GLS_STENCIL_FUNC_ALWAYS | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ));
                GL_Cull( CT_TWO_SIDED);

                renderProgManager.BindShader_Depth;

                -- set the matrix for deforming the 'zeroOneCubeModel' into the frustum to exactly cover the light volume
                RB_SetMVP( backEnd.viewDef.worldSpace.mvp * vLight.inverseBaseLightProject);

                -- two-sided stencil test
                qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_REPLACE, GL_ZERO);
                qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_ZERO, GL_REPLACE);
                RB_DrawElementsWithCounters( &backEnd.zeroOneCubeSurface);

                -- reset stencil state
                GL_Cull( CT_FRONT_SIDED);
                renderProgManager.Unbind;

                -- unset the depthbounds
                GL_DepthBoundsTest( 0.0f, 0.0f);
              else

                -- Always clear whole S-Cull tiles
                idScreenRect rect; rect.x1 := ( vLight.scissorRect.x1 +  0 ) & ~15;
                                  rect.y1 := ( vLight.scissorRect.y1 +  0 ) & ~15;
                                  rect.x2 := ( vLight.scissorRect.x2 + 15 ) & ~15;
                                  rect.y2 := ( vLight.scissorRect.y2 + 15 ) & ~15;
                if not backEnd.currentScissor = rect then
                  GL_Scissor (backEnd.viewDef.viewport.x1 + rect.x1,
                              backEnd.viewDef.viewport.y1 + rect.y1,
                              rect.x2 + 1 - rect.x1,
                              rect.y2 + 1 - rect.y1);
                  backEnd.currentScissor := rect;
                end if;

                -- Make sure stencil mask passes for the clear
                GL_State (GLS_DEFAULT);  
                GL_Clear (false, false, true, STENCIL_SHADOW_TEST_VALUE, 0.0f, 0.0f, 0.0f, 0.0f);
              end if;
            end if;

            for Pass_Sort in Pass_Sort_Kind'Range loop -- Global_Shadow_Sort Local_Light_Sort Local_Shadow_Sort Global_Light_Sort Translucent_Light_Sort
              case Pass_Sort is

                -------------------------
                -- Stencil Shadow Pass --
                -------------------------
                --
                --
                --

                when Shadow_Sort'Range =>
                  renderProgManager.BindShader_Shadow;
                  GL_SelectTexture( 0);
                  globalImages.BindNull;
                  uint64 glState := 0;

                  -- don't write to the color or depth buffer, just the stencil buffer
                  glState := GLS_DEPTHMASK | GLS_COLORMASK | GLS_ALPHAMASK | GLS_DEPTHFUNC_LESS;

                  GL_PolygonOffset( r_shadowPolygonFactor.GetFloat(), -r_shadowPolygonOffset.GetFloat());

                  -- the actual stencil func will be set in the draw code, but we need to make sure it isn't
                  -- disabled here, and that the value will get reset for the interactions without looking
                  -- like a no-change-required
                  GL_State( glState | GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_KEEP | GLS_STENCIL_OP_PASS_INCR | 
                    GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ) | GLS_POLYGON_OFFSET);

                  -- Two Sided Stencil reduces two draw calls to one for slightly faster shadows
                  GL_Cull( CT_TWO_SIDED);


                  -- process the chain of shadows with the current rendering state
                  backEnd.currentSpace := NULL;

                  for ( const drawSurf_t * drawSurf := drawSurfs; drawSurf /= NULL; drawSurf := drawSurf.nextOnLight ) 

                    -- make sure the shadow volume is done
                    if drawSurf.shadowVolumeState /= SHADOWVOLUME_DONE ) 
                      assert( drawSurf.shadowVolumeState = SHADOWVOLUME_UNFINISHED or drawSurf.shadowVolumeState = SHADOWVOLUME_DONE);

                      uint64 start := Sys_Microseconds;
                      while ( drawSurf.shadowVolumeState = SHADOWVOLUME_UNFINISHED ) 
                        Sys_Yield;
                      end
                      uint64 end := Sys_Microseconds;

                      backEnd.pc.shadowMicroSec += end - start;
                    end

                    -- a job may have created an empty shadow volume
                    if drawSurf.numIndexes > 0 then 
    

                    if !backEnd.currentScissor.Equals( drawSurf.scissorRect ) && r_useScissor.GetBool() ) 
                      -- change the scissor
                      GL_Scissor( backEnd.viewDef.viewport.x1 + drawSurf.scissorRect.x1,
                            backEnd.viewDef.viewport.y1 + drawSurf.scissorRect.y1,
                            drawSurf.scissorRect.x2 + 1 - drawSurf.scissorRect.x1,
                            drawSurf.scissorRect.y2 + 1 - drawSurf.scissorRect.y1);
                      backEnd.currentScissor := drawSurf.scissorRect;
                    end

                    if drawSurf.space /= backEnd.currentSpace ) 
                      -- change the matrix
                      RB_SetMVP( drawSurf.space.mvp);

                      -- set the local light position to allow the vertex program to project the shadow volume end cap to infinity
                      idVec4 localLight( 0.0f);
                      R_GlobalPointToLocal( drawSurf.space.modelMatrix, vLight.globalLightOrigin, localLight.ToVec3());
                      SetVertexParm( RENDERPARM_LOCALLIGHTORIGIN, localLight.ToFloatPtr());

                      backEnd.currentSpace := drawSurf.space;
                    end

                    renderProgManager.BindShader_Shadow;

                    -- set depth bounds per shadow
                    if r_useShadowDepthBounds.GetBool() ) 
                      GL_DepthBoundsTest( drawSurf.scissorRect.zmin, drawSurf.scissorRect.zmax);
                    end

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
                    const bool renderZPass := ( drawSurf.renderZFail = 0 ) or r_forceZPassStencilShadows.GetBool;


                    if renderZPass ) 
                      -- Z-pass
                      qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                      qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
                    elsif r_useStencilShadowPreload.GetBool() ) 
                      -- preload + Z-pass
                      qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_DECR, GL_DECR);
                      qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_INCR, GL_INCR);
                    else 
                      -- Z-fail
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
                      vertexBuffer := &vertexCache.frameData[vertexCache.drawListNum].vertexBuffer;
                    end if;
                    const int vertOffset := (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK;

                    -- get index buffer
                    const vertCacheHandle_t ibHandle := drawSurf.indexCache;
                    idIndexBuffer * indexBuffer;
                    if vertexCache.CacheIsStatic( ibHandle ) ) 
                      indexBuffer := &vertexCache.staticData.indexBuffer;
                    else 
                      const uint64 frameNum := (int)( ibHandle >> VERTCACHE_FRAME_SHIFT ) & VERTCACHE_FRAME_MASK;
                      if frameNum /= ( ( vertexCache.currentFrame - 1 ) & VERTCACHE_FRAME_MASK ) ) 
                        continue;
                      end if;
                      indexBuffer := &vertexCache.frameData[vertexCache.drawListNum].indexBuffer;
                    end if;
                    const uint64 indexOffset := (int)( ibHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK;

                    if backEnd.glState.currentIndexBuffer /= (GLuint)indexBuffer.GetAPIObject() or !r_useStateCaching.GetBool() ) 
                      qglBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer.GetAPIObject());
                      backEnd.glState.currentIndexBuffer := (GLuint)indexBuffer.GetAPIObject;
                    end if;

                    if drawSurf.jointCache ) 
                      assert( renderProgManager.ShaderUsesJoints());

                      idJointBuffer jointBuffer;
                      if !vertexCache.GetJointBuffer( drawSurf.jointCache, &jointBuffer ) ) 
                        continue;
                      end
                      assert( ( jointBuffer.GetOffset() & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) = 0);

                      const GLuint ubo := reinterpret_cast< GLuint >( jointBuffer.GetAPIObject());
                      qglBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset(), jointBuffer.GetNumJoints() * sizeof( idJointMat ));

                      if ( backEnd.glState.vertexLayout /= LAYOUT_DRAW_SHADOW_VERT_SKINNED) or ( backEnd.glState.currentVertexBuffer /= (GLuint)vertexBuffer.GetAPIObject() ) or !r_useStateCaching.GetBool() ) 
                        qglBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer.GetAPIObject());
                        backEnd.glState.currentVertexBuffer := (GLuint)vertexBuffer.GetAPIObject;

                        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX);
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

                    else 

                      if ( backEnd.glState.vertexLayout /= LAYOUT_DRAW_SHADOW_VERT ) or ( backEnd.glState.currentVertexBuffer /= (GLuint)vertexBuffer.GetAPIObject() ) or !r_useStateCaching.GetBool() ) 
                        qglBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer.GetAPIObject());
                        backEnd.glState.currentVertexBuffer := (GLuint)vertexBuffer.GetAPIObject;
                        qglEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX);
                        qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL);
                        qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR);
                        qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2);
                        qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST);
                        qglDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT);
                        qglVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof( idShadowVert ), (void *)( SHADOWVERT_XYZW_OFFSET ));
                        backEnd.glState.vertexLayout := LAYOUT_DRAW_SHADOW_VERT;
                      end
                    end

                    renderProgManager.CommitUniforms;

                    if drawSurf.jointCache ) 
                      qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVertSkinned ));
                    end else 
                      qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVert ));
                    end

                    if !renderZPass && r_useStencilShadowPreload.GetBool() ) 
                      -- render again with Z-pass
                      qglStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                      qglStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);

                      if drawSurf.jointCache ) 
                        qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof ( idShadowVertSkinned ));
                      end else 
                        qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf.numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof ( idShadowVert ));
                      end
                    end
                  end

                  -- cleanup the shadow specific rendering state and reset depth bounds
                  GL_Cull( CT_FRONT_SIDED);
                  GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);

                ----------------------
                -- Perform lighting --
                ----------------------
                --
                --
                --

                when Light_Sort'Range =>
                  if vLight.translucentInteractions /= NULL and not r_skipTranslucent.GetBool() then

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

                    const idMaterial * surfaceShader := walk.material;
                    if surfaceShader.GetFastPathBumpImage() ) 
                      allSurfaces.Append( walk);
                    end else 
                      complexSurfaces.Append( walk);
                    end
                  end
                  for ( int i := 0; i < complexSurfaces.Num; i++ ) 
                    allSurfaces.Append( complexSurfaces[i]);
                  end

                  bool lightDepthBoundsDisabled := false;

                  for ( int lightStageNum := 0; lightStageNum < lightShader.GetNumStages; lightStageNum++ ) 
                    const shaderStage_t *lightStage := lightShader.GetStage( lightStageNum);

                    -- ignore stages that fail the condition
                    if !lightRegs[ lightStage.conditionRegister ] ) 
                      continue;
                    end

                    const float lightScale := r_lightScale.GetFloat;
                    const idVec4 lightColor(
                      lightScale * lightRegs[ lightStage.color.registers[0] ],
                      lightScale * lightRegs[ lightStage.color.registers[1] ],
                      lightScale * lightRegs[ lightStage.color.registers[2] ],
                      lightRegs[ lightStage.color.registers[3] ]);
                    -- apply the world-global overbright and the 2x factor for specular
                    const idVec4 diffuseColor := lightColor;
                    const idVec4 specularColor := lightColor * 2.0f;

                    float lightTextureMatrix[16];
                    if lightStage.texture.hasMatrix ) 
                      RB_GetShaderTextureMatrix( lightRegs, &lightStage.texture, lightTextureMatrix);
                    end

                    -- texture 1 will be the light falloff texture
                    GL_SelectTexture( INTERACTION_TEXUNIT_FALLOFF);
                    vLight.falloffImage.Bind;

                    -- texture 2 will be the light projection texture
                    GL_SelectTexture( INTERACTION_TEXUNIT_PROJECTION);
                    lightStage.texture.image.Bind;

                    -- force the light textures to not use anisotropic filtering, which is wasted on them
                    -- all of the texture sampler parms should be constant for all interactions, only
                    -- the actual texture image bindings will change

                    -- For all surfaces on this light list, generate an interaction for this light stage
                    -- setup renderparms assuming we will be drawing trivial surfaces first
                    RB_SetupForFastPathInteractions( diffuseColor, specularColor);

                    -- even if the space does not change between light stages, each light stage may need a different lightTextureMatrix baked in
                    backEnd.currentSpace := NULL;

                    for ( int sortedSurfNum := 0; sortedSurfNum < allSurfaces.Num; sortedSurfNum++ ) 
                      const drawSurf_t * const surf := allSurfaces[ sortedSurfNum ];

                      -- select the render prog
                      if lightShader.IsAmbientLight then renderProgManager.BindShader_InteractionAmbient;
                      else renderProgManager.BindShader_Interaction; end if;

                      const idMaterial * surfaceShader := surf.material;
                      const float * surfaceRegs := surf.shaderRegisters;

                      inter.surf := surf;

                      -- change the MVP matrix, view/light origin and light projection vectors if needed
                      if surf.space /= backEnd.currentSpace ) 
                        backEnd.currentSpace := surf.space;

                        -- turn off the light depth bounds test if this model is rendered with a depth hack
                        if useLightDepthBounds ) 
                          if !surf.space.weaponDepthHack && surf.space.modelDepthHack = 0.0f ) 
                            if lightDepthBoundsDisabled ) 
                              GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);
                              lightDepthBoundsDisabled := false;
                            end
                          end else 
                            if !lightDepthBoundsDisabled ) 
                              GL_DepthBoundsTest( 0.0f, 0.0f);
                              lightDepthBoundsDisabled := true;
                            end
                          end
                        end

                        -- model-view-projection
                        RB_SetMVP( surf.space.mvp);

                        -- tranform the light/view origin into model local space
                        idVec4 localLightOrigin( 0.0f);
                        idVec4 localViewOrigin( 1.0f);
                        R_GlobalPointToLocal( surf.space.modelMatrix, vLight.globalLightOrigin, localLightOrigin.ToVec3());
                        R_GlobalPointToLocal( surf.space.modelMatrix, backEnd.viewDef.renderView.vieworg, localViewOrigin.ToVec3());

                        -- set the local light/view origin
                        SetVertexParm( RENDERPARM_LOCALLIGHTORIGIN, localLightOrigin.ToFloatPtr());
                        SetVertexParm( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr());

                        -- transform the light project into model local space
                        idPlane lightProjection[4];
                        for ( int i := 0; i < 4; i++ ) 
                          R_GlobalPlaneToLocal( surf.space.modelMatrix, vLight.lightProject[i], lightProjection[i]);
                        end

                        -- optionally multiply the local light projection by the light texture matrix
                        if lightStage.texture.hasMatrix then
                          RB_BakeTextureMatrixIntoTexgen( lightProjection, lightTextureMatrix);
                          float genMatrix[16];
                          float final[16];

                          genMatrix[0*4+0] = lightProject[0][0]; genMatrix[1*4+0] = lightProject[0][1]; genMatrix[2*4+0] = lightProject[0][2]; genMatrix[3*4+0] = lightProject[0][3];
                          genMatrix[0*4+1] = lightProject[1][0]; genMatrix[1*4+1] = lightProject[1][1]; genMatrix[2*4+1] = lightProject[1][2]; genMatrix[3*4+1] = lightProject[1][3];
                          genMatrix[0*4+2] = 0.0f; genMatrix[1*4+2] = 0.0f; genMatrix[2*4+2] = 0.0f; genMatrix[3*4+2] = 0.0f;
                          genMatrix[0*4+3] = lightProject[2][0]; genMatrix[1*4+3] = lightProject[2][1]; genMatrix[2*4+3] = lightProject[2][2]; genMatrix[3*4+3] = lightProject[2][3];

                          R_MatrixMultiply( genMatrix, textureMatrix, final);

                          lightProject[0][0] = final[0*4+0]; lightProject[0][1] = final[1*4+0]; lightProject[0][2] = final[2*4+0]; lightProject[0][3] = final[3*4+0];
                          lightProject[1][0] = final[0*4+1]; lightProject[1][1] = final[1*4+1]; lightProject[1][2] = final[2*4+1]; lightProject[1][3] = final[3*4+1];
                        end

                        -- set the light projection
                        SetVertexParm( RENDERPARM_LIGHTPROJECTION_S, lightProjection[0].ToFloatPtr());
                        SetVertexParm( RENDERPARM_LIGHTPROJECTION_T, lightProjection[1].ToFloatPtr());
                        SetVertexParm( RENDERPARM_LIGHTPROJECTION_Q, lightProjection[2].ToFloatPtr());
                        SetVertexParm( RENDERPARM_LIGHTFALLOFF_S, lightProjection[3].ToFloatPtr());
                      end

                      -- check for the fast path
                      if surfaceShader.GetFastPathBumpImage() && !r_skipInteractionFastPath.GetBool() ) 

                        -- texture 0 will be the per-surface bump map
                        GL_SelectTexture( INTERACTION_TEXUNIT_BUMP);
                        surfaceShader.GetFastPathBumpImage().Bind;

                        -- texture 3 is the per-surface diffuse map
                        GL_SelectTexture( INTERACTION_TEXUNIT_DIFFUSE);
                        surfaceShader.GetFastPathDiffuseImage().Bind;

                        -- texture 4 is the per-surface specular map
                        GL_SelectTexture( INTERACTION_TEXUNIT_SPECULAR);
                        surfaceShader.GetFastPathSpecularImage().Bind;

                        RB_DrawElementsWithCounters( surf);

                      else
                        inter.bumpImage := NULL;
                        inter.specularImage := NULL;
                        inter.diffuseImage := NULL;
                        inter.diffuseColor[0] := inter.diffuseColor[1] := inter.diffuseColor[2] := inter.diffuseColor[3] := 0;
                        inter.specularColor[0] := inter.specularColor[1] := inter.specularColor[2] := inter.specularColor[3] := 0;

                        -- go through the individual surface stages
                        -- This is somewhat arcane because of the old support for video cards that had to render
                        -- interactions in multiple passes.
                        -- We also have the very rare case of some materials that have conditional interactions
                        -- for the "hell writing" that can be shined on them.
                        for ( int surfaceStageNum := 0; surfaceStageNum < surfaceShader.GetNumStages; surfaceStageNum++ ) 
                          const shaderStage_t *surfaceStage := surfaceShader.GetStage( surfaceStageNum);

                          switch( surfaceStage.lighting ) 
                            case SL_BUMP: 
                              -- ignore stage that fails the condition
                              if !surfaceRegs[ surfaceStage.conditionRegister ] ) 
                                break;
                              end
                              -- draw any previous interaction
                              if inter.bumpImage /= NULL ) 
                                RB_DrawSingleInteraction( &inter);
                              end
                              inter.bumpImage := surfaceStage.texture.image;
                              inter.diffuseImage := NULL;
                              inter.specularImage := NULL;
                              RB_SetupInteractionStage( surfaceStage, surfaceRegs, NULL,
                                          inter.bumpMatrix, NULL);
                              break;
                            end
                            case SL_DIFFUSE: 
                              -- ignore stage that fails the condition
                              if !surfaceRegs[ surfaceStage.conditionRegister ] ) 
                                break;
                              end
                              -- draw any previous interaction
                              if inter.diffuseImage /= NULL ) 
                                RB_DrawSingleInteraction( &inter);
                              end
                              inter.diffuseImage := surfaceStage.texture.image;
                              inter.vertexColor := surfaceStage.vertexColor;
                              RB_SetupInteractionStage( surfaceStage, surfaceRegs, diffuseColor.ToFloatPtr(),
                                          inter.diffuseMatrix, inter.diffuseColor.ToFloatPtr());
                              break;
                            end
                            case SL_SPECULAR: 
                              -- ignore stage that fails the condition
                              if !surfaceRegs[ surfaceStage.conditionRegister ] ) 
                                break;
                              end
                              -- draw any previous interaction
                              if inter.specularImage /= NULL ) 
                                RB_DrawSingleInteraction( &inter);
                              end
                              inter.specularImage := surfaceStage.texture.image;
                              inter.vertexColor := surfaceStage.vertexColor;
                              RB_SetupInteractionStage( surfaceStage, surfaceRegs, specularColor.ToFloatPtr(),
                                          inter.specularMatrix, inter.specularColor.ToFloatPtr());
                              break;
                            end
                          end
                        end
                      end if;

                      -- draw the final interaction
                      RB_DrawSingleInteraction( &inter);
                    end
                  end

                  if useLightDepthBounds && lightDepthBoundsDisabled ) 
                    GL_DepthBoundsTest( vLight.scissorRect.zmin, vLight.scissorRect.zmax);
                  end

                  renderProgManager.Unbind;
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

        --------------------------
        -- Shade Ambient Lights --
        --------------------------
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
        if backEnd.viewDef.viewEntitys && r_skipAmbient.GetBool()
          return numDrawSurfs;
        end

        GL_SelectTexture( 1);
        globalImages.BindNull;
        GL_SelectTexture( 0);
        backEnd.currentSpace = (const viewEntity_t *)1; -- using NULL makes /analyze think surf.space needs to be checked...
        float currentGuiStereoOffset = 0.0f;

        for Surface of Surfaces loop
          const drawSurf_t * surf = drawSurfs[i];
          const idMaterial * shader = ;

          -- some deforms may disable themselves by setting numIndexes = 0
          if surf.material.HasAmbient and not surf.material.IsPortalSky() and surf.numIndexes /= 0 and not surf.material.SuppressInSubview then

            if backEnd.viewDef.isXraySubview && surf.space.entityDef
              if surf.space.entityDef.parms.xrayIndex != 2
                continue;
              end
            end

            -- we need to draw the post process shaders after we have drawn the fog lights
            exit when surf.material.GetSort() >= SS_POST_PROCESS && !backEnd.currentRenderCopied

            -- if we are rendering a 3D view and the surface's eye index doesn't match 
            -- the current view's eye index then we skip the surface
            -- if the stereoEye value of a surface is 0 then we need to draw it for both eyes.
            const int shaderStereoEye = surf.material.GetStereoEye;
            const bool isEyeValid = stereoRender_swapEyes.GetBool() ? ( shaderStereoEye == stereoEye ) : ( shaderStereoEye != stereoEye);
            if ( stereoEye != 0 ) && ( shaderStereoEye != 0 ) && ( isEyeValid )
              continue;
            end

            -- determine the stereoDepth offset 
            -- guiStereoScreenOffset will always be zero for 3D views, so the !=
            -- check will never force an update due to the current sort value.
            const float thisGuiStereoOffset = guiStereoScreenOffset * surf.sort;

            -- change the matrix and other space related vars if needed
            if surf.space != backEnd.currentSpace || thisGuiStereoOffset != currentGuiStereoOffset
              backEnd.currentSpace = surf.space;
              currentGuiStereoOffset = thisGuiStereoOffset;

              const viewEntity_t *space = backEnd.currentSpace;

              if guiStereoScreenOffset != 0.0f
                RB_SetMVPWithStereoOffset( space.mvp, currentGuiStereoOffset);
              end else {
                RB_SetMVP( space.mvp);
              end

              -- set eye position in local space
              idVec4 localViewOrigin( 1.0f);
              R_GlobalPointToLocal( space.modelMatrix, backEnd.viewDef.renderView.vieworg, localViewOrigin.ToVec3());
              SetVertexParm( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr());

              -- set model Matrix
              float modelMatrixTranspose[16];
              R_MatrixTranspose( space.modelMatrix, modelMatrixTranspose);
              SetVertexParms( RENDERPARM_MODELMATRIX_X, modelMatrixTranspose, 4);

              -- Set ModelView Matrix
              float modelViewMatrixTranspose[16];
              R_MatrixTranspose( space.modelViewMatrix, modelViewMatrixTranspose);
              SetVertexParms( RENDERPARM_MODELVIEWMATRIX_X, modelViewMatrixTranspose, 4);
            end

            -- change the scissor if needed
            if !backEnd.currentScissor.Equals( surf.scissorRect ) && r_useScissor.GetBool()
              GL_Scissor( backEnd.viewDef.viewport.x1 + surf.scissorRect.x1, 
                    backEnd.viewDef.viewport.y1 + surf.scissorRect.y1,
                    surf.scissorRect.x2 + 1 - surf.scissorRect.x1,
                    surf.scissorRect.y2 + 1 - surf.scissorRect.y1);
              backEnd.currentScissor = surf.scissorRect;
            end

            -- set face culling appropriately
            GL_Cull ((if surf.space.isGuiSurface then CT_TWO_SIDED else shader.GetCullType));
            uint64 surfGLState = surf.extraGLState;

            -- set polygon offset if necessary
            if shader.TestMaterialFlag(MF_POLYGONOFFSET)
              GL_PolygonOffset( r_offsetFactor.GetFloat(), r_offsetUnits.GetFloat() * shader.GetPolygonOffset());
              surfGLState = GLS_POLYGON_OFFSET;
            end

            -- Ambient lighting
            for ( int stage = 0; stage < shader.GetNumStages; stage++    

              -- check the enable condition and skip the stages involved in lighting
              if surf.shaderRegisters[ shader.GetStage(stage).conditionRegister ] /= 0 and shader.GetStage(stage).lighting = SL_AMBIENT then

                uint64 stageGLState = surfGLState;
                if ( surfGLState & GLS_OVERRIDE ) == 0
                  stageGLState |= shader.GetStage(stage).drawStateBits;
                end

                -- skip if the stage is ( GL_ZERO, GL_ONE ), which is used for some alpha masks
                if ( stageGLState & ( GLS_SRCBLEND_BITS | GLS_DSTBLEND_BITS ) ) == ( GLS_SRCBLEND_ZERO | GLS_DSTBLEND_ONE )
                  continue;
                end

                -- stages
                newShaderStage_t *newStage = pStage.newStage;
                GL_State( stageGLState);
              
                renderProgManager.BindShader( newStage.glslProgram, newStage.glslProgram);

                for ( int j = 0; j < newStage.numVertexParms; j++ -- get the expressions for conditionals / color / texcoords
                  SetVertexParm( (renderParm_t)( RENDERPARM_USER + j ), surf.shaderRegisters[ newStage.vertexParms[j]);
                end

                -- set rpEnableSkinning if the shader has optional support for skinning
                if surf.jointCache && renderProgManager.ShaderHasOptionalSkinning()
                  const idVec4 skinningParm( 1.0f);
                  SetVertexParm( RENDERPARM_ENABLE_SKINNING, skinningParm.ToFloatPtr());
                end

                -- bind texture units
                for ( int j = 0; j < newStage.numFragmentProgramImages; j++
                  idImage * image = newStage.fragmentProgramImages[j];
                  if image != NULL
                    GL_SelectTexture( j);
                    image.Bind;
                  end
                end

                -- draw it
                RB_DrawElementsWithCounters( surf);

                -- unbind texture units
                for ( int j = 0; j < newStage.numFragmentProgramImages; j++
                  idImage * image = newStage.fragmentProgramImages[j];
                  if image != NULL
                    GL_SelectTexture( j);
                    globalImages.BindNull;
                  end
                end

                -- clear rpEnableSkinning if it was set
                if surf.jointCache && renderProgManager.ShaderHasOptionalSkinning()
                  const idVec4 skinningParm( 0.0f);
                  SetVertexParm( RENDERPARM_ENABLE_SKINNING, skinningParm.ToFloatPtr());
                end

                GL_SelectTexture( 0);
                renderProgManager.Unbind;
                continue;
              end if;
            end loop;
          end if;
        end loop;

        GL_Cull( CT_FRONT_SIDED);
        GL_Color( 1.0, 1.0, 1.0);

        -- fog and blend lights, drawn after emissive surfaces
        -- so they are properly dimmed down
        if r_skipFogLights.GetBool() || r_showOverDraw.GetInteger() != 0 
           || backEnd.viewDef.isXraySubview /* don't fog in xray mode*/
          return;
        end

        -- force fog plane to recalculate
        backEnd.currentSpace = NULL;
        for ( viewLight_t * vLight = backEnd.viewDef.viewLights; vLight != NULL; vLight = vLight.next
          case Light.Kind is

            ---------------
            -- Fog light --
            ---------------
            --
            --
            --

            when Fog_Light =>
              const float FOG_SCALE = 0.001f;
              float a;

              -- find the current color and density of the fog
              const idMaterial * lightShader = vLight.lightShader;
              const float * regs = vLight.shaderRegisters;
              -- assume fog shaders have only a single stage
              const shaderStage_t * stage = lightShader.GetStage( 0);

              GL_Color( regs[ stage.color.registers]);

              -- calculate the falloff planes -- if they left the default value on, set a fog distance of 500 otherwise, distance = alpha color
              a := (if regs[ stage.color.registers][3] <= 1.0 then -0.5 / DEFAULT_FOG_DISTANCE
                    else -0.5 / regs[ stage.color.registers][3]);

              -- texture 0 is the falloff image
              GL_SelectTexture( 0);
              globalImages.fogImage.Bind;

              -- texture 1 is the entering plane fade correction
              GL_SelectTexture( 1);
              globalImages.fogEnterImage.Bind;

              

              -- S-0
              fogPlanes[0][0] = a * backEnd.viewDef.worldSpace.modelViewMatrix[0*4+2]; fogPlanes[0][1] = a * backEnd.viewDef.worldSpace.modelViewMatrix[1*4+2]; fogPlanes[0][2] = a * backEnd.viewDef.worldSpace.modelViewMatrix[2*4+2]; fogPlanes[0][3] = a * backEnd.viewDef.worldSpace.modelViewMatrix[3*4+2] + 0.5f;

              -- T-0
              fogPlanes[1][0] = 0.0f;--a * backEnd.viewDef.worldSpace.modelViewMatrix[0*4+0];
              fogPlanes[1][1] = 0.0f;--a * backEnd.viewDef.worldSpace.modelViewMatrix[1*4+0];
              fogPlanes[1][2] = 0.0f;--a * backEnd.viewDef.worldSpace.modelViewMatrix[2*4+0];
              fogPlanes[1][3] = 0.5f;--a * backEnd.viewDef.worldSpace.modelViewMatrix[3*4+0] + 0.5f;

              -- T-1 will get a texgen for the fade plane, which is always the "top" plane on unrotated lights
              fogPlanes[2][0] = FOG_SCALE * vLight.fogPlane[0]; fogPlanes[2][1] = FOG_SCALE * vLight.fogPlane[1]; fogPlanes[2][2] = FOG_SCALE * vLight.fogPlane[2]; fogPlanes[2][3] = FOG_SCALE * vLight.fogPlane[3] + FOG_ENTER;

              -- S-1
              fogPlanes[3][0] = 0.0f; fogPlanes[3][1] = 0.0f; fogPlanes[3][2] = 0.0f; fogPlanes[3][3] = FOG_SCALE * vLight.fogPlane.Distance( backEnd.viewDef.renderView.vieworg) + FOG_ENTER;

              -- draw it
              GL_State( GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_EQUAL);
              RB_T_BasicFog( drawSurfs, fogPlanes, NULL);
              RB_T_BasicFog( drawSurfs2, fogPlanes, NULL);

              -- the light frustum bounding planes aren't in the depth buffer, so use depthfunc_less instead
              -- of depthfunc_equal
              GL_State( GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_LESS);
              GL_Cull( CT_BACK_SIDED);

              backEnd.zeroOneCubeSurface.space = &backEnd.viewDef.worldSpace;
              backEnd.zeroOneCubeSurface.scissorRect = backEnd.viewDef.scissor;

              -- S is based on the view origin
              RB_T_BasicFog( &backEnd.zeroOneCubeSurface, fogPlanes, &vLight.inverseBaseLightProject);

              GL_Cull( CT_FRONT_SIDED);

              GL_SelectTexture( 1);
              globalImages.BindNull;

              GL_SelectTexture( 0);

              renderProgManager.Unbind;

            -----------------
            -- Blend light --
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

              renderProgManager.BindShader_BlendLight;

              for ( int i = 0; i < vLight.lightShader.GetNumStages; i++
                const shaderStage_t *stage = vLight.lightShader.GetStage(i);

                if !vLight.shaderRegisters[ stage.conditionRegister ]
                  continue;
                end

                GL_State( GLS_DEPTHMASK | stage.drawStateBits | GLS_DEPTHFUNC_EQUAL);

                GL_SelectTexture( 0);
                stage.texture.image.Bind;

                if stage.texture.hasMatrix
                  RB_LoadShaderTextureMatrix( vLight.shaderRegisters, &stage.texture);
                end

                -- get the modulate values from the light, including alpha, unlike normal lights
                GL_Color( vLight.shaderRegisters[ stage.color.registers[);

                RB_T_BlendLight( drawSurfs, vLight);
                RB_T_BlendLight( drawSurfs2, vLight);
              end

              GL_SelectTexture( 1);
              globalImages.BindNull;
              GL_SelectTexture( 0);
              renderProgManager.Unbind;
          end case;
        end loop;

        -- capture the depth for the motion blur before rendering any post process surfaces that may contribute to the depth
        if r_motionBlur.GetInteger() > 0
          const idScreenRect & viewport = backEnd.viewDef.viewport;
          globalImages.currentDepthImage.CopyDepthbuffer( viewport.x1, viewport.y1, viewport.GetWidth(), viewport.GetHeight());
        end
      end loop;
    end;
