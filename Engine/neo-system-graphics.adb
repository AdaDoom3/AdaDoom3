package body Neo.System.Graphics is
    procedure Test is
      begin
        Put_Title("GRAPHICS TEST");
      end Test;
    package body Direct3D is separate;
    package body OpenGL   is separate;
    package body GCM      is separate;
    procedure Clear(Color : in Record_Color) is
      begin
        Status.Set_Busy(True);
        Scissor(0, 0, tr.GetWidth, tr.GetHeight);
        Clear(True, False, False, 0, Clear_Color);
        Status.Set_Busy(False);
      end Clear;
    procedure Get_Frame(Card : in Integer_4_Positive := 1) return Record_Graphic is
      begin
        Status.Set_Busy(True);
        declare Frame := API.Get_Frame; begin
          Status.Set_Busy(False);
          return Frame
        end; 
      end Get_Frame; 
    procedure Post_Process(Card : in Integer_4_Positive := 1) is-- resolve the scaled rendering to a temporary texture
      begin
        Status.Set_Busy(True);
        postProcessCommand_t * cmd = (postProcessCommand_t *)data;
        const idScreenRect & viewport = cmd->viewDef->viewport;
        globalImages->currentRenderImage->CopyFramebuffer( viewport.x1, viewport.y1, viewport.GetWidth(), viewport.GetHeight() );
        State(GLS_SRCBLEND_ONE | GLS_DSTBLEND_ZERO | GLS_DEPTHMASK | GLS_DEPTHFUNC_ALWAYS);
        Cull(CT_TWO_SIDED);
        Viewport(0, 0, renderSystem->GetWidth(), renderSystem->GetHeight());
        Scissor(0, 0, renderSystem->GetWidth(), renderSystem->GetHeight());
        Select_Texture(0);
        globalImages->currentRenderImage->Bind();
        renderProgManager.BindShader_PostProcess();
        Draw(&backEnd.unitSquareSurface);
        Status.Set_Busy(False);
      end Post_Process;
    procedure Draw(View : in Record_View; Card : in Integer_4_Positive := 1) is
      procedure Load(Texture : in Record_Texture) is
        begin
          if texture->hasMatrix then
            SetVertexParm(RENDERPARM_TEXTUREMATRIX_S, matrix[0*4+0]; matrix[1*4+0]; matrix[2*4+0]; matrix[3*4+0];
            SetVertexParm(RENDERPARM_TEXTUREMATRIX_T, matrix[0*4+1];matrix[1*4+1]; matrix[2*4+1]; matrix[3*4+1];
          else
            SetVertexParm(RENDERPARM_TEXTUREMATRIX_S, { 1.0f, 0.0f, 0.0f, 0.0f end ;;);
            SetVertexParm(RENDERPARM_TEXTUREMATRIX_T, { 0.0f, 1.0f, 0.0f, 0.0f end ;;);
          end if;
        end Load;
      procedure Texture(Surface : in Record_Surface) is
        begin
          case Surface.Texture.Generator is
            when Sky_Box_Generator =>
              renderProgManager.BindShader_SkyBox();
            when Diffuse_Cube_Generator | Glass_Warp_Generator =>
              Put_Debug_Line(Localize("Using ") & Enumerated_Generator'wide_image(Surface.Texture.Generator) & Localize("! Please contact Brian!"));
            when Reflect_Cube_Generator => 
              const shaderStage_t *bumpStage = surf->material->GetBumpStage();
              if bumpStage != NULL then
                -- per-pixel reflection mapping with bump mapping
                GL_SelectTexture( 1 );
                bumpStage->texture.image->Bind();
                GL_SelectTexture( 0 );
                if ( surf->jointCache ) { renderProgManager.BindShader_BumpyEnvironmentSkinned();
                else { renderProgManager.BindShader_BumpyEnvironment();
              else
                if ( surf->jointCache ) { renderProgManager.BindShader_EnvironmentSkinned();
                else renderProgManager.BindShader_Environment();
              end if;
            when Wobble_Sky_Generator =>
              const int * parms = surf->material->GetTexGenRegisters();
              float wobbleDegrees = surf->shaderRegisters[ parms[0] ] * ( idMath::PI / 180.0f );
              float wobbleSpeed = surf->shaderRegisters[ parms[1] ] * ( 2.0f * idMath::PI / 60.0f );
              float rotateSpeed = surf->shaderRegisters[ parms[2] ] * ( 2.0f * idMath::PI / 60.0f );
              idVec3 axis[3]; -- very ad-hoc "wobble" transform
              float s, c;
              idMath::SinCos( wobbleSpeed * backEnd.viewDef->renderView.time[0] * 0.001f, s, c );
              float ws, wc;
              idMath::SinCos( wobbleDegrees, ws, wc );
              axis[2][0] = ws * c;
              axis[2][1] = ws * s;
              axis[2][2] = wc;
              axis[1][0] = -s * s * ws;
              axis[1][2] = -s * ws * ws;
              axis[1][1] = idMath::Sqrt( idMath::Fabs( 1.0f - ( axis[1][0] * axis[1][0] + axis[1][2] * axis[1][2] ) ) );
              axis[1] -= ( axis[2] * axis[1] ) * axis[2]; -- make the second vector exactly perpendicular to the first
              axis[1].Normalize();
              axis[0].Cross( axis[1], axis[2] ); -- construct the third with a cross
              float rs, rc; -- add the rotate
              idMath::SinCos( rotateSpeed * backEnd.viewDef->renderView.time[0] * 0.001f, rs, rc );
              float transform[12];
              transform[0*4+0] = axis[0][0] * rc + axis[1][0] * rs;
              transform[0*4+1] = axis[0][1] * rc + axis[1][1] * rs;
              transform[0*4+2] = axis[0][2] * rc + axis[1][2] * rs;
              transform[0*4+3] = 0.0f;
              transform[1*4+0] = axis[1][0] * rc - axis[0][0] * rs;
              transform[1*4+1] = axis[1][1] * rc - axis[0][1] * rs;
              transform[1*4+2] = axis[1][2] * rc - axis[0][2] * rs;
              transform[1*4+3] = 0.0f;
              transform[2*4+0] = axis[2][0];
              transform[2*4+1] = axis[2][1];
              transform[2*4+2] = axis[2][2];
              transform[2*4+3] = 0.0f;
              SetVertexParms( RENDERPARM_WOBBLESKY_X, transform, 3 );
              renderProgManager.BindShader_WobbleSky();
            when Screen_Generator =>
              useTexGenParm[0] = 1.0f;
              useTexGenParm[1] = 1.0f;
              useTexGenParm[2] = 1.0f;
              useTexGenParm[3] = 1.0f;
              float mat[16];
              R_MatrixMultiply( surf->space->modelViewMatrix, backEnd.viewDef->projectionMatrix, mat );
              float plane[4];
              SetVertexParm( RENDERPARM_TEXGEN_0_S, mat[0*4+0];mat[1*4+0];mat[2*4+0];mat[3*4+0];
              SetVertexParm( RENDERPARM_TEXGEN_0_T, mat[0*4+1]; mat[1*4+1];mat[2*4+1]; mat[3*4+1];
              SetVertexParm( RENDERPARM_TEXGEN_0_Q, mat[0*4+3];mat[1*4+3];mat[2*4+3];mat[3*4+3];
          end case;
          SetVertexParm( RENDERPARM_TEXGEN_0_ENABLED, useTexGenParm );
          Draw();
          if Stage.texture.is_cinematic then -- unbind the extra bink textures
            SelectTexture(1);
            globalImages->BindNull();
            SelectTexture(2);
            globalImages->BindNull();
            SelectTexture(0);
          end if;
          if Stage->texture.texgen == TG_REFLECT_CUBE then -- see if there is also a bump map specified
            if surf->material->GetBumpStage(); != NULL then -- per-pixel reflection mapping with bump mapping
              SelectTexture(1);
              globalImages->BindNull;
              SelectTexture(0);
            end if;
            renderProgManager.Unbind();
          end if;
        end Texture;
      procedure Shade is( const drawSurf_t * const * const drawSurfs, const int numDrawSurfs, const float guiStereoScreenOffset, const int stereoEye ) {
        begin
          SelectTexture(1);
          globalImages->BindNull
          SelectTexture(0);
          for Surface in Surfaces loop
            if Surface.Material.Shader.Has_Ambient and not Surface.Material.Shader.Is_Portal_Sky and not shader->SuppressInSubview then
              if backEnd.viewDef->isXraySubview && surf->space->entityDef then {
                if surf->space->entityDef->parms.xrayIndex != 2 then
                  continue;
                end if;
              end if;
              -- we need to draw the post process shaders after we have drawn the fog lights
              if shader->GetSort() >= SS_POST_PROCESS && !backEnd.currentRenderCopied ) {
                break;
              end if;
              -- if we are rendering a 3D view and the surface's eye index doesn't match the current view's eye index then we skip the surface if the stereoEye value of a surface is 0 then we need to draw it for both eyes.
              const int shaderStereoEye = shader->GetStereoEye();
              const bool isEyeValid = stereoRender_swapEyes.GetBool() ? ( shaderStereoEye == stereoEye ) : ( shaderStereoEye != stereoEye );
              if ( stereoEye != 0 ) && ( shaderStereoEye != 0 ) && ( isEyeValid ) ) {
                continue;
              end if;
              -- determine the stereoDepth offset guiStereoScreenOffset will always be zero for 3D views, so the != check will never force an update due to the current sort value.
              const float thisGuiStereoOffset = guiStereoScreenOffset * surf->sort;
              -- change the matrix and other space related vars if needed
              if surf->space != backEnd.currentSpace || thisGuiStereoOffset != currentGuiStereoOffset then
                backEnd.currentSpace = surf->space;
                currentGuiStereoOffset = thisGuiStereoOffset;
                const viewEntity_t *space = backEnd.currentSpace;
                if guiStereoScreenOffset != 0.0f ) { RB_SetMVPWithStereoOffset( space->mvp, currentGuiStereoOffset );
                else Set_MVP( space->mvp ); end ;
                -- set eye position in local space
                idVec4 localViewOrigin( 1.0f );
                R_GlobalPointToLocal( space->modelMatrix, backEnd.viewDef->renderView.vieworg, localViewOrigin.ToVec3() );
                SetVertexParm( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr() );
                -- set model Matrix
                float modelMatrixTranspose[16];
                R_MatrixTranspose( space->modelMatrix, modelMatrixTranspose );
                SetVertexParms( RENDERPARM_MODELMATRIX_X, modelMatrixTranspose, 4 );
                -- Set ModelView Matrix
                float modelViewMatrixTranspose[16];
                R_MatrixTranspose( space->modelViewMatrix, modelViewMatrixTranspose );
                SetVertexParms( RENDERPARM_MODELVIEWMATRIX_X, modelViewMatrixTranspose, 4 );
              end if;
              -- change the scissor if needed
              if Surfaces.Scissor /= Get_Current_Scissor then 
                GL_Scissor( backEnd.viewDef->viewport.x1 + surf->scissorRect.x1, 
                      backEnd.viewDef->viewport.y1 + surf->scissorRect.y1,
                      surf->scissorRect.x2 + 1 - surf->scissorRect.x1,
                      surf->scissorRect.y2 + 1 - surf->scissorRect.y1 );
              end if;
              -- get the expressions for conditionals / color / texcoords
              if ( surf->space->isGuiSurface ) { GL_Cull( CT_TWO_SIDED ); -- set face culling appropriately
              else { GL_Cull( shader->GetCullType() ); end if;
              if shader->TestMaterialFlag(MF_POLYGONOFFSET) then -- set polygon offset if necessary
                GL_PolygonOffset( r_offsetFactor.GetFloat(), r_offsetUnits.GetFloat() * shader->GetPolygonOffset() );
                surfGLState = GLS_POLYGON_OFFSET;
              end if;
              for Stage of Surface.Material.Stages loop  
                const shaderStage_t *pStage = shader->GetStage(stage);
                -- check the enable condition
                if ( regs[ pStage->conditionRegister ] = 0 then
                  continue;
                end if;
                -- skip the stages involved in lighting
                if pStage->lighting != SL_AMBIENT then
                  continue;
                end if;
                if (surfGLState & GLS_OVERRIDE) = 0 then
                  stageGLState |= pStage->drawStateBits;
                end if;
                -- skip if the stage is ( GL_ZERO, GL_ONE ), which is used for some alpha masks
                if (stageGLState & ( GLS_SRCBLEND_BITS | GLS_DSTBLEND_BITS)) = (GLS_SRCBLEND_ZERO | GLS_DSTBLEND_ONE) then
                  continue;
                end if;
                State(stageGLState);
                renderProgManager.BindShader( newStage->glslProgram, newStage->glslProgram );
                for ( int j = 0; j < newStage->numVertexParms; j++ ) {
                  float parm[4];
                  parm[0] = regs[ newStage->vertexParms[j][0] ];
                  parm[1] = regs[ newStage->vertexParms[j][1] ];
                  parm[2] = regs[ newStage->vertexParms[j][2] ];
                  parm[3] = regs[ newStage->vertexParms[j][3] ];
                  SetVertexParm( (renderParm_t)( RENDERPARM_USER + j ), parm );
                end ;
                -- set rpEnableSkinning if the shader has optional support for skinning
                if Surface.Joints.Are_Present and renderProgManager.ShaderHasOptionalSkinning() then
                  SetVertexParm( RENDERPARM_ENABLE_SKINNING, s 1.0f ).ToFloatPtr() );
                end if;
                for Image in Stage.Fragment_Program_Images loop -- bind texture units
                  GL_SelectTexture(Image.Index);
                  image->Bind();
                end loop;
                -- draw it
                Draw(surf);
                -- unbind texture units
                for Image in Stage.Fragment_Program_Images loop -- bind texture units
                  GL_SelectTexture(Image.Index);
                  globalImages->BindNull();
                end loop;
                -- clear rpEnableSkinning if it was set
                if Surface.Joints.Are_Present and renderProgManager.ShaderHasOptionalSkinning() then
                  SetVertexParm( RENDERPARM_ENABLE_SKINNING, ( 0.0f ).ToFloatPtr() );
                end if;
                GL_SelectTexture(0);
                renderProgManager.Unbind();
              end loop;
            end if;
          end loop;
          GL_Cull( CT_FRONT_SIDED );
          GL_Color( 1.0f, 1.0f, 1.0f );
          return i;
        end Shade;
      begin
        -- Initialize
        Status.Set_Busy(True);
        Viewport(View.Port.X1, View.Port.X2,
                 View.Port.X2 + 1 - View.Port.X1,
                 View.Port.Y2 + 1 - View.Port.Y1);
        Scissor(View.Port.X1 + View.Scissor.X1,
                View.Port.Y1 + View.Scissor.Y1,
                View.Port.X2 + 1 - View.Scissor.X1,
                View.Port.Y2 + 1 - View.Scissor.Y1);
        Reset;
        Clear(False, True, True STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0);
        Cull(Front_Side);
        Bind_Vertex_Array(Level.VAO);
        Set_Vertex_Parameter(Global_Eye_Position_Parameter, View.Render.Organization(0..2), 1.0);
        Set_Vertex_Parameter(Project_Matrix_X_Parameter, Transpose(View.Projection), 4);
        Set_Fragment_Parameter(Over_Bright_Parameter, Float_4_Real(Light_Scale.Get) * 0.5);
        -- Fill depth buffer
        for Surface of View.Surfaces loop
          case Surface.Material.Coverage is
            when Translucent_Coverage => null;
            when Opaque_Coverage =>
              if Surface.Material.Sort = Subview_Sort then
                GLSL.Bind_Shader_Color;
                Color(Color);
                State(Surface.State);
              else
                if Surfaces.Joints.Are_Present then GLSL.Bind_Shader_Depth_Skinned; else GLSL.Bind_Shader_Depth; end if;
                GL_State( surfGLState | GLS_ALPHAMASK );
                assert((GL_GetCurrentState & GLS_DEPTHFUNC_BITS) = GLS_DEPTHFUNC_LESS);
                Texture;
              end if;
            when Perferated_Coverage =>
              if Surface.Material.Coverage = Perferated_Coverage then
                for Stage of Surface.Material.Stages loop
                end loop;
              end if;
          end case;
        end loop;
        -- Interact with lights
        for Light of View.Lights loop
          case Light.Shade is
            when Fog_Shade | Blend_Shade => null;
            when Normal_Shade =>
              if Do_Stencil_Light.Get then
                RB_StencilSelectLight(vLight);
              else
                Create_Mask:declare
                  Screen : Record_Rectangle :=(Light.scissorRect.x1 +  0) & ~15,
                    (Light.scissorRect.y1 +  0) & ~15;
                    (Light.scissorRect.x2 + 15) & ~15;
                    (Light.scissorRect.y2 + 15) & ~15;
                  begin
                    if Screen /= Get_Current_Scissor then Scissor(View.Port.X1 + Screen.X1, View.Port.Y1 + Screen.Y1, Screen.X1 + 1 - Screen.X1, Screen.Y2 + 1 - Screen.Y1) end if;
                    State(DEFAULT);
                    Clear(False, False, True, STENCIL_SHADOW_TEST_VALUE, 0.0, 0.0, 0.0, 0.0;
                  end Create_Mask;
              end if;
              Set_Fragment_Parameter(RENDERPARM_COLOR, colorMagenta);
              State(GLS_DEPTHMASK | GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHFUNC_LESS | GLS_STENCIL_OP_FAIL_KEEP | GLS_STENCIL_OP_ZFAIL_KEEP | GLS_STENCIL_OP_PASS_INCR |  GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ) | GLS_POLYGON_OFFSET );
              Cull(TWO_SIDED);
              for Shadow in Light.Shadows loop
                -- make sure the shadow volume is done
                if Shadow.Scissor /= Get_Current_Scissor then
                  Scissor(View.Port.x1 + Shadow.Scissor.x1,
                          View.Port.y1 + Shadow.Scissor.y1,
                          Shadow.Scissor.x2 + 1 - Shadow.Scissor.x1,
                          Shadow.Scissor.y2 + 1 - Shadow.Scissor.y1);
                end if;
                if Shadow.Space /= Get_Current_Space then
                  Set_MVP(Shadow.Space.MVP);
                  Global_Point_To_Local(Shadow.Space.Model, Light.Origin, 0.0);
                  Set_Vertex_Parameter(Local_Light_Origin_Parameter, 0.0);
                end if;
                if Shadow.Joints.Are_Present then BindShader_ShadowSkinned; 
                else BindShader_Shadow; end if;
                DepthBoundsTest(drawSurf->scissorRect.zmin, drawSurf->scissorRect.zmax);
                if Shadow.Render_Z_Fail then 
                  qglStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                  qglStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
                elsif Do_Preload_Shadow_Stencil then
                  qglStencilOpSeparate(GL_FRONT, GL_KEEP, GL_DECR, GL_DECR);
                  qglStencilOpSeparate(GL_BACK, GL_KEEP, GL_INCR, GL_INCR);
                end if;
                if Shadow.Indexes /= Get_Current_Indexes then qglBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer->GetAPIObject()); end if;
                if Shadow.Joints.Are_Present then
                  qglBindBufferRange(GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset jointBuffer.GetNumJoints * sizeof idJointMat));
                  if glState.vertexLayout != LAYOUT_DRAW_SHADOW_VERT_SKINNED or glState.currentVertexBuffer /= vertexBuffer->GetAPIObject then
                    qglBindBufferARB(GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer->GetAPIObject;
                    backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer->GetAPIObject;
                    qglEnableVertexAttribArrayARB(PC_ATTRIB_INDEX_VERTEX);
                    qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_NORMAL);
                    qglEnableVertexAttribArrayARB(PC_ATTRIB_INDEX_COLOR);
                    qglEnableVertexAttribArrayARB(PC_ATTRIB_INDEX_COLOR2 );
                    qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_ST);
                    qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_TANGENT);
                    qglVertexAttribPointerARB(PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof(idShadowVertSkinned), (void *)( SHADOWVERTSKINNED_XYZW_OFFSET);
                    qglVertexAttribPointerARB(PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(idShadowVertSkinned), (void *)( SHADOWVERTSKINNED_COLOR_OFFSET);
                    qglVertexAttribPointerARB(PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(idShadowVertSkinned), (void *)( SHADOWVERTSKINNED_COLOR2_OFFSET);
                    backEnd.glState.vertexLayout = LAYOUT_DRAW_SHADOW_VERT_SKINNED;
                  end if;
                elsif lState.vertexLayout /= LAYOUT_DRAW_SHADOW_VERT or glState.currentVertexBuffer /= (GLuint)vertexBuffer->GetAPIObject() then
                  qglBindBufferARB(GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer->GetAPIObject);
                  backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer->GetAPIObject;
                  qglEnableVertexAttribArrayARB(PC_ATTRIB_INDEX_VERTEX);
                  qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_NORMAL);
                  qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_COLOR);
                  qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_COLOR2);
                  qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_ST);
                  qglDisableVertexAttribArrayARB(PC_ATTRIB_INDEX_TANGENT);
                  qglVertexAttribPointerARB(PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof( idShadowVert ), (void *)( SHADOWVERT_XYZW_OFFSET ) );
                  backEnd.glState.vertexLayout = LAYOUT_DRAW_SHADOW_VERT;
                end if;
                Commit_Uniforms;
                if Shadow.Joints.Are_Present then qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVertSkinned ) );
                else qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVert ) ); end if;
                if not Shadow.Render_Z_Fail then 
                  qglStencilOpSeparate(GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR);
                  qglStencilOpSeparate(GL_BACK, GL_KEEP, GL_KEEP, GL_DECR);
                  if Shadow.Joints.Are_Present then qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVertSkinned ) );
                  else qglDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool() ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVert ) ); end if;
                end if;
                if Light.Interactions.Scissor /= Get_Current_Scissor then
                  Scissor(backEnd.viewDef->viewport.x1 + vLight->scissorRect.x1, 
                          backEnd.viewDef->viewport.y1 + vLight->scissorRect.y1,
                          vLight->scissorRect.x2 + 1 - vLight->scissorRect.x1,
                          vLight->scissorRect.y2 + 1 - vLight->scissorRect.y1);
                end if;
                if Perform_Stencil_Text then State( GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHMASK | depthFunc | GLS_STENCIL_FUNC_EQUAL | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ) );
                else GL_State( GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHMASK | depthFunc | GLS_STENCIL_FUNC_ALWAYS ); end if;
                for Interaction of View.Interactions loop
                  -- make Complex_Surfaces
                end loop;
                for Surface of Complex_Surfaces loop
                  -- make All_Surfaces 
                end loop;
                for Stage of Light.Material.Stages loop
                  for Surface of All_Surfaces loop
                    for Stage of Surface.Material.Stages loop
                      case Stage is 
                        when Coverage_Stage | Ambient_Stage => null;
                        when Normal_Stage =>
                        when Diffuse_Stage =>
                        when Specular_Stage => 
                      end case;
                    end loop;
                  end loop;
                end loop;
          end case;
        end loop;
        -- Render GUIS
        Shade( drawSurfs, numDrawSurfs, if ( viewDef->viewEntitys != NULL ) { guiScreenOffset = 0.0f; else {guiScreenOffset = stereoEye * viewDef->renderView.stereoScreenSeparation;, stereoEye );
        -- Fog lights
        for Light of Level.Lights loop
          case Light.Shade is
            when Fog_Shade => 
              renderLog.OpenBlock( vLight->lightShader->GetName() ); -- find the current color and density of the fog
              const idMaterial * lightShader = vLight->lightS -- assume fog shaders have only a single stageader;
              const float * regs = vLight->shaderRegisters;
              const shaderStage_t * stage = lightShader->GetStage( 0 );
              float lightColor[4];
              lightColor[0] = regs[ stage->color.registers[0] ];
              lightColor[1] = regs[ stage->color.registers[1] ];
              lightColor[2] = regs[ stage->color.registers[2] ];
              lightColor[3] = regs[ stage->color.registers[3] ];
              GL_Color(lightColor);
              -- calculate the falloff planes
              float a;
              if lightColor[3] <= 1.0f then a = -0.5f / DEFAULT_FOG_DISTANCE; -- if they left the default value on, set a fog distance of 500
              else  = -0.5f / lightColor[3]; end if; -- otherwise, distance = alpha color
              SelectTexture(0); -- texture 0 is the falloff image
              globalImages->fogImage->Bind
              SelectTexture(1); -- texture 1 is the entering plane fade correction
              globalImages->fogEnterImage->Bind();
              -- S is based on the view origin
              const float s = vLight->fogPlane.Distance( backEnd.viewDef->renderView.vieworg );
              const float FOG_SCALE = 0.001f;
              idPlane fogPlanes[4] :=((a * backEnd.viewDef->worldSpace.modelViewMatrix[0*4+2], a * backEnd.viewDef->worldSpace.modelViewMatrix[1*4+2], -- S-0
                                       a * backEnd.viewDef->worldSpace.modelViewMatrix[2*4+2], a * backEnd.viewDef->worldSpace.modelViewMatrix[3*4+2] + 0.5f), -- T-0
                                      (others => 0.0), --a * backEnd.viewDef->worldSpace.modelViewMatrix[0*4+0];)) -- T-1 will get a texgen for the fade plane, which is always the "top" plane on unrotated lights
                                      (FOG_SCALE * vLight->fogPlane[0], FOG_SCALE * vLight->fogPlane[1], FOG_SCALE * vLight->fogPlane[2], FOG_SCALE * vLight->fogPlane[3] + FOG_ENTER), -- S-1
                                      (1..3 => 0.0, 4 => FOG_SCALE * s + FOG_ENTER));
              -- draw it
              State(GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_EQUAL);
              for I in 1..3 loop
                if I = 3 then
                  GL_State( GLS_DEPTHMASK | GLS_SRCBLEND_SRC_ALPHA | GLS_DSTBLEND_ONE_MINUS_SRC_ALPHA | GLS_DEPTHFUNC_LESS );
                  GL_Cull( CT_BACK_SIDED );
                  backEnd.zeroOneCubeSurface.space = &backEnd.viewDef->worldSpace;
                  backEnd.zeroOneCubeSurface.scissorRect = backEnd.viewDef->scissor;
                end if;
                for Surface of ???Next_On_Light loop
                      if ( !backEnd.currentScissor.Equals( drawSurf->scissorRect ) && r_useScissor.GetBool() ) {
                          -- change the scissor
                          GL_Scissor( backEnd.viewDef->viewport.x1 + drawSurf->scissorRect.x1,
                                      backEnd.viewDef->viewport.y1 + drawSurf->scissorRect.y1,
                                      drawSurf->scissorRect.x2 + 1 - drawSurf->scissorRect.x1,
                                      drawSurf->scissorRect.y2 + 1 - drawSurf->scissorRect.y1 );
                          backEnd.currentScissor = drawSurf->scissorRect;
                      end ;
                      if ( drawSurf->space != backEnd.currentSpace ) {
                          idPlane localFogPlanes[4];
                          if ( inverseBaseLightProject == NULL ) {
                              RB_SetMVP( drawSurf->space->mvp );
                              for ( int i = 0; i < 4; i++ ) {
                                  R_GlobalPlaneToLocal( drawSurf->space->modelMatrix, fogPlanes[i], localFogPlanes[i] );
                              end ;
                          else {
                              idRenderMatrix invProjectMVPMatrix;
                              idRenderMatrix::Multiply( backEnd.viewDef->worldSpace.mvp, *inverseBaseLightProject, invProjectMVPMatrix );
                              RB_SetMVP( invProjectMVPMatrix );
                              for ( int i = 0; i < 4; i++ ) {
                                  inverseBaseLightProject->InverseTransformPlane( fogPlanes[i], localFogPlanes[i], false );
                              end ;
                          end ;
                          SetVertexParm( RENDERPARM_TEXGEN_0_S, localFogPlanes[0].ToFloatPtr() );
                          SetVertexParm( RENDERPARM_TEXGEN_0_T, localFogPlanes[1].ToFloatPtr() );
                          SetVertexParm( RENDERPARM_TEXGEN_1_T, localFogPlanes[2].ToFloatPtr() );
                          SetVertexParm( RENDERPARM_TEXGEN_1_S, localFogPlanes[3].ToFloatPtr() );
                          backEnd.currentSpace = ( inverseBaseLightProject == NULL ) ? drawSurf->space : NULL;
                      end ;
                      if ( drawSurf->jointCache ) { renderProgManager.BindShader_FogSkinned();
                      else { renderProgManager.BindShader_Fog();
                      RB_DrawElementsWithCounters( drawSurf );
                  end ;
                --RB_T_BasicFog( drawSurfs, fogPlanes, NULL );
                --RB_T_BasicFog( drawSurfs2, fogPlanes, NULL );
                --RB_T_BasicFog( &backEnd.zeroOneCubeSurface, fogPlanes, &vLight->inverseBaseLightProject );
                GL_Cull( CT_FRONT_SIDED );
                GL_SelectTexture( 1 );
                globalImages->BindNull();
                GL_SelectTexture( 0 );
                renderProgManager.Unbind();
              end loop;
            when Blend_Shade =>
              const idMaterial * lightShader = vLight->lightShader;
              const float * regs = vLight->shaderRegisters;
              SelectTexture(1); -- texture 1 will get the falloff texture
              vLight->falloffImage->Bind();
              SelectTexture(0); -- texture 0 will get the projected texture
              renderProgManager.BindShader_BlendLight();
              for Stage in ???Light.Material.Stages loop
                GL_State( GLS_DEPTHMASK | stage->drawStateBits | GLS_DEPTHFUNC_EQUAL );
                GL_SelectTexture( 0 );
                stage->texture.image->Bind();
                if stage->texture.hasMatrix then Load(&stage->texture); end if;

              for ( int i = 0; i < lightShader->GetNumStages(); i++ ) {
                const shaderStage_t *stage = lightShader->GetStage(i);
                if ( !regs[ stage->conditionRegister ] ) {
                  continue;
                end ;
                GL_State( GLS_DEPTHMASK | stage->drawStateBits | GLS_DEPTHFUNC_EQUAL );
                GL_SelectTexture( 0 );
                stage->texture.image->Bind();
                if ( stage->texture.hasMatrix ) {
                  RB_LoadShaderTextureMatrix( regs, &stage->texture );
                end ;
                -- get the modulate values from the light, including alpha, unlike normal lights
                float lightColor[4];
                lightColor[0] = regs[ stage->color.registers[0] ];
                lightColor[1] = regs[ stage->color.registers[1] ];
                lightColor[2] = regs[ stage->color.registers[2] ];
                lightColor[3] = regs[ stage->color.registers[3] ];
                GL_Color( lightColor );
                backEnd.currentSpace = NULL;
                for ( const drawSurf_t * drawSurf = drawSurfs; drawSurf != NULL; drawSurf = drawSurf->nextOnLight ) {
                 if ( drawSurf->scissorRect.IsEmpty() ) {
                   continue; -- !@# FIXME: find out why this is sometimes being hit!
                         -- temporarily jump over the scissor and draw so the gl error callback doesn't get hit
                 end ;
                 if ( !backEnd.currentScissor.Equals( drawSurf->scissorRect ) && r_useScissor.GetBool() ) {
                   -- change the scissor
                   GL_Scissor( backEnd.viewDef->viewport.x1 + drawSurf->scissorRect.x1,
                         backEnd.viewDef->viewport.y1 + drawSurf->scissorRect.y1,
                         drawSurf->scissorRect.x2 + 1 - drawSurf->scissorRect.x1,
                         drawSurf->scissorRect.y2 + 1 - drawSurf->scissorRect.y1 );
                   backEnd.currentScissor = drawSurf->scissorRect;
                 end ;
                 if (drawSurf->space != backEnd.currentSpace ) {
                   -- change the matrix
                   RB_SetMVP( drawSurf->space->mvp );

                   -- change the light projection matrix
                   idPlane lightProjectInCurrentSpace[4];
                   for ( int i = 0; i < 4; i++ ) {
                     R_GlobalPlaneToLocal( drawSurf->space->modelMatrix, vLight->lightProject[i], lightProjectInCurrentSpace[i] );
                   end ;
                   SetVertexParm( RENDERPARM_TEXGEN_0_S, lightProjectInCurrentSpace[0].ToFloatPtr() );
                   SetVertexParm( RENDERPARM_TEXGEN_0_T, lightProjectInCurrentSpace[1].ToFloatPtr() );
                   SetVertexParm( RENDERPARM_TEXGEN_0_Q, lightProjectInCurrentSpace[2].ToFloatPtr() );
                   SetVertexParm( RENDERPARM_TEXGEN_1_S, lightProjectInCurrentSpace[3].ToFloatPtr() ); -- falloff
                   backEnd.currentSpace = drawSurf->space;
                 end ;
                 Draw(drawSurf);
                end ;
              end ;
              GL_SelectTexture(1);
              globalImages->BindNull();
              GL_SelectTexture(0);
              renderProgManager.Unbind();
          when Normal_Shade => null; end case;
        end loop;
        -- Post processing
        if ( processed < numDrawSurfs && !r_skipPostProcess.GetBool() ) {
          int x = backEnd.viewDef->viewport.x1;
          int y = backEnd.viewDef->viewport.y1;
          int w = backEnd.viewDef->viewport.x2 - backEnd.viewDef->viewport.x1 + 1;
          int h = backEnd.viewDef->viewport.y2 - backEnd.viewDef->viewport.y1 + 1;
          RENDERLOG_PRINTF( "Resolve to %i x %i buffer\n", w, h );
          GL_SelectTexture( 0 );
          -- resolve the screen
          globalImages->currentRenderImage->CopyFramebuffer( x, y, w, h );
          backEnd.currentRenderCopied = true;
          -- RENDERPARM_SCREENCORRECTIONFACTOR amd RENDERPARM_WINDOWCOORD overlap
          -- diffuseScale and specularScale
          -- screen power of two correction factor (no longer relevant now)
          float screenCorrectionParm[4];
          screenCorrectionParm[0] = 1.0f;
          screenCorrectionParm[1] = 1.0f;
          screenCorrectionParm[2] = 0.0f;
          screenCorrectionParm[3] = 1.0f;
          SetFragmentParm( RENDERPARM_SCREENCORRECTIONFACTOR, screenCorrectionParm ); -- rpScreenCorrectionFactor
          -- window coord to 0.0 to 1.0 conversion
          float windowCoordParm[4];
          windowCoordParm[0] = 1.0f / w;
          windowCoordParm[1] = 1.0f / h;
          windowCoordParm[2] = 0.0f;
          windowCoordParm[3] = 1.0f;
          SetFragmentParm( RENDERPARM_WINDOWCOORD, windowCoordParm ); -- rpWindowCoord
          -- render the remaining surfaces
          renderLog.OpenMainBlock( MRB_DRAW_SHADER_PASSES_POST );
          RB_DrawShaderPasses( drawSurfs + processed, numDrawSurfs - processed, 0.0f /* definitely not a gui */, stereoEye );
          renderLog.CloseMainBlock();
        end ;
      end Draw;
  end Neo.System.Graphics;





    -- if ( surfList == NULL ) {
    --     return;
    -- end ;

    -- -- change the scissor if needed, it will be constant across all the surfaces lit by the light
    -- if ( !backEnd.currentScissor.Equals( vLight->scissorRect ) && r_useScissor.GetBool() ) {
    --     GL_Scissor( backEnd.viewDef->viewport.x1 + vLight->scissorRect.x1, 
    --                 backEnd.viewDef->viewport.y1 + vLight->scissorRect.y1,
    --                 vLight->scissorRect.x2 + 1 - vLight->scissorRect.x1,
    --                 vLight->scissorRect.y2 + 1 - vLight->scissorRect.y1 );
    --     backEnd.currentScissor = vLight->scissorRect;
    -- end ;

    -- -- perform setup here that will be constant for all interactions
    -- if ( performStencilTest ) {
    --     GL_State( GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHMASK | depthFunc | GLS_STENCIL_FUNC_EQUAL | GLS_STENCIL_MAKE_REF( STENCIL_SHADOW_TEST_VALUE ) | GLS_STENCIL_MAKE_MASK( STENCIL_SHADOW_MASK_VALUE ) );

    -- else {
    --     GL_State( GLS_SRCBLEND_ONE | GLS_DSTBLEND_ONE | GLS_DEPTHMASK | depthFunc | GLS_STENCIL_FUNC_ALWAYS );
    -- end ;

    -- -- some rare lights have multiple animating stages, loop over them outside the surface list
    -- const idMaterial * lightShader = vLight->lightShader;
    -- const float * lightRegs = vLight->shaderRegisters;

    -- drawInteraction_t inter = {end ;;
    -- inter.ambientLight = lightShader->IsAmbientLight();

    -- //---------------------------------
    -- -- Split out the complex surfaces from the fast-path surfaces
    -- -- so we can do the fast path ones all in a row.
    -- -- The surfaces should already be sorted by space because they
    -- -- are added single-threaded, and there is only a negligable amount
    -- -- of benefit to trying to sort by materials.
    -- //---------------------------------
    -- static const int MAX_INTERACTIONS_PER_LIGHT = 1024;
    -- static const int MAX_COMPLEX_INTERACTIONS_PER_LIGHT = 128;
    -- idStaticList< const drawSurf_t *, MAX_INTERACTIONS_PER_LIGHT > allSurfaces;
    -- idStaticList< const drawSurf_t *, MAX_COMPLEX_INTERACTIONS_PER_LIGHT > complexSurfaces;
    -- for ( const drawSurf_t * walk = surfList; walk != NULL; walk = walk->nextOnLight ) {

    --     -- make sure the triangle culling is done
    --     if ( walk->shadowVolumeState != SHADOWVOLUME_DONE ) {
    --         assert( walk->shadowVolumeState == SHADOWVOLUME_UNFINISHED || walk->shadowVolumeState == SHADOWVOLUME_DONE );

    --         uint64 start = Sys_Microseconds();
    --         while ( walk->shadowVolumeState == SHADOWVOLUME_UNFINISHED ) {
    --             Sys_Yield();
    --         end ;
    --         uint64 end = Sys_Microseconds();

    --         backEnd.pc.shadowMicroSec += end - start;
    --     end ;

    --     const idMaterial * surfaceShader = walk->material;
    --     if ( surfaceShader->GetFastPathBumpImage() ) {
    --         allSurfaces.Append( walk );
    --     else {
    --         complexSurfaces.Append( walk );
    --     end ;
    -- end ;
    -- for ( int i = 0; i < complexSurfaces.Num(); i++ ) {
    --     allSurfaces.Append( complexSurfaces[i] );
    -- end ;

    -- bool lightDepthBoundsDisabled = false;

    -- for ( int lightStageNum = 0; lightStageNum < lightShader->GetNumStages(); lightStageNum++ ) {
    --     const shaderStage_t *lightStage = lightShader->GetStage( lightStageNum );

    --     -- ignore stages that fail the condition
    --     if ( !lightRegs[ lightStage->conditionRegister ] ) {
    --         continue;
    --     end ;

    --     const float lightScale = r_lightScale.GetFloat();
    --     const idVec4 lightColor(
    --         lightScale * lightRegs[ lightStage->color.registers[0] ],
    --         lightScale * lightRegs[ lightStage->color.registers[1] ],
    --         lightScale * lightRegs[ lightStage->color.registers[2] ],
    --         lightRegs[ lightStage->color.registers[3] ] );
    --     -- apply the world-global overbright and the 2x factor for specular
    --     const idVec4 diffuseColor = lightColor;
    --     const idVec4 specularColor = lightColor * 2.0f;

    --     float lightTextureMatrix[16];
    --     if ( lightStage->texture.hasMatrix ) {
    --         RB_GetShaderTextureMatrix( lightRegs, &lightStage->texture, lightTextureMatrix );
    --     end ;

    --     -- texture 1 will be the light falloff texture
    --     GL_SelectTexture( INTERACTION_TEXUNIT_FALLOFF );
    --     vLight->falloffImage->Bind();

    --     -- texture 2 will be the light projection texture
    --     GL_SelectTexture( INTERACTION_TEXUNIT_PROJECTION );
    --     lightStage->texture.image->Bind();

    --     -- force the light textures to not use anisotropic filtering, which is wasted on them
    --     -- all of the texture sampler parms should be constant for all interactions, only
    --     -- the actual texture image bindings will change

    --     //----------------------------------
    --     -- For all surfaces on this light list, generate an interaction for this light stage
    --     //----------------------------------

    --     -- setup renderparms assuming we will be drawing trivial surfaces first
    --     -- RB_SetupForFastPathInteractions( diffuseColor, specularColor );
    --     const idVec4 sMatrix( 1, 0, 0, 0 );
    --     const idVec4 tMatrix( 0, 1, 0, 0 );

    --     -- bump matrix
    --     SetVertexParm( RENDERPARM_BUMPMATRIX_S, sMatrix.ToFloatPtr() );
    --     SetVertexParm( RENDERPARM_BUMPMATRIX_T, tMatrix.ToFloatPtr() );

    --     -- diffuse matrix
    --     SetVertexParm( RENDERPARM_DIFFUSEMATRIX_S, sMatrix.ToFloatPtr() );
    --     SetVertexParm( RENDERPARM_DIFFUSEMATRIX_T, tMatrix.ToFloatPtr() );

    --     -- specular matrix
    --     SetVertexParm( RENDERPARM_SPECULARMATRIX_S, sMatrix.ToFloatPtr() );
    --     SetVertexParm( RENDERPARM_SPECULARMATRIX_T, tMatrix.ToFloatPtr() );

    --     RB_SetVertexColorParms( SVC_IGNORE );

    --     SetFragmentParm( RENDERPARM_DIFFUSEMODIFIER, diffuseColor.ToFloatPtr() );
    --     SetFragmentParm( RENDERPARM_SPECULARMODIFIER, specularColor.ToFloatPtr() );

    --     -- even if the space does not change between light stages, each light stage may need a different lightTextureMatrix baked in
    --     backEnd.currentSpace = NULL;

    --     for ( int sortedSurfNum = 0; sortedSurfNum < allSurfaces.Num(); sortedSurfNum++ ) {
    --         const drawSurf_t * const surf = allSurfaces[ sortedSurfNum ];

    --         -- select the render prog
    --         if ( lightShader->IsAmbientLight() ) {
    --             if ( surf->jointCache ) {
    --                 renderProgManager.BindShader_InteractionAmbientSkinned();
    --             else {
    --                 renderProgManager.BindShader_InteractionAmbient();
    --             end ;
    --         else {
    --             if ( surf->jointCache ) {
    --                 renderProgManager.BindShader_InteractionSkinned();
    --             else {
    --                 renderProgManager.BindShader_Interaction();
    --             end ;
    --         end ;

    --         const idMaterial * surfaceShader = surf->material;
    --         const float * surfaceRegs = surf->shaderRegisters;

    --         inter.surf = surf;

    --         -- change the MVP matrix, view/light origin and light projection vectors if needed
    --         if ( surf->space != backEnd.currentSpace ) {
    --             backEnd.currentSpace = surf->space;

    --             -- turn off the light depth bounds test if this model is rendered with a depth hack
    --             if ( useLightDepthBounds ) {
    --                 if ( !surf->space->weaponDepthHack && surf->space->modelDepthHack == 0.0f ) {
    --                     if ( lightDepthBoundsDisabled ) {
    --                         GL_DepthBoundsTest( vLight->scissorRect.zmin, vLight->scissorRect.zmax );
    --                         lightDepthBoundsDisabled = false;
    --                     end ;
    --                 else {
    --                     if ( !lightDepthBoundsDisabled ) {
    --                         GL_DepthBoundsTest( 0.0f, 0.0f );
    --                         lightDepthBoundsDisabled = true;
    --                     end ;
    --                 end ;
    --             end ;

    --             -- model-view-projection
    --             RB_SetMVP( surf->space->mvp );

    --             -- tranform the light/view origin into model local space
    --             idVec4 localLightOrigin( 0.0f );
    --             idVec4 localViewOrigin( 1.0f );
    --             R_GlobalPointToLocal( surf->space->modelMatrix, vLight->globalLightOrigin, localLightOrigin.ToVec3() );
    --             R_GlobalPointToLocal( surf->space->modelMatrix, backEnd.viewDef->renderView.vieworg, localViewOrigin.ToVec3() );

    --             -- set the local light/view origin
    --             SetVertexParm( RENDERPARM_LOCALLIGHTORIGIN, localLightOrigin.ToFloatPtr() );
    --             SetVertexParm( RENDERPARM_LOCALVIEWORIGIN, localViewOrigin.ToFloatPtr() );

    --             -- transform the light project into model local space
    --             idPlane lightProjection[4];
    --             for ( int i = 0; i < 4; i++ ) {
    --                 R_GlobalPlaneToLocal( surf->space->modelMatrix, vLight->lightProject[i], lightProjection[i] );
    --             end ;

    --             -- optionally multiply the local light projection by the light texture matrix
    --             if ( lightStage->texture.hasMatrix ) {
    --                 RB_BakeTextureMatrixIntoTexgen( lightProjection, lightTextureMatrix );
    --             end ;

    --             -- set the light projection
    --             SetVertexParm( RENDERPARM_LIGHTPROJECTION_S, lightProjection[0].ToFloatPtr() );
    --             SetVertexParm( RENDERPARM_LIGHTPROJECTION_T, lightProjection[1].ToFloatPtr() );
    --             SetVertexParm( RENDERPARM_LIGHTPROJECTION_Q, lightProjection[2].ToFloatPtr() );
    --             SetVertexParm( RENDERPARM_LIGHTFALLOFF_S, lightProjection[3].ToFloatPtr() );
    --         end ;

    --         -- check for the fast path
    --         if ( surfaceShader->GetFastPathBumpImage() && !r_skipInteractionFastPath.GetBool() ) {
    --             renderLog.OpenBlock( surf->material->GetName() );

    --             -- texture 0 will be the per-surface bump map
    --             GL_SelectTexture( INTERACTION_TEXUNIT_BUMP );
    --             surfaceShader->GetFastPathBumpImage()->Bind();

    --             -- texture 3 is the per-surface diffuse map
    --             GL_SelectTexture( INTERACTION_TEXUNIT_DIFFUSE );
    --             surfaceShader->GetFastPathDiffuseImage()->Bind();

    --             -- texture 4 is the per-surface specular map
    --             GL_SelectTexture( INTERACTION_TEXUNIT_SPECULAR );
    --             surfaceShader->GetFastPathSpecularImage()->Bind();

    --             RB_DrawElementsWithCounters( surf );

    --             renderLog.CloseBlock();
    --             continue;
    --         end ;
            
    --         renderLog.OpenBlock( surf->material->GetName() );

    --         inter.bumpImage = NULL;
    --         inter.specularImage = NULL;
    --         inter.diffuseImage = NULL;
    --         inter.diffuseColor[0] = inter.diffuseColor[1] = inter.diffuseColor[2] = inter.diffuseColor[3] = 0;
    --         inter.specularColor[0] = inter.specularColor[1] = inter.specularColor[2] = inter.specularColor[3] = 0;

    --         -- go through the individual surface stages
    --         //
    --         -- This is somewhat arcane because of the old support for video cards that had to render
    --         -- interactions in multiple passes.
    --         //
    --         -- We also have the very rare case of some materials that have conditional interactions
    --         -- for the "hell writing" that can be shined on them.
    --         for ( int surfaceStageNum = 0; surfaceStageNum < surfaceShader->GetNumStages(); surfaceStageNum++ ) {
    --             const shaderStage_t *surfaceStage = surfaceShader->GetStage( surfaceStageNum );

    --             switch( surfaceStage->lighting ) {
    --                 case SL_COVERAGE: {
    --                     -- ignore any coverage stages since they should only be used for the depth fill pass
    --                     -- for diffuse stages that use alpha test.
    --                     break;
    --                 end ;
    --                 case SL_AMBIENT: {
    --                     -- ignore ambient stages while drawing interactions
    --                     break;
    --                 end ;
    --                 case SL_BUMP: {
    --                     -- ignore stage that fails the condition
    --                     if ( !surfaceRegs[ surfaceStage->conditionRegister ] ) {
    --                         break;
    --                     end ;
    --                     -- draw any previous interaction
    --                     if ( inter.bumpImage != NULL ) {
    --                         RB_DrawSingleInteraction( &inter );
    --                     end ;
    --                     inter.bumpImage = surfaceStage->texture.image;
    --                     inter.diffuseImage = NULL;
    --                     inter.specularImage = NULL;
    --                     RB_SetupInteractionStage( surfaceStage, surfaceRegs, NULL,
    --                                             inter.bumpMatrix, NULL );
    --                     break;
    --                 end ;
    --                 case SL_DIFFUSE: {
    --                     -- ignore stage that fails the condition
    --                     if ( !surfaceRegs[ surfaceStage->conditionRegister ] ) {
    --                         break;
    --                     end ;
    --                     -- draw any previous interaction
    --                     if ( inter.diffuseImage != NULL ) {
    --                         RB_DrawSingleInteraction( &inter );
    --                     end ;
    --                     inter.diffuseImage = surfaceStage->texture.image;
    --                     inter.vertexColor = surfaceStage->vertexColor;
    --                     RB_SetupInteractionStage( surfaceStage, surfaceRegs, diffuseColor.ToFloatPtr(),
    --                                             inter.diffuseMatrix, inter.diffuseColor.ToFloatPtr() );
    --                     break;
    --                 end ;
    --                 case SL_SPECULAR: {
    --                     -- ignore stage that fails the condition
    --                     if ( !surfaceRegs[ surfaceStage->conditionRegister ] ) {
    --                         break;
    --                     end ;
    --                     -- draw any previous interaction
    --                     if ( inter.specularImage != NULL ) {
    --                         RB_DrawSingleInteraction( &inter );
    --                     end ;
    --                     inter.specularImage = surfaceStage->texture.image;
    --                     inter.vertexColor = surfaceStage->vertexColor;
    --                     RB_SetupInteractionStage( surfaceStage, surfaceRegs, specularColor.ToFloatPtr(),
    --                                             inter.specularMatrix, inter.specularColor.ToFloatPtr() );
    --                     break;
    --                 end ;
    --             end ;
    --         end ;

    --         -- draw the final interaction
    --         RB_DrawSingleInteraction( &inter );

    --         renderLog.CloseBlock();
    --     end ;
    -- end ;

    -- if ( useLightDepthBounds && lightDepthBoundsDisabled ) {
    --     GL_DepthBoundsTest( vLight->scissorRect.zmin, vLight->scissorRect.zmax );
    -- end ;

    -- renderProgManager.Unbind();

    --             if ( vLight->globalShadows != NULL ) {
    --                     renderLog.OpenBlock( "Global Light Shadows" );
    --                     RB_StencilShadowPass( vLight->globalShadows, vLight );
    --                     renderLog.CloseBlock();
    --             end ;

    --             if ( vLight->localInteractions != NULL ) {
    --                     renderLog.OpenBlock( "Local Light Interactions" );
    --                     RB_RenderInteractions( vLight->localInteractions, vLight, GLS_DEPTHFUNC_EQUAL, performStencilTest, useLightDepthBounds );
    --                     renderLog.CloseBlock();
    --             end ;

    --             if ( vLight->localShadows != NULL ) {
    --                     renderLog.OpenBlock( "Local Light Shadows" );
    --                     RB_StencilShadowPass( vLight->localShadows, vLight );
    --                     renderLog.CloseBlock();
    --             end ;

    --             if ( vLight->globalInteractions != NULL ) {
    --                     renderLog.OpenBlock( "Global Light Interactions" );
    --                     RB_RenderInteractions( vLight->globalInteractions, vLight, GLS_DEPTHFUNC_EQUAL, performStencilTest, useLightDepthBounds );
    --                     renderLog.CloseBlock();
    --             end ;


    --             if ( vLight->translucentInteractions != NULL && !r_skipTranslucent.GetBool() ) {
    --                     renderLog.OpenBlock( "Translucent Interactions" );

    --                     -- Disable the depth bounds test because translucent surfaces don't work with
    --                     -- the depth bounds tests since they did not write depth during the depth pass.
    --                     if ( useLightDepthBounds ) {
    --                             GL_DepthBoundsTest( 0.0f, 0.0f );
    --                     end ;

    --                     -- The depth buffer wasn't filled in for translucent surfaces, so they
    --                     -- can never be constrained to perforated surfaces with the depthfunc equal.

    --                     -- Translucent surfaces do not receive shadows. This is a case where a
    --                     -- shadow buffer solution would work but stencil shadows do not because
    --                     -- stencil shadows only affect surfaces that contribute to the view depth
    --                     -- buffer and translucent surfaces do not contribute to the view depth buffer.

    --                     RB_RenderInteractions( vLight->translucentInteractions, vLight, GLS_DEPTHFUNC_LESS, false, false );

    --                     renderLog.CloseBlock();
    --             end ;

    --             renderLog.CloseBlock();
    --     end ;

    --     -- disable stencil shadow test
    --     GL_State( GLS_DEFAULT );

    --     -- unbind texture units
    --     for ( int i = 0; i < 5; i++ ) {
    --             GL_SelectTexture( i );
    --             globalImages->BindNull();
    --     end ;
    --     GL_SelectTexture( 0 );

    --     -- reset depth bounds
    --     if ( useLightDepthBounds ) {
    --             GL_DepthBoundsTest( 0.0f, 0.0f );
    --     end ;



