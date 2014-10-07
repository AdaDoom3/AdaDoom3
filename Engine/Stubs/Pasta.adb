
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


