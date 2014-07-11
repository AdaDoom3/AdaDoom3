package Neo.System.Graphics.World is



















    type Record_Level is record
        Is_Taking_Snapshot :
        Number_Of_Frames :
        Number_Of_Views :
        Shader_Time_For_2D :
        Ambient_Bump_Mapping_Light : 
        Render : Record_Render
        View : 
        White :
        Character_Sheet : 
        Point_Light :
        Projected_Light :
        Identity_Space : Record_Entity
        Render_Crops :  vector idScreenRect  
        fonts : ;idList<idFont *, TAG_FONT>    
        unitSquareTriangles : ;srfTriangles_t *    
        zeroOneCubeTriangles : ;srfTriangles_t *    
        testImageTriangles : ;srfTriangles_t *    
        -- these are allocated at buffer swap time, but the back end should only use the ones in the backEnd stucture which are copied over from the frame that was just swapped.
        unitSquareSurface_ : ;drawSurf_t        
        zeroOneCubeSurface_ : ;drawSurf_t        
        testImageSurface_ : ;drawSurf_t        
        timerQueryId : ;unsigned           -- for GL_TIME_ELAPSED_EXT queries
      end record;

-- extern idCVar r_debugContext;				// enable various levels of context debug
-- extern idCVar r_glDriver;					// "opengl32", etc
-- extern idCVar r_skipIntelWorkarounds;		// skip work arounds for Intel driver bugs
-- extern idCVar r_vidMode;					// video mode number
-- extern idCVar r_displayRefresh;				// optional display refresh rate option for vid mode
-- extern idCVar r_fullscreen;					// 0 = windowed, 1 = full screen
-- extern idCVar r_multiSamples;				// number of antialiasing samples

-- extern idCVar r_znear;						// near Z clip plane

-- extern idCVar r_swapInterval;				// changes wglSwapIntarval
-- extern idCVar r_offsetFactor;				// polygon offset parameter
-- extern idCVar r_offsetUnits;				// polygon offset parameter
-- extern idCVar r_singleTriangle;				// only draw a single triangle per primitive
-- extern idCVar r_logFile;					// number of frames to emit GL logs
-- extern idCVar r_clear;						// force screen clear every frame
-- extern idCVar r_subviewOnly;				// 1 = don't render main view, allowing subviews to be debugged
-- extern idCVar r_lightScale;					// all light intensities are multiplied by this, which is normally 2
-- extern idCVar r_flareSize;					// scale the flare deforms from the material def

-- extern idCVar r_gamma;						// changes gamma tables
-- extern idCVar r_brightness;					// changes gamma tables

-- extern idCVar r_checkBounds;				// compare all surface bounds with precalculated ones
-- extern idCVar r_maxAnisotropicFiltering;	// texture filtering parameter
-- extern idCVar r_useTrilinearFiltering;		// Extra quality filtering
-- extern idCVar r_lodBias;					// lod bias

-- extern idCVar r_useLightPortalFlow;			// 1 = do a more precise area reference determination
-- extern idCVar r_useShadowSurfaceScissor;	// 1 = scissor shadows by the scissor rect of the interaction surfaces
-- extern idCVar r_useConstantMaterials;		// 1 = use pre-calculated material registers if possible
-- extern idCVar r_useNodeCommonChildren;		// stop pushing reference bounds early when possible
-- extern idCVar r_useSilRemap;				// 1 = consider verts with the same XYZ, but different ST the same for shadows
-- extern idCVar r_useLightPortalCulling;		// 0 = none, 1 = box, 2 = exact clip of polyhedron faces, 3 MVP to plane culling
-- extern idCVar r_useLightAreaCulling;		// 0 = off, 1 = on
-- extern idCVar r_useLightScissors;			// 1 = use custom scissor rectangle for each light
-- extern idCVar r_useEntityPortalCulling;		// 0 = none, 1 = box
-- extern idCVar r_skipPrelightShadows;		// 1 = skip the dmap generated static shadow volumes
-- extern idCVar r_useCachedDynamicModels;		// 1 = cache snapshots of dynamic models
-- extern idCVar r_useScissor;					// 1 = scissor clip as portals and lights are processed
-- extern idCVar r_usePortals;					// 1 = use portals to perform area culling, otherwise draw everything
-- extern idCVar r_useStateCaching;			// avoid redundant state changes in GL_*() calls
-- extern idCVar r_useEntityCallbacks;			// if 0, issue the callback immediately at update time, rather than defering
-- extern idCVar r_lightAllBackFaces;			// light all the back faces, even when they would be shadowed
-- extern idCVar r_useLightDepthBounds;		// use depth bounds test on lights to reduce both shadow and interaction fill
-- extern idCVar r_useShadowDepthBounds;		// use depth bounds test on individual shadows to reduce shadow fill

-- extern idCVar r_skipStaticInteractions;		// skip interactions created at level load
-- extern idCVar r_skipDynamicInteractions;	// skip interactions created after level load
-- extern idCVar r_skipPostProcess;			// skip all post-process renderings
-- extern idCVar r_skipSuppress;				// ignore the per-view suppressions
-- extern idCVar r_skipInteractions;			// skip all light/surface interaction drawing
-- extern idCVar r_skipFrontEnd;				// bypasses all front end work, but 2D gui rendering still draws
-- extern idCVar r_skipBackEnd;				// don't draw anything
-- extern idCVar r_skipCopyTexture;			// do all rendering, but don't actually copyTexSubImage2D
-- extern idCVar r_skipRender;					// skip 3D rendering, but pass 2D
-- extern idCVar r_skipRenderContext;			// NULL the rendering context during backend 3D rendering
-- extern idCVar r_skipTranslucent;			// skip the translucent interaction rendering
-- extern idCVar r_skipAmbient;				// bypasses all non-interaction drawing
-- extern idCVar r_skipNewAmbient;				// bypasses all vertex/fragment program ambients
-- extern idCVar r_skipBlendLights;			// skip all blend lights
-- extern idCVar r_skipFogLights;				// skip all fog lights
-- extern idCVar r_skipSubviews;				// 1 = don't render any mirrors / cameras / etc
-- extern idCVar r_skipGuiShaders;				// 1 = don't render any gui elements on surfaces
-- extern idCVar r_skipParticles;				// 1 = don't render any particles
-- extern idCVar r_skipUpdates;				// 1 = don't accept any entity or light updates, making everything static
-- extern idCVar r_skipDeforms;				// leave all deform materials in their original state
-- extern idCVar r_skipDynamicTextures;		// don't dynamically create textures
-- extern idCVar r_skipBump;					// uses a flat surface instead of the bump map
-- extern idCVar r_skipSpecular;				// use black for specular
-- extern idCVar r_skipDiffuse;				// use black for diffuse
-- extern idCVar r_skipDecals;					// skip decal surfaces
-- extern idCVar r_skipOverlays;				// skip overlay surfaces
-- extern idCVar r_skipShadows;				// disable shadows

-- extern idCVar r_ignoreGLErrors;

-- extern idCVar r_screenFraction;				// for testing fill rate, the resolution of the entire screen can be changed
-- extern idCVar r_showUnsmoothedTangents;		// highlight geometry rendered with unsmoothed tangents
-- extern idCVar r_showSilhouette;				// highlight edges that are casting shadow planes
-- extern idCVar r_showVertexColor;			// draws all triangles with the solid vertex color
-- extern idCVar r_showUpdates;				// report entity and light updates and ref counts
-- extern idCVar r_showDemo;					// report reads and writes to the demo file
-- extern idCVar r_showDynamic;				// report stats on dynamic surface generation
-- extern idCVar r_showIntensity;				// draw the screen colors based on intensity, red = 0, green = 128, blue = 255
-- extern idCVar r_showTrace;					// show the intersection of an eye trace with the world
-- extern idCVar r_showDepth;					// display the contents of the depth buffer and the depth range
-- extern idCVar r_showTris;					// enables wireframe rendering of the world
-- extern idCVar r_showSurfaceInfo;			// show surface material name under crosshair
-- extern idCVar r_showNormals;				// draws wireframe normals
-- extern idCVar r_showEdges;					// draw the sil edges
-- extern idCVar r_showViewEntitys;			// displays the bounding boxes of all view models and optionally the index
-- extern idCVar r_showTexturePolarity;		// shade triangles by texture area polarity
-- extern idCVar r_showTangentSpace;			// shade triangles by tangent space
-- extern idCVar r_showDominantTri;			// draw lines from vertexes to center of dominant triangles
-- extern idCVar r_showTextureVectors;			// draw each triangles texture (tangent) vectors
-- extern idCVar r_showLights;					// 1 = print light info, 2 = also draw volumes
-- extern idCVar r_showLightCount;				// colors surfaces based on light count
-- extern idCVar r_showShadows;				// visualize the stencil shadow volumes
-- extern idCVar r_showLightScissors;			// show light scissor rectangles
-- extern idCVar r_showMemory;					// print frame memory utilization
-- extern idCVar r_showCull;					// report sphere and box culling stats
-- extern idCVar r_showAddModel;				// report stats from tr_addModel
-- extern idCVar r_showSurfaces;				// report surface/light/shadow counts
-- extern idCVar r_showPrimitives;				// report vertex/index/draw counts
-- extern idCVar r_showPortals;				// draw portal outlines in color based on passed / not passed
-- extern idCVar r_showSkel;					// draw the skeleton when model animates
-- extern idCVar r_showOverDraw;				// show overdraw
-- extern idCVar r_jointNameScale;				// size of joint names when r_showskel is set to 1
-- extern idCVar r_jointNameOffset;			// offset of joint names when r_showskel is set to 1

-- extern idCVar r_testGamma;					// draw a grid pattern to test gamma levels
-- extern idCVar r_testGammaBias;				// draw a grid pattern to test gamma levels

-- extern idCVar r_singleLight;				// suppress all but one light
-- extern idCVar r_singleEntity;				// suppress all but one entity
-- extern idCVar r_singleArea;					// only draw the portal area the view is actually in
-- extern idCVar r_singleSurface;				// suppress all but one surface on each entity
-- extern idCVar r_shadowPolygonOffset;		// bias value added to depth test for stencil shadow drawing
-- extern idCVar r_shadowPolygonFactor;		// scale value for stencil shadow drawing

-- extern idCVar r_jitter;						// randomly subpixel jitter the projection matrix
-- extern idCVar r_orderIndexes;				// perform index reorganization to optimize vertex use

-- extern idCVar r_debugLineDepthTest;			// perform depth test on debug lines
-- extern idCVar r_debugLineWidth;				// width of debug lines
-- extern idCVar r_debugArrowStep;				// step size of arrow cone line rotation in degrees
-- extern idCVar r_debugPolygonFilled;

-- extern idCVar r_materialOverride;			// override all materials

-- extern idCVar r_debugRenderToTexture;

-- extern idCVar stereoRender_deGhost;			// subtract from opposite eye to reduce ghosting

-- extern idCVar r_useGPUSkinning;
--  static drawSurf_t * R_FinishDeform( drawSurf_t * surf, srfTriangles_t * newTri, const idDrawVert * newVerts, const triIndex_t * newIndexes ) {
--  static drawSurf_t * R_AutospriteDeform( drawSurf_t *surf ) {
--  static drawSurf_t * R_TubeDeform( drawSurf_t * surf ) {
--  int	R_WindingFromTriangles( const srfTriangles_t *tri, triIndex_t indexes[MAX_TRI_WINDING_INDEXES] ) {
--  static drawSurf_t * R_FlareDeform( drawSurf_t * surf ) {
--  static drawSurf_t * R_ExpandDeform( drawSurf_t * surf ) {
--  static drawSurf_t * R_MoveDeform( drawSurf_t * surf ) {
--  static drawSurf_t * R_TurbulentDeform( drawSurf_t * surf ) {
--  static void AddTriangleToIsland_r( const srfTriangles_t *tri, int triangleNum, bool *usedList, eyeIsland_t *island ) {
--  static drawSurf_t * R_EyeballDeform( drawSurf_t * surf ) {
--  static drawSurf_t * R_ParticleDeform( drawSurf_t *surf, bool useArea ) {
--  drawSurf_t * R_DeformDrawSurf( drawSurf_t * drawSurf ) {
--  viewEntity_t * R_SortViewEntities( viewEntity_t * vEntities ) {
--  void R_ClearEntityDefDynamicModel( idRenderEntityLocal *def ) {
--  bool R_IssueEntityDefCallback( idRenderEntityLocal *def ) {
--  idRenderModel *R_EntityDefDynamicModel( idRenderEntityLocal *def ) {
--  void R_SetupDrawSurfShader( drawSurf_t * drawSurf, const idMaterial * shader, const renderEntity_t * renderEntity ) {
--  void R_SetupDrawSurfJoints( drawSurf_t * drawSurf, const srfTriangles_t * tri, const idMaterial * shader ) {
--  void R_AddSingleModel( viewEntity_t * vEntity ) {
--  void R_LinkDrawSurfToView( drawSurf_t * drawSurf, viewDef_t * viewDef ) {
--  void R_AddModels() {
--  void R_ShadowBounds( const idBounds & modelBounds, const idBounds & lightBounds, const idVec3 & lightOrigin, idBounds & shadowBounds ) {
--  bool idRenderEntityLocal::IsDirectlyVisible() const {
--  static void R_AddSingleLight( viewLight_t * vLight ) {
--  void R_AddLights() {
--  void R_OptimizeViewLightsList() {
--  static void R_MirrorPoint( const idVec3 in, orientation_t *surface, orientation_t *camera, idVec3 &out ) {
--  static void R_MirrorVector( const idVec3 in, orientation_t *surface, orientation_t *camera, idVec3 &out ) {
--  static void R_PlaneForSurface( const srfTriangles_t *tri, idPlane &plane ) {
--  bool R_PreciseCullSurface( const drawSurf_t *drawSurf, idBounds &ndcBounds ) {
--  static viewDef_t *R_MirrorViewBySurface( const drawSurf_t *drawSurf ) {
--  static viewDef_t *R_XrayViewBySurface( const drawSurf_t *drawSurf ) {
--  static void R_RemoteRender( const drawSurf_t *surf, textureStage_t *stage ) {
--  void R_MirrorRender( const drawSurf_t *surf, textureStage_t *stage, idScreenRect scissor ) {
--  void R_XrayRender( const drawSurf_t *surf, textureStage_t *stage, idScreenRect scissor ) {
--  bool R_GenerateSurfaceSubview( const drawSurf_t *drawSurf ) {
--  bool R_GenerateSubViews( const drawSurf_t * const drawSurfs[], const int numDrawSurfs ) {
--  int R_TriSurfMemory( const srfTriangles_t *tri ) {
--  void R_FreeStaticTriSurfVertexCaches( srfTriangles_t * tri ) {
--  void R_FreeStaticTriSurf( srfTriangles_t *tri ) {
--  void R_FreeStaticTriSurfVerts( srfTriangles_t *tri ) {
--  srfTriangles_t *R_AllocStaticTriSurf() {
--  srfTriangles_t *R_CopyStaticTriSurf( const srfTriangles_t *tri ) {
--  void R_AllocStaticTriSurfVerts( srfTriangles_t *tri, int numVerts ) {
--  void R_AllocStaticTriSurfIndexes( srfTriangles_t *tri, int numIndexes ) {
--  void R_AllocStaticTriSurfSilIndexes( srfTriangles_t *tri, int numIndexes ) {
--  void R_AllocStaticTriSurfDominantTris( srfTriangles_t *tri, int numVerts ) {
--  void R_AllocStaticTriSurfMirroredVerts( srfTriangles_t *tri, int numMirroredVerts ) {
--  void R_AllocStaticTriSurfDupVerts( srfTriangles_t *tri, int numDupVerts ) {
--  void R_AllocStaticTriSurfSilEdges( srfTriangles_t *tri, int numSilEdges ) {
--  void R_AllocStaticTriSurfPreLightShadowVerts( srfTriangles_t *tri, int numVerts ) {
--  void R_ResizeStaticTriSurfVerts( srfTriangles_t *tri, int numVerts ) {
--  void R_ResizeStaticTriSurfIndexes( srfTriangles_t *tri, int numIndexes ) {
--  void R_ReferenceStaticTriSurfVerts( srfTriangles_t *tri, const srfTriangles_t *reference ) {
--  void R_ReferenceStaticTriSurfIndexes( srfTriangles_t *tri, const srfTriangles_t *reference ) {
--  void R_FreeStaticTriSurfSilIndexes( srfTriangles_t *tri ) {
--  void R_RangeCheckIndexes( const srfTriangles_t *tri ) {
--  void R_BoundTriSurf( srfTriangles_t *tri ) {
--  static int *R_CreateSilRemap( const srfTriangles_t *tri ) {
--  void R_CreateSilIndexes( srfTriangles_t *tri ) {
--  void R_CreateDupVerts( srfTriangles_t *tri ) {
--  static void R_DefineEdge( const int v1, const int v2, const int planeNum, const int numPlanes,idList<silEdge_t> & silEdges, idHashIndex	& silEdgeHash ) {
--  static int SilEdgeSort( const void *a, const void *b ) {
--  void R_IdentifySilEdges( srfTriangles_t *tri, bool omitCoplanarEdges ) {
--  static bool R_FaceNegativePolarity( const srfTriangles_t *tri, int firstIndex ) {
--  static void	R_DuplicateMirroredVertexes( srfTriangles_t *tri ) {
--  void R_DeriveNormalsAndTangents( srfTriangles_t *tri ) {
--  void R_DeriveUnsmoothedNormalsAndTangents( srfTriangles_t * tri ) {
--  void R_CreateVertexNormals( srfTriangles_t *tri ) {
--  void R_DeriveTangentsWithoutNormals( srfTriangles_t *tri ) {
--  void R_BuildDominantTris( srfTriangles_t *tri ) {
--  void R_DeriveTangents( srfTriangles_t *tri ) {
--  void R_RemoveDuplicatedTriangles( srfTriangles_t *tri ) {
--  void R_RemoveDegenerateTriangles( srfTriangles_t *tri ) {
--  void R_TestDegenerateTextureSpace( srfTriangles_t *tri ) {
--  void R_RemoveUnusedVerts( srfTriangles_t *tri ) {
--  srfTriangles_t * R_MergeSurfaceList( const srfTriangles_t **surfaces, int numSurfaces ) {
--  srfTriangles_t * R_MergeTriangles( const srfTriangles_t *tri1, const srfTriangles_t *tri2 ) {
--  void R_ReverseTriangles( srfTriangles_t *tri ) {
--  void R_CleanupTriangles( srfTriangles_t *tri, bool createNormals, bool identifySilEdges, bool useUnsmoothedTangents ) {
--  deformInfo_t *R_BuildDeformInfo( int numVerts, const idDrawVert *verts, int numIndexes, const int *indexes,bool useUnsmoothedTangents ) {
--  void R_FreeDeformInfo( deformInfo_t *deformInfo ) {
--  int R_DeformInfoMemoryUsed( deformInfo_t *deformInfo ) {
--  void R_InitDrawSurfFromTri( drawSurf_t & ds, srfTriangles_t & tri ) {
--  void R_CreateStaticBuffersForTri( srfTriangles_t & tri ) {
  end Neo.System.Graphics.World;
