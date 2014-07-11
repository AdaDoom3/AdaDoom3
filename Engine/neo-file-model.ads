with Neo.System.Processor.Geometry; use Neo.System.Processor.Geometry;
package Neo.File.Model is
    type Enumerated_Format is(
      Wavefront_Format,     -- 1988 Wavefront Technologies
      Studio_Max_Format,    -- 1990 Autodesk
      Id_Tech_Format,       -- 1991 Id Software
      Light_Wave_3D_Format, -- 1994 NewTek
      Blender_Format,       -- 1995 Neo Geo, Not a Number Technologies
      Half_Life_Format,     -- 1996 Valve Software
      Maya_Format);         -- 1997 Alias Technologies
    type Record_Model is record
        bounds;idBounds                    -- for culling
        generateNormals;bool                -- create normals from geometry, instead of using explicit ones
        tangentsCalculated; bool               -- set when the vertex tangents have been calculated
        perfectHull; bool                 -- true if there aren't any dangling edges
        referencedVerts;bool                -- if true the 'verts' are referenced and should not be freed
        referencedIndexes;bool                -- if true, indexes, silIndexes, mirrorVerts, and silEdges are pointers into the original surface, and should not be freed
        numVerts;int                      -- number of vertices
        verts;   idDrawVert *               -- vertices, allocated with special allocator
        numIndexes;int                      -- for shadows, this has both front and rear end caps and silhouette planes
        numIndexes; triIndex_t *            -- indexes, allocated with special allocator
        silIndexes; triIndex_t *               -- indexes changed to be the first vertex with same XYZ, ignoring normal and texcoords
        numMirroredVerts;int                  -- this many verts at the end of the vert list are tangent mirrors
        mirroredVerts; int *                 -- tri->mirroredVerts[0] is the mirror of tri->numVerts - tri->numMirroredVerts + 0
        numDupVerts;  int                  -- number of duplicate vertexes
        dupVerts; int *                   -- pairs of the number of the first vertex and the number of the duplicate vertex
        numSilEdges;int                    -- number of silhouette edges
        silEdges;silEdge_t *                  -- silhouette edges
        dominantTri_t *        dominantTris;      -- [numVerts] for deformed surface fast tangent calculation
        int              numShadowIndexesNoFrontCaps;  -- shadow volumes with front caps omitted
        int              numShadowIndexesNoCaps;      -- shadow volumes with the front and rear caps omitted
        int              shadowCapPlaneBits;    -- bits 0-5 are set when that plane of the interacting light has triangles projected on it, which means that if the view is on the outside of that plane, we need to draw the rear caps of the shadow volume dynamic shadows will have SHADOW_CAP_INFINITE
        idShadowVert *        preLightShadowVertexes;  -- shadow vertices in CPU memory for pre-light shadow volumes
        idShadowVert *        staticShadowVertexes;  -- shadow vertices in CPU memory for static shadow volumes
        srfTriangles_t *      ambientSurface;      -- for light interactions, point back at the original surface that generated
                                -- the interaction, which we will get the ambientCache from
        srfTriangles_t *      nextDeferredFree;    -- chain of tris to free next frame
        -- for deferred normal / tangent transformations by joints
        -- the jointsInverted list / buffer object on md5WithJoints may be
        -- shared by multiple srfTriangles_t
        idRenderModelStatic *    staticModelWithJoints;
        -- data in vertex object space, not directly readable by the CPU
        vertCacheHandle_t      indexCache;        -- GL_INDEX_TYPE
        vertCacheHandle_t      ambientCache;      -- idDrawVert
        vertCacheHandle_t      shadowCache;      -- idVec4
        DISALLOW_COPY_AND_ASSIGN( srfTriangles_t );
      end record;
    function Load(Path : in String_2) return Record_Animation;
    function Load(Path : in String_2) return Record_Camera;
    function Load(Path : in String_2) return Record_Model;
  end Neo.File.Model;
