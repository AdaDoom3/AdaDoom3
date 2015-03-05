-- GLvoid
-- GLenum        Integer_4_Unsigned_C
-- GLboolean     Integer_1_Unsigned_C
-- GLchar        Integer_1_Unsigned_C
-- GLcharARB     Integer_1_Unsigned_C
-- GLuint        Integer_4_Unsigned_C
-- GLsizei       Integer_4_Signed_C
-- GLbitfield    Integer_4_Unsigned_C
-- GLintptr      Integer_Address
-- GLsizeiptr    Integer_Address
-- GLintptrARB   Integer_4_Signed_C
-- GLsizeiptrARB Integer_4_Signed_C
-- GLSync        Address
-- GLfloat       Float_4_Real_C
-- GLclampd      Float_8_Real_C
-- GLhandleARB   Integer_Address
with Neo.System; use Neo.System;
package Neo.OpenGL is
  GL_VERSION_1_1                         : constant Integer_4_Unsigned_C := 16#0000_0001#;
  GL_VERSION_1_2                         : constant Integer_4_Unsigned_C := 16#0000_0001#;
  GL_VERSION_1_3                         : constant Integer_4_Unsigned_C := 16#0000_0001#;
  GL_ARB_IMAGING                         : constant Integer_4_Unsigned_C := 16#0000_0001#;
  GL_FALSE                               : constant Integer_1_Unsigned_C := 16#00#; --  Boolean values
  GL_TRUE                                : constant Integer_1_Unsigned_C := 16#01#;
  GL_BYTE                                : constant Integer_4_Unsigned_C := 16#1400#; --  Data types
  GL_UNSIGNED_BYTE                       : constant Integer_4_Unsigned_C := 16#1401#;
  GL_SHORT                               : constant Integer_4_Unsigned_C := 16#1402#;
  GL_UNSIGNED_SHORT                      : constant Integer_4_Unsigned_C := 16#1403#;
  GL_INT                                 : constant Integer_4_Unsigned_C := 16#1404#;
  GL_UNSIGNED_INT                        : constant Integer_4_Unsigned_C := 16#1405#;
  GL_FLOAT                               : constant Integer_4_Unsigned_C := 16#1406#;
  GL_DOUBLE                              : constant Integer_4_Unsigned_C := 16#140A#;
  GL_2_BYTES                             : constant Integer_4_Unsigned_C := 16#1407#;
  GL_3_BYTES                             : constant Integer_4_Unsigned_C := 16#1408#;
  GL_4_BYTES                             : constant Integer_4_Unsigned_C := 16#1409#;
  GL_POINTS                              : constant Integer_4_Unsigned_C := 16#0000#; --  Primitives
  GL_LINES                               : constant Integer_4_Unsigned_C := 16#0001#;
  GL_LINE_LOOP                           : constant Integer_4_Unsigned_C := 16#0002#;
  GL_LINE_STRIP                          : constant Integer_4_Unsigned_C := 16#0003#;
  GL_TRIANGLES                           : constant Integer_4_Unsigned_C := 16#0004#;
  GL_TRIANGLE_STRIP                      : constant Integer_4_Unsigned_C := 16#0005#;
  GL_TRIANGLE_FAN                        : constant Integer_4_Unsigned_C := 16#0006#;
  GL_QUADS                               : constant Integer_4_Unsigned_C := 16#0007#;
  GL_QUAD_STRIP                          : constant Integer_4_Unsigned_C := 16#0008#;
  GL_POLYGON                             : constant Integer_4_Unsigned_C := 16#0009#;
  GL_VERTEX_ARRAY                        : constant Integer_4_Unsigned_C := 16#0000_8074#; --  Vertex Arrays
  GL_NORMAL_ARRAY                        : constant Integer_4_Unsigned_C := 16#0000_8075#;
  GL_COLOR_ARRAY                         : constant Integer_4_Unsigned_C := 16#0000_8076#;
  GL_INDEX_ARRAY                         : constant Integer_4_Unsigned_C := 16#0000_8077#;
  GL_TEXTURE_COORD_ARRAY                 : constant Integer_4_Unsigned_C := 16#0000_8078#;
  GL_EDGE_FLAG_ARRAY                     : constant Integer_4_Unsigned_C := 16#0000_8079#;
  GL_VERTEX_ARRAY_SIZE                   : constant Integer_4_Unsigned_C := 16#0000_807A#;
  GL_VERTEX_ARRAY_TYPE                   : constant Integer_4_Unsigned_C := 16#0000_807B#;
  GL_VERTEX_ARRAY_STRIDE                 : constant Integer_4_Unsigned_C := 16#0000_807C#;
  GL_NORMAL_ARRAY_TYPE                   : constant Integer_4_Unsigned_C := 16#0000_807E#;
  GL_NORMAL_ARRAY_STRIDE                 : constant Integer_4_Unsigned_C := 16#0000_807F#;
  GL_COLOR_ARRAY_SIZE                    : constant Integer_4_Unsigned_C := 16#0000_8081#;
  GL_COLOR_ARRAY_TYPE                    : constant Integer_4_Unsigned_C := 16#0000_8082#;
  GL_COLOR_ARRAY_STRIDE                  : constant Integer_4_Unsigned_C := 16#0000_8083#;
  GL_INDEX_ARRAY_TYPE                    : constant Integer_4_Unsigned_C := 16#0000_8085#;
  GL_INDEX_ARRAY_STRIDE                  : constant Integer_4_Unsigned_C := 16#0000_8086#;
  GL_TEXTURE_COORD_ARRAY_SIZE            : constant Integer_4_Unsigned_C := 16#0000_8088#;
  GL_TEXTURE_COORD_ARRAY_TYPE            : constant Integer_4_Unsigned_C := 16#0000_8089#;
  GL_TEXTURE_COORD_ARRAY_STRIDE          : constant Integer_4_Unsigned_C := 16#0000_808A#;
  GL_EDGE_FLAG_ARRAY_STRIDE              : constant Integer_4_Unsigned_C := 16#0000_808C#;
  GL_VERTEX_ARRAY_POINTER                : constant Integer_4_Unsigned_C := 16#0000_808E#;
  GL_NORMAL_ARRAY_POINTER                : constant Integer_4_Unsigned_C := 16#0000_808F#;
  GL_COLOR_ARRAY_POINTER                 : constant Integer_4_Unsigned_C := 16#0000_8090#;
  GL_INDEX_ARRAY_POINTER                 : constant Integer_4_Unsigned_C := 16#0000_8091#;
  GL_TEXTURE_COORD_ARRAY_POINTER         : constant Integer_4_Unsigned_C := 16#0000_8092#;
  GL_EDGE_FLAG_ARRAY_POINTER             : constant Integer_4_Unsigned_C := 16#0000_8093#;
  GL_V2F                                 : constant Integer_4_Unsigned_C := 16#2A20#;
  GL_V3F                                 : constant Integer_4_Unsigned_C := 16#2A21#;
  GL_C4UB_V2F                            : constant Integer_4_Unsigned_C := 16#2A22#;
  GL_C4UB_V3F                            : constant Integer_4_Unsigned_C := 16#2A23#;
  GL_C3F_V3F                             : constant Integer_4_Unsigned_C := 16#2A24#;
  GL_N3F_V3F                             : constant Integer_4_Unsigned_C := 16#2A25#;
  GL_C4F_N3F_V3F                         : constant Integer_4_Unsigned_C := 16#2A26#;
  GL_T2F_V3F                             : constant Integer_4_Unsigned_C := 16#2A27#;
  GL_T4F_V4F                             : constant Integer_4_Unsigned_C := 16#2A28#;
  GL_T2F_C4UB_V3F                        : constant Integer_4_Unsigned_C := 16#2A29#;
  GL_T2F_C3F_V3F                         : constant Integer_4_Unsigned_C := 16#2A2A#;
  GL_T2F_N3F_V3F                         : constant Integer_4_Unsigned_C := 16#2A2B#;
  GL_T2F_C4F_N3F_V3F                     : constant Integer_4_Unsigned_C := 16#2A2C#;
  GL_T4F_C4F_N3F_V4F                     : constant Integer_4_Unsigned_C := 16#2A2D#;
  GL_MATRIX_MODE                         : constant Integer_4_Unsigned_C := 16#0BA0#; --  Matrix Mode
  GL_MODELVIEW                           : constant Integer_4_Unsigned_C := 16#1700#;
  GL_PROJECTION                          : constant Integer_4_Unsigned_C := 16#1701#;
  GL_TEXTURE                             : constant Integer_4_Unsigned_C := 16#1702#;
  GL_POINT_SMOOTH                        : constant Integer_4_Unsigned_C := 16#0B10#; --  Points
  GL_POINT_SIZE                          : constant Integer_4_Unsigned_C := 16#0B11#;
  GL_POINT_SIZE_GRANULARITY              : constant Integer_4_Unsigned_C := 16#0B13#;
  GL_POINT_SIZE_RANGE                    : constant Integer_4_Unsigned_C := 16#0B12#;
  GL_LINE_SMOOTH                         : constant Integer_4_Unsigned_C := 16#0B20#; --  Lines
  GL_LINE_STIPPLE                        : constant Integer_4_Unsigned_C := 16#0B24#;
  GL_LINE_STIPPLE_PATTERN                : constant Integer_4_Unsigned_C := 16#0B25#;
  GL_LINE_STIPPLE_REPEAT                 : constant Integer_4_Unsigned_C := 16#0B26#;
  GL_LINE_WIDTH                          : constant Integer_4_Unsigned_C := 16#0B21#;
  GL_LINE_WIDTH_GRANULARITY              : constant Integer_4_Unsigned_C := 16#0B23#;
  GL_LINE_WIDTH_RANGE                    : constant Integer_4_Unsigned_C := 16#0B22#;
  GL_POINT                               : constant Integer_4_Unsigned_C := 16#1B00#; --  Polygons
  GL_LINE                                : constant Integer_4_Unsigned_C := 16#1B01#;
  GL_FILL                                : constant Integer_4_Unsigned_C := 16#1B02#;
  GL_CW                                  : constant Integer_4_Unsigned_C := 16#0900#;
  GL_CCW                                 : constant Integer_4_Unsigned_C := 16#0901#;
  GL_FRONT                               : constant Integer_4_Unsigned_C := 16#0404#;
  GL_BACK                                : constant Integer_4_Unsigned_C := 16#0405#;
  GL_POLYGON_MODE                        : constant Integer_4_Unsigned_C := 16#0B40#;
  GL_POLYGON_SMOOTH                      : constant Integer_4_Unsigned_C := 16#0B41#;
  GL_POLYGON_STIPPLE                     : constant Integer_4_Unsigned_C := 16#0B42#;
  GL_EDGE_FLAG                           : constant Integer_4_Unsigned_C := 16#0B43#;
  GL_CULL_FACE                           : constant Integer_4_Unsigned_C := 16#0B44#;
  GL_CULL_FACE_MODE                      : constant Integer_4_Unsigned_C := 16#0B45#;
  GL_FRONT_FACE                          : constant Integer_4_Unsigned_C := 16#0B46#;
  GL_POLYGON_OFFSET_FACTOR               : constant Integer_4_Unsigned_C := 16#0000_8038#;
  GL_POLYGON_OFFSET_UNITS                : constant Integer_4_Unsigned_C := 16#2A00#;
  GL_POLYGON_OFFSET_POINT                : constant Integer_4_Unsigned_C := 16#2A01#;
  GL_POLYGON_OFFSET_LINE                 : constant Integer_4_Unsigned_C := 16#2A02#;
  GL_POLYGON_OFFSET_FILL                 : constant Integer_4_Unsigned_C := 16#0000_8037#;
  GL_COMPILE                             : constant Integer_4_Unsigned_C := 16#1300#; --  Display lists
  GL_COMPILE_AND_EXECUTE                 : constant Integer_4_Unsigned_C := 16#1301#;
  GL_LIST_BASE                           : constant Integer_4_Unsigned_C := 16#0B32#;
  GL_LIST_INDEX                          : constant Integer_4_Unsigned_C := 16#0B33#;
  GL_LIST_MODE                           : constant Integer_4_Unsigned_C := 16#0B30#;
  GL_NEVER                               : constant Integer_4_Unsigned_C := 16#0200#; --  Depth buffer
  GL_LESS                                : constant Integer_4_Unsigned_C := 16#0201#;
  GL_EQUAL                               : constant Integer_4_Unsigned_C := 16#0202#;
  GL_LEQUAL                              : constant Integer_4_Unsigned_C := 16#0203#;
  GL_GREATER                             : constant Integer_4_Unsigned_C := 16#0204#;
  GL_NOTEQUAL                            : constant Integer_4_Unsigned_C := 16#0205#;
  GL_GEQUAL                              : constant Integer_4_Unsigned_C := 16#0206#;
  GL_ALWAYS                              : constant Integer_4_Unsigned_C := 16#0207#;
  GL_DEPTH_TEST                          : constant Integer_4_Unsigned_C := 16#0B71#;
  GL_DEPTH_BITS                          : constant Integer_4_Unsigned_C := 16#0D56#;
  GL_DEPTH_CLEAR_VALUE                   : constant Integer_4_Unsigned_C := 16#0B73#;
  GL_DEPTH_FUNC                          : constant Integer_4_Unsigned_C := 16#0B74#;
  GL_DEPTH_RANGE                         : constant Integer_4_Unsigned_C := 16#0B70#;
  GL_DEPTH_WRITEMASK                     : constant Integer_4_Unsigned_C := 16#0B72#;
  GL_DEPTH_COMPONENT                     : constant Integer_4_Unsigned_C := 16#1902#;
  GL_LIGHTING                            : constant Integer_4_Unsigned_C := 16#0B50#; --  Lighting
  GL_LIGHT0                              : constant Integer_4_Unsigned_C := 16#4000#;
  GL_LIGHT1                              : constant Integer_4_Unsigned_C := 16#4001#;
  GL_LIGHT2                              : constant Integer_4_Unsigned_C := 16#4002#;
  GL_LIGHT3                              : constant Integer_4_Unsigned_C := 16#4003#;
  GL_LIGHT4                              : constant Integer_4_Unsigned_C := 16#4004#;
  GL_LIGHT5                              : constant Integer_4_Unsigned_C := 16#4005#;
  GL_LIGHT6                              : constant Integer_4_Unsigned_C := 16#4006#;
  GL_LIGHT7                              : constant Integer_4_Unsigned_C := 16#4007#;
  GL_SPOT_EXPONENT                       : constant Integer_4_Unsigned_C := 16#1205#;
  GL_SPOT_CUTOFF                         : constant Integer_4_Unsigned_C := 16#1206#;
  GL_CONSTANT_ATTENUATION                : constant Integer_4_Unsigned_C := 16#1207#;
  GL_LINEAR_ATTENUATION                  : constant Integer_4_Unsigned_C := 16#1208#;
  GL_QUADRATIC_ATTENUATION               : constant Integer_4_Unsigned_C := 16#1209#;
  GL_AMBIENT                             : constant Integer_4_Unsigned_C := 16#1200#;
  GL_DIFFUSE                             : constant Integer_4_Unsigned_C := 16#1201#;
  GL_SPECULAR                            : constant Integer_4_Unsigned_C := 16#1202#;
  GL_SHININESS                           : constant Integer_4_Unsigned_C := 16#1601#;
  GL_EMISSION                            : constant Integer_4_Unsigned_C := 16#1600#;
  GL_POSITION                            : constant Integer_4_Unsigned_C := 16#1203#;
  GL_SPOT_DIRECTION                      : constant Integer_4_Unsigned_C := 16#1204#;
  GL_AMBIENT_AND_DIFFUSE                 : constant Integer_4_Unsigned_C := 16#1602#;
  GL_COLOR_INDEXES                       : constant Integer_4_Unsigned_C := 16#1603#;
  GL_LIGHT_MODEL_TWO_SIDE                : constant Integer_4_Unsigned_C := 16#0B52#;
  GL_LIGHT_MODEL_LOCAL_VIEWER            : constant Integer_4_Unsigned_C := 16#0B51#;
  GL_LIGHT_MODEL_AMBIENT                 : constant Integer_4_Unsigned_C := 16#0B53#;
  GL_FRONT_AND_BACK                      : constant Integer_4_Unsigned_C := 16#0408#;
  GL_SHADE_MODEL                         : constant Integer_4_Unsigned_C := 16#0B54#;
  GL_FLAT                                : constant Integer_4_Unsigned_C := 16#1D00#;
  GL_SMOOTH                              : constant Integer_4_Unsigned_C := 16#1D01#;
  GL_COLOR_MATERIAL                      : constant Integer_4_Unsigned_C := 16#0B57#;
  GL_COLOR_MATERIAL_FACE                 : constant Integer_4_Unsigned_C := 16#0B55#;
  GL_COLOR_MATERIAL_PARAMETER            : constant Integer_4_Unsigned_C := 16#0B56#;
  GL_NORMALIZE                           : constant Integer_4_Unsigned_C := 16#0BA1#;
  GL_CLIP_PLANE0                         : constant Integer_4_Unsigned_C := 16#3000#; --  User clipping planes
  GL_CLIP_PLANE1                         : constant Integer_4_Unsigned_C := 16#3001#;
  GL_CLIP_PLANE2                         : constant Integer_4_Unsigned_C := 16#3002#;
  GL_CLIP_PLANE3                         : constant Integer_4_Unsigned_C := 16#3003#;
  GL_CLIP_PLANE4                         : constant Integer_4_Unsigned_C := 16#3004#;
  GL_CLIP_PLANE5                         : constant Integer_4_Unsigned_C := 16#3005#;
  GL_ACCUM_RED_BITS                      : constant Integer_4_Unsigned_C := 16#0D58#; --  Accumulation buffer
  GL_ACCUM_GREEN_BITS                    : constant Integer_4_Unsigned_C := 16#0D59#;
  GL_ACCUM_BLUE_BITS                     : constant Integer_4_Unsigned_C := 16#0D5A#;
  GL_ACCUM_ALPHA_BITS                    : constant Integer_4_Unsigned_C := 16#0D5B#;
  GL_ACCUM_CLEAR_VALUE                   : constant Integer_4_Unsigned_C := 16#0B80#;
  GL_ACCUM                               : constant Integer_4_Unsigned_C := 16#0100#;
  GL_ADD                                 : constant Integer_4_Unsigned_C := 16#0104#;
  GL_LOAD                                : constant Integer_4_Unsigned_C := 16#0101#;
  GL_MULT                                : constant Integer_4_Unsigned_C := 16#0103#;
  GL_RETURN                              : constant Integer_4_Unsigned_C := 16#0102#;
  GL_ALPHA_TEST                          : constant Integer_4_Unsigned_C := 16#0BC0#; --  Alpha testing
  GL_ALPHA_TEST_REF                      : constant Integer_4_Unsigned_C := 16#0BC2#;
  GL_ALPHA_TEST_FUNC                     : constant Integer_4_Unsigned_C := 16#0BC1#;
  GL_BLEND                               : constant Integer_4_Unsigned_C := 16#0BE2#; --  Blending
  GL_BLEND_SRC                           : constant Integer_4_Unsigned_C := 16#0BE1#;
  GL_BLEND_DST                           : constant Integer_4_Unsigned_C := 16#0BE0#;
  GL_ZERO                                : constant Integer_4_Unsigned_C := 16#0000#;
  GL_ONE                                 : constant Integer_4_Unsigned_C := 16#0001#;
  GL_SRC_COLOR                           : constant Integer_4_Unsigned_C := 16#0300#;
  GL_ONE_MINUS_SRC_COLOR                 : constant Integer_4_Unsigned_C := 16#0301#;
  GL_SRC_ALPHA                           : constant Integer_4_Unsigned_C := 16#0302#;
  GL_ONE_MINUS_SRC_ALPHA                 : constant Integer_4_Unsigned_C := 16#0303#;
  GL_DST_ALPHA                           : constant Integer_4_Unsigned_C := 16#0304#;
  GL_ONE_MINUS_DST_ALPHA                 : constant Integer_4_Unsigned_C := 16#0305#;
  GL_DST_COLOR                           : constant Integer_4_Unsigned_C := 16#0306#;
  GL_ONE_MINUS_DST_COLOR                 : constant Integer_4_Unsigned_C := 16#0307#;
  GL_SRC_ALPHA_SATURATE                  : constant Integer_4_Unsigned_C := 16#0308#;
  GL_FEEDBACK                            : constant Integer_4_Unsigned_C := 16#1C01#; --  Render mode
  GL_RENDER                              : constant Integer_4_Unsigned_C := 16#1C00#;
  GL_SELECT                              : constant Integer_4_Unsigned_C := 16#1C02#;
  GL_2D                                  : constant Integer_4_Unsigned_C := 16#0600#; --  Feedback
  GL_3D                                  : constant Integer_4_Unsigned_C := 16#0601#;
  GL_3D_COLOR                            : constant Integer_4_Unsigned_C := 16#0602#;
  GL_3D_COLOR_TEXTURE                    : constant Integer_4_Unsigned_C := 16#0603#;
  GL_4D_COLOR_TEXTURE                    : constant Integer_4_Unsigned_C := 16#0604#;
  GL_POINT_TOKEN                         : constant Integer_4_Unsigned_C := 16#0701#;
  GL_LINE_TOKEN                          : constant Integer_4_Unsigned_C := 16#0702#;
  GL_LINE_RESET_TOKEN                    : constant Integer_4_Unsigned_C := 16#0707#;
  GL_POLYGON_TOKEN                       : constant Integer_4_Unsigned_C := 16#0703#;
  GL_BITMAP_TOKEN                        : constant Integer_4_Unsigned_C := 16#0704#;
  GL_DRAW_PIXEL_TOKEN                    : constant Integer_4_Unsigned_C := 16#0705#;
  GL_COPY_PIXEL_TOKEN                    : constant Integer_4_Unsigned_C := 16#0706#;
  GL_PASS_THROUGH_TOKEN                  : constant Integer_4_Unsigned_C := 16#0700#;
  GL_FEEDBACK_BUFFER_POINTER             : constant Integer_4_Unsigned_C := 16#0DF0#;
  GL_FEEDBACK_BUFFER_SIZE                : constant Integer_4_Unsigned_C := 16#0DF1#;
  GL_FEEDBACK_BUFFER_TYPE                : constant Integer_4_Unsigned_C := 16#0DF2#;
  GL_SELECTION_BUFFER_POINTER            : constant Integer_4_Unsigned_C := 16#0DF3#; --  Selection
  GL_SELECTION_BUFFER_SIZE               : constant Integer_4_Unsigned_C := 16#0DF4#;
  GL_FOG                                 : constant Integer_4_Unsigned_C := 16#0B60#; --  Fog
  GL_FOG_MODE                            : constant Integer_4_Unsigned_C := 16#0B65#;
  GL_FOG_DENSITY                         : constant Integer_4_Unsigned_C := 16#0B62#;
  GL_FOG_COLOR                           : constant Integer_4_Unsigned_C := 16#0B66#;
  GL_FOG_INDEX                           : constant Integer_4_Unsigned_C := 16#0B61#;
  GL_FOG_START                           : constant Integer_4_Unsigned_C := 16#0B63#;
  GL_FOG_END                             : constant Integer_4_Unsigned_C := 16#0B64#;
  GL_LINEAR                              : constant Integer_4_Unsigned_C := 16#2601#;
  GL_EXP                                 : constant Integer_4_Unsigned_C := 16#0800#;
  GL_EXP2                                : constant Integer_4_Unsigned_C := 16#0801#;
  GL_LOGIC_OP                            : constant Integer_4_Unsigned_C := 16#0BF1#; --  Logic ops
  GL_INDEX_LOGIC_OP                      : constant Integer_4_Unsigned_C := 16#0BF1#;
  GL_COLOR_LOGIC_OP                      : constant Integer_4_Unsigned_C := 16#0BF2#;
  GL_LOGIC_OP_MODE                       : constant Integer_4_Unsigned_C := 16#0BF0#;
  GL_CLEAR                               : constant Integer_4_Unsigned_C := 16#1500#;
  GL_SET                                 : constant Integer_4_Unsigned_C := 16#150F#;
  GL_COPY                                : constant Integer_4_Unsigned_C := 16#1503#;
  GL_COPY_INVERTED                       : constant Integer_4_Unsigned_C := 16#150C#;
  GL_NOOP                                : constant Integer_4_Unsigned_C := 16#1505#;
  GL_INVERT                              : constant Integer_4_Unsigned_C := 16#150A#;
  GL_AND                                 : constant Integer_4_Unsigned_C := 16#1501#;
  GL_NAND                                : constant Integer_4_Unsigned_C := 16#150E#;
  GL_OR                                  : constant Integer_4_Unsigned_C := 16#1507#;
  GL_NOR                                 : constant Integer_4_Unsigned_C := 16#1508#;
  GL_XOR                                 : constant Integer_4_Unsigned_C := 16#1506#;
  GL_EQUIV                               : constant Integer_4_Unsigned_C := 16#1509#;
  GL_AND_REVERSE                         : constant Integer_4_Unsigned_C := 16#1502#;
  GL_AND_INVERTED                        : constant Integer_4_Unsigned_C := 16#1504#;
  GL_OR_REVERSE                          : constant Integer_4_Unsigned_C := 16#150B#;
  GL_OR_INVERTED                         : constant Integer_4_Unsigned_C := 16#150D#;
  GL_STENCIL_TEST                        : constant Integer_4_Unsigned_C := 16#0B90#; --  Stencil
  GL_STENCIL_WRITEMASK                   : constant Integer_4_Unsigned_C := 16#0B98#;
  GL_STENCIL_BITS                        : constant Integer_4_Unsigned_C := 16#0D57#;
  GL_STENCIL_FUNC                        : constant Integer_4_Unsigned_C := 16#0B92#;
  GL_STENCIL_VALUE_MASK                  : constant Integer_4_Unsigned_C := 16#0B93#;
  GL_STENCIL_REF                         : constant Integer_4_Unsigned_C := 16#0B97#;
  GL_STENCIL_FAIL                        : constant Integer_4_Unsigned_C := 16#0B94#;
  GL_STENCIL_PASS_DEPTH_PASS             : constant Integer_4_Unsigned_C := 16#0B96#;
  GL_STENCIL_PASS_DEPTH_FAIL             : constant Integer_4_Unsigned_C := 16#0B95#;
  GL_STENCIL_CLEAR_VALUE                 : constant Integer_4_Unsigned_C := 16#0B91#;
  GL_STENCIL_INDEX                       : constant Integer_4_Unsigned_C := 16#1901#;
  GL_KEEP                                : constant Integer_4_Unsigned_C := 16#1E00#;
  GL_REPLACE                             : constant Integer_4_Unsigned_C := 16#1E01#;
  GL_INCR                                : constant Integer_4_Unsigned_C := 16#1E02#;
  GL_DECR                                : constant Integer_4_Unsigned_C := 16#1E03#;
  GL_NONE                                : constant Integer_4_Unsigned_C := 16#0000#; --  Buffers, Pixel Drawing/Reading
  GL_LEFT                                : constant Integer_4_Unsigned_C := 16#0406#;
  GL_RIGHT                               : constant Integer_4_Unsigned_C := 16#0407#;
  GL_FRONT_LEFT                          : constant Integer_4_Unsigned_C := 16#0400#;
  GL_FRONT_RIGHT                         : constant Integer_4_Unsigned_C := 16#0401#;
  GL_BACK_LEFT                           : constant Integer_4_Unsigned_C := 16#0402#;
  GL_BACK_RIGHT                          : constant Integer_4_Unsigned_C := 16#0403#;
  GL_AUX0                                : constant Integer_4_Unsigned_C := 16#0409#;
  GL_AUX1                                : constant Integer_4_Unsigned_C := 16#040A#;
  GL_AUX2                                : constant Integer_4_Unsigned_C := 16#040B#;
  GL_AUX3                                : constant Integer_4_Unsigned_C := 16#040C#;
  GL_COLOR_INDEX                         : constant Integer_4_Unsigned_C := 16#1900#;
  GL_RED                                 : constant Integer_4_Unsigned_C := 16#1903#;
  GL_GREEN                               : constant Integer_4_Unsigned_C := 16#1904#;
  GL_BLUE                                : constant Integer_4_Unsigned_C := 16#1905#;
  GL_ALPHA                               : constant Integer_4_Unsigned_C := 16#1906#;
  GL_LUMINANCE                           : constant Integer_4_Unsigned_C := 16#1909#;
  GL_LUMINANCE_ALPHA                     : constant Integer_4_Unsigned_C := 16#190A#;
  GL_ALPHA_BITS                          : constant Integer_4_Unsigned_C := 16#0D55#;
  GL_RED_BITS                            : constant Integer_4_Unsigned_C := 16#0D52#;
  GL_GREEN_BITS                          : constant Integer_4_Unsigned_C := 16#0D53#;
  GL_BLUE_BITS                           : constant Integer_4_Unsigned_C := 16#0D54#;
  GL_INDEX_BITS                          : constant Integer_4_Unsigned_C := 16#0D51#;
  GL_SUBPIXEL_BITS                       : constant Integer_4_Unsigned_C := 16#0D50#;
  GL_AUX_BUFFERS                         : constant Integer_4_Unsigned_C := 16#0C00#;
  GL_READ_BUFFER                         : constant Integer_4_Unsigned_C := 16#0C02#;
  GL_DRAW_BUFFER                         : constant Integer_4_Unsigned_C := 16#0C01#;
  GL_DOUBLEBUFFER                        : constant Integer_4_Unsigned_C := 16#0C32#;
  GL_STEREO                              : constant Integer_4_Unsigned_C := 16#0C33#;
  GL_BITMAP                              : constant Integer_4_Unsigned_C := 16#1A00#;
  GL_COLOR                               : constant Integer_4_Unsigned_C := 16#1800#;
  GL_DEPTH                               : constant Integer_4_Unsigned_C := 16#1801#;
  GL_STENCIL                             : constant Integer_4_Unsigned_C := 16#1802#;
  GL_DITHER                              : constant Integer_4_Unsigned_C := 16#0BD0#;
  GL_RGB                                 : constant Integer_4_Unsigned_C := 16#1907#;
  GL_RGBA                                : constant Integer_4_Unsigned_C := 16#1908#;
  GL_MAX_LIST_NESTING                    : constant Integer_4_Unsigned_C := 16#0B31#; --  Implementation limits
  GL_MAX_ATTRIB_STACK_DEPTH              : constant Integer_4_Unsigned_C := 16#0D35#;
  GL_MAX_MODELVIEW_STACK_DEPTH           : constant Integer_4_Unsigned_C := 16#0D36#;
  GL_MAX_NAME_STACK_DEPTH                : constant Integer_4_Unsigned_C := 16#0D37#;
  GL_MAX_PROJECTION_STACK_DEPTH          : constant Integer_4_Unsigned_C := 16#0D38#;
  GL_MAX_TEXTURE_STACK_DEPTH             : constant Integer_4_Unsigned_C := 16#0D39#;
  GL_MAX_EVAL_ORDER                      : constant Integer_4_Unsigned_C := 16#0D30#;
  GL_MAX_LIGHTS                          : constant Integer_4_Unsigned_C := 16#0D31#;
  GL_MAX_CLIP_PLANES                     : constant Integer_4_Unsigned_C := 16#0D32#;
  GL_MAX_TEXTURE_SIZE                    : constant Integer_4_Unsigned_C := 16#0D33#;
  GL_MAX_PIXEL_MAP_TABLE                 : constant Integer_4_Unsigned_C := 16#0D34#;
  GL_MAX_VIEWPORT_DIMS                   : constant Integer_4_Unsigned_C := 16#0D3A#;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH       : constant Integer_4_Unsigned_C := 16#0D3B#;
  GL_ATTRIB_STACK_DEPTH                  : constant Integer_4_Unsigned_C := 16#0BB0#; --  Gets
  GL_CLIENT_ATTRIB_STACK_DEPTH           : constant Integer_4_Unsigned_C := 16#0BB1#;
  GL_COLOR_CLEAR_VALUE                   : constant Integer_4_Unsigned_C := 16#0C22#;
  GL_COLOR_WRITEMASK                     : constant Integer_4_Unsigned_C := 16#0C23#;
  GL_CURRENT_INDEX                       : constant Integer_4_Unsigned_C := 16#0B01#;
  GL_CURRENT_COLOR                       : constant Integer_4_Unsigned_C := 16#0B00#;
  GL_CURRENT_NORMAL                      : constant Integer_4_Unsigned_C := 16#0B02#;
  GL_CURRENT_RASTER_COLOR                : constant Integer_4_Unsigned_C := 16#0B04#;
  GL_CURRENT_RASTER_DISTANCE             : constant Integer_4_Unsigned_C := 16#0B09#;
  GL_CURRENT_RASTER_INDEX                : constant Integer_4_Unsigned_C := 16#0B05#;
  GL_CURRENT_RASTER_POSITION             : constant Integer_4_Unsigned_C := 16#0B07#;
  GL_CURRENT_RASTER_TEXTURE_COORDS       : constant Integer_4_Unsigned_C := 16#0B06#;
  GL_CURRENT_RASTER_POSITION_VALID       : constant Integer_4_Unsigned_C := 16#0B08#;
  GL_CURRENT_TEXTURE_COORDS              : constant Integer_4_Unsigned_C := 16#0B03#;
  GL_INDEX_CLEAR_VALUE                   : constant Integer_4_Unsigned_C := 16#0C20#;
  GL_INDEX_MODE                          : constant Integer_4_Unsigned_C := 16#0C30#;
  GL_INDEX_WRITEMASK                     : constant Integer_4_Unsigned_C := 16#0C21#;
  GL_MODELVIEW_MATRIX                    : constant Integer_4_Unsigned_C := 16#0BA6#;
  GL_MODELVIEW_STACK_DEPTH               : constant Integer_4_Unsigned_C := 16#0BA3#;
  GL_NAME_STACK_DEPTH                    : constant Integer_4_Unsigned_C := 16#0D70#;
  GL_PROJECTION_MATRIX                   : constant Integer_4_Unsigned_C := 16#0BA7#;
  GL_PROJECTION_STACK_DEPTH              : constant Integer_4_Unsigned_C := 16#0BA4#;
  GL_RENDER_MODE                         : constant Integer_4_Unsigned_C := 16#0C40#;
  GL_RGBA_MODE                           : constant Integer_4_Unsigned_C := 16#0C31#;
  GL_TEXTURE_MATRIX                      : constant Integer_4_Unsigned_C := 16#0BA8#;
  GL_TEXTURE_STACK_DEPTH                 : constant Integer_4_Unsigned_C := 16#0BA5#;
  GL_VIEWPORT                            : constant Integer_4_Unsigned_C := 16#0BA2#;
  GL_AUTO_NORMAL                         : constant Integer_4_Unsigned_C := 16#0D80#; --  Evaluators
  GL_MAP1_COLOR_4                        : constant Integer_4_Unsigned_C := 16#0D90#;
  GL_MAP1_INDEX                          : constant Integer_4_Unsigned_C := 16#0D91#;
  GL_MAP1_NORMAL                         : constant Integer_4_Unsigned_C := 16#0D92#;
  GL_MAP1_TEXTURE_COORD_1                : constant Integer_4_Unsigned_C := 16#0D93#;
  GL_MAP1_TEXTURE_COORD_2                : constant Integer_4_Unsigned_C := 16#0D94#;
  GL_MAP1_TEXTURE_COORD_3                : constant Integer_4_Unsigned_C := 16#0D95#;
  GL_MAP1_TEXTURE_COORD_4                : constant Integer_4_Unsigned_C := 16#0D96#;
  GL_MAP1_VERTEX_3                       : constant Integer_4_Unsigned_C := 16#0D97#;
  GL_MAP1_VERTEX_4                       : constant Integer_4_Unsigned_C := 16#0D98#;
  GL_MAP2_COLOR_4                        : constant Integer_4_Unsigned_C := 16#0DB0#;
  GL_MAP2_INDEX                          : constant Integer_4_Unsigned_C := 16#0DB1#;
  GL_MAP2_NORMAL                         : constant Integer_4_Unsigned_C := 16#0DB2#;
  GL_MAP2_TEXTURE_COORD_1                : constant Integer_4_Unsigned_C := 16#0DB3#;
  GL_MAP2_TEXTURE_COORD_2                : constant Integer_4_Unsigned_C := 16#0DB4#;
  GL_MAP2_TEXTURE_COORD_3                : constant Integer_4_Unsigned_C := 16#0DB5#;
  GL_MAP2_TEXTURE_COORD_4                : constant Integer_4_Unsigned_C := 16#0DB6#;
  GL_MAP2_VERTEX_3                       : constant Integer_4_Unsigned_C := 16#0DB7#;
  GL_MAP2_VERTEX_4                       : constant Integer_4_Unsigned_C := 16#0DB8#;
  GL_MAP1_GRID_DOMAIN                    : constant Integer_4_Unsigned_C := 16#0DD0#;
  GL_MAP1_GRID_SEGMENTS                  : constant Integer_4_Unsigned_C := 16#0DD1#;
  GL_MAP2_GRID_DOMAIN                    : constant Integer_4_Unsigned_C := 16#0DD2#;
  GL_MAP2_GRID_SEGMENTS                  : constant Integer_4_Unsigned_C := 16#0DD3#;
  GL_COEFF                               : constant Integer_4_Unsigned_C := 16#0A00#;
  GL_DOMAIN                              : constant Integer_4_Unsigned_C := 16#0A02#;
  GL_ORDER                               : constant Integer_4_Unsigned_C := 16#0A01#;
  GL_FOG_HINT                            : constant Integer_4_Unsigned_C := 16#0C54#; --  Hints
  GL_LINE_SMOOTH_HINT                    : constant Integer_4_Unsigned_C := 16#0C52#;
  GL_PERSPECTIVE_CORRECTION_HINT         : constant Integer_4_Unsigned_C := 16#0C50#;
  GL_POINT_SMOOTH_HINT                   : constant Integer_4_Unsigned_C := 16#0C51#;
  GL_POLYGON_SMOOTH_HINT                 : constant Integer_4_Unsigned_C := 16#0C53#;
  GL_DONT_CARE                           : constant Integer_4_Unsigned_C := 16#1100#;
  GL_FASTEST                             : constant Integer_4_Unsigned_C := 16#1101#;
  GL_NICEST                              : constant Integer_4_Unsigned_C := 16#1102#;
  GL_SCISSOR_TEST                        : constant Integer_4_Unsigned_C := 16#0C11#; --  Scissor box
  GL_SCISSOR_BOX                         : constant Integer_4_Unsigned_C := 16#0C10#;
  GL_MAP_COLOR                           : constant Integer_4_Unsigned_C := 16#0D10#; --  Pixel Mode / Transfer
  GL_MAP_STENCIL                         : constant Integer_4_Unsigned_C := 16#0D11#;
  GL_INDEX_SHIFT                         : constant Integer_4_Unsigned_C := 16#0D12#;
  GL_INDEX_OFFSET                        : constant Integer_4_Unsigned_C := 16#0D13#;
  GL_RED_SCALE                           : constant Integer_4_Unsigned_C := 16#0D14#;
  GL_RED_BIAS                            : constant Integer_4_Unsigned_C := 16#0D15#;
  GL_GREEN_SCALE                         : constant Integer_4_Unsigned_C := 16#0D18#;
  GL_GREEN_BIAS                          : constant Integer_4_Unsigned_C := 16#0D19#;
  GL_BLUE_SCALE                          : constant Integer_4_Unsigned_C := 16#0D1A#;
  GL_BLUE_BIAS                           : constant Integer_4_Unsigned_C := 16#0D1B#;
  GL_ALPHA_SCALE                         : constant Integer_4_Unsigned_C := 16#0D1C#;
  GL_ALPHA_BIAS                          : constant Integer_4_Unsigned_C := 16#0D1D#;
  GL_DEPTH_SCALE                         : constant Integer_4_Unsigned_C := 16#0D1E#;
  GL_DEPTH_BIAS                          : constant Integer_4_Unsigned_C := 16#0D1F#;
  GL_DECR_WRAP                           : constant Integer_4_Unsigned_C := 16#8508#;
  GL_INCR_WRAP                           : constant Integer_4_Unsigned_C := 16#8507#;
  GL_PIXEL_MAP_S_TO_S_SIZE               : constant Integer_4_Unsigned_C := 16#0CB1#;
  GL_PIXEL_MAP_I_TO_I_SIZE               : constant Integer_4_Unsigned_C := 16#0CB0#;
  GL_PIXEL_MAP_I_TO_R_SIZE               : constant Integer_4_Unsigned_C := 16#0CB2#;
  GL_PIXEL_MAP_I_TO_G_SIZE               : constant Integer_4_Unsigned_C := 16#0CB3#;
  GL_PIXEL_MAP_I_TO_B_SIZE               : constant Integer_4_Unsigned_C := 16#0CB4#;
  GL_PIXEL_MAP_I_TO_A_SIZE               : constant Integer_4_Unsigned_C := 16#0CB5#;
  GL_PIXEL_MAP_R_TO_R_SIZE               : constant Integer_4_Unsigned_C := 16#0CB6#;
  GL_PIXEL_MAP_G_TO_G_SIZE               : constant Integer_4_Unsigned_C := 16#0CB7#;
  GL_PIXEL_MAP_B_TO_B_SIZE               : constant Integer_4_Unsigned_C := 16#0CB8#;
  GL_PIXEL_MAP_A_TO_A_SIZE               : constant Integer_4_Unsigned_C := 16#0CB9#;
  GL_PIXEL_MAP_S_TO_S                    : constant Integer_4_Unsigned_C := 16#0C71#;
  GL_PIXEL_MAP_I_TO_I                    : constant Integer_4_Unsigned_C := 16#0C70#;
  GL_PIXEL_MAP_I_TO_R                    : constant Integer_4_Unsigned_C := 16#0C72#;
  GL_PIXEL_MAP_I_TO_G                    : constant Integer_4_Unsigned_C := 16#0C73#;
  GL_PIXEL_MAP_I_TO_B                    : constant Integer_4_Unsigned_C := 16#0C74#;
  GL_PIXEL_MAP_I_TO_A                    : constant Integer_4_Unsigned_C := 16#0C75#;
  GL_PIXEL_MAP_R_TO_R                    : constant Integer_4_Unsigned_C := 16#0C76#;
  GL_PIXEL_MAP_G_TO_G                    : constant Integer_4_Unsigned_C := 16#0C77#;
  GL_PIXEL_MAP_B_TO_B                    : constant Integer_4_Unsigned_C := 16#0C78#;
  GL_PIXEL_MAP_A_TO_A                    : constant Integer_4_Unsigned_C := 16#0C79#;
  GL_PACK_ALIGNMENT                      : constant Integer_4_Unsigned_C := 16#0D05#;
  GL_PACK_LSB_FIRST                      : constant Integer_4_Unsigned_C := 16#0D01#;
  GL_PACK_ROW_LENGTH                     : constant Integer_4_Unsigned_C := 16#0D02#;
  GL_PACK_SKIP_PIXELS                    : constant Integer_4_Unsigned_C := 16#0D04#;
  GL_PACK_SKIP_ROWS                      : constant Integer_4_Unsigned_C := 16#0D03#;
  GL_PACK_SWAP_BYTES                     : constant Integer_4_Unsigned_C := 16#0D00#;
  GL_UNPACK_ALIGNMENT                    : constant Integer_4_Unsigned_C := 16#0CF5#;
  GL_UNPACK_LSB_FIRST                    : constant Integer_4_Unsigned_C := 16#0CF1#;
  GL_UNPACK_ROW_LENGTH                   : constant Integer_4_Unsigned_C := 16#0CF2#;
  GL_UNPACK_SKIP_PIXELS                  : constant Integer_4_Unsigned_C := 16#0CF4#;
  GL_UNPACK_SKIP_ROWS                    : constant Integer_4_Unsigned_C := 16#0CF3#;
  GL_UNPACK_SWAP_BYTES                   : constant Integer_4_Unsigned_C := 16#0CF0#;
  GL_ZOOM_X                              : constant Integer_4_Unsigned_C := 16#0D16#;
  GL_ZOOM_Y                              : constant Integer_4_Unsigned_C := 16#0D17#;
  GL_TEXTURE_ENV                         : constant Integer_4_Unsigned_C := 16#2300#; --  Texture mapping
  GL_TEXTURE_ENV_MODE                    : constant Integer_4_Unsigned_C := 16#2200#;
  GL_TEXTURE_1D                          : constant Integer_4_Unsigned_C := 16#0DE0#;
  GL_TEXTURE_2D                          : constant Integer_4_Unsigned_C := 16#0DE1#;
  GL_TEXTURE_WRAP_S                      : constant Integer_4_Unsigned_C := 16#2802#;
  GL_TEXTURE_WRAP_T                      : constant Integer_4_Unsigned_C := 16#2803#;
  GL_TEXTURE_MAG_FILTER                  : constant Integer_4_Unsigned_C := 16#2800#;
  GL_TEXTURE_MIN_FILTER                  : constant Integer_4_Unsigned_C := 16#2801#;
  GL_TEXTURE_ENV_COLOR                   : constant Integer_4_Unsigned_C := 16#2201#;
  GL_TEXTURE_GEN_S                       : constant Integer_4_Unsigned_C := 16#0C60#;
  GL_TEXTURE_GEN_T                       : constant Integer_4_Unsigned_C := 16#0C61#;
  GL_TEXTURE_GEN_MODE                    : constant Integer_4_Unsigned_C := 16#2500#;
  GL_TEXTURE_BORDER_COLOR                : constant Integer_4_Unsigned_C := 16#1004#;
  GL_TEXTURE_WIDTH                       : constant Integer_4_Unsigned_C := 16#1000#;
  GL_TEXTURE_HEIGHT                      : constant Integer_4_Unsigned_C := 16#1001#;
  GL_TEXTURE_BORDER                      : constant Integer_4_Unsigned_C := 16#1005#;
  GL_TEXTURE_COMPONENTS                  : constant Integer_4_Unsigned_C := 16#1003#;
  GL_TEXTURE_RED_SIZE                    : constant Integer_4_Unsigned_C := 16#0000_805C#;
  GL_TEXTURE_GREEN_SIZE                  : constant Integer_4_Unsigned_C := 16#0000_805D#;
  GL_TEXTURE_BLUE_SIZE                   : constant Integer_4_Unsigned_C := 16#0000_805E#;
  GL_TEXTURE_ALPHA_SIZE                  : constant Integer_4_Unsigned_C := 16#0000_805F#;
  GL_TEXTURE_LUMINANCE_SIZE              : constant Integer_4_Unsigned_C := 16#0000_8060#;
  GL_TEXTURE_INTENSITY_SIZE              : constant Integer_4_Unsigned_C := 16#0000_8061#;
  GL_NEAREST_MIPMAP_NEAREST              : constant Integer_4_Unsigned_C := 16#2700#;
  GL_NEAREST_MIPMAP_LINEAR               : constant Integer_4_Unsigned_C := 16#2702#;
  GL_LINEAR_MIPMAP_NEAREST               : constant Integer_4_Unsigned_C := 16#2701#;
  GL_LINEAR_MIPMAP_LINEAR                : constant Integer_4_Unsigned_C := 16#2703#;
  GL_OBJECT_LINEAR                       : constant Integer_4_Unsigned_C := 16#2401#;
  GL_OBJECT_PLANE                        : constant Integer_4_Unsigned_C := 16#2501#;
  GL_EYE_LINEAR                          : constant Integer_4_Unsigned_C := 16#2400#;
  GL_EYE_PLANE                           : constant Integer_4_Unsigned_C := 16#2502#;
  GL_SPHERE_MAP                          : constant Integer_4_Unsigned_C := 16#2402#;
  GL_DECAL                               : constant Integer_4_Unsigned_C := 16#2101#;
  GL_MODULATE                            : constant Integer_4_Unsigned_C := 16#2100#;
  GL_NEAREST                             : constant Integer_4_Unsigned_C := 16#2600#;
  GL_REPEAT                              : constant Integer_4_Unsigned_C := 16#2901#;
  GL_CLAMP                               : constant Integer_4_Unsigned_C := 16#2900#;
  GL_S                                   : constant Integer_4_Unsigned_C := 16#2000#;
  GL_T                                   : constant Integer_4_Unsigned_C := 16#2001#;
  GL_R                                   : constant Integer_4_Unsigned_C := 16#2002#;
  GL_Q                                   : constant Integer_4_Unsigned_C := 16#2003#;
  GL_TEXTURE_GEN_R                       : constant Integer_4_Unsigned_C := 16#0C62#;
  GL_TEXTURE_GEN_Q                       : constant Integer_4_Unsigned_C := 16#0C63#;
  GL_VENDOR                              : constant Integer_4_Unsigned_C := 16#1F00#; --  Utility
  GL_RENDERER                            : constant Integer_4_Unsigned_C := 16#1F01#;
  GL_VERSION                             : constant Integer_4_Unsigned_C := 16#1F02#;
  GL_NUM_EXTENSIONS                      : constant Integer_4_Unsigned_C := 16#821D#;
  GL_EXTENSIONS                          : constant Integer_4_Unsigned_C := 16#1F03#;
  GL_NO_ERROR                            : constant Integer_4_Unsigned_C := 16#0000#; --  Errors
  GL_INVALID_VALUE                       : constant Integer_4_Unsigned_C := 16#0501#;
  GL_INVALID_ENUM                        : constant Integer_4_Unsigned_C := 16#0500#;
  GL_INVALID_OPERATION                   : constant Integer_4_Unsigned_C := 16#0502#;
  GL_STACK_OVERFLOW                      : constant Integer_4_Unsigned_C := 16#0503#;
  GL_STACK_UNDERFLOW                     : constant Integer_4_Unsigned_C := 16#0504#;
  GL_OUT_OF_MEMORY                       : constant Integer_4_Unsigned_C := 16#0505#;
  GL_CURRENT_BIT                         : constant Integer_4_Unsigned_C := 16#0001#; --  glPushAttrib/glPopAttrib bits
  GL_POINT_BIT                           : constant Integer_4_Unsigned_C := 16#0002#;
  GL_LINE_BIT                            : constant Integer_4_Unsigned_C := 16#0004#;
  GL_POLYGON_BIT                         : constant Integer_4_Unsigned_C := 16#0008#;
  GL_POLYGON_STIPPLE_BIT                 : constant Integer_4_Unsigned_C := 16#0010#;
  GL_PIXEL_MODE_BIT                      : constant Integer_4_Unsigned_C := 16#0020#;
  GL_LIGHTING_BIT                        : constant Integer_4_Unsigned_C := 16#0040#;
  GL_FOG_BIT                             : constant Integer_4_Unsigned_C := 16#0080#;
  GL_DEPTH_BUFFER_BIT                    : constant Integer_4_Unsigned_C := 16#0100#;
  GL_ACCUM_BUFFER_BIT                    : constant Integer_4_Unsigned_C := 16#0200#;
  GL_STENCIL_BUFFER_BIT                  : constant Integer_4_Unsigned_C := 16#0400#;
  GL_VIEWPORT_BIT                        : constant Integer_4_Unsigned_C := 16#0800#;
  GL_TRANSFORM_BIT                       : constant Integer_4_Unsigned_C := 16#1000#;
  GL_ENABLE_BIT                          : constant Integer_4_Unsigned_C := 16#2000#;
  GL_COLOR_BUFFER_BIT                    : constant Integer_4_Unsigned_C := 16#4000#;
  GL_HINT_BIT                            : constant Integer_4_Unsigned_C := 16#8000#;
  GL_EVAL_BIT                            : constant Integer_4_Unsigned_C := 16#0001_0000#;
  GL_LIST_BIT                            : constant Integer_4_Unsigned_C := 16#0002_0000#;
  GL_TEXTURE_BIT                         : constant Integer_4_Unsigned_C := 16#0004_0000#;
  GL_SCISSOR_BIT                         : constant Integer_4_Unsigned_C := 16#0008_0000#;
  GL_ALL_ATTRIB_BITS                     : constant Integer_4_Unsigned_C := 16#000F_FFFF#;
  GL_PROXY_TEXTURE_1D                    : constant Integer_4_Unsigned_C := 16#0000_8063#; --  OpenGL 1.1
  GL_PROXY_TEXTURE_2D                    : constant Integer_4_Unsigned_C := 16#0000_8064#;
  GL_TEXTURE_PRIORITY                    : constant Integer_4_Unsigned_C := 16#0000_8066#;
  GL_TEXTURE_RESIDENT                    : constant Integer_4_Unsigned_C := 16#0000_8067#;
  GL_TEXTURE_BINDING_1D                  : constant Integer_4_Unsigned_C := 16#0000_8068#;
  GL_TEXTURE_BINDING_2D                  : constant Integer_4_Unsigned_C := 16#0000_8069#;
  GL_TEXTURE_INTERNAL_FORMAT             : constant Integer_4_Unsigned_C := 16#1003#;
  GL_ALPHA4                              : constant Integer_4_Unsigned_C := 16#0000_803B#;
  GL_ALPHA8                              : constant Integer_4_Unsigned_C := 16#0000_803C#;
  GL_ALPHA12                             : constant Integer_4_Unsigned_C := 16#0000_803D#;
  GL_ALPHA16                             : constant Integer_4_Unsigned_C := 16#0000_803E#;
  GL_LUMINANCE4                          : constant Integer_4_Unsigned_C := 16#0000_803F#;
  GL_LUMINANCE8                          : constant Integer_4_Unsigned_C := 16#0000_8040#;
  GL_LUMINANCE12                         : constant Integer_4_Unsigned_C := 16#0000_8041#;
  GL_LUMINANCE16                         : constant Integer_4_Unsigned_C := 16#0000_8042#;
  GL_LUMINANCE4_ALPHA4                   : constant Integer_4_Unsigned_C := 16#0000_8043#;
  GL_LUMINANCE6_ALPHA2                   : constant Integer_4_Unsigned_C := 16#0000_8044#;
  GL_LUMINANCE8_ALPHA8                   : constant Integer_4_Unsigned_C := 16#0000_8045#;
  GL_LUMINANCE12_ALPHA4                  : constant Integer_4_Unsigned_C := 16#0000_8046#;
  GL_LUMINANCE12_ALPHA12                 : constant Integer_4_Unsigned_C := 16#0000_8047#;
  GL_LUMINANCE16_ALPHA16                 : constant Integer_4_Unsigned_C := 16#0000_8048#;
  GL_INTENSITY                           : constant Integer_4_Unsigned_C := 16#0000_8049#;
  GL_INTENSITY4                          : constant Integer_4_Unsigned_C := 16#0000_804A#;
  GL_INTENSITY8                          : constant Integer_4_Unsigned_C := 16#0000_804B#;
  GL_INTENSITY12                         : constant Integer_4_Unsigned_C := 16#0000_804C#;
  GL_INTENSITY16                         : constant Integer_4_Unsigned_C := 16#0000_804D#;
  GL_R3_G3_B2                            : constant Integer_4_Unsigned_C := 16#2A10#;
  GL_RGB4                                : constant Integer_4_Unsigned_C := 16#0000_804F#;
  GL_RGB5                                : constant Integer_4_Unsigned_C := 16#0000_8050#;
  GL_RGB8                                : constant Integer_4_Unsigned_C := 16#0000_8051#;
  GL_RGB10                               : constant Integer_4_Unsigned_C := 16#0000_8052#;
  GL_RGB12                               : constant Integer_4_Unsigned_C := 16#0000_8053#;
  GL_RGB16                               : constant Integer_4_Unsigned_C := 16#0000_8054#;
  GL_RGBA2                               : constant Integer_4_Unsigned_C := 16#0000_8055#;
  GL_RGBA4                               : constant Integer_4_Unsigned_C := 16#0000_8056#;
  GL_RGB5_A1                             : constant Integer_4_Unsigned_C := 16#0000_8057#;
  GL_RGBA8                               : constant Integer_4_Unsigned_C := 16#0000_8058#;
  GL_RGB10_A2                            : constant Integer_4_Unsigned_C := 16#0000_8059#;
  GL_RGBA12                              : constant Integer_4_Unsigned_C := 16#0000_805A#;
  GL_RGBA16                              : constant Integer_4_Unsigned_C := 16#0000_805B#;
  GL_CLIENT_PIXEL_STORE_BIT              : constant Integer_4_Unsigned_C := 16#0001#;
  GL_CLIENT_VERTEX_ARRAY_BIT             : constant Integer_4_Unsigned_C := 16#0002#;
  GL_ALL_CLIENT_ATTRIB_BITS              : constant Integer_4_Unsigned_C := 16#FFFF_FFFF#;
  GL_CLIENT_ALL_ATTRIB_BITS              : constant Integer_4_Unsigned_C := 16#FFFF_FFFF#;
  GL_RESCALE_NORMAL                      : constant Integer_4_Unsigned_C := 16#0000_803A#; --  OpenGL 1.2
  GL_CLAMP_TO_EDGE                       : constant Integer_4_Unsigned_C := 16#0000_812F#;
  GL_MAX_ELEMENTS_VERTICES               : constant Integer_4_Unsigned_C := 16#0000_80E8#;
  GL_MAX_ELEMENTS_INDICES                : constant Integer_4_Unsigned_C := 16#0000_80E9#;
  GL_BGR                                 : constant Integer_4_Unsigned_C := 16#0000_80E0#;
  GL_BGRA                                : constant Integer_4_Unsigned_C := 16#0000_80E1#;
  GL_UNSIGNED_BYTE_3_3_2                 : constant Integer_4_Unsigned_C := 16#0000_8032#;
  GL_UNSIGNED_BYTE_2_3_3_REV             : constant Integer_4_Unsigned_C := 16#0000_8362#;
  GL_UNSIGNED_SHORT_5_6_5                : constant Integer_4_Unsigned_C := 16#0000_8363#;
  GL_UNSIGNED_SHORT_5_6_5_REV            : constant Integer_4_Unsigned_C := 16#0000_8364#;
  GL_UNSIGNED_SHORT_4_4_4_4              : constant Integer_4_Unsigned_C := 16#0000_8033#;
  GL_UNSIGNED_SHORT_4_4_4_4_REV          : constant Integer_4_Unsigned_C := 16#0000_8365#;
  GL_UNSIGNED_SHORT_5_5_5_1              : constant Integer_4_Unsigned_C := 16#0000_8034#;
  GL_UNSIGNED_SHORT_1_5_5_5_REV          : constant Integer_4_Unsigned_C := 16#0000_8366#;
  GL_UNSIGNED_INT_8_8_8_8                : constant Integer_4_Unsigned_C := 16#0000_8035#;
  GL_UNSIGNED_INT_8_8_8_8_REV            : constant Integer_4_Unsigned_C := 16#0000_8367#;
  GL_UNSIGNED_INT_10_10_10_2             : constant Integer_4_Unsigned_C := 16#0000_8036#;
  GL_UNSIGNED_INT_2_10_10_10_REV         : constant Integer_4_Unsigned_C := 16#0000_8368#;
  GL_LIGHT_MODEL_COLOR_CONTROL           : constant Integer_4_Unsigned_C := 16#0000_81F8#;
  GL_SINGLE_COLOR                        : constant Integer_4_Unsigned_C := 16#0000_81F9#;
  GL_SEPARATE_SPECULAR_COLOR             : constant Integer_4_Unsigned_C := 16#0000_81FA#;
  GL_TEXTURE_MIN_LOD                     : constant Integer_4_Unsigned_C := 16#0000_813A#;
  GL_TEXTURE_MAX_LOD                     : constant Integer_4_Unsigned_C := 16#0000_813B#;
  GL_TEXTURE_BASE_LEVEL                  : constant Integer_4_Unsigned_C := 16#0000_813C#;
  GL_TEXTURE_MAX_LEVEL                   : constant Integer_4_Unsigned_C := 16#0000_813D#;
  GL_SMOOTH_POINT_SIZE_RANGE             : constant Integer_4_Unsigned_C := 16#0B12#;
  GL_SMOOTH_POINT_SIZE_GRANULARITY       : constant Integer_4_Unsigned_C := 16#0B13#;
  GL_SMOOTH_LINE_WIDTH_RANGE             : constant Integer_4_Unsigned_C := 16#0B22#;
  GL_SMOOTH_LINE_WIDTH_GRANULARITY       : constant Integer_4_Unsigned_C := 16#0B23#;
  GL_ALIASED_POINT_SIZE_RANGE            : constant Integer_4_Unsigned_C := 16#0000_846D#;
  GL_ALIASED_LINE_WIDTH_RANGE            : constant Integer_4_Unsigned_C := 16#0000_846E#;
  GL_PACK_SKIP_IMAGES                    : constant Integer_4_Unsigned_C := 16#0000_806B#;
  GL_PACK_IMAGE_HEIGHT                   : constant Integer_4_Unsigned_C := 16#0000_806C#;
  GL_UNPACK_SKIP_IMAGES                  : constant Integer_4_Unsigned_C := 16#0000_806D#;
  GL_UNPACK_IMAGE_HEIGHT                 : constant Integer_4_Unsigned_C := 16#0000_806E#;
  GL_TEXTURE_3D                          : constant Integer_4_Unsigned_C := 16#0000_806F#;
  GL_PROXY_TEXTURE_3D                    : constant Integer_4_Unsigned_C := 16#0000_8070#;
  GL_TEXTURE_DEPTH                       : constant Integer_4_Unsigned_C := 16#0000_8071#;
  GL_TEXTURE_WRAP_R                      : constant Integer_4_Unsigned_C := 16#0000_8072#;
  GL_MAX_3D_TEXTURE_SIZE                 : constant Integer_4_Unsigned_C := 16#0000_8073#;
  GL_TEXTURE_BINDING_3D                  : constant Integer_4_Unsigned_C := 16#0000_806A#;
  GL_CONSTANT_COLOR                      : constant Integer_4_Unsigned_C := 16#0000_8001#; -- GL_ARB_imaging
  GL_ONE_MINUS_CONSTANT_COLOR            : constant Integer_4_Unsigned_C := 16#0000_8002#;
  GL_CONSTANT_ALPHA                      : constant Integer_4_Unsigned_C := 16#0000_8003#;
  GL_ONE_MINUS_CONSTANT_ALPHA            : constant Integer_4_Unsigned_C := 16#0000_8004#;
  GL_COLOR_TABLE                         : constant Integer_4_Unsigned_C := 16#0000_80D0#;
  GL_POST_CONVOLUTION_COLOR_TABLE        : constant Integer_4_Unsigned_C := 16#0000_80D1#;
  GL_POST_COLOR_MATRIX_COLOR_TABLE       : constant Integer_4_Unsigned_C := 16#0000_80D2#;
  GL_PROXY_COLOR_TABLE                   : constant Integer_4_Unsigned_C := 16#0000_80D3#;
  GL_PROXY_POST_CONVOLUTION_COLOR_TABLE  : constant Integer_4_Unsigned_C := 16#0000_80D4#;
  GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE : constant Integer_4_Unsigned_C := 16#0000_80D5#;
  GL_COLOR_TABLE_SCALE                   : constant Integer_4_Unsigned_C := 16#0000_80D6#;
  GL_COLOR_TABLE_BIAS                    : constant Integer_4_Unsigned_C := 16#0000_80D7#;
  GL_COLOR_TABLE_FORMAT                  : constant Integer_4_Unsigned_C := 16#0000_80D8#;
  GL_COLOR_TABLE_WIDTH                   : constant Integer_4_Unsigned_C := 16#0000_80D9#;
  GL_COLOR_TABLE_RED_SIZE                : constant Integer_4_Unsigned_C := 16#0000_80DA#;
  GL_COLOR_TABLE_GREEN_SIZE              : constant Integer_4_Unsigned_C := 16#0000_80DB#;
  GL_COLOR_TABLE_BLUE_SIZE               : constant Integer_4_Unsigned_C := 16#0000_80DC#;
  GL_COLOR_TABLE_ALPHA_SIZE              : constant Integer_4_Unsigned_C := 16#0000_80DD#;
  GL_COLOR_TABLE_LUMINANCE_SIZE          : constant Integer_4_Unsigned_C := 16#0000_80DE#;
  GL_COLOR_TABLE_INTENSITY_SIZE          : constant Integer_4_Unsigned_C := 16#0000_80DF#;
  GL_CONVOLUTION_1D                      : constant Integer_4_Unsigned_C := 16#0000_8010#;
  GL_CONVOLUTION_2D                      : constant Integer_4_Unsigned_C := 16#0000_8011#;
  GL_SEPARABLE_2D                        : constant Integer_4_Unsigned_C := 16#0000_8012#;
  GL_CONVOLUTION_BORDER_MODE             : constant Integer_4_Unsigned_C := 16#0000_8013#;
  GL_CONVOLUTION_FILTER_SCALE            : constant Integer_4_Unsigned_C := 16#0000_8014#;
  GL_CONVOLUTION_FILTER_BIAS             : constant Integer_4_Unsigned_C := 16#0000_8015#;
  GL_REDUCE                              : constant Integer_4_Unsigned_C := 16#0000_8016#;
  GL_CONVOLUTION_FORMAT                  : constant Integer_4_Unsigned_C := 16#0000_8017#;
  GL_CONVOLUTION_WIDTH                   : constant Integer_4_Unsigned_C := 16#0000_8018#;
  GL_CONVOLUTION_HEIGHT                  : constant Integer_4_Unsigned_C := 16#0000_8019#;
  GL_MAX_CONVOLUTION_WIDTH               : constant Integer_4_Unsigned_C := 16#0000_801A#;
  GL_MAX_CONVOLUTION_HEIGHT              : constant Integer_4_Unsigned_C := 16#0000_801B#;
  GL_POST_CONVOLUTION_RED_SCALE          : constant Integer_4_Unsigned_C := 16#0000_801C#;
  GL_POST_CONVOLUTION_GREEN_SCALE        : constant Integer_4_Unsigned_C := 16#0000_801D#;
  GL_POST_CONVOLUTION_BLUE_SCALE         : constant Integer_4_Unsigned_C := 16#0000_801E#;
  GL_POST_CONVOLUTION_ALPHA_SCALE        : constant Integer_4_Unsigned_C := 16#0000_801F#;
  GL_POST_CONVOLUTION_RED_BIAS           : constant Integer_4_Unsigned_C := 16#0000_8020#;
  GL_POST_CONVOLUTION_GREEN_BIAS         : constant Integer_4_Unsigned_C := 16#0000_8021#;
  GL_POST_CONVOLUTION_BLUE_BIAS          : constant Integer_4_Unsigned_C := 16#0000_8022#;
  GL_POST_CONVOLUTION_ALPHA_BIAS         : constant Integer_4_Unsigned_C := 16#0000_8023#;
  GL_CONSTANT_BORDER                     : constant Integer_4_Unsigned_C := 16#0000_8151#;
  GL_REPLICATE_BORDER                    : constant Integer_4_Unsigned_C := 16#0000_8153#;
  GL_CONVOLUTION_BORDER_COLOR            : constant Integer_4_Unsigned_C := 16#0000_8154#;
  GL_COLOR_MATRIX                        : constant Integer_4_Unsigned_C := 16#0000_80B1#;
  GL_COLOR_MATRIX_STACK_DEPTH            : constant Integer_4_Unsigned_C := 16#0000_80B2#;
  GL_MAX_COLOR_MATRIX_STACK_DEPTH        : constant Integer_4_Unsigned_C := 16#0000_80B3#;
  GL_POST_COLOR_MATRIX_RED_SCALE         : constant Integer_4_Unsigned_C := 16#0000_80B4#;
  GL_POST_COLOR_MATRIX_GREEN_SCALE       : constant Integer_4_Unsigned_C := 16#0000_80B5#;
  GL_POST_COLOR_MATRIX_BLUE_SCALE        : constant Integer_4_Unsigned_C := 16#0000_80B6#;
  GL_POST_COLOR_MATRIX_ALPHA_SCALE       : constant Integer_4_Unsigned_C := 16#0000_80B7#;
  GL_POST_COLOR_MATRIX_RED_BIAS          : constant Integer_4_Unsigned_C := 16#0000_80B8#;
  GL_POST_COLOR_MATRIX_GREEN_BIAS        : constant Integer_4_Unsigned_C := 16#0000_80B9#;
  GL_POST_COLOR_MATRIX_BLUE_BIAS         : constant Integer_4_Unsigned_C := 16#0000_80BA#;
  GL_POST_COLOR_MATRIX_ALPHA_BIAS        : constant Integer_4_Unsigned_C := 16#0000_80BB#;
  GL_HISTOGRAM                           : constant Integer_4_Unsigned_C := 16#0000_8024#;
  GL_PROXY_HISTOGRAM                     : constant Integer_4_Unsigned_C := 16#0000_8025#;
  GL_HISTOGRAM_WIDTH                     : constant Integer_4_Unsigned_C := 16#0000_8026#;
  GL_HISTOGRAM_FORMAT                    : constant Integer_4_Unsigned_C := 16#0000_8027#;
  GL_HISTOGRAM_RED_SIZE                  : constant Integer_4_Unsigned_C := 16#0000_8028#;
  GL_HISTOGRAM_GREEN_SIZE                : constant Integer_4_Unsigned_C := 16#0000_8029#;
  GL_HISTOGRAM_BLUE_SIZE                 : constant Integer_4_Unsigned_C := 16#0000_802A#;
  GL_HISTOGRAM_ALPHA_SIZE                : constant Integer_4_Unsigned_C := 16#0000_802B#;
  GL_HISTOGRAM_LUMINANCE_SIZE            : constant Integer_4_Unsigned_C := 16#0000_802C#;
  GL_HISTOGRAM_SINK                      : constant Integer_4_Unsigned_C := 16#0000_802D#;
  GL_MINMAX                              : constant Integer_4_Unsigned_C := 16#0000_802E#;
  GL_MINMAX_FORMAT                       : constant Integer_4_Unsigned_C := 16#0000_802F#;
  GL_MINMAX_SINK                         : constant Integer_4_Unsigned_C := 16#0000_8030#;
  GL_TABLE_TOO_LARGE                     : constant Integer_4_Unsigned_C := 16#0000_8031#;
  GL_BLEND_EQUATION                      : constant Integer_4_Unsigned_C := 16#0000_8009#;
  GL_MIN                                 : constant Integer_4_Unsigned_C := 16#0000_8007#;
  GL_MAX                                 : constant Integer_4_Unsigned_C := 16#0000_8008#;
  GL_FUNC_ADD                            : constant Integer_4_Unsigned_C := 16#0000_8006#;
  GL_FUNC_SUBTRACT                       : constant Integer_4_Unsigned_C := 16#0000_800A#;
  GL_FUNC_REVERSE_SUBTRACT               : constant Integer_4_Unsigned_C := 16#0000_800B#;
  GL_BLEND_COLOR                         : constant Integer_4_Unsigned_C := 16#0000_8005#;
  GL_TEXTURE0                            : constant Integer_4_Unsigned_C := 16#0000_84C0#; --  OpenGL 1.3 multitexture
  GL_TEXTURE1                            : constant Integer_4_Unsigned_C := 16#0000_84C1#;
  GL_TEXTURE2                            : constant Integer_4_Unsigned_C := 16#0000_84C2#;
  GL_TEXTURE3                            : constant Integer_4_Unsigned_C := 16#0000_84C3#;
  GL_TEXTURE4                            : constant Integer_4_Unsigned_C := 16#0000_84C4#;
  GL_TEXTURE5                            : constant Integer_4_Unsigned_C := 16#0000_84C5#;
  GL_TEXTURE6                            : constant Integer_4_Unsigned_C := 16#0000_84C6#;
  GL_TEXTURE7                            : constant Integer_4_Unsigned_C := 16#0000_84C7#;
  GL_TEXTURE8                            : constant Integer_4_Unsigned_C := 16#0000_84C8#;
  GL_TEXTURE9                            : constant Integer_4_Unsigned_C := 16#0000_84C9#;
  GL_TEXTURE10                           : constant Integer_4_Unsigned_C := 16#0000_84CA#;
  GL_TEXTURE11                           : constant Integer_4_Unsigned_C := 16#0000_84CB#;
  GL_TEXTURE12                           : constant Integer_4_Unsigned_C := 16#0000_84CC#;
  GL_TEXTURE13                           : constant Integer_4_Unsigned_C := 16#0000_84CD#;
  GL_TEXTURE14                           : constant Integer_4_Unsigned_C := 16#0000_84CE#;
  GL_TEXTURE15                           : constant Integer_4_Unsigned_C := 16#0000_84CF#;
  GL_TEXTURE16                           : constant Integer_4_Unsigned_C := 16#0000_84D0#;
  GL_TEXTURE17                           : constant Integer_4_Unsigned_C := 16#0000_84D1#;
  GL_TEXTURE18                           : constant Integer_4_Unsigned_C := 16#0000_84D2#;
  GL_TEXTURE19                           : constant Integer_4_Unsigned_C := 16#0000_84D3#;
  GL_TEXTURE20                           : constant Integer_4_Unsigned_C := 16#0000_84D4#;
  GL_TEXTURE21                           : constant Integer_4_Unsigned_C := 16#0000_84D5#;
  GL_TEXTURE22                           : constant Integer_4_Unsigned_C := 16#0000_84D6#;
  GL_TEXTURE23                           : constant Integer_4_Unsigned_C := 16#0000_84D7#;
  GL_TEXTURE24                           : constant Integer_4_Unsigned_C := 16#0000_84D8#;
  GL_TEXTURE25                           : constant Integer_4_Unsigned_C := 16#0000_84D9#;
  GL_TEXTURE26                           : constant Integer_4_Unsigned_C := 16#0000_84DA#;
  GL_TEXTURE27                           : constant Integer_4_Unsigned_C := 16#0000_84DB#;
  GL_TEXTURE28                           : constant Integer_4_Unsigned_C := 16#0000_84DC#;
  GL_TEXTURE29                           : constant Integer_4_Unsigned_C := 16#0000_84DD#;
  GL_TEXTURE30                           : constant Integer_4_Unsigned_C := 16#0000_84DE#;
  GL_TEXTURE31                           : constant Integer_4_Unsigned_C := 16#0000_84DF#;
  GL_ACTIVE_TEXTURE                      : constant Integer_4_Unsigned_C := 16#0000_84E0#;
  GL_CLIENT_ACTIVE_TEXTURE               : constant Integer_4_Unsigned_C := 16#0000_84E1#;
  GL_MAX_TEXTURE_UNITS                   : constant Integer_4_Unsigned_C := 16#0000_84E2#;
  GL_NORMAL_MAP                          : constant Integer_4_Unsigned_C := 16#0000_8511#; --  texture_cube_map
  GL_REFLECTION_MAP                      : constant Integer_4_Unsigned_C := 16#0000_8512#;
  GL_TEXTURE_CUBE_MAP                    : constant Integer_4_Unsigned_C := 16#0000_8513#;
  GL_TEXTURE_BINDING_CUBE_MAP            : constant Integer_4_Unsigned_C := 16#0000_8514#;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X         : constant Integer_4_Unsigned_C := 16#0000_8515#;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X         : constant Integer_4_Unsigned_C := 16#0000_8516#;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y         : constant Integer_4_Unsigned_C := 16#0000_8517#;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y         : constant Integer_4_Unsigned_C := 16#0000_8518#;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z         : constant Integer_4_Unsigned_C := 16#0000_8519#;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z         : constant Integer_4_Unsigned_C := 16#0000_851A#;
  GL_PROXY_TEXTURE_CUBE_MAP              : constant Integer_4_Unsigned_C := 16#0000_851B#;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE           : constant Integer_4_Unsigned_C := 16#0000_851C#;
  GL_COMPRESSED_ALPHA                    : constant Integer_4_Unsigned_C := 16#0000_84E9#; --  texture_compression
  GL_COMPRESSED_LUMINANCE                : constant Integer_4_Unsigned_C := 16#0000_84EA#;
  GL_COMPRESSED_LUMINANCE_ALPHA          : constant Integer_4_Unsigned_C := 16#0000_84EB#;
  GL_COMPRESSED_INTENSITY                : constant Integer_4_Unsigned_C := 16#0000_84EC#;
  GL_COMPRESSED_RGB                      : constant Integer_4_Unsigned_C := 16#0000_84ED#;
  GL_COMPRESSED_RGBA                     : constant Integer_4_Unsigned_C := 16#0000_84EE#;
  GL_TEXTURE_COMPRESSION_HINT            : constant Integer_4_Unsigned_C := 16#0000_84EF#;
  GL_TEXTURE_COMPRESSED_IMAGE_SIZE       : constant Integer_4_Unsigned_C := 16#0000_86A0#;
  GL_TEXTURE_COMPRESSED                  : constant Integer_4_Unsigned_C := 16#0000_86A1#;
  GL_NUM_COMPRESSED_TEXTURE_FORMATS      : constant Integer_4_Unsigned_C := 16#0000_86A2#;
  GL_COMPRESSED_TEXTURE_FORMATS          : constant Integer_4_Unsigned_C := 16#0000_86A3#;
  GL_MULTISAMPLE                         : constant Integer_4_Unsigned_C := 16#0000_809D#; --  multisample
  GL_SAMPLE_ALPHA_TO_COVERAGE            : constant Integer_4_Unsigned_C := 16#0000_809E#;
  GL_SAMPLE_ALPHA_TO_ONE                 : constant Integer_4_Unsigned_C := 16#0000_809F#;
  GL_SAMPLE_COVERAGE                     : constant Integer_4_Unsigned_C := 16#0000_80A0#;
  GL_SAMPLE_BUFFERS                      : constant Integer_4_Unsigned_C := 16#0000_80A8#;
  GL_SAMPLES                             : constant Integer_4_Unsigned_C := 16#0000_80A9#;
  GL_SAMPLE_COVERAGE_VALUE               : constant Integer_4_Unsigned_C := 16#0000_80AA#;
  GL_SAMPLE_COVERAGE_INVERT              : constant Integer_4_Unsigned_C := 16#0000_80AB#;
  GL_MULTISAMPLE_BIT                     : constant Integer_4_Unsigned_C := 16#2000_0000#;
  GL_TRANSPOSE_MODELVIEW_MATRIX          : constant Integer_4_Unsigned_C := 16#0000_84E3#; --  transpose_matrix
  GL_TRANSPOSE_PROJECTION_MATRIX         : constant Integer_4_Unsigned_C := 16#0000_84E4#;
  GL_TRANSPOSE_TEXTURE_MATRIX            : constant Integer_4_Unsigned_C := 16#0000_84E5#;
  GL_TRANSPOSE_COLOR_MATRIX              : constant Integer_4_Unsigned_C := 16#0000_84E6#;
  GL_COMBINE                             : constant Integer_4_Unsigned_C := 16#0000_8570#; --  texture_env_combine
  GL_COMBINE_RGB                         : constant Integer_4_Unsigned_C := 16#0000_8571#;
  GL_COMBINE_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_8572#;
  GL_SOURCE0_RGB                         : constant Integer_4_Unsigned_C := 16#0000_8580#;
  GL_SOURCE1_RGB                         : constant Integer_4_Unsigned_C := 16#0000_8581#;
  GL_SOURCE2_RGB                         : constant Integer_4_Unsigned_C := 16#0000_8582#;
  GL_SOURCE0_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_8588#;
  GL_SOURCE1_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_8589#;
  GL_SOURCE2_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_858A#;
  GL_OPERAND0_RGB                        : constant Integer_4_Unsigned_C := 16#0000_8590#;
  GL_OPERAND1_RGB                        : constant Integer_4_Unsigned_C := 16#0000_8591#;
  GL_OPERAND2_RGB                        : constant Integer_4_Unsigned_C := 16#0000_8592#;
  GL_OPERAND0_ALPHA                      : constant Integer_4_Unsigned_C := 16#0000_8598#;
  GL_OPERAND1_ALPHA                      : constant Integer_4_Unsigned_C := 16#0000_8599#;
  GL_OPERAND2_ALPHA                      : constant Integer_4_Unsigned_C := 16#0000_859A#;
  GL_RGB_SCALE                           : constant Integer_4_Unsigned_C := 16#0000_8573#;
  GL_ADD_SIGNED                          : constant Integer_4_Unsigned_C := 16#0000_8574#;
  GL_INTERPOLATE                         : constant Integer_4_Unsigned_C := 16#0000_8575#;
  GL_SUBTRACT                            : constant Integer_4_Unsigned_C := 16#0000_84E7#;
  GL_CONSTANT                            : constant Integer_4_Unsigned_C := 16#0000_8576#;
  GL_PRIMARY_COLOR                       : constant Integer_4_Unsigned_C := 16#0000_8577#;
  GL_PREVIOUS                            : constant Integer_4_Unsigned_C := 16#0000_8578#;
  GL_DOT3_RGB                            : constant Integer_4_Unsigned_C := 16#0000_86AE#; --  texture_env_dot3
  GL_DOT3_RGBA                           : constant Integer_4_Unsigned_C := 16#0000_86AF#;
  GL_CLAMP_TO_BORDER                     : constant Integer_4_Unsigned_C := 16#0000_812D#; --  texture_border_clamp
  GL_ARB_MULTITEXTURE                    : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_ARB_multitexture  (ARB extension 1 and OpenGL 1.2.1)
  GL_TEXTURE0_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C0#;
  GL_TEXTURE1_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C1#;
  GL_TEXTURE2_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C2#;
  GL_TEXTURE3_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C3#;
  GL_TEXTURE4_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C4#;
  GL_TEXTURE5_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C5#;
  GL_TEXTURE6_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C6#;
  GL_TEXTURE7_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C7#;
  GL_TEXTURE8_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C8#;
  GL_TEXTURE9_ARB                        : constant Integer_4_Unsigned_C := 16#0000_84C9#;
  GL_TEXTURE10_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CA#;
  GL_TEXTURE11_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CB#;
  GL_TEXTURE12_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CC#;
  GL_TEXTURE13_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CD#;
  GL_TEXTURE14_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CE#;
  GL_TEXTURE15_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84CF#;
  GL_TEXTURE16_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D0#;
  GL_TEXTURE17_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D1#;
  GL_TEXTURE18_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D2#;
  GL_TEXTURE19_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D3#;
  GL_TEXTURE20_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D4#;
  GL_TEXTURE21_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D5#;
  GL_TEXTURE22_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D6#;
  GL_TEXTURE23_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D7#;
  GL_TEXTURE24_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D8#;
  GL_TEXTURE25_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84D9#;
  GL_TEXTURE26_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DA#;
  GL_TEXTURE27_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DB#;
  GL_TEXTURE28_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DC#;
  GL_TEXTURE29_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DD#;
  GL_TEXTURE30_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DE#;
  GL_TEXTURE31_ARB                       : constant Integer_4_Unsigned_C := 16#0000_84DF#;
  GL_ACTIVE_TEXTURE_ARB                  : constant Integer_4_Unsigned_C := 16#0000_84E0#;
  GL_CLIENT_ACTIVE_TEXTURE_ARB           : constant Integer_4_Unsigned_C := 16#0000_84E1#;
  GL_MAX_TEXTURE_UNITS_ARB               : constant Integer_4_Unsigned_C := 16#0000_84E2#;
  GL_MESA_TRACE                          : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_MESA_trace
  GL_TRACE_ALL_BITS_MESA                 : constant Integer_4_Unsigned_C := 16#0000_FFFF#;
  GL_TRACE_OPERATIONS_BIT_MESA           : constant Integer_4_Unsigned_C := 16#0001#;
  GL_TRACE_PRIMITIVES_BIT_MESA           : constant Integer_4_Unsigned_C := 16#0002#;
  GL_TRACE_ARRAYS_BIT_MESA               : constant Integer_4_Unsigned_C := 16#0004#;
  GL_TRACE_TEXTURES_BIT_MESA             : constant Integer_4_Unsigned_C := 16#0008#;
  GL_TRACE_PIXELS_BIT_MESA               : constant Integer_4_Unsigned_C := 16#0010#;
  GL_TRACE_ERRORS_BIT_MESA               : constant Integer_4_Unsigned_C := 16#0020#;
  GL_TRACE_MASK_MESA                     : constant Integer_4_Unsigned_C := 16#0000_8755#;
  GL_TRACE_NAME_MESA                     : constant Integer_4_Unsigned_C := 16#0000_8756#;
  GL_MESA_PACKED_DEPTH_STENCIL           : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_MESA_packed_depth_stencil
  GL_DEPTH_STENCIL_MESA                  : constant Integer_4_Unsigned_C := 16#0000_8750#;
  GL_UNSIGNED_INT_24_8_MESA              : constant Integer_4_Unsigned_C := 16#0000_8751#;
  GL_UNSIGNED_INT_8_24_REV_MESA          : constant Integer_4_Unsigned_C := 16#0000_8752#;
  GL_UNSIGNED_SHORT_15_1_MESA            : constant Integer_4_Unsigned_C := 16#0000_8753#;
  GL_UNSIGNED_SHORT_1_15_REV_MESA        : constant Integer_4_Unsigned_C := 16#0000_8754#;
  GL_MESA_YCBCR_TEXTURE                  : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_MESA_texture_ycbcr
  GL_YCBCR_MESA                          : constant Integer_4_Unsigned_C := 16#0000_8757#;
  GL_UNSIGNED_SHORT_8_8_MESA             : constant Integer_4_Unsigned_C := 16#0000_85BA#;
  GL_UNSIGNED_SHORT_8_8_REV_MESA         : constant Integer_4_Unsigned_C := 16#0000_85BB#;
  GL_MESA_PACK_INVERT                    : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_MESA_pack_invert
  GL_PACK_INVERT_MESA                    : constant Integer_4_Unsigned_C := 16#0000_8758#;
  GL_DEPTH_BOUNDS_TEST_EXT               : constant Integer_4_Unsigned_C := 16#0000_8890#; -- GL_DEPTH_BOUNDS
  GL_DEPTH_BOUNDS_EXT                    : constant Integer_4_Unsigned_C := 16#0000_8891#;
  GL_ARB_OCCLUSION_QUERY                 : constant Integer_4_Unsigned_C := 16#0000_0001#; -- GL_ARB_occlusion_query
  GL_SAMPLES_PASSED_ARB                  : constant Integer_4_Unsigned_C := 16#0000_8914#; -- GL_ARB
  GL_QUERY_COUNTER_BITS_ARB              : constant Integer_4_Unsigned_C := 16#0000_8864#;
  GL_CURRENT_QUERY_ARB                   : constant Integer_4_Unsigned_C := 16#0000_8865#;
  GL_QUERY_RESULT_ARB                    : constant Integer_4_Unsigned_C := 16#0000_8866#;
  GL_QUERY_RESULT_AVAILABLE_ARB          : constant Integer_4_Unsigned_C := 16#0000_8867#;
  type Access_Active_Texture is access procedure( -- glActiveTextureARB
    Texture : in Integer_4_Unsigned_C);           -- GLenum texture
    pragma Convention(Stdcall, Access_Active_Texture);
  type Access_Client_Active_Texture is access procedure( -- glClientActiveTextureARB
    Texture : in Integer_4_Unsigned_C);                  -- GLenum texture
    pragma Convention(Stdcall, Access_Client_Active_Texture);
  type Access_Bind_Buffer is access procedure( -- glBindBufferARB
    Target : in Integer_4_Unsigned_C;          -- GLenum target
    Buffer : in Integer_4_Unsigned_C);         -- GLuint buffer
    pragma Convention(Stdcall, Access_Bind_Buffer);
  type Access_Bind_Buffer_Range is access procedure( -- glBindBufferRange
    Index  : in Integer_4_Unsigned;                  -- GLenum target
    Buffer : in Integer_4_Unsigned;                  -- GLuint buffer
    Offset : in Integer_Address;                     -- GLintptr offset
    Size   : in Integer_Address);                    -- GLsizeiptr size
    pragma Convention(Stdcall, Access_Bind_Buffer_Range);
  type Access_Delete_Buffers is access procedure( -- glDeleteBuffersARB
    Number  : in Integer_4_Signed_C;              -- GLsizei n
    Buffers : in Access_Integer_4_Unsigned_C);    -- const GLuint *buffers
    pragma Convention(Stdcall, Access_Delete_Buffers);
  type Access_Generate_Buffers is access procedure( -- glGenBuffersARB
    Number  : in Integer_4_Signed_C;                -- GLsizei n
    Buffers : in Access_Integer_4_Unsigned_C);      -- GLuint *buffers
    pragma Convention(Stdcall, Access_Generate_Buffers);
  type Access_Is_Buffer is access function( -- glIsBufferARB
    Buffer : in Integer_4_Unsigned_C)       -- GLuint buffer
    return Integer_1_Unsigned_C;            -- GLboolean
    pragma Convention(Stdcall, Access_Is_Buffer);
  type Access_Buffer_Data is access procedure( -- glBufferDataARB
    Target : in Integer_4_Unsigned_C;          -- GLenum target
    Size   : in Integer_4_Signed_C;            -- GLsizeiptrARB size
    Data   : in Address;                       -- const GLvoid *data
    Usage  : in Integer_4_Unsigned_C);         -- GLenum usage
    pragma Convention(Stdcall, Access_Buffer_Data);
  type Access_Buffer_Subdata is access procedure( -- glBufferSubDataARB
    Target : in Integer_4_Unsigned_C;             -- GLenum target
    Offset : in Integer_4_Signed_C;               -- GLintptrARB offset
    Size   : in Integer_4_Signed_C;               -- GLsizeiptrARB size
    Data   : in Address);                         -- const GLvoid *data
    pragma Convention(Stdcall, Access_Buffer_Subdata);
  type Access_Get_Buffer_Subdata is access procedure( -- glGetBufferSubDataARB
    Target : in Integer_4_Unsigned_C;                 -- GLenum target
    Offset : in Integer_4_Signed_C;                   -- GLintptrARB offset
    Size   : in Integer_4_Signed_C;                   -- GLsizeiptrARB size
    Data   : in Address);                             -- GLvoid *data
    pragma Convention(Stdcall, Access_Get_Buffer_Subdata);
  type Access_Map_Buffer is access function( -- glMapBufferARB
    Target : in Integer_4_Unsigned_C;        -- GLenum target
    Item   : in Integer_4_Unsigned_C)        -- GLenum access
    return Address;                          -- GLvoid*
    pragma Convention(Stdcall, Access_Map_Buffer);
  type Access_Unmap_Buffer is access function( -- glUnmapBufferARB
    Target : in Integer_4_Unsigned_C)          -- GLenum target
    return Integer_4_Signed_C;                 -- GLboolean
    pragma Convention(Stdcall, Access_Unmap_Buffer);
  type Access_Get_Buffer_Parameter is access procedure( -- glGetBufferParameterivARB
    Target     : in Integer_4_Unsigned_C;               -- GLenum target
    Name       : in Integer_4_Unsigned_C;               -- GLenum pname
    Parameters : in Access_Integer_4_Signed_C);         -- GLint *params
    pragma Convention(Stdcall, Access_Get_Buffer_Parameter);
  type Access_Get_Buffer_Pointer is access procedure( -- glGetBufferPointervARB
    Target     : in Integer_4_Unsigned_C; -- GLenum target
    Name       : in Integer_4_Unsigned_C; -- GLenum pname
    Parameters : in Address);             -- GLvoid* *params
    pragma Convention(Stdcall, Access_Get_Buffer_Pointer);
  type Access_Debug_Message_Control is access procedure( -- glDebugMessageControlARB
    Source   : in Integer_4_Unsigned_C;                  -- GLenum source
    Kind     : in Integer_4_Unsigned_C;                  -- GLenum type
    Severity : in Integer_4_Unsigned_C;                  -- GLenum severity
    Count    : in Integer_4_Signed_C;                    -- GLsizei count
    IDs      : in Access_Integer_4_Unsigned_C;           -- const GLuint * ids
    Enabled  : in Integer_4_Signed_C);                   -- GLboolean enabled
    pragma Convention(Stdcall, Access_Debug_Message_Control);
  type Access_Debug_Message_Insert is access procedure( -- glDebugMessageInsertARB
    Source   : in Integer_4_Unsigned_C;                 -- GLenum source
    Kind     : in Integer_4_Unsigned_C;                 -- GLenum type
    ID       : in Integer_4_Unsigned_C;                 -- GLuint id
    Severity : in Integer_4_Unsigned_C;                 -- GLenum severity
    Length   : in Integer_4_Signed_C;                   -- GLsizei length
    Buffer   : in Access_Integer_1_Unsigned_C);         -- const GLchar * buf
    pragma Convention(Stdcall, Access_Debug_Message_Insert);
  type Access_Debug_Procedure is access procedure(   -- glDebugProcARB
    Source         : in Integer_4_Unsigned_C;        -- GLenum source
    Kind           : in Integer_4_Unsigned_C;        -- GLenum type
    ID             : in Integer_4_Unsigned_C;        -- GLuint id
    Severity       : in Integer_4_Unsigned_C;        -- GLenum severity
    Length         : in Integer_4_Signed_C;          -- GLsizei length
    Message        : in Access_Integer_1_Unsigned_C; -- const GLchar* message
    User_Parameter : in Address);                    -- GLvoid* userParam
    pragma Convention(Stdcall, Access_Debug_Procedure);
  type Access_Debug_Message_Callback is access procedure( -- glDebugMessageCallbackARB
    Callback       : in Access_Debug_Procedure;           -- CARB callback
    User_Parameter : in Address);                         -- GLvoid * userParam
    pragma Convention(Stdcall, Access_Debug_Message_Callback);
  type Access_Get_Debug_Message_Log is access procedure( -- glGetDebugMessageLogARB
    Count       : in Integer_4_Unsigned_C;               -- GLuint count
    Log_Size    : in Integer_4_Signed_C;                 -- GLsizei logSize
    Sources     : in Access_Integer_4_Unsigned_C;        -- GLenum * sources
    Types       : in Access_Integer_4_Unsigned_C;        -- GLenum * types
    IDs         : in Access_Integer_4_Unsigned_C;        -- GLuint * ids
    Severities  : in Access_Integer_4_Unsigned_C;        -- GLenum * severities
    Lengths     : in Access_Integer_4_Signed_C;          -- GLsizei * lengths
    Message_Log : in Access_Integer_1_Unsigned_C);       -- GLchar * messageLog
    pragma Convention(Stdcall, Access_Get_Debug_Message_Log);
  type Access_Generate_Queries is access procedure( -- glGenQueriesARB
    Number : in Integer_4_Signed_C;                 -- GLsizei n
    IDs    : in Access_Integer_4_Unsigned_C);       -- GLuint *ids
    pragma Convention(Stdcall, Access_Generate_Queries);
  type Access_Delete_Quries is access procedure( -- glDeleteQueriesARB
    Number : in Integer_4_Signed_C;              -- GLsizei n
    IDs    : in Access_Integer_4_Unsigned_C);    -- const GLuint *ids
    pragma Convention(Stdcall, Access_Delete_Quries);
  type Access_Is_Query is access function( -- glIsQueryARB
    ID : in Integer_4_Unsigned_C)          -- GLuint id
    return Integer_4_Signed_C;             -- GLboolean
    pragma Convention(Stdcall, Access_Is_Query);
  type Access_Begin_Query is access procedure( -- glBeginQueryARB
    Target : in Integer_4_Unsigned_C;          -- GLenum target
    ID     : in Integer_4_Unsigned_C);         -- GLuint id
    pragma Convention(Stdcall, Access_Begin_Query);
  type Access_End_Query is access procedure( -- glEndQueryARB
    Target : in Integer_4_Unsigned_C);       -- GLenum target
    pragma Convention(Stdcall, Access_End_Query);
  type Access_Get_Query is access procedure(    -- glGetQueryivARB
    Target     : in Integer_4_Unsigned_C;       -- GLenum target
    Name       : in Integer_4_Unsigned_C;       -- GLenum pname
    Parameters : in Access_Integer_4_Signed_C); -- GLint *params
    pragma Convention(Stdcall, Access_Get_Query);
  type Access_Get_Query_Object is access procedure( -- glGetQueryObjectivARB
    ID         : in Integer_4_Unsigned_C;           -- GLuint id
    Name       : in Integer_4_Unsigned_C;           -- GLenum pname
    Parameters : in Access_Integer_4_Signed_C);     -- GLint *params
    pragma Convention(Stdcall, Access_Get_Query_Object);
  type Access_Get_Query_Object_Unsigned is access procedure( -- glGetQueryObjectuivARB
    ID         : in Integer_4_Unsigned_C;                    -- GLuint id
    Name       : in Integer_4_Unsigned_C;                    -- GLenum pname
    Parameters : in Access_Integer_4_Unsigned_C);            -- GLuint *params
    pragma Convention(Stdcall, Access_Get_Query_Object_Unsigned);
  type Access_Stencil_Operation_Separate is access procedure( -- glStencilOpSeparate
    Face         : in Integer_4_Unsigned_C;          -- GLenum face
    Stencil_Fail : in Integer_4_Unsigned_C;          -- GLenum sfail
    Depth_Fail   : in Integer_4_Unsigned_C;          -- GLenum dpfail
    Depth_Pass   : in Integer_4_Unsigned_C);         -- GLenum dppass
    pragma Convention(Stdcall, Access_Stencil_Operation_Separate);
  type Access_Stencil_Function_Separate is access procedure( -- glStencilFuncSeparate
    Front_Function : in Integer_4_Unsigned_C;       -- GLenum frontfunc
    Back_Function  : in Integer_4_Unsigned_C;       -- GLenum backfunc
    Reference      : in Integer_4_Signed_C;         -- GLint ref
    Mask           : in Integer_4_Unsigned_C);      -- GLuint mask
    pragma Convention(Stdcall, Access_Stencil_Function_Separate);
  type Access_Get_Uniform_From_Index is access function( -- glGetUniformBlockIndex
    Program            : in Integer_4_Unsigned_C;        -- GLuint program
    Uniform_Block_Name : in Access_Integer_1_Unsigned_C) -- const GLchar * uniformBlockName
    return Integer_4_Unsigned_C;                         -- GLuint
    pragma Convention(Stdcall, Access_Get_Uniform_From_Index);
  type Access_Uniform_Block_Binding is access procedure( -- glUniformBlockBinding
    Program               : in Integer_4_Unsigned_C;     -- GLuint program
    Uniform_Block_Index   : in Integer_4_Unsigned_C;     -- GLuint uniformBlockIndex
    Uniform_Block_Binding : in Integer_4_Unsigned_C);    -- GLuint uniformBlockBinding
    pragma Convention(Stdcall, Access_Uniform_Block_Binding);
  type Access_Fence_Sync is access function( -- glFenceSync
    Condition : in Integer_4_Unsigned_C;     -- GLenum condition
    Flags     : in Integer_4_Unsigned_C)     -- GLbitfield flags
    return Address;                          -- GLsync
    pragma Convention(Stdcall, Access_Fence_Sync);
  type Access_Is_Sync is access function( -- glIsSync
    Sync : in Address)                    -- GLsync sync
    return Address;                       -- GLsync
    pragma Convention(Stdcall, Access_Is_Sync);
  type Access_Client_Wait_Sync is access function( -- glClientWaitSync
    Sync    : in Address;                          -- GLsync sync
    Flags   : in Integer_4_Unsigned_C;             -- GLbitfield flags
    Timeout : in Integer_8_Unsigned_C)             -- GLuint64 timeout
    return Integer_4_Unsigned_C;                   -- GLenum
    pragma Convention(Stdcall, Access_Client_Wait_Sync);
  type Access_Delete_Sync is access procedure( -- glDeleteSync
    Sync : in Address);                        -- GLsync sync
    pragma Convention(Stdcall, Access_Delete_Sync);
  type Access_Depth_Bounds is access procedure( -- glDepthBoundsEXT
    Z_Minimum : in Float_8_Real_C;              -- GLclampd zmin
    Z_Maximum : in Float_8_Real_C);             -- GLclampd zmax
    pragma Convention(Stdcall, Access_Depth_Bounds);
  type Access_Create_Shader is access function( -- glCreateShader
    Kind : in Integer_4_Unsigned_C)             -- GLenum type
    return Integer_4_Unsigned_C;                -- GLuint
    pragma Convention(Stdcall, Access_Create_Shader);
  type Access_Delete_Shader is access procedure( -- glDeleteShader
    Shader : in Integer_4_Unsigned_C);           -- GLuint shader
    pragma Convention(Stdcall, Access_Delete_Shader);
  type Access_Shader_Source is access procedure( -- glShaderSource
    Shader : in Integer_4_Unsigned_C;            -- GLuint shader
    Count  : in Integer_4_Signed_C;              -- GLsizei count
    Text   : in Access_Character_1_C;            -- const GLchar* *string
    Length : in Integer_4_Signed_C);             -- const GLint *length
    pragma Convention(Stdcall, Access_Shader_Source);
  type Access_Compile_Shader is access procedure( -- glCompileShader
    Shader_Object : in Integer_Address);          -- GLhandleARB shaderObj
    pragma Convention(Stdcall, Access_Compile_Shader);
  type Access_Get_Shader is access procedure(   -- glGetShaderiv
    Shader     : in Integer_4_Unsigned_C;       -- GLuint shader
    Name       : in Integer_4_Unsigned_C;       -- GLenum pname
    Parameters : in Access_Integer_4_Signed_C); -- GLint *params
    pragma Convention(Stdcall, Access_Get_Shader);
  type Access_Get_Shader_Info_Log is access procedure( -- glGetShaderInfoLog
    Shader          : in Integer_4_Unsigned_C;         -- GLuint shader
    Buffer_Size     : in Integer_4_Signed_C;           -- GLsizei bufSize
    Length          : in Access_Integer_4_Signed_C;    -- GLsizei *length
    Information_Log : in Access_Character_1_C);        -- GLchar *infoLog
    pragma Convention(Stdcall, Access_Get_Shader_Info_Log);
  type Access_Create_Program is access function -- glCreateProgram
    return Integer_4_Unsigned_C;                -- GLuint
    pragma Convention(Stdcall, Access_Create_Program);
  type Access_Delete_Program is access procedure( -- glDeleteProgram
    Program : in Integer_4_Unsigned_C);           -- GLuint program
    pragma Convention(Stdcall, Access_Delete_Program);
  type Access_Attach_Shader is access procedure( -- glAttachShader
    Program : in Integer_4_Unsigned_C;           -- GLuint program
    Shader  : in Integer_4_Unsigned_C);          -- GLuint shader
    pragma Convention(Stdcall, Access_Attach_Shader);
  type Access_Detach_Shader is access procedure( -- glDetachShader
    Program : in Integer_4_Unsigned_C;           -- GLuint program,
    Shader  : in Integer_4_Unsigned_C);          -- GLuint shader
    pragma Convention(Stdcall, Access_Detach_Shader);
  type Access_Link_Program is access procedure( -- glLinkProgram
    Program : in Integer_4_Unsigned_C);         -- GLuint program
    pragma Convention(Stdcall, Access_Link_Program);
  type Access_Use_Program is access procedure( -- glUseProgram
    Program : in Integer_Address);             -- GLhandleARB programObj
    pragma Convention(Stdcall, Access_Use_Program);
  type Access_Get_Program is access procedure(    -- glGetProgramiv
    Target       : in Integer_4_Unsigned_C;       -- GLenum target
    Program_Name : in Integer_4_Unsigned_C;       -- GLenum pname
    Parameters   : in Access_Integer_4_Signed_C); -- GLint *params
    pragma Convention(Stdcall, Access_Get_Program);
  type Access_Get_Program_Info_Log is access procedure( -- glGetProgramInfoLog
    Program     : in Integer_4_Unsigned_C;              -- GLuint program
    Buffer_Size : in Integer_4_Signed_C;                -- GLsizei bufSize
    Length      : in Integer_4_Signed_C;                -- GLsizei *length
    Info_Log    : in Access_Integer_1_Unsigned_C);      -- GLchar *infoLog
    pragma Convention(Stdcall, Access_Get_Program_Info_Log);
  type Access_Bind_Attribute_Location is access function( -- glBindAttribLocation
    Program : in Integer_Address;                         -- GLhandleARB programObj,
    Name    : in Access_Integer_1_Unsigned_C)             -- const GLcharARB *name
    return Integer_4_Signed_C;                            -- GLint
    pragma Convention(Stdcall, Access_Bind_Attribute_Location);
  type Access_Get_Uniform_Location is access function( -- glGetUniformLocation
    Program : in Integer_Address;                      -- GLhandleARB programObj
    Name : in Access_Integer_1_Unsigned_C)             -- const GLcharARB *name
    return Integer_4_Signed_C;                         -- GLint
    pragma Convention(Stdcall, Access_Get_Uniform_Location);
  type Access_Uniform is access procedure( -- glUniform1i
    Location : in Integer_4_Signed_C;      -- GLint location
    Vertex   : in Integer_4_Signed_C);     -- GLint v0
    pragma Convention(Stdcall, Access_Uniform);
  type Access_Uniform_Vector is access procedure( -- glUniform4fv
    Location : in Integer_4_Signed_C;             -- GLint location
    Count    : in Integer_4_Signed_C;             -- GLsizei count
    Value    : in Access_Float_4_Real_C);         -- const GLfloat *value
    pragma Convention(Stdcall, Access_Uniform_Vector);
  type Access_Vertex_Attribute_Pointer is access procedure( -- glVertexAttribPointerARB
    Index      : in Integer_4_Unsigned_C;                   -- GLuint index
    Size       : in Integer_4_Signed_C;                     -- GLint size
    Kind       : in Integer_4_Unsigned_C;                   -- GLenum type
    Normalized : in Integer_1_Unsigned_C;                   -- GLboolean normalized
    Stride     : in Integer_4_Signed_C;                     -- GLsizei stride
    Pointer    : in Address);                               -- const GLvoid *pointer
    pragma Convention(Stdcall, Access_Vertex_Attribute_Pointer);
  type Access_Enable_Vertex_Attribute_Array  is access procedure( -- glEnableVertexAttribArrayARB
    Index : in Integer_4_Unsigned_C);                             -- GLuint index
    pragma Convention(Stdcall, Access_Enable_Vertex_Attribute_Array);
  type Access_Disable_Vertex_Attirbute_Array is access procedure( -- glDisableVertexAttribArrayARB
    Index : in Integer_4_Unsigned_C);                             -- GLuint index
    pragma Convention(Stdcall, Access_Disable_Vertex_Attirbute_Array);
  type Access_Program_String is access procedure( -- glProgramStringARB
    Target : in Integer_4_Unsigned_C;             -- GLenum target
    Format : in Integer_4_Unsigned_C;             -- GLenum format
    Length : in Integer_4_Signed_C;               -- GLsizei len
    Text   : in Address);                         -- const GLvoid *string
    pragma Convention(Stdcall, Access_Program_String);
  type Access_Bind_Program is access procedure( -- glBindProgramARB
    Target  : in Integer_4_Unsigned_C;          -- GLenum target;
    Program : in Integer_4_Unsigned_C);         -- GLuint program
    pragma Convention(Stdcall, Access_Bind_Program);
  type Access_Generate_Programs is access procedure( -- glGenProgramsARB
    Index    : in Integer_4_Signed_C;                -- GLsizei n
    Programs : in Access_Integer_4_Unsigned_C);      -- GLuint *programs
    pragma Convention(Stdcall, Access_Generate_Programs);
  type Access_Delete_Programs is access procedure( -- glDeleteProgramsARB
    Index    : in Integer_4_Signed_C;              -- GLsizei n
    Programs : in Access_Integer_4_Unsigned_C);    -- const GLuint *programs
    pragma Convention(Stdcall, Access_Delete_Programs);
  type Access_Program_Environment_Parameter  is access procedure( -- glProgramEnvParameter4fvARB
    Target     : in Integer_4_Unsigned_C;                         -- GLenum target
    Index      : in Integer_4_Unsigned_C;                         -- GLuint index
    Parameters : in Float_4_Real_C);                              -- const GLfloat *params
    pragma Convention(Stdcall, Access_Program_Environment_Parameter);
  type Access_Program_Local_Parameter is access procedure( -- glProgramLocalParameter4fvARB
    Target : in Integer_4_Unsigned_C;                      -- GLenum target
    Index  : in Integer_4_Unsigned_C;                      -- GLuint index
    Params : in Access_Float_4_Real_C);                    -- const GLfloat *params
    pragma Convention(Stdcall, Access_Program_Local_Parameter);
  type Access_Draw_Elements_Base_Vertex is access procedure( -- glDrawElementsBaseVertex
    Mode        : in Integer_4_Unsigned_C;                   -- GLenum mode
    Count       : in Integer_4_Signed_C;                     -- GLsizei count
    Kind        : in Integer_4_Unsigned_C;                   -- GLenum type
    Indices     : in Address;                                -- GLvoid *indices
    Base_Vertex : in Integer_4_Signed_C);                    -- GLint basevertex
    pragma Convention(Stdcall, Access_Draw_Elements_Base_Vertex);
  type Access_Generate_Vertex_Arrays is access procedure( -- glGenVertexArrays
    Index : in Integer_4_Signed_C;                        -- GLsizei n
    Item  : in Access_Integer_4_Unsigned_C);              -- GLuint *arrays
    pragma Convention(Stdcall, Access_Generate_Vertex_Arrays);
  type Access_Bind_Vertex_Array is access procedure( -- glBindVertexArray
    Item : in Integer_4_Unsigned_C);                 -- GLuint array
    pragma Convention(Stdcall, Access_Bind_Vertex_Array);
  type Access_Delete_Vertex_Arrays is access procedure( -- glDeleteVertexArrays
    Index  : in Integer_4_Signed_C;                     -- GLsizei n,
    Arrays : in Access_Integer_4_Unsigned_C);           -- const GLuint *arrays
    pragma Convention(Stdcall, Access_Delete_Vertex_Arrays);
  type Access_Map_Buffer_Range is access function( -- glMapBufferRange
    Target : in Integer_4_Unsigned_C;              -- GLenum target
    Offset : in Integer_Address;                   -- GLintptr offset
    Length : in Integer_Address;                   -- GLsizeiptr length
    Item   : in Integer_4_Unsigned_C)              -- GLbitfield access
    return Address;
    pragma Convention(Stdcall, Access_Map_Buffer_Range);
  type Access_Get_String_Index is access function( -- glGetStringi
    Name  : in Integer_4_Unsigned_C;      -- GLenum name
    Index : in Integer_4_Unsigned_C)      -- index
    return Access_Constant_Character_1_C; -- const GLubyte*
    pragma Convention(Stdcall, Access_Get_String_Index);
  type Access_Function_Load is access function(
    Item : in String_1)
    return Address;
  Get_String_Index               : Access_Get_String_Index               := null;
  Active_Texture                 : Access_Active_Texture                 := null;
  Client_Active_Texture          : Access_Client_Active_Texture          := null;
  Bind_Buffer                    : Access_Bind_Buffer                    := null;
  Bind_Buffer_Range              : Access_Bind_Buffer_Range              := null;
  Delete_Buffers                 : Access_Delete_Buffers                 := null;
  Generate_Buffers               : Access_Generate_Buffers               := null;
  Is_Buffer                      : Access_Is_Buffer                      := null;
  Buffer_Data                    : Access_Buffer_Data                    := null;
  Buffer_Subdata                 : Access_Buffer_Subdata                 := null;
  Get_Buffer_Subdata             : Access_Get_Buffer_Subdata             := null;
  Map_Buffer                     : Access_Map_Buffer                     := null;
  Unmap_Buffer                   : Access_Unmap_Buffer                   := null;
  Get_Buffer_Parameter           : Access_Get_Buffer_Parameter           := null;
  Get_Buffer_Pointer             : Access_Get_Buffer_Pointer             := null;
  Debug_Message_Control          : Access_Debug_Message_Control          := null;
  Debug_Message_Insert           : Access_Debug_Message_Insert           := null;
  Debug_Message_Callback         : Access_Debug_Message_Callback         := null;
  Get_Debug_Message_Log          : Access_Get_Debug_Message_Log          := null;
  Generate_Queries               : Access_Generate_Queries               := null;
  Delete_Quries                  : Access_Delete_Quries                  := null;
  Is_Query                       : Access_Is_Query                       := null;
  Begin_Query                    : Access_Begin_Query                    := null;
  End_Query                      : Access_End_Query                      := null;
  Get_Query                      : Access_Get_Query                      := null;
  Get_Query_Object               : Access_Get_Query_Object               := null;
  Get_Query_Object_Unsigned      : Access_Get_Query_Object_Unsigned      := null;
  Stencil_Operation_Separate     : Access_Stencil_Operation_Separate     := null;
  Stencil_Function_Separate      : Access_Stencil_Function_Separate      := null;
  Get_Uniform_From_Index         : Access_Get_Uniform_From_Index         := null;
  Uniform_Block_Binding          : Access_Uniform_Block_Binding          := null;
  Fence_Sync                     : Access_Fence_Sync                     := null;
  Is_Sync                        : Access_Is_Sync                        := null;
  Client_Wait_Sync               : Access_Client_Wait_Sync               := null;
  Delete_Sync                    : Access_Delete_Sync                    := null;
  Depth_Bounds                   : Access_Depth_Bounds                   := null;
  Create_Shader                  : Access_Create_Shader                  := null;
  Delete_Shader                  : Access_Delete_Shader                  := null;
  Shader_Source                  : Access_Shader_Source                  := null;
  Compile_Shader                 : Access_Compile_Shader                 := null;
  Get_Shader                     : Access_Get_Shader                     := null;
  Get_Shader_Info_Log            : Access_Get_Shader_Info_Log            := null;
  Create_Program                 : Access_Create_Program                 := null;
  Delete_Program                 : Access_Delete_Program                 := null;
  Attach_Shader                  : Access_Attach_Shader                  := null;
  Detach_Shader                  : Access_Detach_Shader                  := null;
  Link_Program                   : Access_Link_Program                   := null;
  Use_Program                    : Access_Use_Program                    := null;
  Get_Program                    : Access_Get_Program                    := null;
  Get_Program_Info_Log           : Access_Get_Program_Info_Log           := null;
  Bind_Attribute_Location        : Access_Bind_Attribute_Location        := null;
  Get_Uniform_Location           : Access_Get_Uniform_Location           := null;
  Uniform                        : Access_Uniform                        := null;
  Uniform_Vector                 : Access_Uniform_Vector                 := null;
  Vertex_Attribute_Pointer       : Access_Vertex_Attribute_Pointer       := null;
  Enable_Vertex_Attribute_Array  : Access_Enable_Vertex_Attribute_Array  := null;
  Disable_Vertex_Attirbute_Array : Access_Disable_Vertex_Attirbute_Array := null;
  Program_String                 : Access_Program_String                 := null;
  Bind_Program                   : Access_Bind_Program                   := null;
  Generate_Programs              : Access_Generate_Programs              := null;
  Delete_Programs                : Access_Delete_Programs                := null;
  Program_Environment_Parameter  : Access_Program_Environment_Parameter  := null;
  Program_Local_Parameter        : Access_Program_Local_Parameter        := null;
  Draw_Elements_Base_Vertex      : Access_Draw_Elements_Base_Vertex      := null;
  Generate_Vertex_Arrays         : Access_Generate_Vertex_Arrays         := null;
  Bind_Vertex_Array              : Access_Bind_Vertex_Array              := null;
  Delete_Vertex_Arrays           : Access_Delete_Vertex_Arrays           := null;
  Map_Buffer_Range               : Access_Map_Buffer_Range               := null;
  function To_Unchecked_Access_Get_String_Index               is new Ada.Unchecked_Conversion(Address, Access_Get_String_Index);
  function To_Unchecked_Access_Active_Texture                 is new Ada.Unchecked_Conversion(Address, Access_Active_Texture);
  function To_Unchecked_Access_Client_Active_Texture          is new Ada.Unchecked_Conversion(Address, Access_Client_Active_Texture);
  function To_Unchecked_Access_Bind_Buffer                    is new Ada.Unchecked_Conversion(Address, Access_Bind_Buffer);
  function To_Unchecked_Access_Bind_Buffer_Range              is new Ada.Unchecked_Conversion(Address, Access_Bind_Buffer_Range);
  function To_Unchecked_Access_Delete_Buffers                 is new Ada.Unchecked_Conversion(Address, Access_Delete_Buffers);
  function To_Unchecked_Access_Generate_Buffers               is new Ada.Unchecked_Conversion(Address, Access_Generate_Buffers);
  function To_Unchecked_Access_Is_Buffer                      is new Ada.Unchecked_Conversion(Address, Access_Is_Buffer);
  function To_Unchecked_Access_Buffer_Data                    is new Ada.Unchecked_Conversion(Address, Access_Buffer_Data);
  function To_Unchecked_Access_Buffer_Subdata                 is new Ada.Unchecked_Conversion(Address, Access_Buffer_Subdata);
  function To_Unchecked_Access_Get_Buffer_Subdata             is new Ada.Unchecked_Conversion(Address, Access_Get_Buffer_Subdata);
  function To_Unchecked_Access_Map_Buffer                     is new Ada.Unchecked_Conversion(Address, Access_Map_Buffer);
  function To_Unchecked_Access_Unmap_Buffer                   is new Ada.Unchecked_Conversion(Address, Access_Unmap_Buffer);
  function To_Unchecked_Access_Get_Buffer_Parameter           is new Ada.Unchecked_Conversion(Address, Access_Get_Buffer_Parameter);
  function To_Unchecked_Access_Get_Buffer_Pointer             is new Ada.Unchecked_Conversion(Address, Access_Get_Buffer_Pointer);
  function To_Unchecked_Access_Debug_Message_Control          is new Ada.Unchecked_Conversion(Address, Access_Debug_Message_Control);
  function To_Unchecked_Access_Debug_Message_Insert           is new Ada.Unchecked_Conversion(Address, Access_Debug_Message_Insert);
  function To_Unchecked_Access_Debug_Message_Callback         is new Ada.Unchecked_Conversion(Address, Access_Debug_Message_Callback);
  function To_Unchecked_Access_Get_Debug_Message_Log          is new Ada.Unchecked_Conversion(Address, Access_Get_Debug_Message_Log);
  function To_Unchecked_Access_Generate_Queries               is new Ada.Unchecked_Conversion(Address, Access_Generate_Queries);
  function To_Unchecked_Access_Delete_Quries                  is new Ada.Unchecked_Conversion(Address, Access_Delete_Quries);
  function To_Unchecked_Access_Is_Query                       is new Ada.Unchecked_Conversion(Address, Access_Is_Query);
  function To_Unchecked_Access_Begin_Query                    is new Ada.Unchecked_Conversion(Address, Access_Begin_Query);
  function To_Unchecked_Access_End_Query                      is new Ada.Unchecked_Conversion(Address, Access_End_Query);
  function To_Unchecked_Access_Get_Query                      is new Ada.Unchecked_Conversion(Address, Access_Get_Query);
  function To_Unchecked_Access_Get_Query_Object               is new Ada.Unchecked_Conversion(Address, Access_Get_Query_Object);
  function To_Unchecked_Access_Get_Query_Object_Unsigned      is new Ada.Unchecked_Conversion(Address, Access_Get_Query_Object_Unsigned);
  function To_Unchecked_Access_Stencil_Operation_Separate     is new Ada.Unchecked_Conversion(Address, Access_Stencil_Operation_Separate);
  function To_Unchecked_Access_Stencil_Function_Separate      is new Ada.Unchecked_Conversion(Address, Access_Stencil_Function_Separate);
  function To_Unchecked_Access_Get_Uniform_From_Index         is new Ada.Unchecked_Conversion(Address, Access_Get_Uniform_From_Index);
  function To_Unchecked_Access_Uniform_Block_Binding          is new Ada.Unchecked_Conversion(Address, Access_Uniform_Block_Binding);
  function To_Unchecked_Access_Fence_Sync                     is new Ada.Unchecked_Conversion(Address, Access_Fence_Sync);
  function To_Unchecked_Access_Is_Sync                        is new Ada.Unchecked_Conversion(Address, Access_Is_Sync);
  function To_Unchecked_Access_Client_Wait_Sync               is new Ada.Unchecked_Conversion(Address, Access_Client_Wait_Sync);
  function To_Unchecked_Access_Delete_Sync                    is new Ada.Unchecked_Conversion(Address, Access_Delete_Sync);
  function To_Unchecked_Access_Depth_Bounds                   is new Ada.Unchecked_Conversion(Address, Access_Depth_Bounds);
  function To_Unchecked_Access_Create_Shader                  is new Ada.Unchecked_Conversion(Address, Access_Create_Shader);
  function To_Unchecked_Access_Delete_Shader                  is new Ada.Unchecked_Conversion(Address, Access_Delete_Shader);
  function To_Unchecked_Access_Shader_Source                  is new Ada.Unchecked_Conversion(Address, Access_Shader_Source);
  function To_Unchecked_Access_Compile_Shader                 is new Ada.Unchecked_Conversion(Address, Access_Compile_Shader);
  function To_Unchecked_Access_Get_Shader                     is new Ada.Unchecked_Conversion(Address, Access_Get_Shader);
  function To_Unchecked_Access_Get_Shader_Info_Log            is new Ada.Unchecked_Conversion(Address, Access_Get_Shader_Info_Log);
  function To_Unchecked_Access_Create_Program                 is new Ada.Unchecked_Conversion(Address, Access_Create_Program);
  function To_Unchecked_Access_Delete_Program                 is new Ada.Unchecked_Conversion(Address, Access_Delete_Program);
  function To_Unchecked_Access_Attach_Shader                  is new Ada.Unchecked_Conversion(Address, Access_Attach_Shader);
  function To_Unchecked_Access_Link_Program                   is new Ada.Unchecked_Conversion(Address, Access_Link_Program);
  function To_Unchecked_Access_Use_Program                    is new Ada.Unchecked_Conversion(Address, Access_Use_Program);
  function To_Unchecked_Access_Get_Program                    is new Ada.Unchecked_Conversion(Address, Access_Get_Program);
  function To_Unchecked_Access_Get_Program_Info_Log           is new Ada.Unchecked_Conversion(Address, Access_Get_Program_Info_Log);
  function To_Unchecked_Access_Bind_Attribute_Location        is new Ada.Unchecked_Conversion(Address, Access_Bind_Attribute_Location);
  function To_Unchecked_Access_Get_Uniform_Location           is new Ada.Unchecked_Conversion(Address, Access_Get_Uniform_Location);
  function To_Unchecked_Access_Uniform                        is new Ada.Unchecked_Conversion(Address, Access_Uniform);
  function To_Unchecked_Access_Uniform_Vector                 is new Ada.Unchecked_Conversion(Address, Access_Uniform_Vector);
  function To_Unchecked_Access_Vertex_Attribute_Pointer              is new Ada.Unchecked_Conversion(Address, Access_Vertex_Attribute_Pointer);
  function To_Unchecked_Access_Enable_Vertex_Attribute_Array         is new Ada.Unchecked_Conversion(Address, Access_Enable_Vertex_Attribute_Array);
  function To_Unchecked_Access_Disable_Vertex_Attirbute_Array        is new Ada.Unchecked_Conversion(Address, Access_Disable_Vertex_Attirbute_Array);
  function To_Unchecked_Access_Program_String                        is new Ada.Unchecked_Conversion(Address, Access_Program_String);
  function To_Unchecked_Access_Bind_Program                          is new Ada.Unchecked_Conversion(Address, Access_Bind_Program);
  function To_Unchecked_Access_Generate_Programs                     is new Ada.Unchecked_Conversion(Address, Access_Generate_Programs);
  function To_Unchecked_Access_Delete_Programs                       is new Ada.Unchecked_Conversion(Address, Access_Delete_Programs);
  function To_Unchecked_Access_Program_Environment_Parameter         is new Ada.Unchecked_Conversion(Address, Access_Program_Environment_Parameter);
  function To_Unchecked_Access_Program_Local_Parameter        is new Ada.Unchecked_Conversion(Address, Access_Program_Local_Parameter);
  function To_Unchecked_Access_Draw_Elements_Base_Vertex      is new Ada.Unchecked_Conversion(Address, Access_Draw_Elements_Base_Vertex);
  function To_Unchecked_Access_Generate_Vertex_Arrays         is new Ada.Unchecked_Conversion(Address, Access_Generate_Vertex_Arrays);
  function To_Unchecked_Access_Bind_Vertex_Array              is new Ada.Unchecked_Conversion(Address, Access_Bind_Vertex_Array);
  function To_Unchecked_Access_Delete_Vertex_Arrays           is new Ada.Unchecked_Conversion(Address, Access_Delete_Vertex_Arrays);
  function To_Unchecked_Access_Map_Buffer_Range               is new Ada.Unchecked_Conversion(Address, Access_Map_Buffer_Range);
  function To_Unchecked_Access_Detach_Shader                  is new Ada.Unchecked_Conversion(Address, Access_Detach_Shader );
  procedure Initialize(Load_Function : in Access_Function_Load);
  -- procedure Accumulate(
  --   Operation : in Integer_4_Unsigned_C;
  --   Value     : in Float_4_Real_C);
  -- procedure Alpha_Function(
  --   Function  : in Integer_4_Unsigned_C;
  --   Reference : in Float_4_Real_C);
   procedure Begin_Drawing(
     Mode : in Integer_4_Unsigned_C);
     pragma Import(Stdcall, Begin_Drawing, "glBegin");
   procedure End_Drawing;
     pragma Import(Stdcall, End_Drawing, "glEnd");
  -- procedure Bitmap(
  --   Width    : in Integer_4_Signed_C;
  --   Height   : in Integer_4_Signed_C;
  --   X_Origin : in Float_4_Real_C;
  --   Y_Origin : in Float_4_Real_C;
  --   X_Move   : in Float_4_Real_C;
  --   Y_Move   : in Float_4_Real_C;
  --   Bitmap   : in Access_Integer_1_Unsigned_C);
  procedure Blend_Function(
    S_Factor : in Integer_4_Unsigned_C;
    D_Factor : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Blend_Function, "glBlendFunc");
  -- procedure Call_List(
  --   List : in Integer_4_Unsigned_C);
  -- procedure Call_Lists(
  --   Number   : in Integer_4_Signed_C;
  --   Grouping : in Integer_4_Unsigned_C; -- Default: UNSigned_C_BYTE -- Others: 2_BYTES, 3_BYTES, 4_BYTES
  --   Lists    : in Array_Integer_1_Unsigned_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Integer_2_Unsigned_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Integer_4_Unsigned_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Integer_1_Signed_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Integer_2_Signed_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Integer_4_Signed_C);
  -- procedure Call_Lists(
  --   Number : in Integer_4_Signed_C;
  --   Lists  : in Array_Float_4_Real_C);
  procedure Clear(
    Mask : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Clear, "glClear");
  -- procedure Clear_Accumulation(
  --   Red   : in Float_4_Real_C;
  --   Green : in Float_4_Real_C;
  --   Blue  : in Float_4_Real_C;
  --   Alpha : in Float_4_Real_C);
  -- procedure Clear_Color(
  --   Red   : in Float_4_Real_C;
  --   Green : in Float_4_Real_C;
  --   Blue  : in Float_4_Real_C;
  --   Alpha : in Float_4_Real_C);
  procedure Clear_Depth(
    Depth : in Float_8_Real_C);
    pragma Import(Stdcall, Clear_Depth, "glClearDepth");
  -- procedure Clear_Index(
  --   C : in Float_4_Real_C);
  procedure Clear_Stencil(
    S : in Integer_4_Signed_C);
    pragma Import(Stdcall, Clear_Stencil, "glClearStencil");
  -- procedure Clip_Plane(
  --   Plane    : in Integer_4_Unsigned_C;
  --   Equation : in Access_Constant_Float_8_Real_C);
  -- procedure Color(
  --   Red   : in Integer_1_Unsigned_C;
  --   Green : in Integer_1_Unsigned_C;
  --   Blue  : in Integer_1_Unsigned_C);
  -- procedure Color(
  --   V : in Win32.PCSTR);
  -- procedure Color(
  --   Red   : in Float_8_Real_C;
  --   Green : in Float_8_Real_C;
  --   Blue  : in Float_8_Real_C);
  -- procedure Color(
  --   V : in Access_Constant_Float_8_Real_C);
   procedure Color(
     Red   : in Float_4_Real_C;
     Green : in Float_4_Real_C;
     Blue  : in Float_4_Real_C);
     pragma Import(Stdcall, Color, "glColor3f");
  -- procedure Color(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Color(
  --   Red   : in Integer_4_Signed_C;
  --   Green : in Integer_4_Signed_C;
  --   Blue  : in Integer_4_Signed_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Color(
  --   Red   : in Integer_2_Signed_C;
  --   Green : in Integer_2_Signed_C;
  --   Blue  : in Integer_2_Signed_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Color(
  --   Red   : in Integer_1_Unsigned_C;
  --   Green : in Integer_1_Unsigned_C;
  --   Blue  : in Integer_1_Unsigned_C);
  -- procedure Color(
  --   V : in Access_Integer_1_Unsigned_C);
  -- procedure Color(
  --   Red   : in Integer_4_Unsigned_C;
  --   Green : in Integer_4_Unsigned_C;
  --   Blue  : in Integer_4_Unsigned_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_4_Unsigned_C);
  -- procedure Color(
  --   Red   : in Integer_2_Unsigned_C;
  --   Green : in Integer_2_Unsigned_C;
  --   Blue  : in Integer_2_Unsigned_C);
  -- procedure Color(
  --   V : in Win32.PCWSTR);
  -- procedure Color(
  --   Red   : in Integer_1_Unsigned_C;
  --   Green : in Integer_1_Unsigned_C;
  --   Blue  : in Integer_1_Unsigned_C;
  --   Alpha : in Integer_1_Unsigned_C);
  -- procedure Color(
  --   V : in Win32.PCSTR);
  -- procedure Color(
  --   Red   : in Float_8_Real_C;
  --   Green : in Float_8_Real_C;
  --   Blue  : in Float_8_Real_C;
  --   Alpha : in Float_8_Real_C);
  -- procedure Color(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Color(
  --   Red   : in Float_4_Real_C;
  --   Green : in Float_4_Real_C;
  --   Blue  : in Float_4_Real_C;
  --   Alpha : in Float_4_Real_C);
  -- procedure Color(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Color(
  --   Red   : in Integer_4_Signed_C;
  --   Green : in Integer_4_Signed_C;
  --   Blue  : in Integer_4_Signed_C;
  --   Alpha : in Integer_4_Signed_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_4_Unsigned_C);
  -- procedure Color(
  --   Red   : in Integer_2_Signed_C;
  --   Green : in Integer_2_Signed_C;
  --   Blue  : in Integer_2_Signed_C;
  --   Alpha : in Integer_2_Signed_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Color(
  --   Red   : in Integer_1_Unsigned_C;
  --   Green : in Integer_1_Unsigned_C;
  --   Blue  : in Integer_1_Unsigned_C;
  --   Alpha : in Integer_1_Unsigned_C);
  -- procedure Color(
  --   V : in Access_Integer_1_Unsigned_C);
  -- procedure Color(
  --   Red   : in Integer_4_Unsigned_C;
  --   Green : in Integer_4_Unsigned_C;
  --   Blue  : in Integer_4_Unsigned_C;
  --   Alpha : in Integer_4_Unsigned_C);
  -- procedure Color(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Color(
  --   Red   : in Integer_2_Unsigned_C;
  --   Green : in Integer_2_Unsigned_C;
  --   Blue  : in Integer_2_Unsigned_C;
  --   Alpha : in Integer_2_Unsigned_C);
  -- procedure Color(
  --   V : in Win32.PCWSTR);
  procedure Color_Mask(
    Red   : in Integer_1_Unsigned_C;
    Green : in Integer_1_Unsigned_C;
    Blue  : in Integer_1_Unsigned_C;
    Alpha : in Integer_1_Unsigned_C);
    pragma Import(Stdcall, Color_Mask, "glColorMask");
  -- procedure Color_Material(
  --   Race : in Integer_4_Unsigned_C;
  --   Mode : in Integer_4_Unsigned_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_1_Unsigned_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_2_Unsigned_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_4_Unsigned_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_1_Signed_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_2_Signed_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Integer_4_Signed_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Float_4_Real_C);
  -- procedure Color_Pointer(
  --   Size           : in Integer_4_Signed_C;
  --   Stride         : in Integer_4_Signed_C;
  --   Color_Elements : in Array_Float_8_Real_C);
  -- procedure Copy_Pixels(
  --   X      : in Integer_4_Signed_C;
  --   Y      : in Integer_4_Signed_C;
  --   Width  : in Integer_4_Signed_C;
  --   Height : in Integer_4_Signed_C;
  --   Kind   : in Integer_4_Unsigned_C);
  procedure Cull_Face(
    Mode : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Cull_Face, "glCullFace");
  -- procedure Delete_Lists(
  --   List   : in Integer_4_Unsigned_C;
  --   Bounds : in Integer_4_Signed_C);
  procedure Depth_Function(
    Kind : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Depth_Function, "glDepthFunc");
  procedure Depth_Mask(
    Flag : in Integer_1_Unsigned_C);
    pragma Import(Stdcall, Depth_Mask, "glDepthMask");
  -- procedure Depth_Range(
  --   Z_Near : in Float_8_Real_C;
  --   Z_Far  : in Float_8_Real_C);
  procedure Disable(
    Cap : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Disable, "glDisable");
  procedure Draw_Buffer(
    Mode : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Draw_Buffer, "glDrawBuffer");
  -- procedure Draw_Elements(
  --   Mode    : in Integer_4_Unsigned_C;
  --   Indices : in Array_Integer_1_Unsigned_C);
  -- procedure Draw_Elements(
  --   Mode    : in Integer_4_Unsigned_C;
  --   Indices : in Array_Integer_2_Unsigned_C);
  -- procedure Draw_Elements(
  --   Mode    : in Integer_4_Unsigned_C;
  --   Indices : in Array_Integer_4_Unsigned_C);
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_1_Unsigned_C);
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_1_Signed_C);
  --   end Draw_Pixels;
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_2_Unsigned_C);
  --   end Draw_Pixels;
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_2_Signed_C);
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_4_Unsigned_C);
  -- procedure Draw_Pixels(
  --   Format : in Integer_4_Unsigned_C;
  --   Pixels : in Matrix_Integer_4_Signed_C);
  -- procedure Edge_Flag(
  --   Flag : in Integer_1_Unsigned_C);
  -- -- procedure Edge_Flag(
  -- --   Offset  : Integer_4_Signed_C,
  -- --   Pointer : GLvoid *) --UNKNOWN
  -- procedure Edge_Flag(
  --   Flag : in Access_Integer_1_Unsigned_C);
  procedure Enable(
    Cap : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Enable, "glEnable");
  -- procedure End_OpenGL;
  -- procedure End_List;
  -- procedure Evaluate_Coordinate(
  --   U : in Float_8_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Access_Constant_Float_8_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Float_4_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Access_Constant_Float_4_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Float_8_Real_C;
  --   V : in Float_8_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Access_Constant_Float_8_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Float_4_Real_C;
  --   V : in Float_4_Real_C);
  -- procedure Evaluate_Coordinate(
  --   U : in Access_Constant_Float_4_Real_C);
  -- procedure Evaluate_Mesh(
  --   Mode : in Integer_4_Unsigned_C;
  --   I_1  : in Integer_4_Signed_C;
  --   J_2  : in Integer_4_Signed_C);
  -- procedure Evaluate_Mesh(
  --   Mode : in Integer_4_Unsigned_C;
  --   I_1  : in Integer_4_Signed_C;
  --   I_2  : in Integer_4_Signed_C;
  --   J_1  : in Integer_4_Signed_C;
  --   J_2  : in Integer_4_Signed_C);
  -- procedure Evaluate_Point(
  --   I : in Integer_4_Signed_C);
  -- procedure Evaluate_Point(
  --   I : in Integer_4_Signed_C;
  --   J : in Integer_4_Signed_C);
  -- function Get_Feedback_Buffer(
  --   Size   : in Integer_4_Signed_C;
  --   Kind   : in Integer_4_Unsigned_C)
  --   return Array_Float_4_Real_C;
  -- procedure Finish;
  -- procedure Flush;
  -- procedure Fog(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Fog(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Fog(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Fog(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Signed_C);
  -- procedure Front_Face(
  --   Mode : in Integer_4_Unsigned_C);
  -- procedure Frustum(
  --   Left   : in Float_8_Real_C;
  --   Right  : in Float_8_Real_C;
  --   Bottom : in Float_8_Real_C;
  --   Top    : in Float_8_Real_C;
  --   Z_Near : in Float_8_Real_C;
  --   Z_Far  : in Float_8_Real_C);
  -- function Generate_Lists(
  --   Bounds : in Integer_4_Signed_C)
  --   return Integer_4_Unsigned_C;
  -- function Generate_Textures(
  --   Size : in Integer_4_Positive)
  --   return Array_Integer_4_Unsigned_C;
  -- function Get_Bytes(
  --   Name : in Integer_4_Unsigned_C)
  --   return Array_Integer_1_Unsigned_C;
  -- procedure Get_Clip_Plane(
  --   Plane    : in Integer_4_Unsigned_C;
  --   Equation : in Access_Float_8_Real_C);
  -- procedure Get_Double(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_8_Real_C);
  function Get_Error
    return Integer_4_Unsigned_C;
    pragma Import(Stdcall, Get_Error, "glGetError");
  procedure Get_Float_Value(
    Name       : in Integer_4_Unsigned_C;
    Parameters : in Access_Float_4_Real_C);
    pragma Import(Stdcall, Get_Float_Value, "glGetFloatv");
  procedure Get_Integer_Value(                 -- glGetIntegerv
    Name       : in Integer_4_Unsigned_C;       -- pname
    Parameters : in Access_Integer_4_Signed_C); -- params
    pragma Import(Stdcall, Get_Integer_Value, "glGetIntegerv");
  -- procedure Get_Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Get_Map(
  --   Target : in Integer_4_Unsigned_C;
  --   Query  : in Integer_4_Unsigned_C;
  --   V      : in Access_Float_8_Real_C);
  -- procedure Get_Map(
  --   Target : in Integer_4_Unsigned_C;
  --   Query  : in Integer_4_Unsigned_C;
  --   V      : in Access_Float_4_Real_C);
  -- procedure Get_Map(
  --   Target : in Integer_4_Unsigned_C;
  --   Query  : in Integer_4_Unsigned_C;
  --   V      : in Access_Integer_4_Signed_C);
  -- procedure Get_Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Get_Pixel_Map(
  --   Map    : in Integer_4_Unsigned_C;
  --   Values : in Access_Float_4_Real_C);
  -- procedure Get_Pixel_Map(
  --   Map    : in Integer_4_Unsigned_C;
  --   Values : in Access_Integer_4_Unsigned_C);
  -- procedure Get_Pixel_Map(
  --   Map    : in Integer_4_Unsigned_C;
  --   Values : in Win32.PWSTR);
  -- procedure Get_Polygon_Stipple(
  --   Mask : in Access_Integer_1_Unsigned_C);
  function Get_String(                    -- glGetString
    Name : in Integer_4_Unsigned_C)       -- GLenum name
    return Access_Constant_Character_1_C; -- const GLubyte*
    pragma Import(Stdcall, Get_String, "glGetString");
  -- procedure Get_Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Get_Texture_Generation(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_8_Real_C);
  -- procedure Get_Texture_Generation(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Texture_Generation(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Get_Texture_Image(
  --   Target : in Integer_4_Unsigned_C;
  --   Level  : in Integer_4_Signed_C;
  --   Format : in Integer_4_Unsigned_C;
  --   Kind   : in Integer_4_Unsigned_C;
  --   Pixels : in PGLvoid);
  -- procedure Get_Texture_Level_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Level      : in Integer_4_Signed_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Texture_Level_Parameter(
  --   Target : in Integer_4_Unsigned_C;
  --   Level  : in Integer_4_Signed_C;
  --   Name   : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Get_Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Float_4_Real_C);
  -- procedure Get_Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Integer_4_Signed_C);
  -- procedure Hint(
  --   Target : in Integer_4_Unsigned_C;
  --   Mode   : in Integer_4_Unsigned_C);
  -- procedure Index_Mask(
  --   Mask : in Integer_4_Unsigned_C);
  -- procedure Index(
  --   C : in Float_8_Real_C);
  -- procedure Index(
  --   C : in Access_Constant_Float_8_Real_C);
  -- procedure Index(
  --   C : in Float_4_Real_C);
  -- procedure Index(
  --   C : in Access_Constant_Float_4_Real_C);
  -- procedure Index(
  --   C : in Integer_4_Signed_C);
  -- procedure Index(
  --   C : in Access_Constant_Integer_4_Unsigned_C);
  -- procedure Index(
  --   C : in Integer_2_Signed_C);
  -- procedure Index(
  --   C : in Access_Constant_Integer_2_Signed_C);
  -- procedure Initialize_Names;
  -- function Is_Enabled(
  --   Cap : in Integer_4_Unsigned_C)
  --   return Integer_1_Unsigned_C;
  -- function Is_List(
  --   List : in Integer_4_Unsigned_C)
  --   return Integer_1_Unsigned_C;
  -- procedure Light_Model(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Light_Model(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Light_Model(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Light_Model(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Unsigned_C);
  -- procedure Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Light(
  --   Light      : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Unsigned_C);
  -- procedure Line_Stipple(
  --   Factor  : in Integer_4_Signed_C;
  --   Pattern : in Integer_2_Unsigned_C);
  -- procedure Line_Width(
  --   Width : in Float_4_Real_C);
  -- procedure List_Base(
  --   Base : in Integer_4_Unsigned_C);
  -- procedure Load_Identity;
  -- procedure Load_Matrix(
  --   M : in Access_Constant_Float_8_Real_C);
  -- procedure Load_Matrix(
  --   M : in Access_Constant_Float_4_Real_C);
  -- procedure Load_Name(
  --   Name : in Integer_4_Unsigned_C);
  -- procedure Logic_Operation(
  --   Operation_Code : in Integer_4_Unsigned_C);
  -- procedure Map(
  --   Target : in Integer_4_Unsigned_C;
  --   U_1    : in Float_8_Real_C;
  --   U_2    : in Float_8_Real_C;
  --   Stride : in Integer_4_Signed_C;
  --   Order  : in Integer_4_Signed_C;
  --   Points : in Access_Constant_Float_8_Real_C);
  -- procedure Map(
  --   Target : in Integer_4_Unsigned_C;
  --   U_1     : in Float_4_Real_C;
  --   U_2     : in Float_4_Real_C;
  --   Stride : in Integer_4_Signed_C;
  --   Order  : in Integer_4_Signed_C;
  --   Points : in Access_Constant_Float_4_Real_C);
  -- procedure Map(
  --   Target   : in Integer_4_Unsigned_C;
  --   U_1      : in Float_8_Real_C;
  --   U_2      : in Float_8_Real_C;
  --   U_Stride : in Integer_4_Signed_C;
  --   U_Order  : in Integer_4_Signed_C;
  --   V_1      : in Float_8_Real_C;
  --   V_2      : in Float_8_Real_C;
  --   V_Stride : in Integer_4_Signed_C;
  --   V_Order  : in Integer_4_Signed_C;
  --   Points   : in Access_Constant_Float_8_Real_C);
  -- procedure Map(
  --   Target   : in Integer_4_Unsigned_C;
  --   U_1      : in Float_4_Real_C;
  --   U_2      : in Float_4_Real_C;
  --   U_Stride : in Integer_4_Signed_C;
  --   U_Order  : in Integer_4_Signed_C;
  --   V_1      : in Float_4_Real_C;
  --   V_2      : in Float_4_Real_C;
  --   V_Stride : in Integer_4_Signed_C;
  --   V_Order  : in Integer_4_Signed_C;
  --   Points   : in Access_Constant_Float_4_Real_C);
  -- procedure Map_Grid(
  --   U_N : in Integer_4_Signed_C;
  --   U_1 : in Float_8_Real_C;
  --   U_2 : in Float_8_Real_C);
  -- procedure Map_Grid(
  --   U_N : in Integer_4_Signed_C;
  --   U_1 : in Float_4_Real_C;
  --   U_2 : in Float_4_Real_C);
  -- procedure Map_Grid(
  --   U_N : in Integer_4_Signed_C;
  --   U_1 : in Float_8_Real_C;
  --   U_2 : in Float_8_Real_C;
  --   V_N : in Integer_4_Signed_C;
  --   V_1 : in Float_8_Real_C;
  --   V_2 : in Float_8_Real_C);
  -- procedure Map_Grid(
  --   U_N : in Integer_4_Signed_C;
  --   U_1 : in Float_4_Real_C;
  --   U_2 : in Float_4_Real_C;
  --   V_N : in Integer_4_Signed_C;
  --   V_1 : in Float_4_Real_C;
  --   V_2 : in Float_4_Real_C);
  -- procedure Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Material(
  --   Face       : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Signed_C);
  -- procedure Matrix_Mode(
  --   Mode : in Integer_4_Unsigned_C);
  -- procedure Multiply_Matrix(
  --   M : in Access_Constant_Float_8_Real_C);
  -- procedure Multiply_Matrix(
  --   M : in Access_Constant_Float_4_Real_C);
  -- procedure New_List(
  --   List : in Integer_4_Unsigned_C;
  --   Mode : in Integer_4_Unsigned_C);
  -- procedure Normal(
  --   N_X : in Integer_1_Unsigned_C;
  --   N_Y : in Integer_1_Unsigned_C;
  --   N_Z : in Integer_1_Unsigned_C);
  -- procedure Normal(
  --   V : in Win32.PCSTR);
  -- procedure Normal(
  --   N_X : in Float_8_Real_C;
  --   N_Y : in Float_8_Real_C;
  --   N_Z : in Float_8_Real_C);
  -- procedure Normal(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Normal(
  --   N_X : in Float_4_Real_C;
  --   N_Y : in Float_4_Real_C;
  --   N_Z : in Float_4_Real_C);
  -- procedure Normal(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Normal(
  --   N_X : in Integer_4_Signed_C;
  --   N_Y : in Integer_4_Signed_C;
  --   N_Z : in Integer_4_Signed_C);
  -- procedure Normal(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Normal(
  --   N_X : in Integer_2_Signed_C;
  --   N_Y : in Integer_2_Signed_C;
  --   N_Z : in Integer_2_Signed_C);
  -- procedure Normal(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Orthographic(
  --   Left   : in Float_8_Real_C;
  --   Right  : in Float_8_Real_C;
  --   Bottom : in Float_8_Real_C;
  --   Top    : in Float_8_Real_C;
  --   Z_Near : in Float_8_Real_C;
  --   Z_Far  : in Float_8_Real_C);
  -- procedure Pass_Through(
  --   Token : in Float_4_Real_C);
  -- procedure Pixel_Map(
  --   Map      : in Integer_4_Unsigned_C;
  --   Map_Size : in Integer_4_Signed_C;
  --   Values   : in Access_Constant_Float_4_Real_C);
  -- procedure Pixel_Map(
  --   Map      : in Integer_4_Unsigned_C;
  --   Map_Size : in Integer_4_Signed_C;
  --   Values   : in Access_Constant_Integer_4_Signed_C);
  -- procedure Pixel_Map(
  --   Map      : in Integer_4_Unsigned_C;
  --   Map_Size : in Integer_4_Signed_C;
  --   Values   : in Win32.PCWSTR);
  -- procedure Pixel_Store(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Pixel_Store(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Pixel_Transfer(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Pixel_Transfer(
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Pixel_Zoom(
  --   X_Factor : in Float_4_Real_C;
  --   Y_Factor : in Float_4_Real_C);
  -- procedure Point_Size(
  --   Size : in Float_4_Real_C);
  procedure Polygon_Offset( -- glPolygonOffset
    Factor : in Float_4_Real_C; -- GLfloat factor
    Units  : in Float_4_Real_C); -- GLfloat units
    pragma Import(Stdcall, Polygon_Offset, "glPolygonOffset");
  procedure Polygon_Mode(
    Face : in Integer_4_Unsigned_C;
    Mode : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Polygon_Mode, "glPolygonMode");
  -- procedure Polygon_Stipple(
  --   Mask : in Access_Integer_1_Unsigned_C);
  -- procedure Pop_Attribute;
  -- procedure Pop_Client_Attribute;
  -- procedure Pop_Matrix;
  -- procedure Pop_Name;
  -- procedure Push_Attribute(
  --   Mask : in Integer_4_Unsigned_C);
  -- procedure Push_Matrix;
  -- procedure Push_Name(
  --   Name : in Integer_4_Unsigned_C);
  -- procedure Raster_Position(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Raster_Position(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Raster_Position(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Raster_Position(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Raster_Position(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C;
  --   Z : in Float_4_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Raster_Position(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C;
  --   Z : in Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C;
  --   Z : in Integer_2_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Raster_Position(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C;
  --   W : in Float_8_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Raster_Position(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C;
  --   Z : in Float_4_Real_C;
  --   W : in Float_4_Real_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Raster_Position(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C;
  --   Z : in Integer_4_Signed_C;
  --   W : in Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Raster_Position(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C;
  --   Z : in Integer_2_Signed_C;
  --   W : in Integer_2_Signed_C);
  -- procedure Raster_Position(
  --   V : in Access_Constant_Integer_2_Signed_C);
  procedure Read_Buffer(
    Mode : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Read_Buffer, "glReadBuffer");
  -- procedure Read_Pixels(
  --   X      : in Integer_4_Signed_C;
  --   Y      : in Integer_4_Signed_C;
  --   Width  : in Integer_4_Signed_C;
  --   Height : in Integer_4_Signed_C;
  --   Format : in Integer_4_Unsigned_C;
  --   Kind   : in Integer_4_Unsigned_C;
  --   Pixels : in PGLvoid);
  -- procedure Rectangle(
  --   X_1 : in Float_8_Real_C;
  --   Y_1 : in Float_8_Real_C;
  --   X_2 : in Float_8_Real_C;
  --   Y_2 : in Float_8_Real_C);
  -- procedure Rectangle(
  --   V_1 : in Access_Constant_Float_8_Real_C;
  --   V_2 : in Access_Constant_Float_8_Real_C);
  -- procedure Rectangle(
  --   X_1 : in Float_4_Real_C;
  --   Y_1 : in Float_4_Real_C;
  --   X_2 : in Float_4_Real_C;
  --   Y_2 : in Float_4_Real_C);
  -- procedure Rectangle(
  --   V_1 : in Access_Constant_Float_4_Real_C;
  --   V_2 : in Access_Constant_Float_4_Real_C);
  -- procedure Rectangle(
  --   X_1 : in Integer_4_Signed_C;
  --   Y_1 : in Integer_4_Signed_C;
  --   X_2 : in Integer_4_Signed_C;
  --   Y_2 : in Integer_4_Signed_C);
  -- procedure Rectangle(
  --   V_1 : in Access_Constant_Integer_4_Signed_C;
  --   V_2 : in Access_Constant_Integer_4_Signed_C);
  -- procedure Rectangle(
  --   X_1 : in Integer_2_Signed_C;
  --   Y_1 : in Integer_2_Signed_C;
  --   X_2 : in Integer_2_Signed_C;
  --   Y_2 : in Integer_2_Signed_C);
  -- procedure Rectangle(
  --   V_1 : in Access_Constant_Integer_2_Signed_C;
  --   V_2 : in Access_Constant_Integer_2_Signed_C);
  -- function Render_Mode(
  --   Mode : in Integer_4_Unsigned_C)
  --   return Integer_4_Signed_C;
  -- procedure Rotate(
  --   Angle : in Float_8_Real_C;
  --   X     : in Float_8_Real_C;
  --   Y     : in Float_8_Real_C;
  --   Z     : in Float_8_Real_C);
  -- procedure Rotate(
  --   Angle : in Float_4_Real_C;
  --   X     : in Float_4_Real_C;
  --   Y     : in Float_4_Real_C;
  --   Z     : in Float_4_Real_C);
  -- procedure Scale(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C);
  -- procedure Scale(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C;
  --   Z : in Float_4_Real_C);
  procedure Scissor(
    X      : in Integer_4_Signed_C;
    Y      : in Integer_4_Signed_C;
    Width  : in Integer_4_Signed_C;
    Height : in Integer_4_Signed_C);
    pragma Import(Stdcall, Scissor, "glScissor");
  -- procedure Select_Buffer(
  --   Size   : in Integer_4_Signed_C;
  --   Buffer : in Access_Integer_4_Unsigned_C);
  procedure Shade_Model(
    Mode : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Shade_Model, "glShadeModel");
  -- procedure Stencil_Function(
  --   Function  : in Integer_4_Unsigned_C;
  --   Reference : in Integer_4_Signed_C;
  --   Mask      : in Integer_4_Unsigned_C);
  -- procedure Stencil_Mask(
  --   Mask : in Integer_4_Unsigned_C);
  procedure Stencil_Operation(
    Fail   : in Integer_4_Unsigned_C;
    Z_Fail : in Integer_4_Unsigned_C;
    Z_Pass : in Integer_4_Unsigned_C);
    pragma Import(Stdcall, Stencil_Operation, "glStencilOp");
  -- procedure Texture_Coordinate(
  --   S : in Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_8_Real_C;
  --   T : in Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_4_Real_C;
  --   T : in Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_4_Signed_C;
  --   T : in Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_2_Signed_C;
  --   T : in Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_8_Real_C;
  --   T : in Float_8_Real_C;
  --   R : in Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_4_Real_C;
  --   T : in Float_4_Real_C;
  --   R : in Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_4_Signed_C;
  --   T : in Integer_4_Signed_C;
  --   R : in Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_2_Signed_C;
  --   T : in Integer_2_Signed_C;
  --   R : in Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_8_Real_C;
  --   T : in Float_8_Real_C;
  --   R : in Float_8_Real_C;
  --   Q : in Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Float_4_Real_C;
  --   T : in Float_4_Real_C;
  --   R : in Float_4_Real_C;
  --   Q : in Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_4_Signed_C;
  --   T : in Integer_4_Signed_C;
  --   R : in Integer_4_Signed_C;
  --   Q : in Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Coordinate(
  --   S : in Integer_2_Signed_C;
  --   T : in Integer_2_Signed_C;
  --   R : in Integer_2_Signed_C;
  --   Q : in Integer_2_Signed_C);
  -- procedure Texture_Coordinate(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Texture_Environment(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_8_Real_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_8_Real_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Texture_Generate(
  --   Coordinate : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Signed_C);
  -- procedure Texture_Image_1D(
  --   Target     : in Integer_4_Unsigned_C;
  --   level      : in Integer_4_Signed_C;
  --   Components : in Integer_4_Signed_C;
  --   Width      : in Integer_4_Signed_C;
  --   Border     : in Integer_4_Signed_C;
  --   Format     : in Integer_4_Unsigned_C;
  --   Kind       : in Integer_4_Unsigned_C;
  --   Pixels     : in Win32.PCVOID);
  -- procedure Texture_Image_2D(
  --   Target     : in Integer_4_Unsigned_C;
  --   level      : in Integer_4_Signed_C;
  --   Components : in Integer_4_Signed_C;
  --   Width      : in Integer_4_Signed_C;
  --   Height     : in Integer_4_Signed_C;
  --   Border     : in Integer_4_Signed_C;
  --   Format     : in Integer_4_Unsigned_C;
  --   Kind       : in Integer_4_Unsigned_C;
  --   Pixels     : in Win32.PCVOID);
  -- procedure Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Float_4_Real_C);
  -- procedure Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Float_4_Real_C);
  -- procedure Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Integer_4_Signed_C);
  -- procedure Texture_Parameter(
  --   Target     : in Integer_4_Unsigned_C;
  --   Name       : in Integer_4_Unsigned_C;
  --   Parameters : in Access_Constant_Integer_4_Signed_C);
  -- procedure Translate(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C);
  -- procedure Translate(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C;
  --   Z : in Float_4_Real_C);
  -- procedure Vertex(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Vertex(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Vertex(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Vertex(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Vertex(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Float_8_Real_C);
  procedure Vertex(
    X : in Float_4_Real_C;
    Y : in Float_4_Real_C;
    Z : in Float_4_Real_C);
    pragma Import(Stdcall, Vertex, "glVertex3f");
  -- procedure Vertex(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Vertex(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C;
  --   Z : in Integer_4_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Vertex(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C;
  --   Z : in Integer_2_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_2_Signed_C);
  -- procedure Vertex(
  --   X : in Float_8_Real_C;
  --   Y : in Float_8_Real_C;
  --   Z : in Float_8_Real_C;
  --   W : in Float_8_Real_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Float_8_Real_C);
  -- procedure Vertex(
  --   X : in Float_4_Real_C;
  --   Y : in Float_4_Real_C;
  --   Z : in Float_4_Real_C;
  --   W : in Float_4_Real_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Float_4_Real_C);
  -- procedure Vertex(
  --   X : in Integer_4_Signed_C;
  --   Y : in Integer_4_Signed_C;
  --   Z : in Integer_4_Signed_C;
  --   W : in Integer_4_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_4_Signed_C);
  -- procedure Vertex(
  --   X : in Integer_2_Signed_C;
  --   Y : in Integer_2_Signed_C;
  --   Z : in Integer_2_Signed_C;
  --   W : in Integer_2_Signed_C);
  -- procedure Vertex(
  --   V : in Access_Constant_Integer_2_Signed_C);
  procedure Viewport(
    X      : in Integer_4_Signed_C;
    Y      : in Integer_4_Signed_C;
    Width  : in Integer_4_Signed_C;
    Height : in Integer_4_Signed_C);
    pragma Import(Stdcall, Viewport, "glViewport");
end Neo.OpenGL;
