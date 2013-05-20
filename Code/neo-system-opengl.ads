--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  System,
  Neo.Foundation.Data_Types,
  Ada.Wide_Text_IO,
  Ada.Characters.Handling;
use
  System,
  Neo.Foundation.Data_Types,
  Ada.Wide_Text_IO,
  Ada.Characters.Handling;
package Neo.System.OpenGL
  is
  ---------------
  -- Constants --
  ---------------
    GL_TRUE                            : constant Integer_4_Signed   := Integer_4_Signed(C_TRUE);
    GL_FALSE                           : constant Integer_4_Signed   := Integer_4_Signed(C_FALSE);
    GL_VERSION_1_1                     : constant Integer_4_Unsigned := 16#0000_0001#;
    GL_ACCUM                           : constant Integer_4_Unsigned := 16#0000_0100#;
    GL_LOAD                            : constant Integer_4_Unsigned := 16#0000_0101#;
    GL_RETURN                          : constant Integer_4_Unsigned := 16#0000_0102#;
    GL_MULT                            : constant Integer_4_Unsigned := 16#0000_0103#;
    GL_ADD                             : constant Integer_4_Unsigned := 16#0000_0104#;
    GL_NEVER                           : constant Integer_4_Unsigned := 16#0000_0200#;
    GL_LESS                            : constant Integer_4_Unsigned := 16#0000_0201#;
    GL_EQUAL                           : constant Integer_4_Unsigned := 16#0000_0202#;
    GL_LEQUAL                          : constant Integer_4_Unsigned := 16#0000_0203#;
    GL_GREATER                         : constant Integer_4_Unsigned := 16#0000_0204#;
    GL_NOTEQUAL                        : constant Integer_4_Unsigned := 16#0000_0205#;
    GL_GEQUAL                          : constant Integer_4_Unsigned := 16#0000_0206#;
    GL_ALWAYS                          : constant Integer_4_Unsigned := 16#0000_0207#;
    GL_CURRENT_BIT                     : constant Integer_4_Unsigned := 16#0000_0001#;
    GL_POINT_BIT                       : constant Integer_4_Unsigned := 16#0000_0002#;
    GL_LINE_BIT                        : constant Integer_4_Unsigned := 16#0000_0004#;
    GL_POLYGON_BIT                     : constant Integer_4_Unsigned := 16#0000_0008#;
    GL_POLYGON_STIPPLE_BIT             : constant Integer_4_Unsigned := 16#0000_0010#;
    GL_PIXEL_MODE_BIT                  : constant Integer_4_Unsigned := 16#0000_0020#;
    GL_LIGHTING_BIT                    : constant Integer_4_Unsigned := 16#0000_0040#;
    GL_FOG_BIT                         : constant Integer_4_Unsigned := 16#0000_0080#;
    GL_DEPTH_BUFFER_BIT                : constant Integer_4_Unsigned := 16#0000_0100#;
    GL_ACCUM_BUFFER_BIT                : constant Integer_4_Unsigned := 16#0000_0200#;
    GL_STENCIL_BUFFER_BIT              : constant Integer_4_Unsigned := 16#0000_0400#;
    GL_VIEWPORT_BIT                    : constant Integer_4_Unsigned := 16#0000_0800#;
    GL_TRANSFORM_BIT                   : constant Integer_4_Unsigned := 16#0000_1000#;
    GL_ENABLE_BIT                      : constant Integer_4_Unsigned := 16#0000_2000#;
    GL_COLOR_BUFFER_BIT                : constant Integer_4_Unsigned := 16#0000_4000#;
    GL_HINT_BIT                        : constant Integer_4_Unsigned := 16#0000_8000#;
    GL_EVAL_BIT                        : constant Integer_4_Unsigned := 16#0001_0000#;
    GL_LIST_BIT                        : constant Integer_4_Unsigned := 16#0002_0000#;
    GL_TEXTURE_BIT                     : constant Integer_4_Unsigned := 16#0004_0000#;
    GL_SCISSOR_BIT                     : constant Integer_4_Unsigned := 16#0008_0000#;
    GL_ALL_ATTRIB_BITS                 : constant Integer_4_Unsigned := 16#000F_FFFF#;
    GL_POINTS                          : constant Integer_4_Unsigned := 16#0000_0000#;
    GL_LINES                           : constant Integer_4_Unsigned := 16#0000_0001#;
    GL_LINE_LOOP                       : constant Integer_4_Unsigned := 16#0000_0002#;
    GL_LINE_STRIP                      : constant Integer_4_Unsigned := 16#0000_0003#;
    GL_TRIANGLES                       : constant Integer_4_Unsigned := 16#0000_0004#;
    GL_TRIANGLE_STRIP                  : constant Integer_4_Unsigned := 16#0000_0005#;
    GL_TRIANGLE_FAN                    : constant Integer_4_Unsigned := 16#0000_0006#;
    GL_QUADS                           : constant Integer_4_Unsigned := 16#0000_0007#;
    GL_QUAD_STRIP                      : constant Integer_4_Unsigned := 16#0000_0008#;
    GL_POLYGON                         : constant Integer_4_Unsigned := 16#0000_0009#;
    GL_ZERO                            : constant Integer_4_Unsigned := 16#0000_0000#;
    GL_ONE                             : constant Integer_4_Unsigned := 16#0000_0001#;
    GL_SRC_COLOR                       : constant Integer_4_Unsigned := 16#0000_0300#;
    GL_ONE_MINUS_SRC_COLOR             : constant Integer_4_Unsigned := 16#0000_0301#;
    GL_SRC_ALPHA                       : constant Integer_4_Unsigned := 16#0000_0302#;
    GL_ONE_MINUS_SRC_ALPHA             : constant Integer_4_Unsigned := 16#0000_0303#;
    GL_DST_ALPHA                       : constant Integer_4_Unsigned := 16#0000_0304#;
    GL_ONE_MINUS_DST_ALPHA             : constant Integer_4_Unsigned := 16#0000_0305#;
    GL_DST_COLOR                       : constant Integer_4_Unsigned := 16#0000_0306#;
    GL_ONE_MINUS_DST_COLOR             : constant Integer_4_Unsigned := 16#0000_0307#;
    GL_SRC_ALPHA_SATURATE              : constant Integer_4_Unsigned := 16#0000_0308#;
    GL_CLIP_PLANE0                     : constant Integer_4_Unsigned := 16#0000_3000#;
    GL_CLIP_PLANE1                     : constant Integer_4_Unsigned := 16#0000_3001#;
    GL_CLIP_PLANE2                     : constant Integer_4_Unsigned := 16#0000_3002#;
    GL_CLIP_PLANE3                     : constant Integer_4_Unsigned := 16#0000_3003#;
    GL_CLIP_PLANE4                     : constant Integer_4_Unsigned := 16#0000_3004#;
    GL_CLIP_PLANE5                     : constant Integer_4_Unsigned := 16#0000_3005#;
    GL_BYTE                            : constant Integer_4_Unsigned := 16#0000_1400#;
    GL_UNSIGNED_BYTE                   : constant Integer_4_Unsigned := 16#0000_1401#;
    GL_SHORT                           : constant Integer_4_Unsigned := 16#0000_1402#;
    GL_UNSIGNED_SHORT                  : constant Integer_4_Unsigned := 16#0000_1403#;
    GL_INT                             : constant Integer_4_Unsigned := 16#0000_1404#;
    GL_UNSIGNED_INT                    : constant Integer_4_Unsigned := 16#0000_1405#;
    GL_FLOAT                           : constant Integer_4_Unsigned := 16#0000_1406#;
    GL_2_BYTES                         : constant Integer_4_Unsigned := 16#0000_1407#;
    GL_3_BYTES                         : constant Integer_4_Unsigned := 16#0000_1408#;
    GL_4_BYTES                         : constant Integer_4_Unsigned := 16#0000_1409#;
    GL_DOUBLE                          : constant Integer_4_Unsigned := 16#0000_140A#;
    GL_NONE                            : constant Integer_4_Unsigned := 16#0000_0000#;
    GL_FRONT_LEFT                      : constant Integer_4_Unsigned := 16#0000_0400#;
    GL_FRONT_RIGHT                     : constant Integer_4_Unsigned := 16#0000_0401#;
    GL_BACK_LEFT                       : constant Integer_4_Unsigned := 16#0000_0402#;
    GL_BACK_RIGHT                      : constant Integer_4_Unsigned := 16#0000_0403#;
    GL_FRONT                           : constant Integer_4_Unsigned := 16#0000_0404#;
    GL_BACK                            : constant Integer_4_Unsigned := 16#0000_0405#;
    GL_LEFT                            : constant Integer_4_Unsigned := 16#0000_0406#;
    GL_RIGHT                           : constant Integer_4_Unsigned := 16#0000_0407#;
    GL_FRONT_AND_BACK                  : constant Integer_4_Unsigned := 16#0000_0408#;
    GL_AUX0                            : constant Integer_4_Unsigned := 16#0000_0409#;
    GL_AUX1                            : constant Integer_4_Unsigned := 16#0000_040A#;
    GL_AUX2                            : constant Integer_4_Unsigned := 16#0000_040B#;
    GL_AUX3                            : constant Integer_4_Unsigned := 16#0000_040C#;
    GL_NO_ERROR                        : constant Integer_4_Unsigned := 16#0000_0000#;
    GL_INVALID_ENUM                    : constant Integer_4_Unsigned := 16#0000_0500#;
    GL_INVALID_VALUE                   : constant Integer_4_Unsigned := 16#0000_0501#;
    GL_INVALID_OPERATION               : constant Integer_4_Unsigned := 16#0000_0502#;
    GL_STACK_OVERFLOW                  : constant Integer_4_Unsigned := 16#0000_0503#;
    GL_STACK_UNDERFLOW                 : constant Integer_4_Unsigned := 16#0000_0504#;
    GL_OUT_OF_MEMORY                   : constant Integer_4_Unsigned := 16#0000_0505#;
    GL_2D                              : constant Integer_4_Unsigned := 16#0000_0600#;
    GL_3D                              : constant Integer_4_Unsigned := 16#0000_0601#;
    GL_3D_COLOR                        : constant Integer_4_Unsigned := 16#0000_0602#;
    GL_3D_COLOR_TEXTURE                : constant Integer_4_Unsigned := 16#0000_0603#;
    GL_4D_COLOR_TEXTURE                : constant Integer_4_Unsigned := 16#0000_0604#;
    GL_PASS_THROUGH_TOKEN              : constant Integer_4_Unsigned := 16#0000_0700#;
    GL_POINT_TOKEN                     : constant Integer_4_Unsigned := 16#0000_0701#;
    GL_LINE_TOKEN                      : constant Integer_4_Unsigned := 16#0000_0702#;
    GL_POLYGON_TOKEN                   : constant Integer_4_Unsigned := 16#0000_0703#;
    GL_BITMAP_TOKEN                    : constant Integer_4_Unsigned := 16#0000_0704#;
    GL_DRAW_PIXEL_TOKEN                : constant Integer_4_Unsigned := 16#0000_0705#;
    GL_COPY_PIXEL_TOKEN                : constant Integer_4_Unsigned := 16#0000_0706#;
    GL_LINE_RESET_TOKEN                : constant Integer_4_Unsigned := 16#0000_0707#;
    GL_EXP                             : constant Integer_4_Unsigned := 16#0000_0800#;
    GL_EXP2                            : constant Integer_4_Unsigned := 16#0000_0801#;
    GL_CW                              : constant Integer_4_Unsigned := 16#0000_0900#;
    GL_CCW                             : constant Integer_4_Unsigned := 16#0000_0901#;
    GL_COEFF                           : constant Integer_4_Unsigned := 16#0000_0A00#;
    GL_ORDER                           : constant Integer_4_Unsigned := 16#0000_0A01#;
    GL_DOMAIN                          : constant Integer_4_Unsigned := 16#0000_0A02#;
    GL_CURRENT_COLOR                   : constant Integer_4_Unsigned := 16#0000_0B00#;
    GL_CURRENT_INDEX                   : constant Integer_4_Unsigned := 16#0000_0B01#;
    GL_CURRENT_NORMAL                  : constant Integer_4_Unsigned := 16#0000_0B02#;
    GL_CURRENT_TEXTURE_COORDS          : constant Integer_4_Unsigned := 16#0000_0B03#;
    GL_CURRENT_RASTER_COLOR            : constant Integer_4_Unsigned := 16#0000_0B04#;
    GL_CURRENT_RASTER_INDEX            : constant Integer_4_Unsigned := 16#0000_0B05#;
    GL_CURRENT_RASTER_TEXTURE_COORDS   : constant Integer_4_Unsigned := 16#0000_0B06#;
    GL_CURRENT_RASTER_POSITION         : constant Integer_4_Unsigned := 16#0000_0B07#;
    GL_CURRENT_RASTER_POSITION_VALID   : constant Integer_4_Unsigned := 16#0000_0B08#;
    GL_CURRENT_RASTER_DISTANCE         : constant Integer_4_Unsigned := 16#0000_0B09#;
    GL_POINT_SMOOTH                    : constant Integer_4_Unsigned := 16#0000_0B10#;
    GL_POINT_SIZE                      : constant Integer_4_Unsigned := 16#0000_0B11#;
    GL_POINT_SIZE_RANGE                : constant Integer_4_Unsigned := 16#0000_0B12#;
    GL_POINT_SIZE_GRANULARITY          : constant Integer_4_Unsigned := 16#0000_0B13#;
    GL_LINE_SMOOTH                     : constant Integer_4_Unsigned := 16#0000_0B20#;
    GL_LINE_WIDTH                      : constant Integer_4_Unsigned := 16#0000_0B21#;
    GL_LINE_WIDTH_RANGE                : constant Integer_4_Unsigned := 16#0000_0B22#;
    GL_LINE_WIDTH_GRANULARITY          : constant Integer_4_Unsigned := 16#0000_0B23#;
    GL_LINE_STIPPLE                    : constant Integer_4_Unsigned := 16#0000_0B24#;
    GL_LINE_STIPPLE_PATTERN            : constant Integer_4_Unsigned := 16#0000_0B25#;
    GL_LINE_STIPPLE_REPEAT             : constant Integer_4_Unsigned := 16#0000_0B26#;
    GL_LIST_MODE                       : constant Integer_4_Unsigned := 16#0000_0B30#;
    GL_MAX_LIST_NESTING                : constant Integer_4_Unsigned := 16#0000_0B31#;
    GL_LIST_BASE                       : constant Integer_4_Unsigned := 16#0000_0B32#;
    GL_LIST_INDEX                      : constant Integer_4_Unsigned := 16#0000_0B33#;
    GL_POLYGON_MODE                    : constant Integer_4_Unsigned := 16#0000_0B40#;
    GL_POLYGON_SMOOTH                  : constant Integer_4_Unsigned := 16#0000_0B41#;
    GL_POLYGON_STIPPLE                 : constant Integer_4_Unsigned := 16#0000_0B42#;
    GL_EDGE_FLAG                       : constant Integer_4_Unsigned := 16#0000_0B43#;
    GL_CULL_FACE                       : constant Integer_4_Unsigned := 16#0000_0B44#;
    GL_CULL_FACE_MODE                  : constant Integer_4_Unsigned := 16#0000_0B45#;
    GL_FRONT_FACE                      : constant Integer_4_Unsigned := 16#0000_0B46#;
    GL_LIGHTING                        : constant Integer_4_Unsigned := 16#0000_0B50#;
    GL_LIGHT_MODEL_LOCAL_VIEWER        : constant Integer_4_Unsigned := 16#0000_0B51#;
    GL_LIGHT_MODEL_TWO_SIDE            : constant Integer_4_Unsigned := 16#0000_0B52#;
    GL_LIGHT_MODEL_AMBIENT             : constant Integer_4_Unsigned := 16#0000_0B53#;
    GL_SHADE_MODEL                     : constant Integer_4_Unsigned := 16#0000_0B54#;
    GL_COLOR_MATERIAL_FACE             : constant Integer_4_Unsigned := 16#0000_0B55#;
    GL_COLOR_MATERIAL_PARAMETER        : constant Integer_4_Unsigned := 16#0000_0B56#;
    GL_COLOR_MATERIAL                  : constant Integer_4_Unsigned := 16#0000_0B57#;
    GL_FOG                             : constant Integer_4_Unsigned := 16#0000_0B60#;
    GL_FOG_INDEX                       : constant Integer_4_Unsigned := 16#0000_0B61#;
    GL_FOG_DENSITY                     : constant Integer_4_Unsigned := 16#0000_0B62#;
    GL_FOG_START                       : constant Integer_4_Unsigned := 16#0000_0B63#;
    GL_FOG_END                         : constant Integer_4_Unsigned := 16#0000_0B64#;
    GL_FOG_MODE                        : constant Integer_4_Unsigned := 16#0000_0B65#;
    GL_FOG_COLOR                       : constant Integer_4_Unsigned := 16#0000_0B66#;
    GL_DEPTH_RANGE                     : constant Integer_4_Unsigned := 16#0000_0B70#;
    GL_DEPTH_TEST                      : constant Integer_4_Unsigned := 16#0000_0B71#;
    GL_DEPTH_WRITEMASK                 : constant Integer_4_Unsigned := 16#0000_0B72#;
    GL_DEPTH_CLEAR_VALUE               : constant Integer_4_Unsigned := 16#0000_0B73#;
    GL_DEPTH_FUNC                      : constant Integer_4_Unsigned := 16#0000_0B74#;
    GL_ACCUM_CLEAR_VALUE               : constant Integer_4_Unsigned := 16#0000_0B80#;
    GL_STENCIL_TEST                    : constant Integer_4_Unsigned := 16#0000_0B90#;
    GL_STENCIL_CLEAR_VALUE             : constant Integer_4_Unsigned := 16#0000_0B91#;
    GL_STENCIL_FUNC                    : constant Integer_4_Unsigned := 16#0000_0B92#;
    GL_STENCIL_VALUE_MASK              : constant Integer_4_Unsigned := 16#0000_0B93#;
    GL_STENCIL_FAIL                    : constant Integer_4_Unsigned := 16#0000_0B94#;
    GL_STENCIL_PASS_DEPTH_FAIL         : constant Integer_4_Unsigned := 16#0000_0B95#;
    GL_STENCIL_PASS_DEPTH_PASS         : constant Integer_4_Unsigned := 16#0000_0B96#;
    GL_STENCIL_REF                     : constant Integer_4_Unsigned := 16#0000_0B97#;
    GL_STENCIL_WRITEMASK               : constant Integer_4_Unsigned := 16#0000_0B98#;
    GL_MATRIX_MODE                     : constant Integer_4_Unsigned := 16#0000_0BA0#;
    GL_NORMALIZE                       : constant Integer_4_Unsigned := 16#0000_0BA1#;
    GL_VIEWPORT                        : constant Integer_4_Unsigned := 16#0000_0BA2#;
    GL_MODELVIEW_STACK_DEPTH           : constant Integer_4_Unsigned := 16#0000_0BA3#;
    GL_PROJECTION_STACK_DEPTH          : constant Integer_4_Unsigned := 16#0000_0BA4#;
    GL_TEXTURE_STACK_DEPTH             : constant Integer_4_Unsigned := 16#0000_0BA5#;
    GL_MODELVIEW_MATRIX                : constant Integer_4_Unsigned := 16#0000_0BA6#;
    GL_PROJECTION_MATRIX               : constant Integer_4_Unsigned := 16#0000_0BA7#;
    GL_TEXTURE_MATRIX                  : constant Integer_4_Unsigned := 16#0000_0BA8#;
    GL_ATTRIB_STACK_DEPTH              : constant Integer_4_Unsigned := 16#0000_0BB0#;
    GL_CLIENT_ATTRIB_STACK_DEPTH       : constant Integer_4_Unsigned := 16#0000_0BB1#;
    GL_ALPHA_TEST                      : constant Integer_4_Unsigned := 16#0000_0BC0#;
    GL_ALPHA_TEST_FUNC                 : constant Integer_4_Unsigned := 16#0000_0BC1#;
    GL_ALPHA_TEST_REF                  : constant Integer_4_Unsigned := 16#0000_0BC2#;
    GL_DITHER                          : constant Integer_4_Unsigned := 16#0000_0BD0#;
    GL_BLEND_DST                       : constant Integer_4_Unsigned := 16#0000_0BE0#;
    GL_BLEND_SRC                       : constant Integer_4_Unsigned := 16#0000_0BE1#;
    GL_BLEND                           : constant Integer_4_Unsigned := 16#0000_0BE2#;
    GL_LOGIC_OP_MODE                   : constant Integer_4_Unsigned := 16#0000_0BF0#;
    GL_INDEX_LOGIC_OP                  : constant Integer_4_Unsigned := 16#0000_0BF1#;
    GL_COLOR_LOGIC_OP                  : constant Integer_4_Unsigned := 16#0000_0BF2#;
    GL_AUX_BUFFERS                     : constant Integer_4_Unsigned := 16#0000_0C00#;
    GL_DRAW_BUFFER                     : constant Integer_4_Unsigned := 16#0000_0C01#;
    GL_READ_BUFFER                     : constant Integer_4_Unsigned := 16#0000_0C02#;
    GL_SCISSOR_BOX                     : constant Integer_4_Unsigned := 16#0000_0C10#;
    GL_SCISSOR_TEST                    : constant Integer_4_Unsigned := 16#0000_0C11#;
    GL_INDEX_CLEAR_VALUE               : constant Integer_4_Unsigned := 16#0000_0C20#;
    GL_INDEX_WRITEMASK                 : constant Integer_4_Unsigned := 16#0000_0C21#;
    GL_COLOR_CLEAR_VALUE               : constant Integer_4_Unsigned := 16#0000_0C22#;
    GL_COLOR_WRITEMASK                 : constant Integer_4_Unsigned := 16#0000_0C23#;
    GL_INDEX_MODE                      : constant Integer_4_Unsigned := 16#0000_0C30#;
    GL_RGBA_MODE                       : constant Integer_4_Unsigned := 16#0000_0C31#;
    GL_DOUBLEBUFFER                    : constant Integer_4_Unsigned := 16#0000_0C32#;
    GL_STEREO                          : constant Integer_4_Unsigned := 16#0000_0C33#;
    GL_RENDER_MODE                     : constant Integer_4_Unsigned := 16#0000_0C40#;
    GL_PERSPECTIVE_CORRECTION_HINT     : constant Integer_4_Unsigned := 16#0000_0C50#;
    GL_POINT_SMOOTH_HINT               : constant Integer_4_Unsigned := 16#0000_0C51#;
    GL_LINE_SMOOTH_HINT                : constant Integer_4_Unsigned := 16#0000_0C52#;
    GL_POLYGON_SMOOTH_HINT             : constant Integer_4_Unsigned := 16#0000_0C53#;
    GL_FOG_HINT                        : constant Integer_4_Unsigned := 16#0000_0C54#;
    GL_TEXTURE_GEN_S                   : constant Integer_4_Unsigned := 16#0000_0C60#;
    GL_TEXTURE_GEN_T                   : constant Integer_4_Unsigned := 16#0000_0C61#;
    GL_TEXTURE_GEN_R                   : constant Integer_4_Unsigned := 16#0C62#;
    GL_TEXTURE_GEN_Q                   : constant Integer_4_Unsigned := 16#0C63#;
    GL_PIXEL_MAP_I_TO_I                : constant Integer_4_Unsigned := 16#0C70#;
    GL_PIXEL_MAP_S_TO_S                : constant Integer_4_Unsigned := 16#0C71#;
    GL_PIXEL_MAP_I_TO_R                : constant Integer_4_Unsigned := 16#0C72#;
    GL_PIXEL_MAP_I_TO_G                : constant Integer_4_Unsigned := 16#0C73#;
    GL_PIXEL_MAP_I_TO_B                : constant Integer_4_Unsigned := 16#0C74#;
    GL_PIXEL_MAP_I_TO_A                : constant Integer_4_Unsigned := 16#0C75#;
    GL_PIXEL_MAP_R_TO_R                : constant Integer_4_Unsigned := 16#0C76#;
    GL_PIXEL_MAP_G_TO_G                : constant Integer_4_Unsigned := 16#0C77#;
    GL_PIXEL_MAP_B_TO_B                : constant Integer_4_Unsigned := 16#0C78#;
    GL_PIXEL_MAP_A_TO_A                : constant Integer_4_Unsigned := 16#0C79#;
    GL_PIXEL_MAP_I_TO_I_SIZE           : constant Integer_4_Unsigned := 16#0CB0#;
    GL_PIXEL_MAP_S_TO_S_SIZE           : constant Integer_4_Unsigned := 16#0CB1#;
    GL_PIXEL_MAP_I_TO_R_SIZE           : constant Integer_4_Unsigned := 16#0CB2#;
    GL_PIXEL_MAP_I_TO_G_SIZE           : constant Integer_4_Unsigned := 16#0CB3#;
    GL_PIXEL_MAP_I_TO_B_SIZE           : constant Integer_4_Unsigned := 16#0CB4#;
    GL_PIXEL_MAP_I_TO_A_SIZE           : constant Integer_4_Unsigned := 16#0CB5#;
    GL_PIXEL_MAP_R_TO_R_SIZE           : constant Integer_4_Unsigned := 16#0CB6#;
    GL_PIXEL_MAP_G_TO_G_SIZE           : constant Integer_4_Unsigned := 16#0CB7#;
    GL_PIXEL_MAP_B_TO_B_SIZE           : constant Integer_4_Unsigned := 16#0CB8#;
    GL_PIXEL_MAP_A_TO_A_SIZE           : constant Integer_4_Unsigned := 16#0CB9#;
    GL_UNPACK_SWAP_BYTES               : constant Integer_4_Unsigned := 16#0CF0#;
    GL_UNPACK_LSB_FIRST                : constant Integer_4_Unsigned := 16#0CF1#;
    GL_UNPACK_ROW_LENGTH               : constant Integer_4_Unsigned := 16#0CF2#;
    GL_UNPACK_SKIP_ROWS                : constant Integer_4_Unsigned := 16#0CF3#;
    GL_UNPACK_SKIP_PIXELS              : constant Integer_4_Unsigned := 16#0CF4#;
    GL_UNPACK_ALIGNMENT                : constant Integer_4_Unsigned := 16#0CF5#;
    GL_PACK_SWAP_BYTES                 : constant Integer_4_Unsigned := 16#0D00#;
    GL_PACK_LSB_FIRST                  : constant Integer_4_Unsigned := 16#0D01#;
    GL_PACK_ROW_LENGTH                 : constant Integer_4_Unsigned := 16#0D02#;
    GL_PACK_SKIP_ROWS                  : constant Integer_4_Unsigned := 16#0D03#;
    GL_PACK_SKIP_PIXELS                : constant Integer_4_Unsigned := 16#0D04#;
    GL_PACK_ALIGNMENT                  : constant Integer_4_Unsigned := 16#0D05#;
    GL_MAP_COLOR                       : constant Integer_4_Unsigned := 16#0D10#;
    GL_MAP_STENCIL                     : constant Integer_4_Unsigned := 16#0D11#;
    GL_INDEX_SHIFT                     : constant Integer_4_Unsigned := 16#0D12#;
    GL_INDEX_OFFSET                    : constant Integer_4_Unsigned := 16#0D13#;
    GL_RED_SCALE                       : constant Integer_4_Unsigned := 16#0D14#;
    GL_RED_BIAS                        : constant Integer_4_Unsigned := 16#0D15#;
    GL_ZOOM_X                          : constant Integer_4_Unsigned := 16#0D16#;
    GL_ZOOM_Y                          : constant Integer_4_Unsigned := 16#0D17#;
    GL_GREEN_SCALE                     : constant Integer_4_Unsigned := 16#0D18#;
    GL_GREEN_BIAS                      : constant Integer_4_Unsigned := 16#0D19#;
    GL_BLUE_SCALE                      : constant Integer_4_Unsigned := 16#0D1A#;
    GL_BLUE_BIAS                       : constant Integer_4_Unsigned := 16#0D1B#;
    GL_ALPHA_SCALE                     : constant Integer_4_Unsigned := 16#0D1C#;
    GL_ALPHA_BIAS                      : constant Integer_4_Unsigned := 16#0D1D#;
    GL_DEPTH_SCALE                     : constant Integer_4_Unsigned := 16#0D1E#;
    GL_DEPTH_BIAS                      : constant Integer_4_Unsigned := 16#0D1F#;
    GL_MAX_EVAL_ORDER                  : constant Integer_4_Unsigned := 16#0D30#;
    GL_MAX_LIGHTS                      : constant Integer_4_Unsigned := 16#0D31#;
    GL_MAX_CLIP_PLANES                 : constant Integer_4_Unsigned := 16#0D32#;
    GL_MAX_TEXTURE_SIZE                : constant Integer_4_Unsigned := 16#0D33#;
    GL_MAX_PIXEL_MAP_TABLE             : constant Integer_4_Unsigned := 16#0D34#;
    GL_MAX_ATTRIB_STACK_DEPTH          : constant Integer_4_Unsigned := 16#0D35#;
    GL_MAX_MODELVIEW_STACK_DEPTH       : constant Integer_4_Unsigned := 16#0D36#;
    GL_MAX_NAME_STACK_DEPTH            : constant Integer_4_Unsigned := 16#0D37#;
    GL_MAX_PROJECTION_STACK_DEPTH      : constant Integer_4_Unsigned := 16#0D38#;
    GL_MAX_TEXTURE_STACK_DEPTH         : constant Integer_4_Unsigned := 16#0D39#;
    GL_MAX_VIEWPORT_DIMS               : constant Integer_4_Unsigned := 16#0D3A#;
    GL_MAX_CLIENT_ATTRIB_STACK_DEPTH   : constant Integer_4_Unsigned := 16#0D3B#;
    GL_SUBPIXEL_BITS                   : constant Integer_4_Unsigned := 16#0D50#;
    GL_INDEX_BITS                      : constant Integer_4_Unsigned := 16#0D51#;
    GL_RED_BITS                        : constant Integer_4_Unsigned := 16#0D52#;
    GL_GREEN_BITS                      : constant Integer_4_Unsigned := 16#0D53#;
    GL_BLUE_BITS                       : constant Integer_4_Unsigned := 16#0D54#;
    GL_ALPHA_BITS                      : constant Integer_4_Unsigned := 16#0D55#;
    GL_DEPTH_BITS                      : constant Integer_4_Unsigned := 16#0D56#;
    GL_STENCIL_BITS                    : constant Integer_4_Unsigned := 16#0D57#;
    GL_ACCUM_RED_BITS                  : constant Integer_4_Unsigned := 16#0D58#;
    GL_ACCUM_GREEN_BITS                : constant Integer_4_Unsigned := 16#0D59#;
    GL_ACCUM_BLUE_BITS                 : constant Integer_4_Unsigned := 16#0D5A#;
    GL_ACCUM_ALPHA_BITS                : constant Integer_4_Unsigned := 16#0D5B#;
    GL_NAME_STACK_DEPTH                : constant Integer_4_Unsigned := 16#0D70#;
    GL_AUTO_NORMAL                     : constant Integer_4_Unsigned := 16#0D80#;
    GL_MAP1_COLOR_4                    : constant Integer_4_Unsigned := 16#0D90#;
    GL_MAP1_INDEX                      : constant Integer_4_Unsigned := 16#0D91#;
    GL_MAP1_NORMAL                     : constant Integer_4_Unsigned := 16#0D92#;
    GL_MAP1_TEXTURE_COORD_1            : constant Integer_4_Unsigned := 16#0D93#;
    GL_MAP1_TEXTURE_COORD_2            : constant Integer_4_Unsigned := 16#0D94#;
    GL_MAP1_TEXTURE_COORD_3            : constant Integer_4_Unsigned := 16#0D95#;
    GL_MAP1_TEXTURE_COORD_4            : constant Integer_4_Unsigned := 16#0D96#;
    GL_MAP1_VERTEX_3                   : constant Integer_4_Unsigned := 16#0D97#;
    GL_MAP1_VERTEX_4                   : constant Integer_4_Unsigned := 16#0D98#;
    GL_MAP2_COLOR_4                    : constant Integer_4_Unsigned := 16#0DB0#;
    GL_MAP2_INDEX                      : constant Integer_4_Unsigned := 16#0DB1#;
    GL_MAP2_NORMAL                     : constant Integer_4_Unsigned := 16#0DB2#;
    GL_MAP2_TEXTURE_COORD_1            : constant Integer_4_Unsigned := 16#0DB3#;
    GL_MAP2_TEXTURE_COORD_2            : constant Integer_4_Unsigned := 16#0DB4#;
    GL_MAP2_TEXTURE_COORD_3            : constant Integer_4_Unsigned := 16#0DB5#;
    GL_MAP2_TEXTURE_COORD_4            : constant Integer_4_Unsigned := 16#0DB6#;
    GL_MAP2_VERTEX_3                   : constant Integer_4_Unsigned := 16#0DB7#;
    GL_MAP2_VERTEX_4                   : constant Integer_4_Unsigned := 16#0DB8#;
    GL_MAP1_GRID_DOMAIN                : constant Integer_4_Unsigned := 16#0DD0#;
    GL_MAP1_GRID_SEGMENTS              : constant Integer_4_Unsigned := 16#0DD1#;
    GL_MAP2_GRID_DOMAIN                : constant Integer_4_Unsigned := 16#0DD2#;
    GL_MAP2_GRID_SEGMENTS              : constant Integer_4_Unsigned := 16#0DD3#;
    GL_TEXTURE_1D                      : constant Integer_4_Unsigned := 16#0DE0#;
    GL_TEXTURE_2D                      : constant Integer_4_Unsigned := 16#0DE1#;
    GL_FEEDBACK_BUFFER_POINTER         : constant Integer_4_Unsigned := 16#0DF0#;
    GL_FEEDBACK_BUFFER_SIZE            : constant Integer_4_Unsigned := 16#0DF1#;
    GL_FEEDBACK_BUFFER_TYPE            : constant Integer_4_Unsigned := 16#0DF2#;
    GL_SELECTION_BUFFER_POINTER        : constant Integer_4_Unsigned := 16#0DF3#;
    GL_SELECTION_BUFFER_SIZE           : constant Integer_4_Unsigned := 16#0DF4#;
    GL_TEXTURE_WIDTH                   : constant Integer_4_Unsigned := 16#1000#;
    GL_TEXTURE_HEIGHT                  : constant Integer_4_Unsigned := 16#1001#;
    GL_TEXTURE_INTERNAL_FORMAT         : constant Integer_4_Unsigned := 16#1003#;
    GL_TEXTURE_BORDER_COLOR            : constant Integer_4_Unsigned := 16#1004#;
    GL_TEXTURE_BORDER                  : constant Integer_4_Unsigned := 16#1005#;
    GL_DONT_CARE                       : constant Integer_4_Unsigned := 16#1100#;
    GL_FASTEST                         : constant Integer_4_Unsigned := 16#1101#;
    GL_NICEST                          : constant Integer_4_Unsigned := 16#1102#;
    GL_LIGHT0                          : constant Integer_4_Unsigned := 16#4000#;
    GL_LIGHT1                          : constant Integer_4_Unsigned := 16#4001#;
    GL_LIGHT2                          : constant Integer_4_Unsigned := 16#4002#;
    GL_LIGHT3                          : constant Integer_4_Unsigned := 16#4003#;
    GL_LIGHT4                          : constant Integer_4_Unsigned := 16#4004#;
    GL_LIGHT5                          : constant Integer_4_Unsigned := 16#4005#;
    GL_LIGHT6                          : constant Integer_4_Unsigned := 16#4006#;
    GL_LIGHT7                          : constant Integer_4_Unsigned := 16#4007#;
    GL_AMBIENT                         : constant Integer_4_Unsigned := 16#1200#;
    GL_DIFFUSE                         : constant Integer_4_Unsigned := 16#1201#;
    GL_SPECULAR                        : constant Integer_4_Unsigned := 16#1202#;
    GL_POSITION                        : constant Integer_4_Unsigned := 16#1203#;
    GL_SPOT_DIRECTION                  : constant Integer_4_Unsigned := 16#1204#;
    GL_SPOT_EXPONENT                   : constant Integer_4_Unsigned := 16#1205#;
    GL_SPOT_CUTOFF                     : constant Integer_4_Unsigned := 16#1206#;
    GL_CONSTANT_ATTENUATION            : constant Integer_4_Unsigned := 16#1207#;
    GL_LINEAR_ATTENUATION              : constant Integer_4_Unsigned := 16#1208#;
    GL_QUADRATIC_ATTENUATION           : constant Integer_4_Unsigned := 16#1209#;
    GL_COMPILE                         : constant Integer_4_Unsigned := 16#1300#;
    GL_COMPILE_AND_EXECUTE             : constant Integer_4_Unsigned := 16#1301#;
    GL_CLEAR                           : constant Integer_4_Unsigned := 16#1500#;
    GL_AND                             : constant Integer_4_Unsigned := 16#1501#;
    GL_AND_REVERSE                     : constant Integer_4_Unsigned := 16#1502#;
    GL_COPY                            : constant Integer_4_Unsigned := 16#1503#;
    GL_AND_INVERTED                    : constant Integer_4_Unsigned := 16#1504#;
    GL_NOOP                            : constant Integer_4_Unsigned := 16#1505#;
    GL_XOR                             : constant Integer_4_Unsigned := 16#1506#;
    GL_OR                              : constant Integer_4_Unsigned := 16#1507#;
    GL_NOR                             : constant Integer_4_Unsigned := 16#1508#;
    GL_EQUIV                           : constant Integer_4_Unsigned := 16#1509#;
    GL_INVERT                          : constant Integer_4_Unsigned := 16#150A#;
    GL_OR_REVERSE                      : constant Integer_4_Unsigned := 16#150B#;
    GL_COPY_INVERTED                   : constant Integer_4_Unsigned := 16#150C#;
    GL_OR_INVERTED                     : constant Integer_4_Unsigned := 16#150D#;
    GL_NAND                            : constant Integer_4_Unsigned := 16#150E#;
    GL_SET                             : constant Integer_4_Unsigned := 16#150F#;
    GL_EMISSION                        : constant Integer_4_Unsigned := 16#1600#;
    GL_SHININESS                       : constant Integer_4_Unsigned := 16#1601#;
    GL_AMBIENT_AND_DIFFUSE             : constant Integer_4_Unsigned := 16#1602#;
    GL_COLOR_INDEXES                   : constant Integer_4_Unsigned := 16#1603#;
    GL_MODELVIEW                       : constant Integer_4_Unsigned := 16#1700#;
    GL_PROJECTION                      : constant Integer_4_Unsigned := 16#1701#;
    GL_TEXTURE                         : constant Integer_4_Unsigned := 16#1702#;
    GL_COLOR                           : constant Integer_4_Unsigned := 16#1800#;
    GL_DEPTH                           : constant Integer_4_Unsigned := 16#1801#;
    GL_STENCIL                         : constant Integer_4_Unsigned := 16#1802#;
    GL_COLOR_INDEX                     : constant Integer_4_Unsigned := 16#1900#;
    GL_STENCIL_INDEX                   : constant Integer_4_Unsigned := 16#1901#;
    GL_DEPTH_COMPONENT                 : constant Integer_4_Unsigned := 16#1902#;
    GL_RED                             : constant Integer_4_Unsigned := 16#1903#;
    GL_GREEN                           : constant Integer_4_Unsigned := 16#1904#;
    GL_BLUE                            : constant Integer_4_Unsigned := 16#1905#;
    GL_ALPHA                           : constant Integer_4_Unsigned := 16#1906#;
    GL_RGB                             : constant Integer_4_Unsigned := 16#1907#;
    GL_RGBA                            : constant Integer_4_Unsigned := 16#1908#;
    GL_LUMINANCE                       : constant Integer_4_Unsigned := 16#1909#;
    GL_LUMINANCE_ALPHA                 : constant Integer_4_Unsigned := 16#190A#;
    GL_BITMAP                          : constant Integer_4_Unsigned := 16#1A00#;
    GL_POINT                           : constant Integer_4_Unsigned := 16#1B00#;
    GL_LINE                            : constant Integer_4_Unsigned := 16#1B01#;
    GL_FILL                            : constant Integer_4_Unsigned := 16#1B02#;
    GL_RENDER                          : constant Integer_4_Unsigned := 16#1C00#;
    GL_FEEDBACK                        : constant Integer_4_Unsigned := 16#1C01#;
    GL_SELECT                          : constant Integer_4_Unsigned := 16#1C02#;
    GL_FLAT                            : constant Integer_4_Unsigned := 16#1D00#;
    GL_SMOOTH                          : constant Integer_4_Unsigned := 16#1D01#;
    GL_KEEP                            : constant Integer_4_Unsigned := 16#1E00#;
    GL_REPLACE                         : constant Integer_4_Unsigned := 16#1E01#;
    GL_INCR                            : constant Integer_4_Unsigned := 16#1E02#;
    GL_DECR                            : constant Integer_4_Unsigned := 16#1E03#;
    GL_VENDOR                          : constant Integer_4_Unsigned := 16#1F00#;
    GL_RENDERER                        : constant Integer_4_Unsigned := 16#1F01#;
    GL_VERSION                         : constant Integer_4_Unsigned := 16#1F02#;
    GL_EXTENSIONS                      : constant Integer_4_Unsigned := 16#1F03#;
    GL_S                               : constant Integer_4_Unsigned := 16#2000#;
    GL_T                               : constant Integer_4_Unsigned := 16#2001#;
    GL_R                               : constant Integer_4_Unsigned := 16#2002#;
    GL_Q                               : constant Integer_4_Unsigned := 16#2003#;
    GL_MODULATE                        : constant Integer_4_Unsigned := 16#2100#;
    GL_DECAL                           : constant Integer_4_Unsigned := 16#2101#;
    GL_TEXTURE_ENV_MODE                : constant Integer_4_Unsigned := 16#2200#;
    GL_TEXTURE_ENV_COLOR               : constant Integer_4_Unsigned := 16#2201#;
    GL_TEXTURE_ENV                     : constant Integer_4_Unsigned := 16#2300#;
    GL_EYE_LINEAR                      : constant Integer_4_Unsigned := 16#2400#;
    GL_OBJECT_LINEAR                   : constant Integer_4_Unsigned := 16#2401#;
    GL_SPHERE_MAP                      : constant Integer_4_Unsigned := 16#2402#;
    GL_TEXTURE_GEN_MODE                : constant Integer_4_Unsigned := 16#2500#;
    GL_OBJECT_PLANE                    : constant Integer_4_Unsigned := 16#2501#;
    GL_EYE_PLANE                       : constant Integer_4_Unsigned := 16#2502#;
    GL_NEAREST                         : constant Integer_4_Unsigned := 16#2600#;
    GL_LINEAR                          : constant Integer_4_Unsigned := 16#2601#;
    GL_NEAREST_MIPMAP_NEAREST          : constant Integer_4_Unsigned := 16#2700#;
    GL_LINEAR_MIPMAP_NEAREST           : constant Integer_4_Unsigned := 16#2701#;
    GL_NEAREST_MIPMAP_LINEAR           : constant Integer_4_Unsigned := 16#2702#;
    GL_LINEAR_MIPMAP_LINEAR            : constant Integer_4_Unsigned := 16#2703#;
    GL_TEXTURE_MAG_FILTER              : constant Integer_4_Unsigned := 16#2800#;
    GL_TEXTURE_MIN_FILTER              : constant Integer_4_Unsigned := 16#2801#;
    GL_TEXTURE_WRAP_S                  : constant Integer_4_Unsigned := 16#2802#;
    GL_TEXTURE_WRAP_T                  : constant Integer_4_Unsigned := 16#2803#;
    GL_CLAMP                           : constant Integer_4_Unsigned := 16#2900#;
    GL_REPEAT                          : constant Integer_4_Unsigned := 16#2901#;
    GL_CLIENT_PIXEL_STORE_BIT          : constant Integer_4_Unsigned := 16#0000_0001#;
    GL_CLIENT_VERTEX_ARRAY_BIT         : constant Integer_4_Unsigned := 16#0000_0002#;
    GL_CLIENT_ALL_ATTRIB_BITS          : constant Integer_4_Unsigned := 16#FFFF_FFFF#;
    GL_POLYGON_OFFSET_FACTOR           : constant Integer_4_Unsigned := 16#8038#;
    GL_POLYGON_OFFSET_UNITS            : constant Integer_4_Unsigned := 16#2A00#;
    GL_POLYGON_OFFSET_POINT            : constant Integer_4_Unsigned := 16#2A01#;
    GL_POLYGON_OFFSET_LINE             : constant Integer_4_Unsigned := 16#2A02#;
    GL_POLYGON_OFFSET_FILL             : constant Integer_4_Unsigned := 16#8037#;
    GL_ALPHA4                          : constant Integer_4_Unsigned := 16#803B#;
    GL_ALPHA8                          : constant Integer_4_Unsigned := 16#803C#;
    GL_ALPHA12                         : constant Integer_4_Unsigned := 16#803D#;
    GL_ALPHA16                         : constant Integer_4_Unsigned := 16#803E#;
    GL_LUMINANCE4                      : constant Integer_4_Unsigned := 16#803F#;
    GL_LUMINANCE8                      : constant Integer_4_Unsigned := 16#8040#;
    GL_LUMINANCE12                     : constant Integer_4_Unsigned := 16#8041#;
    GL_LUMINANCE16                     : constant Integer_4_Unsigned := 16#8042#;
    GL_LUMINANCE4_ALPHA4               : constant Integer_4_Unsigned := 16#8043#;
    GL_LUMINANCE6_ALPHA2               : constant Integer_4_Unsigned := 16#8044#;
    GL_LUMINANCE8_ALPHA8               : constant Integer_4_Unsigned := 16#8045#;
    GL_LUMINANCE12_ALPHA4              : constant Integer_4_Unsigned := 16#8046#;
    GL_LUMINANCE12_ALPHA12             : constant Integer_4_Unsigned := 16#8047#;
    GL_LUMINANCE16_ALPHA16             : constant Integer_4_Unsigned := 16#8048#;
    GL_INTENSITY                       : constant Integer_4_Unsigned := 16#8049#;
    GL_INTENSITY4                      : constant Integer_4_Unsigned := 16#804A#;
    GL_INTENSITY8                      : constant Integer_4_Unsigned := 16#804B#;
    GL_INTENSITY12                     : constant Integer_4_Unsigned := 16#804C#;
    GL_INTENSITY16                     : constant Integer_4_Unsigned := 16#804D#;
    GL_R3_G3_B2                        : constant Integer_4_Unsigned := 16#2A10#;
    GL_RGB4                            : constant Integer_4_Unsigned := 16#804F#;
    GL_RGB5                            : constant Integer_4_Unsigned := 16#8050#;
    GL_RGB8                            : constant Integer_4_Unsigned := 16#8051#;
    GL_RGB10                           : constant Integer_4_Unsigned := 16#8052#;
    GL_RGB12                           : constant Integer_4_Unsigned := 16#8053#;
    GL_RGB16                           : constant Integer_4_Unsigned := 16#8054#;
    GL_RGBA2                           : constant Integer_4_Unsigned := 16#8055#;
    GL_RGBA4                           : constant Integer_4_Unsigned := 16#8056#;
    GL_RGB5_A1                         : constant Integer_4_Unsigned := 16#8057#;
    GL_RGBA8                           : constant Integer_4_Unsigned := 16#8058#;
    GL_RGB10_A2                        : constant Integer_4_Unsigned := 16#8059#;
    GL_RGBA12                          : constant Integer_4_Unsigned := 16#805A#;
    GL_RGBA16                          : constant Integer_4_Unsigned := 16#805B#;
    GL_TEXTURE_RED_SIZE                : constant Integer_4_Unsigned := 16#805C#;
    GL_TEXTURE_GREEN_SIZE              : constant Integer_4_Unsigned := 16#805D#;
    GL_TEXTURE_BLUE_SIZE               : constant Integer_4_Unsigned := 16#805E#;
    GL_TEXTURE_ALPHA_SIZE              : constant Integer_4_Unsigned := 16#805F#;
    GL_TEXTURE_LUMINANCE_SIZE          : constant Integer_4_Unsigned := 16#8060#;
    GL_TEXTURE_INTENSITY_SIZE          : constant Integer_4_Unsigned := 16#8061#;
    GL_PROXY_TEXTURE_1D                : constant Integer_4_Unsigned := 16#8063#;
    GL_PROXY_TEXTURE_2D                : constant Integer_4_Unsigned := 16#8064#;
    GL_TEXTURE_PRIORITY                : constant Integer_4_Unsigned := 16#8066#;
    GL_TEXTURE_RESIDENT                : constant Integer_4_Unsigned := 16#8067#;
    GL_TEXTURE_BINDING_1D              : constant Integer_4_Unsigned := 16#8068#;
    GL_TEXTURE_BINDING_2D              : constant Integer_4_Unsigned := 16#8069#;
    GL_VERTEX_ARRAY                    : constant Integer_4_Unsigned := 16#8074#;
    GL_NORMAL_ARRAY                    : constant Integer_4_Unsigned := 16#8075#;
    GL_COLOR_ARRAY                     : constant Integer_4_Unsigned := 16#8076#;
    GL_INDEX_ARRAY                     : constant Integer_4_Unsigned := 16#8077#;
    GL_TEXTURE_COORD_ARRAY             : constant Integer_4_Unsigned := 16#8078#;
    GL_EDGE_FLAG_ARRAY                 : constant Integer_4_Unsigned := 16#8079#;
    GL_VERTEX_ARRAY_SIZE               : constant Integer_4_Unsigned := 16#807A#;
    GL_VERTEX_ARRAY_TYPE               : constant Integer_4_Unsigned := 16#807B#;
    GL_VERTEX_ARRAY_STRIDE             : constant Integer_4_Unsigned := 16#807C#;
    GL_NORMAL_ARRAY_TYPE               : constant Integer_4_Unsigned := 16#807E#;
    GL_NORMAL_ARRAY_STRIDE             : constant Integer_4_Unsigned := 16#807F#;
    GL_COLOR_ARRAY_SIZE                : constant Integer_4_Unsigned := 16#8081#;
    GL_COLOR_ARRAY_TYPE                : constant Integer_4_Unsigned := 16#8082#;
    GL_COLOR_ARRAY_STRIDE              : constant Integer_4_Unsigned := 16#8083#;
    GL_INDEX_ARRAY_TYPE                : constant Integer_4_Unsigned := 16#8085#;
    GL_INDEX_ARRAY_STRIDE              : constant Integer_4_Unsigned := 16#8086#;
    GL_TEXTURE_COORD_ARRAY_SIZE        : constant Integer_4_Unsigned := 16#8088#;
    GL_TEXTURE_COORD_ARRAY_TYPE        : constant Integer_4_Unsigned := 16#8089#;
    GL_TEXTURE_COORD_ARRAY_STRIDE      : constant Integer_4_Unsigned := 16#808A#;
    GL_EDGE_FLAG_ARRAY_STRIDE          : constant Integer_4_Unsigned := 16#808C#;
    GL_VERTEX_ARRAY_POINTER            : constant Integer_4_Unsigned := 16#808E#;
    GL_NORMAL_ARRAY_POINTER            : constant Integer_4_Unsigned := 16#808F#;
    GL_COLOR_ARRAY_POINTER             : constant Integer_4_Unsigned := 16#8090#;
    GL_INDEX_ARRAY_POINTER             : constant Integer_4_Unsigned := 16#8091#;
    GL_TEXTURE_COORD_ARRAY_POINTER     : constant Integer_4_Unsigned := 16#8092#;
    GL_EDGE_FLAG_ARRAY_POINTER         : constant Integer_4_Unsigned := 16#8093#;
    GL_V2F                             : constant Integer_4_Unsigned := 16#2A20#;
    GL_V3F                             : constant Integer_4_Unsigned := 16#2A21#;
    GL_C4UB_V2F                        : constant Integer_4_Unsigned := 16#2A22#;
    GL_C4UB_V3F                        : constant Integer_4_Unsigned := 16#2A23#;
    GL_C3F_V3F                         : constant Integer_4_Unsigned := 16#2A24#;
    GL_N3F_V3F                         : constant Integer_4_Unsigned := 16#2A25#;
    GL_C4F_N3F_V3F                     : constant Integer_4_Unsigned := 16#2A26#;
    GL_T2F_V3F                         : constant Integer_4_Unsigned := 16#2A27#;
    GL_T4F_V4F                         : constant Integer_4_Unsigned := 16#2A28#;
    GL_T2F_C4UB_V3F                    : constant Integer_4_Unsigned := 16#2A29#;
    GL_T2F_C3F_V3F                     : constant Integer_4_Unsigned := 16#2A2A#;
    GL_T2F_N3F_V3F                     : constant Integer_4_Unsigned := 16#2A2B#;
    GL_T2F_C4F_N3F_V3F                 : constant Integer_4_Unsigned := 16#2A2C#;
    GL_T4F_C4F_N3F_V4F                 : constant Integer_4_Unsigned := 16#2A2D#;
    GL_EXT_vertex_array                : constant Integer_4_Unsigned := 1;
    GL_EXT_bgra                        : constant Integer_4_Unsigned := 1;
    GL_EXT_paletted_texture            : constant Integer_4_Unsigned := 1;
    GL_WIN_swap_hint                   : constant Integer_4_Unsigned := 1;
    GL_WIN_draw_range_elements         : constant Integer_4_Unsigned := 1;
    GL_VERTEX_ARRAY_EXT                : constant Integer_4_Unsigned := 16#8074#;
    GL_NORMAL_ARRAY_EXT                : constant Integer_4_Unsigned := 16#8075#;
    GL_COLOR_ARRAY_EXT                 : constant Integer_4_Unsigned := 16#8076#;
    GL_INDEX_ARRAY_EXT                 : constant Integer_4_Unsigned := 16#8077#;
    GL_TEXTURE_COORD_ARRAY_EXT         : constant Integer_4_Unsigned := 16#8078#;
    GL_EDGE_FLAG_ARRAY_EXT             : constant Integer_4_Unsigned := 16#8079#;
    GL_VERTEX_ARRAY_SIZE_EXT           : constant Integer_4_Unsigned := 16#807A#;
    GL_VERTEX_ARRAY_TYPE_EXT           : constant Integer_4_Unsigned := 16#807B#;
    GL_VERTEX_ARRAY_STRIDE_EXT         : constant Integer_4_Unsigned := 16#807C#;
    GL_VERTEX_ARRAY_COUNT_EXT          : constant Integer_4_Unsigned := 16#807D#;
    GL_NORMAL_ARRAY_TYPE_EXT           : constant Integer_4_Unsigned := 16#807E#;
    GL_NORMAL_ARRAY_STRIDE_EXT         : constant Integer_4_Unsigned := 16#807F#;
    GL_NORMAL_ARRAY_COUNT_EXT          : constant Integer_4_Unsigned := 16#8080#;
    GL_COLOR_ARRAY_SIZE_EXT            : constant Integer_4_Unsigned := 16#8081#;
    GL_COLOR_ARRAY_TYPE_EXT            : constant Integer_4_Unsigned := 16#8082#;
    GL_COLOR_ARRAY_STRIDE_EXT          : constant Integer_4_Unsigned := 16#8083#;
    GL_COLOR_ARRAY_COUNT_EXT           : constant Integer_4_Unsigned := 16#8084#;
    GL_INDEX_ARRAY_TYPE_EXT            : constant Integer_4_Unsigned := 16#8085#;
    GL_INDEX_ARRAY_STRIDE_EXT          : constant Integer_4_Unsigned := 16#8086#;
    GL_INDEX_ARRAY_COUNT_EXT           : constant Integer_4_Unsigned := 16#8087#;
    GL_TEXTURE_COORD_ARRAY_SIZE_EXT    : constant Integer_4_Unsigned := 16#8088#;
    GL_TEXTURE_COORD_ARRAY_TYPE_EXT    : constant Integer_4_Unsigned := 16#8089#;
    GL_TEXTURE_COORD_ARRAY_STRIDE_EXT  : constant Integer_4_Unsigned := 16#808A#;
    GL_TEXTURE_COORD_ARRAY_COUNT_EXT   : constant Integer_4_Unsigned := 16#808B#;
    GL_EDGE_FLAG_ARRAY_STRIDE_EXT      : constant Integer_4_Unsigned := 16#808C#;
    GL_EDGE_FLAG_ARRAY_COUNT_EXT       : constant Integer_4_Unsigned := 16#808D#;
    GL_VERTEX_ARRAY_POINTER_EXT        : constant Integer_4_Unsigned := 16#808E#;
    GL_NORMAL_ARRAY_POINTER_EXT        : constant Integer_4_Unsigned := 16#808F#;
    GL_COLOR_ARRAY_POINTER_EXT         : constant Integer_4_Unsigned := 16#8090#;
    GL_INDEX_ARRAY_POINTER_EXT         : constant Integer_4_Unsigned := 16#8091#;
    GL_TEXTURE_COORD_ARRAY_POINTER_EXT : constant Integer_4_Unsigned := 16#8092#;
    GL_EDGE_FLAG_ARRAY_POINTER_EXT     : constant Integer_4_Unsigned := 16#8093#;
    GL_DOUBLE_EXT                      : constant Integer_4_Unsigned := GL_DOUBLE;
    GL_BGR_EXT                         : constant Integer_4_Unsigned := 16#80E0#;
    GL_BGRA_EXT                        : constant Integer_4_Unsigned := 16#80E1#;
    GL_COLOR_TABLE_FORMAT_EXT          : constant Integer_4_Unsigned := 16#80D8#;
    GL_COLOR_TABLE_WIDTH_EXT           : constant Integer_4_Unsigned := 16#80D9#;
    GL_COLOR_TABLE_RED_SIZE_EXT        : constant Integer_4_Unsigned := 16#80DA#;
    GL_COLOR_TABLE_GREEN_SIZE_EXT      : constant Integer_4_Unsigned := 16#80DB#;
    GL_COLOR_TABLE_BLUE_SIZE_EXT       : constant Integer_4_Unsigned := 16#80DC#;
    GL_COLOR_TABLE_ALPHA_SIZE_EXT      : constant Integer_4_Unsigned := 16#80DD#;
    GL_COLOR_TABLE_LUMINANCE_SIZE_EXT  : constant Integer_4_Unsigned := 16#80DE#;
    GL_COLOR_TABLE_INTENSITY_SIZE_EXT  : constant Integer_4_Unsigned := 16#80DF#;
    GL_COLOR_INDEX1_EXT                : constant Integer_4_Unsigned := 16#80E2#;
    GL_COLOR_INDEX2_EXT                : constant Integer_4_Unsigned := 16#80E3#;
    GL_COLOR_INDEX4_EXT                : constant Integer_4_Unsigned := 16#80E4#;
    GL_COLOR_INDEX8_EXT                : constant Integer_4_Unsigned := 16#80E5#;
    GL_COLOR_INDEX12_EXT               : constant Integer_4_Unsigned := 16#80E6#;
    GL_COLOR_INDEX16_EXT               : constant Integer_4_Unsigned := 16#80E7#;
    GL_MAX_ELEMENTS_VERTICES_WIN       : constant Integer_4_Unsigned := 16#80E8#;
    GL_MAX_ELEMENTS_INDICES_WIN        : constant Integer_4_Unsigned := 16#80E9#;
    GL_PHONG_WIN                       : constant Integer_4_Unsigned := 16#80EA#;
    GL_PHONG_HINT_WIN                  : constant Integer_4_Unsigned := 16#80EB#;
    GL_FOG_SPECULAR_TEXTURE_WIN        : constant Integer_4_Unsigned := 16#80EC#;
    GL_LOGIC_OP                        : constant Integer_4_Unsigned := GL_INDEX_LOGIC_OP;
    GL_TEXTURE_COMPONENTS              : constant Integer_4_Unsigned := GL_TEXTURE_INTERNAL_FORMAT;
  ---------------
  -- Accessors --
  ---------------
    glAccum                  : Access_Procedure_ := null;
    glAlphaFunc              : Access_Procedure_ := null;
    glAreTexturesResident    : Access_Procedure_ := null;
    glArrayElement           : Access_Procedure_ := null;
    glBegin                  : Access_Procedure_ := null;
    glBindTexture            : Access_Procedure_ := null;
    glBitmap                 : Access_Procedure_ := null;
    glBlendFunc              : Access_Procedure_ := null;
    glCallList               : Access_Procedure_ := null;
    glCallLists              : Access_Procedure_ := null;
    glClear                  : Access_Procedure_ := null;
    glClearAccum             : Access_Procedure_ := null;
    glClearColor             : Access_Procedure_ := null;
    glClearDepth             : Access_Procedure_ := null;
    glClearIndex             : Access_Procedure_ := null;
    glClearStencil           : Access_Procedure_ := null;
    glClipPlane              : Access_Procedure_ := null;
    glColor3b                : Access_Procedure_ := null;
    glColor3bv               : Access_Procedure_ := null;
    glColor3d                : Access_Procedure_ := null;
    glColor3dv               : Access_Procedure_ := null;
    glColor3f                : Access_Procedure_ := null;
    glColor3fv               : Access_Procedure_ := null;
    glColor3i                : Access_Procedure_ := null;
    glColor3iv               : Access_Procedure_ := null;
    glColor3s                : Access_Procedure_ := null;
    glColor3sv               : Access_Procedure_ := null;
    glColor3ub               : Access_Procedure_ := null;
    glColor3ubv              : Access_Procedure_ := null;
    glColor3ui               : Access_Procedure_ := null;
    glColor3uiv              : Access_Procedure_ := null;
    glColor3us               : Access_Procedure_ := null;
    glColor3usv              : Access_Procedure_ := null;
    glColor4b                : Access_Procedure_ := null;
    glColor4bv               : Access_Procedure_ := null;
    glColor4d                : Access_Procedure_ := null;
    glColor4dv               : Access_Procedure_ := null;
    glColor4f                : Access_Procedure_ := null;
    glColor4fv               : Access_Procedure_ := null;
    glColor4i                : Access_Procedure_ := null;
    glColor4iv               : Access_Procedure_ := null;
    glColor4s                : Access_Procedure_ := null;
    glColor4sv               : Access_Procedure_ := null;
    glColor4ub               : Access_Procedure_ := null;
    glColor4ubv              : Access_Procedure_ := null;
    glColor4ui               : Access_Procedure_ := null;
    glColor4uiv              : Access_Procedure_ := null;
    glColor4us               : Access_Procedure_ := null;
    glColor4usv              : Access_Procedure_ := null;
    glColorMask              : Access_Procedure_ := null;
    glColorMaterial          : Access_Procedure_ := null;
    glColorPointer           : Access_Procedure_ := null;
    glCopyPixels             : Access_Procedure_ := null;
    glCopyTexImage1D         : Access_Procedure_ := null;
    glCopyTexImage2D         : Access_Procedure_ := null;
    glCopyTexSubImage1D      : Access_Procedure_ := null;
    glCopyTexSubImage2D      : Access_Procedure_ := null;
    glCullFace               : Access_Procedure_ := null;
    glDeleteLists            : Access_Procedure_ := null;
    glDeleteTextures         : Access_Procedure_ := null;
    glDepthFunc              : Access_Procedure_ := null;
    glDepthMask              : Access_Procedure_ := null;
    glDepthRange             : Access_Procedure_ := null;
    glDisable                : Access_Procedure_ := null;
    glDisableClientState     : Access_Procedure_ := null;
    glDrawArrays             : Access_Procedure_ := null;
    glDrawBuffer             : Access_Procedure_ := null;
    glDrawElements           : Access_Procedure_ := null;
    glDrawPixels             : Access_Procedure_ := null;
    glEdgeFlag               : Access_Procedure_ := null;
    glEdgeFlagPointer        : Access_Procedure_ := null;
    glEdgeFlagv              : Access_Procedure_ := null;
    glEnable                 : Access_Procedure_ := null;
    glEnableClientState      : Access_Procedure_ := null;
    glEnd                    : Access_Procedure_ := null;
    glEndList                : Access_Procedure_ := null;
    glEvalCoord1d            : Access_Procedure_ := null;
    glEvalCoord1dv           : Access_Procedure_ := null;
    glEvalCoord1f            : Access_Procedure_ := null;
    glEvalCoord1fv           : Access_Procedure_ := null;
    glEvalCoord2d            : Access_Procedure_ := null;
    glEvalCoord2dv           : Access_Procedure_ := null;
    glEvalCoord2f            : Access_Procedure_ := null;
    glEvalCoord2fv           : Access_Procedure_ := null;
    glEvalMesh1              : Access_Procedure_ := null;
    glEvalMesh2              : Access_Procedure_ := null;
    glEvalPoint1             : Access_Procedure_ := null;
    glEvalPoint2             : Access_Procedure_ := null;
    glFeedbackBuffer         : Access_Procedure_ := null;
    glFinish                 : Access_Procedure_ := null;
    glFlush                  : Access_Procedure_ := null;
    glFogf                   : Access_Procedure_ := null;
    glFogfv                  : Access_Procedure_ := null;
    glFogi                   : Access_Procedure_ := null;
    glFogiv                  : Access_Procedure_ := null;
    glFrontFace              : Access_Procedure_ := null;
    glFrustum                : Access_Procedure_ := null;
    glGenLists               : Access_Procedure_ := null;
    glGenTextures            : Access_Procedure_ := null;
    glGetBooleanv            : Access_Procedure_ := null;
    glGetClipPlane           : Access_Procedure_ := null;
    glGetDoublev             : Access_Procedure_ := null;
    glGetError               : Access_Procedure_ := null;
    glGetFloatv              : Access_Procedure_ := null;
    glGetIntegerv            : Access_Procedure_ := null;
    glGetLightfv             : Access_Procedure_ := null;
    glGetLightiv             : Access_Procedure_ := null;
    glGetMapdv               : Access_Procedure_ := null;
    glGetMapfv               : Access_Procedure_ := null;
    glGetMapiv               : Access_Procedure_ := null;
    glGetMaterialfv          : Access_Procedure_ := null;
    glGetMaterialiv          : Access_Procedure_ := null;
    glGetPixelMapfv          : Access_Procedure_ := null;
    glGetPixelMapuiv         : Access_Procedure_ := null;
    glGetPixelMapusv         : Access_Procedure_ := null;
    glGetPointerb            : Access_Procedure_ := null;
    glGetPolygonStipple      : Access_Procedure_ := null;
    glGetString              : Access_Procedure_ := null;
    glGetTexEnvfv            : Access_Procedure_ := null;
    glGetTexEnviv            : Access_Procedure_ := null;
    glGetTexGendv            : Access_Procedure_ := null;
    glGetTexGenfv            : Access_Procedure_ := null;
    glGetTexGeniv            : Access_Procedure_ := null;
    glGetTexImage            : Access_Procedure_ := null;
    glGetTexLevelParameterfv : Access_Procedure_ := null;
    glGetTexLevelParameteriv : Access_Procedure_ := null;
    glGetTexParameterfv      : Access_Procedure_ := null;
    glGetTexParameteriv      : Access_Procedure_ := null;
    glHint                   : Access_Procedure_ := null;
    glIndexMask              : Access_Procedure_ := null;
    glIndexPointer           : Access_Procedure_ := null;
    glIndexd                 : Access_Procedure_ := null;
    glIndexdv                : Access_Procedure_ := null;
    glIndexf                 : Access_Procedure_ := null;
    glIndexfv                : Access_Procedure_ := null;
    glIndexi                 : Access_Procedure_ := null;
    glIndexiv                : Access_Procedure_ := null;
    glIndexs                 : Access_Procedure_ := null;
    glIndexsv                : Access_Procedure_ := null;
    glIndexub                : Access_Procedure_ := null;
    glIndexubv               : Access_Procedure_ := null;
    glInitNames              : Access_Procedure_ := null;
    glInterleavedArrays      : Access_Procedure_ := null;
    glIsEnabled              : Access_Procedure_ := null;
    glIsList                 : Access_Procedure_ := null;
    glIsTexture              : Access_Procedure_ := null;
    glLightModelf            : Access_Procedure_ := null;
    glLightModelfv           : Access_Procedure_ := null;
    glLightModeli            : Access_Procedure_ := null;
    glLightModeliv           : Access_Procedure_ := null;
    glLightf                 : Access_Procedure_ := null;
    glLightfv                : Access_Procedure_ := null;
    glLighti                 : Access_Procedure_ := null;
    glLightiv                : Access_Procedure_ := null;
    glLineStipple            : Access_Procedure_ := null;
    glLineWidth              : Access_Procedure_ := null;
    glListBase               : Access_Procedure_ := null;
    glLoadIdentity           : Access_Procedure_ := null;
    glLoadMatrixd            : Access_Procedure_ := null;
    glLoadMatrixf            : Access_Procedure_ := null;
    glLoadName               : Access_Procedure_ := null;
    glLogicOp                : Access_Procedure_ := null;
    glMap1D                  : Access_Procedure_ := null;
    glMap1f                  : Access_Procedure_ := null;
    glMap2D                  : Access_Procedure_ := null;
    glMap2f                  : Access_Procedure_ := null;
    glMapGrid1d              : Access_Procedure_ := null;
    glMapGrid1f              : Access_Procedure_ := null;
    glMapGrid2d              : Access_Procedure_ := null;
    glMapGrid2f              : Access_Procedure_ := null;
    glMaterialf              : Access_Procedure_ := null;
    glMaterialfv             : Access_Procedure_ := null;
    glMateriali              : Access_Procedure_ := null;
    glMaterialiv             : Access_Procedure_ := null;
    glMatrixMode             : Access_Procedure_ := null;
    glMultMatrixd            : Access_Procedure_ := null;
    glMultMatrixf            : Access_Procedure_ := null;
    glNewList                : Access_Procedure_ := null;
    glNormal3b               : Access_Procedure_ := null;
    glNormal3Bv              : Access_Procedure_ := null;
    glNormal3D               : Access_Procedure_ := null;
    glNormal3Dv              : Access_Procedure_ := null;
    glNormal3f               : Access_Procedure_ := null;
    glNormal3fv              : Access_Procedure_ := null;
    glNormal3i               : Access_Procedure_ := null;
    glNormal3iv              : Access_Procedure_ := null;
    glNormal3s               : Access_Procedure_ := null;
    glNormal3sv              : Access_Procedure_ := null;
    glNormalPointer          : Access_Procedure_ := null;
    glOrtho                  : Access_Procedure_ := null;
    glPassThrough            : Access_Procedure_ := null;
    glPixelMapfv             : Access_Procedure_ := null;
    glPixelMapuiv            : Access_Procedure_ := null;
    glPixelMapusv            : Access_Procedure_ := null;
    glPixelStoref            : Access_Procedure_ := null;
    glPixelStorei            : Access_Procedure_ := null;
    glPixelTransferf         : Access_Procedure_ := null;
    glPixelTransferi         : Access_Procedure_ := null;
    glPixelZoom              : Access_Procedure_ := null;
    glPointSize              : Access_Procedure_ := null;
    glPolygonMode            : Access_Procedure_ := null;
    glPolygonOffset          : Access_Procedure_ := null;
    glPolygonStipple         : Access_Procedure_ := null;
    glPopAttrib              : Access_Procedure_ := null;
    glPopClientAttrib        : Access_Procedure_ := null;
    glPopMatrix              : Access_Procedure_ := null;
    glPopName                : Access_Procedure_ := null;
    glPrioritizeTextures     : Access_Procedure_ := null;
    glPushAttrib             : Access_Procedure_ := null;
    glPushClientAttrib       : Access_Procedure_ := null;
    glPushMatrix             : Access_Procedure_ := null;
    glPushName               : Access_Procedure_ := null;
    glRasterPos2D            : Access_Procedure_ := null;
    glRasterPos2Dv           : Access_Procedure_ := null;
    glRasterPos2f            : Access_Procedure_ := null;
    glRasterPos2fv           : Access_Procedure_ := null;
    glRasterPos2i            : Access_Procedure_ := null;
    glRasterPos2iv           : Access_Procedure_ := null;
    glRasterPos2s            : Access_Procedure_ := null;
    glRasterPos2sv           : Access_Procedure_ := null;
    glRasterPos3d            : Access_Procedure_ := null;
    glRasterPos3dv           : Access_Procedure_ := null;
    glRasterPos3f            : Access_Procedure_ := null;
    glRasterPos3fv           : Access_Procedure_ := null;
    glRasterPos3i            : Access_Procedure_ := null;
    glRasterPos3iv           : Access_Procedure_ := null;
    glRasterPos3s            : Access_Procedure_ := null;
    glRasterPos3sv           : Access_Procedure_ := null;
    glRasterPos4d            : Access_Procedure_ := null;
    glRasterPos4dv           : Access_Procedure_ := null;
    glRasterPos4f            : Access_Procedure_ := null;
    glRasterPos4fv           : Access_Procedure_ := null;
    glRasterPos4i            : Access_Procedure_ := null;
    glRasterPos4iv           : Access_Procedure_ := null;
    glRasterPos4s            : Access_Procedure_ := null;
    glRasterPos4sv           : Access_Procedure_ := null;
    glReadBuffer             : Access_Procedure_ := null;
    glReadPixels             : Access_Procedure_ := null;
    glRectd                  : Access_Procedure_ := null;
    glRectdv                 : Access_Procedure_ := null;
    glRectf                  : Access_Procedure_ := null;
    glRectfv                 : Access_Procedure_ := null;
    glRecti                  : Access_Procedure_ := null;
    glRectiv                 : Access_Procedure_ := null;
    glRects                  : Access_Procedure_ := null;
    glRectsv                 : Access_Procedure_ := null;
    glRenderMode             : Access_Procedure_ := null;
    glRotated                : Access_Procedure_ := null;
    glRotatef                : Access_Procedure_ := null;
    glScaled                 : Access_Procedure_ := null;
    glScalef                 : Access_Procedure_ := null;
    glScissor                : Access_Procedure_ := null;
    glSelectBuffer           : Access_Procedure_ := null;
    glShadeModel             : Access_Procedure_ := null;
    glStencilFunc            : Access_Procedure_ := null;
    glStencilMask            : Access_Procedure_ := null;
    glStencilOp              : Access_Procedure_ := null;
    glTexCoord1d             : Access_Procedure_ := null;
    glTexCoord1dv            : Access_Procedure_ := null;
    glTexCoord1f             : Access_Procedure_ := null;
    glTexCoord1fv            : Access_Procedure_ := null;
    glTexCoord1i             : Access_Procedure_ := null;
    glTexCoord1iv            : Access_Procedure_ := null;
    glTexCoord1s             : Access_Procedure_ := null;
    glTexCoord1sv            : Access_Procedure_ := null;
    glTexCoord2d             : Access_Procedure_ := null;
    glTexCoord2dv            : Access_Procedure_ := null;
    glTexCoord2f             : Access_Procedure_ := null;
    glTexCoord2fv            : Access_Procedure_ := null;
    glTexCoord2i             : Access_Procedure_ := null;
    glTexCoord2iv            : Access_Procedure_ := null;
    glTexCoord2s             : Access_Procedure_ := null;
    glTexCoord2sv            : Access_Procedure_ := null;
    glTexCoord3d             : Access_Procedure_ := null;
    glTexCoord3dv            : Access_Procedure_ := null;
    glTexCoord3f             : Access_Procedure_ := null;
    glTexCoord3fv            : Access_Procedure_ := null;
    glTexCoord3i             : Access_Procedure_ := null;
    glTexCoord3iv            : Access_Procedure_ := null;
    glTexCoord3s             : Access_Procedure_ := null;
    glTexCoord3dv            : Access_Procedure_ := null;
    glTexCoord4d             : Access_Procedure_ := null;
    glTexCoord4dv            : Access_Procedure_ := null;
    glTexCoord4f             : Access_Procedure_ := null;
    glTexCoord4fv            : Access_Procedure_ := null;
    glTexCoord4i             : Access_Procedure_ := null;
    glTexCoord4iv            : Access_Procedure_ := null;
    glTexCoord4d             : Access_Procedure_ := null;
    glTexCoord4dv            : Access_Procedure_ := null;
    glTexCoordPointer        : Access_Procedure_ := null;
    glTexEnvf                : Access_Procedure_ := null;
    glTexEnvfv               : Access_Procedure_ := null;
    glTexEnvi                : Access_Procedure_ := null;
    glTexEnviv               : Access_Procedure_ := null;
    glTexGend                : Access_Procedure_ := null;
    glTexGendv               : Access_Procedure_ := null;
    glTexGenf                : Access_Procedure_ := null;
    glTexGenfv               : Access_Procedure_ := null;
    glTexGeni                : Access_Procedure_ := null;
    glTexGeniv               : Access_Procedure_ := null;
    glTexImage1D             : Access_Procedure_ := null;
    glTexImage2D             : Access_Procedure_ := null;
    glTexParameterf          : Access_Procedure_ := null;
    glTexParameterfv         : Access_Procedure_ := null;
    glTexParameteri          : Access_Procedure_ := null;
    glTexParameteriv         : Access_Procedure_ := null;
    glTexSubImage1D          : Access_Procedure_ := null;
    glTexSubImage2D          : Access_Procedure_ := null;
    glTranslated             : Access_Procedure_ := null;
    glTranslatef             : Access_Procedure_ := null;
    glVertex2d               : Access_Procedure_ := null;
    glVertex2dv              : Access_Procedure_ := null;
    glVertex2f               : Access_Procedure_ := null;
    glVertex2fv              : Access_Procedure_ := null;
    glVertex2i               : Access_Procedure_ := null;
    glVertex2iv              : Access_Procedure_ := null;
    glVertex2s               : Access_Procedure_ := null;
    glVertex2sv              : Access_Procedure_ := null;
    glVertex3d               : Access_Procedure_ := null;
    glVertex3dv              : Access_Procedure_ := null;
    glVertex3f               : Access_Procedure_ := null;
    glVertex3fv              : Access_Procedure_ := null;
    glVertex3i               : Access_Procedure_ := null;
    glVertex3iv              : Access_Procedure_ := null;
    glVertex3s               : Access_Procedure_ := null;
    glVertex3sv              : Access_Procedure_ := null;
    glVertex4d               : Access_Procedure_ := null;
    glVertex4dv              : Access_Procedure_ := null;
    glVertex4f               : Access_Procedure_ := null;
    glVertex4fv              : Access_Procedure_ := null;
    glVertex4i               : Access_Procedure_ := null;
    glVertex4iv              : Access_Procedure_ := null;
    glVertex4s               : Access_Procedure_ := null;
    glVertex4sv              : Access_Procedure_ := null;
    glVertexPointer          : Access_Procedure_ := null;
  -----------------
  -- Subprograms --
  -----------------
    procedure Run
    procedure Set_Rendering_Backend
-- 1. Detect all hardware displays
-- 2. Setup for each one
-- 2a. … Create OS window (HWND CreateWindow method)
-- 2b. … Get the HDC device context from the window (GetDC method)
-- 2c. … Create HGLRC opengl context. (wglCreateContext method)
-- 3. Call wglShareLists
-- 4. Set wglMakeCurrent to HDC and HGLRC for context 0 (wglMakeCurrent method)
-- 5. Create textures, VBOs, disp lists, frame buffers, etc.
-- 6. Start main rendering (for each monitor)
-- 6a. … Call wglMakeCurrent for HDC/HGLRC for specific monitor
-- 6b. … Create projection, view matricies for specific monitor
-- 6c. … Clear frame and depth buffer
-- 6d. … Draw scene
-- 6e. … Call wglSwapBuffers to refresh that monitor
-- 6f. End render loop
-- 7. Delete all textures, VBOs, then close contexts.
    procedure Accumulate(
      Operation : in Integer_4_Unsigned;
      Value     : in Float_4_Real);
    procedure Alpha_Function(
      Function  : in Integer_4_Unsigned;
      Reference : in Float_4_Real);
    procedure Begin_Drawing(
      Mode : in Integer_4_Unsigned);
    procedure Bitmap(
      Width    : in Integer_4_Signed;
      Height   : in Integer_4_Signed;
      X_Origin : in Float_4_Real;
      Y_Origin : in Float_4_Real;
      X_Move   : in Float_4_Real;
      Y_Move   : in Float_4_Real;
      Bitmap   : in Access_Integer_1_Unsigned_C);
    procedure Blend_Function(
      S_Factor : in Integer_4_Unsigned;
      D_Factor : in Integer_4_Unsigned);
    procedure Call_List(
      List : in Integer_4_Unsigned);
    procedure Call_Lists(
      Number   : in Integer_4_Signed;
      Grouping : in Integer_4_Unsigned; -- Default: GL_UNSIGNED_BYTE -- Others: GL_2_BYTES, GL_3_BYTES, GL_4_BYTES
      Lists    : in Array_Integer_1_Unsigned);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_2_Unsigned);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_4_Unsigned);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_1_Signed);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_2_Signed);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Integer_4_Signed);
    procedure Call_Lists(
      Number : in Integer_4_Signed;
      Lists  : in Array_Float_4_Real);
    procedure Clear(
      Mask : in Integer_4_Unsigned);
    procedure Clear_Accumulation(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real);
    procedure Clear_Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real);
    procedure Clear_Depth(
      Depth : in Float_8_Real);
    procedure Clear_Index(
      C : in Float_4_Real);
    procedure Clear_Stencil(
      S : in Integer_4_Signed);
    procedure Clip_Plane(
      Plane    : in Integer_4_Unsigned;
      Equation : in Access_Constant_Float_8_Real);
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned);
    procedure Color(
      V : in Win32.PCSTR);
    procedure Color(
      Red   : in Float_8_Real;
      Green : in Float_8_Real;
      Blue  : in Float_8_Real);
    procedure Color(
      V : in Access_Constant_Float_8_Real);
    procedure Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real);
    procedure Color(
      V : in Access_Constant_Float_4_Real);
    procedure Color(
      Red   : in Integer_4_Signed;
      Green : in Integer_4_Signed;
      Blue  : in Integer_4_Signed);
    procedure Color(
      V : in Access_Constant_Integer_4_Signed);
    procedure Color(
      Red   : in Integer_2_Signed;
      Green : in Integer_2_Signed;
      Blue  : in Integer_2_Signed);
    procedure Color(
      V : in Access_Constant_Integer_2_Signed);
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned);
    procedure Color(
      V : in Access_Integer_1_Unsigned_C);
    procedure Color(
      Red   : in Integer_4_Unsigned;
      Green : in Integer_4_Unsigned;
      Blue  : in Integer_4_Unsigned);
    procedure Color(
      V : in Access_Constant_Integer_4_Unsigned);
    procedure Color(
      Red   : in Integer_2_Unsigned;
      Green : in Integer_2_Unsigned;
      Blue  : in Integer_2_Unsigned);
    procedure Color(
      V : in Win32.PCWSTR);
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned;
      Alpha : in Integer_1_Unsigned);
    procedure Color(
      V : in Win32.PCSTR);
    procedure Color(
      Red   : in Float_8_Real;
      Green : in Float_8_Real;
      Blue  : in Float_8_Real;
      Alpha : in Float_8_Real);
    procedure Color(
      V : in Access_Constant_Float_8_Real);
    procedure Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real);
    procedure Color(
      V : in Access_Constant_Float_4_Real);
    procedure Color(
      Red   : in Integer_4_Signed;
      Green : in Integer_4_Signed;
      Blue  : in Integer_4_Signed;
      Alpha : in Integer_4_Signed);
    procedure Color(
      V : in Access_Constant_Integer_4_Unsigned);
    procedure Color(
      Red   : in Integer_2_Signed;
      Green : in Integer_2_Signed;
      Blue  : in Integer_2_Signed;
      Alpha : in Integer_2_Signed);
    procedure Color(
      V : in Access_Constant_Integer_2_Signed);
    procedure Color(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned;
      Alpha : in Integer_1_Unsigned);
    procedure Color(
      V : in Access_Integer_1_Unsigned_C);
    procedure Color(
      Red   : in Integer_4_Unsigned;
      Green : in Integer_4_Unsigned;
      Blue  : in Integer_4_Unsigned;
      Alpha : in Integer_4_Unsigned);
    procedure Color(
      V : in Access_Constant_Integer_4_Signed);
    procedure Color(
      Red   : in Integer_2_Unsigned;
      Green : in Integer_2_Unsigned;
      Blue  : in Integer_2_Unsigned;
      Alpha : in Integer_2_Unsigned);
    procedure Color(
      V : in Win32.PCWSTR);
    procedure Color_Mask(
      Red   : in Integer_1_Unsigned;
      Green : in Integer_1_Unsigned;
      Blue  : in Integer_1_Unsigned;
      Alpha : in Integer_1_Unsigned);
    procedure Color_Material(
      Race : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_1_Unsigned);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_2_Unsigned);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_4_Unsigned);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_1_Signed);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_2_Signed);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Integer_4_Signed);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Float_4_Real);
    procedure Color_Pointer(
      Size           : in Integer_4_Signed;
      Stride         : in Integer_4_Signed;
      Color_Elements : in Array_Float_8_Real);
    procedure Copy_Pixels(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed;
      Kind   : in Integer_4_Unsigned);
    procedure Cull_Face(
      Mode : in Integer_4_Unsigned);
    procedure Delete_Lists(
      List   : in Integer_4_Unsigned;
      Bounds : in Integer_4_Signed);
    procedure Depth_Function(
      Function : in Integer_4_Unsigned);
    procedure Depth_Mask(
      Flag : in Integer_1_Unsigned);
    procedure Depth_Range(
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real);
    procedure Disable(
      Cap : in Integer_4_Unsigned);
    procedure Draw_Buffer(
      Mode : in Integer_4_Unsigned);
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_1_Unsigned);
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_2_Unsigned);
    procedure Draw_Elements(
      Mode    : in Integer_4_Unsigned;
      Indices : in Array_Integer_4_Unsigned);
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_1_Unsigned);
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_1_Signed);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_2_Unsigned);
      end Draw_Pixels;
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_2_Signed);
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_4_Unsigned);
    procedure Draw_Pixels(
      Format : in Integer_4_Unsigned;
      Pixels : in Matrix_Integer_4_Signed);
    procedure Edge_Flag(
      Flag : in Integer_1_Unsigned);
    -- procedure Edge_Flag(
    --   Offset  : Integer_4_Signed_C,
    --   Pointer : GLvoid *) --UNKNOWN
    procedure Edge_Flag(
      Flag : in Access_Integer_1_Unsigned_C);
    procedure Enable(
      Cap : in Integer_4_Unsigned);
    procedure End_OpenGL;
    procedure End_List;
    procedure Evaluate_Coordinate(
      U : in Float_8_Real);
    procedure Evaluate_Coordinate(
      U : in Access_Constant_Float_8_Real);
    procedure Evaluate_Coordinate(
      U : in Float_4_Real);
    procedure Evaluate_Coordinate(
      U : in Access_Constant_Float_4_Real);
    procedure Evaluate_Coordinate(
      U : in Float_8_Real;
      V : in Float_8_Real);
    procedure Evaluate_Coordinate(
      U : in Access_Constant_Float_8_Real);
    procedure Evaluate_Coordinate(
      U : in Float_4_Real;
      V : in Float_4_Real);
    procedure Evaluate_Coordinate(
      U : in Access_Constant_Float_4_Real);
    procedure Evaluate_Mesh(
      Mode : in Integer_4_Unsigned;
      I_1  : in Integer_4_Signed;
      J_2  : in Integer_4_Signed);
    procedure Evaluate_Mesh(
      Mode : in Integer_4_Unsigned;
      I_1  : in Integer_4_Signed;
      I_2  : in Integer_4_Signed;
      J_1  : in Integer_4_Signed;
      J_2  : in Integer_4_Signed);
    procedure Evaluate_Point(
      I : in Integer_4_Signed);
    procedure Evaluate_Point(
      I : in Integer_4_Signed;
      J : in Integer_4_Signed);
    function Get_Feedback_Buffer(
      Size   : in Integer_4_Signed;
      Kind   : in Integer_4_Unsigned)
      return Array_Float_4_Real;
    procedure Finish;
    procedure Flush;
    procedure Fog(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Fog(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Fog(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Fog(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed);
    procedure Front_Face(
      Mode : in Integer_4_Unsigned);
    procedure Frustum(
      Left   : in Float_8_Real;
      Right  : in Float_8_Real;
      Bottom : in Float_8_Real;
      Top    : in Float_8_Real;
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real);
    function Generate_Lists(
      Bounds : in Integer_4_Signed)
      return Integer_4_Unsigned;
    function Generate_Textures(
      Size : in Integer_4_Positive)
      return Array_Integer_4_Unsigned;
    function Get_Bytes(
      Name : in Integer_4_Unsigned)
      return Array_Integer_1_Unsigned;
    procedure Get_Clip_Plane(
      Plane    : in Integer_4_Unsigned;
      Equation : in Access_Float_8_Real);
    procedure Get_Double(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_8_Real);
    function Get_Error
      return Integer_4_Unsigned;
    procedure Get_Float(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Integer(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Float_8_Real);
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Float_4_Real);
    procedure Get_Map(
      Target : in Integer_4_Unsigned;
      Query  : in Integer_4_Unsigned;
      V      : in Access_Integer_4_Signed);
    procedure Get_Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Access_Float_4_Real);
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Access_Integer_4_Unsigned);
    procedure Get_Pixel_Map(
      Map    : in Integer_4_Unsigned;
      Values : in Win32.PWSTR);
    procedure Get_Polygon_Stipple(
      Mask : in Access_Integer_1_Unsigned);
    function Get_String(
      Name : in Integer_4_Unsigned)
      return String_2;
    procedure Get_Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Texture_Generation(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_8_Real);
    procedure Get_Texture_Generation(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Texture_Generation(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Texture_Image(
      Target : in Integer_4_Unsigned;
      Level  : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      Kind   : in Integer_4_Unsigned;
      Pixels : in PGLvoid);
    procedure Get_Texture_Level_Parameter(
      Target     : in Integer_4_Unsigned;
      Level      : in Integer_4_Signed;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Texture_Level_Parameter(
      Target : in Integer_4_Unsigned;
      Level  : in Integer_4_Signed;
      Name   : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Get_Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Float_4_Real);
    procedure Get_Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Integer_4_Signed);
    procedure Hint(
      Target : in Integer_4_Unsigned;
      Mode   : in Integer_4_Unsigned);
    procedure Index_Mask(
      Mask : in Integer_4_Unsigned);
    procedure Index(
      C : in Float_8_Real);
    procedure Index(
      C : in Access_Constant_Float_8_Real);
    procedure Index(
      C : in Float_4_Real);
    procedure Index(
      C : in Access_Constant_Float_4_Real);
    procedure Index(
      C : in Integer_4_Signed);
    procedure Index(
      C : in Access_Constant_Integer_4_Unsigned);
    procedure Index(
      C : in Integer_2_Signed);
    procedure Index(
      C : in Access_Constant_Integer_2_Signed);
    procedure Initialize_Names;
    function Is_Enabled(
      Cap : in Integer_4_Unsigned)
      return Integer_1_Unsigned;
    function Is_List(
      List : in Integer_4_Unsigned)
      return Integer_1_Unsigned;
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Light_Model(
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Unsigned);
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Light(
      Light      : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Unsigned);
    procedure Line_Stipple(
      Factor  : in Integer_4_Signed;
      Pattern : in Integer_2_Unsigned);
    procedure Line_Width(
      Width : in Float_4_Real);
    procedure List_Base(
      Base : in Integer_4_Unsigned);
    procedure Load_Identity;
    procedure Load_Matrix(
      M : in Access_Constant_Float_8_Real);
    procedure Load_Matrix(
      M : in Access_Constant_Float_4_Real);
    procedure Load_Name(
      Name : in Integer_4_Unsigned);
    procedure Logic_Operation(
      Operation_Code : in Integer_4_Unsigned);
    procedure Map(
      Target : in Integer_4_Unsigned;
      U_1    : in Float_8_Real;
      U_2    : in Float_8_Real;
      Stride : in Integer_4_Signed;
      Order  : in Integer_4_Signed;
      Points : in Access_Constant_Float_8_Real);
    procedure Map(
      Target : in Integer_4_Unsigned;
      U_1     : in Float_4_Real;
      U_2     : in Float_4_Real;
      Stride : in Integer_4_Signed;
      Order  : in Integer_4_Signed;
      Points : in Access_Constant_Float_4_Real);
    procedure Map(
      Target   : in Integer_4_Unsigned;
      U_1      : in Float_8_Real;
      U_2      : in Float_8_Real;
      U_Stride : in Integer_4_Signed;
      U_Order  : in Integer_4_Signed;
      V_1      : in Float_8_Real;
      V_2      : in Float_8_Real;
      V_Stride : in Integer_4_Signed;
      V_Order  : in Integer_4_Signed;
      Points   : in Access_Constant_Float_8_Real);
    procedure Map(
      Target   : in Integer_4_Unsigned;
      U_1      : in Float_4_Real;
      U_2      : in Float_4_Real;
      U_Stride : in Integer_4_Signed;
      U_Order  : in Integer_4_Signed;
      V_1      : in Float_4_Real;
      V_2      : in Float_4_Real;
      V_Stride : in Integer_4_Signed;
      V_Order  : in Integer_4_Signed;
      Points   : in Access_Constant_Float_4_Real);
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_8_Real;
      U_2 : in Float_8_Real);
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_4_Real;
      U_2 : in Float_4_Real);
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_8_Real;
      U_2 : in Float_8_Real;
      V_N : in Integer_4_Signed;
      V_1 : in Float_8_Real;
      V_2 : in Float_8_Real);
    procedure Map_Grid(
      U_N : in Integer_4_Signed;
      U_1 : in Float_4_Real;
      U_2 : in Float_4_Real;
      V_N : in Integer_4_Signed;
      V_1 : in Float_4_Real;
      V_2 : in Float_4_Real);
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Material(
      Face       : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed);
    procedure Matrix_Mode(
      Mode : in Integer_4_Unsigned);
    procedure Multiply_Matrix(
      M : in Access_Constant_Float_8_Real);
    procedure Multiply_Matrix(
      M : in Access_Constant_Float_4_Real);
    procedure New_List(
      List : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned);
    procedure Normal(
      N_X : in Integer_1_Unsigned;
      N_Y : in Integer_1_Unsigned;
      N_Z : in Integer_1_Unsigned);
    procedure Normal(
      V : in Win32.PCSTR);
    procedure Normal(
      N_X : in Float_8_Real;
      N_Y : in Float_8_Real;
      N_Z : in Float_8_Real);
    procedure Normal(
      V : in Access_Constant_Float_8_Real);
    procedure Normal(
      N_X : in Float_4_Real;
      N_Y : in Float_4_Real;
      N_Z : in Float_4_Real);
    procedure Normal(
      V : in Access_Constant_Float_4_Real);
    procedure Normal(
      N_X : in Integer_4_Signed;
      N_Y : in Integer_4_Signed;
      N_Z : in Integer_4_Signed);
    procedure Normal(
      V : in Access_Constant_Integer_4_Signed);
    procedure Normal(
      N_X : in Integer_2_Signed;
      N_Y : in Integer_2_Signed;
      N_Z : in Integer_2_Signed);
    procedure Normal(
      V : in Access_Constant_Integer_2_Signed);
    procedure Orthographic(
      Left   : in Float_8_Real;
      Right  : in Float_8_Real;
      Bottom : in Float_8_Real;
      Top    : in Float_8_Real;
      Z_Near : in Float_8_Real;
      Z_Far  : in Float_8_Real);
    procedure Pass_Through(
      Token : in Float_4_Real);
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Access_Constant_Float_4_Real);
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Access_Constant_Integer_4_Signed);
    procedure Pixel_Map(
      Map      : in Integer_4_Unsigned;
      Map_Size : in Integer_4_Signed;
      Values   : in Win32.PCWSTR);
    procedure Pixel_Store(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Pixel_Store(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Pixel_Transfer(
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Pixel_Transfer(
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Pixel_Zoom(
      X_Factor : in Float_4_Real;
      Y_Factor : in Float_4_Real);
    procedure Point_Size(
      Size : in Float_4_Real);
    procedure Polygon_Mode(
      Face : in Integer_4_Unsigned;
      Mode : in Integer_4_Unsigned);
    procedure Polygon_Stipple(
      Mask : in Access_Integer_1_Unsigned_C);
    procedure Pop_Attribute;
    procedure Pop_Client_Attribute;
    procedure Pop_Matrix;
    procedure Pop_Name;
    procedure Push_Attribute(
      Mask : in Integer_4_Unsigned);
    procedure Push_Matrix;
    procedure Push_Name(
      Name : in Integer_4_Unsigned);
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real);
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real);
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed);
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed);
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real);
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real);
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed);
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed);
    procedure Raster_Position(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real;
      W : in Float_8_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_8_Real);
    procedure Raster_Position(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real;
      W : in Float_4_Real);
    procedure Raster_Position(
      V : in Access_Constant_Float_4_Real);
    procedure Raster_Position(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed;
      W : in Integer_4_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_4_Signed);
    procedure Raster_Position(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed;
      W : in Integer_2_Signed);
    procedure Raster_Position(
      V : in Access_Constant_Integer_2_Signed);
    procedure Read_Buffer(
      Mode : in Integer_4_Unsigned);
    procedure Read_Pixels(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed;
      Format : in Integer_4_Unsigned;
      Kind   : in Integer_4_Unsigned;
      Pixels : in PGLvoid);
    procedure Rectangle(
      X_1 : in Float_8_Real;
      Y_1 : in Float_8_Real;
      X_2 : in Float_8_Real;
      Y_2 : in Float_8_Real);
    procedure Rectangle(
      V_1 : in Access_Constant_Float_8_Real;
      V_2 : in Access_Constant_Float_8_Real);
    procedure Rectangle(
      X_1 : in Float_4_Real;
      Y_1 : in Float_4_Real;
      X_2 : in Float_4_Real;
      Y_2 : in Float_4_Real);
    procedure Rectangle(
      V_1 : in Access_Constant_Float_4_Real;
      V_2 : in Access_Constant_Float_4_Real);
    procedure Rectangle(
      X_1 : in Integer_4_Signed;
      Y_1 : in Integer_4_Signed;
      X_2 : in Integer_4_Signed;
      Y_2 : in Integer_4_Signed);
    procedure Rectangle(
      V_1 : in Access_Constant_Integer_4_Signed;
      V_2 : in Access_Constant_Integer_4_Signed);
    procedure Rectangle(
      X_1 : in Integer_2_Signed;
      Y_1 : in Integer_2_Signed;
      X_2 : in Integer_2_Signed;
      Y_2 : in Integer_2_Signed);
    procedure Rectangle(
      V_1 : in Access_Constant_Integer_2_Signed;
      V_2 : in Access_Constant_Integer_2_Signed);
    function Render_Mode(
      Mode : in Integer_4_Unsigned)
      return Integer_4_Signed;
    procedure Rotate(
      Angle : in Float_8_Real;
      X     : in Float_8_Real;
      Y     : in Float_8_Real;
      Z     : in Float_8_Real);
    procedure Rotate(
      Angle : in Float_4_Real;
      X     : in Float_4_Real;
      Y     : in Float_4_Real;
      Z     : in Float_4_Real);
    procedure Scale(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real);
    procedure Scale(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real);
    procedure Scissor(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed);
    procedure Select_Buffer(
      Size   : in Integer_4_Signed;
      Buffer : in Access_Integer_4_Unsigned);
    procedure Shade_Model(
      Mode : in Integer_4_Unsigned);
    procedure Stencil_Function(
      Function  : in Integer_4_Unsigned;
      Reference : in Integer_4_Signed;
      Mask      : in Integer_4_Unsigned);
    procedure Stencil_Mask(
      Mask : in Integer_4_Unsigned);
    procedure Stencil_Operation(
      Fail   : in Integer_4_Unsigned;
      Z_Fail : in Integer_4_Unsigned;
      Z_Pass : in Integer_4_Unsigned);
    procedure Texture_Coordinate(
      S : in Float_8_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real);
    procedure Texture_Coordinate(
      S : in Float_4_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real);
    procedure Texture_Coordinate(
      S : in Integer_4_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed);
    procedure Texture_Coordinate(
      S : in Integer_2_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed);
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real);
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real);
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed);
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed);
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real;
      R : in Float_8_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real);
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real;
      R : in Float_4_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real);
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed;
      R : in Integer_4_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed);
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed;
      R : in Integer_2_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed);
    procedure Texture_Coordinate(
      S : in Float_8_Real;
      T : in Float_8_Real;
      R : in Float_8_Real;
      Q : in Float_8_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_8_Real);
    procedure Texture_Coordinate(
      S : in Float_4_Real;
      T : in Float_4_Real;
      R : in Float_4_Real;
      Q : in Float_4_Real);
    procedure Texture_Coordinate(
      V : in Access_Constant_Float_4_Real);
    procedure Texture_Coordinate(
      S : in Integer_4_Signed;
      T : in Integer_4_Signed;
      R : in Integer_4_Signed;
      Q : in Integer_4_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_4_Signed);
    procedure Texture_Coordinate(
      S : in Integer_2_Signed;
      T : in Integer_2_Signed;
      R : in Integer_2_Signed;
      Q : in Integer_2_Signed);
    procedure Texture_Coordinate(
      V : in Access_Constant_Integer_2_Signed);
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Texture_Environment(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_8_Real);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_8_Real);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Texture_Generate(
      Coordinate : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed);
    procedure Texture_Image_1D(
      Target     : in Integer_4_Unsigned;
      level      : in Integer_4_Signed;
      Components : in Integer_4_Signed;
      Width      : in Integer_4_Signed;
      Border     : in Integer_4_Signed;
      Format     : in Integer_4_Unsigned;
      Kind       : in Integer_4_Unsigned;
      Pixels     : in Win32.PCVOID);
    procedure Texture_Image_2D(
      Target     : in Integer_4_Unsigned;
      level      : in Integer_4_Signed;
      Components : in Integer_4_Signed;
      Width      : in Integer_4_Signed;
      Height     : in Integer_4_Signed;
      Border     : in Integer_4_Signed;
      Format     : in Integer_4_Unsigned;
      Kind       : in Integer_4_Unsigned;
      Pixels     : in Win32.PCVOID);
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Float_4_Real);
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Float_4_Real);
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Integer_4_Signed);
    procedure Texture_Parameter(
      Target     : in Integer_4_Unsigned;
      Name       : in Integer_4_Unsigned;
      Parameters : in Access_Constant_Integer_4_Signed);
    procedure Translate(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real);
    procedure Translate(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real);
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real);
    procedure Vertex(
      V : in Access_Constant_Float_8_Real);
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real);
    procedure Vertex(
      V : in Access_Constant_Float_4_Real);
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed);
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed);
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real);
    procedure Vertex(
      V : in Access_Constant_Float_8_Real);
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real);
    procedure Vertex(
      V : in Access_Constant_Float_4_Real);
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed);
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed);
    procedure Vertex(
      X : in Float_8_Real;
      Y : in Float_8_Real;
      Z : in Float_8_Real;
      W : in Float_8_Real);
    procedure Vertex(
      V : in Access_Constant_Float_8_Real);
    procedure Vertex(
      X : in Float_4_Real;
      Y : in Float_4_Real;
      Z : in Float_4_Real;
      W : in Float_4_Real);
    procedure Vertex(
      V : in Access_Constant_Float_4_Real);
    procedure Vertex(
      X : in Integer_4_Signed;
      Y : in Integer_4_Signed;
      Z : in Integer_4_Signed;
      W : in Integer_4_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_4_Signed);
    procedure Vertex(
      X : in Integer_2_Signed;
      Y : in Integer_2_Signed;
      Z : in Integer_2_Signed;
      W : in Integer_2_Signed);
    procedure Vertex(
      V : in Access_Constant_Integer_2_Signed);
    procedure Viewport(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed);
-------
private
-------
  ---------------
  -- Variables --
  ---------------
    glAccum                  : Access_Procedure_ := null;
    glAlphaFunc              : Access_Procedure_ := null;
    glAreTexturesResident    : Access_Procedure_ := null;
    glArrayElement           : Access_Procedure_ := null;
    glBegin                  : Access_Procedure_ := null;
    glBindTexture            : Access_Procedure_ := null;
    glBitmap                 : Access_Procedure_ := null;
    glBlendFunc              : Access_Procedure_ := null;
    glCallList               : Access_Procedure_ := null;
    glCallLists              : Access_Procedure_ := null;
    glClear                  : Access_Procedure_ := null;
    glClearAccum             : Access_Procedure_ := null;
    glClearColor             : Access_Procedure_ := null;
    glClearDepth             : Access_Procedure_ := null;
    glClearIndex             : Access_Procedure_ := null;
    glClearStencil           : Access_Procedure_ := null;
    glClipPlane              : Access_Procedure_ := null;
    glColor3b                : Access_Procedure_ := null;
    glColor3bv               : Access_Procedure_ := null;
    glColor3d                : Access_Procedure_ := null;
    glColor3dv               : Access_Procedure_ := null;
    glColor3f                : Access_Procedure_ := null;
    glColor3fv               : Access_Procedure_ := null;
    glColor3i                : Access_Procedure_ := null;
    glColor3iv               : Access_Procedure_ := null;
    glColor3s                : Access_Procedure_ := null;
    glColor3sv               : Access_Procedure_ := null;
    glColor3ub               : Access_Procedure_ := null;
    glColor3ubv              : Access_Procedure_ := null;
    glColor3ui               : Access_Procedure_ := null;
    glColor3uiv              : Access_Procedure_ := null;
    glColor3us               : Access_Procedure_ := null;
    glColor3usv              : Access_Procedure_ := null;
    glColor4b                : Access_Procedure_ := null;
    glColor4bv               : Access_Procedure_ := null;
    glColor4d                : Access_Procedure_ := null;
    glColor4dv               : Access_Procedure_ := null;
    glColor4f                : Access_Procedure_ := null;
    glColor4fv               : Access_Procedure_ := null;
    glColor4i                : Access_Procedure_ := null;
    glColor4iv               : Access_Procedure_ := null;
    glColor4s                : Access_Procedure_ := null;
    glColor4sv               : Access_Procedure_ := null;
    glColor4ub               : Access_Procedure_ := null;
    glColor4ubv              : Access_Procedure_ := null;
    glColor4ui               : Access_Procedure_ := null;
    glColor4uiv              : Access_Procedure_ := null;
    glColor4us               : Access_Procedure_ := null;
    glColor4usv              : Access_Procedure_ := null;
    glColorMask              : Access_Procedure_ := null;
    glColorMaterial          : Access_Procedure_ := null;
    glColorPointer           : Access_Procedure_ := null;
    glCopyPixels             : Access_Procedure_ := null;
    glCopyTexImage1D         : Access_Procedure_ := null;
    glCopyTexImage2D         : Access_Procedure_ := null;
    glCopyTexSubImage1D      : Access_Procedure_ := null;
    glCopyTexSubImage2D      : Access_Procedure_ := null;
    glCullFace               : Access_Procedure_ := null;
    glDeleteLists            : Access_Procedure_ := null;
    glDeleteTextures         : Access_Procedure_ := null;
    glDepthFunc              : Access_Procedure_ := null;
    glDepthMask              : Access_Procedure_ := null;
    glDepthRange             : Access_Procedure_ := null;
    glDisable                : Access_Procedure_ := null;
    glDisableClientState     : Access_Procedure_ := null;
    glDrawArrays             : Access_Procedure_ := null;
    glDrawBuffer             : Access_Procedure_ := null;
    glDrawElements           : Access_Procedure_ := null;
    glDrawPixels             : Access_Procedure_ := null;
    glEdgeFlag               : Access_Procedure_ := null;
    glEdgeFlagPointer        : Access_Procedure_ := null;
    glEdgeFlagv              : Access_Procedure_ := null;
    glEnable                 : Access_Procedure_ := null;
    glEnableClientState      : Access_Procedure_ := null;
    glEnd                    : Access_Procedure_ := null;
    glEndList                : Access_Procedure_ := null;
    glEvalCoord1d            : Access_Procedure_ := null;
    glEvalCoord1dv           : Access_Procedure_ := null;
    glEvalCoord1f            : Access_Procedure_ := null;
    glEvalCoord1fv           : Access_Procedure_ := null;
    glEvalCoord2d            : Access_Procedure_ := null;
    glEvalCoord2dv           : Access_Procedure_ := null;
    glEvalCoord2f            : Access_Procedure_ := null;
    glEvalCoord2fv           : Access_Procedure_ := null;
    glEvalMesh1              : Access_Procedure_ := null;
    glEvalMesh2              : Access_Procedure_ := null;
    glEvalPoint1             : Access_Procedure_ := null;
    glEvalPoint2             : Access_Procedure_ := null;
    glFeedbackBuffer         : Access_Procedure_ := null;
    glFinish                 : Access_Procedure_ := null;
    glFlush                  : Access_Procedure_ := null;
    glFogf                   : Access_Procedure_ := null;
    glFogfv                  : Access_Procedure_ := null;
    glFogi                   : Access_Procedure_ := null;
    glFogiv                  : Access_Procedure_ := null;
    glFrontFace              : Access_Procedure_ := null;
    glFrustum                : Access_Procedure_ := null;
    glGenLists               : Access_Procedure_ := null;
    glGenTextures            : Access_Procedure_ := null;
    glGetBooleanv            : Access_Procedure_ := null;
    glGetClipPlane           : Access_Procedure_ := null;
    glGetDoublev             : Access_Procedure_ := null;
    glGetError               : Access_Procedure_ := null;
    glGetFloatv              : Access_Procedure_ := null;
    glGetIntegerv            : Access_Procedure_ := null;
    glGetLightfv             : Access_Procedure_ := null;
    glGetLightiv             : Access_Procedure_ := null;
    glGetMapdv               : Access_Procedure_ := null;
    glGetMapfv               : Access_Procedure_ := null;
    glGetMapiv               : Access_Procedure_ := null;
    glGetMaterialfv          : Access_Procedure_ := null;
    glGetMaterialiv          : Access_Procedure_ := null;
    glGetPixelMapfv          : Access_Procedure_ := null;
    glGetPixelMapuiv         : Access_Procedure_ := null;
    glGetPixelMapusv         : Access_Procedure_ := null;
    glGetPointerb            : Access_Procedure_ := null;
    glGetPolygonStipple      : Access_Procedure_ := null;
    glGetString              : Access_Procedure_ := null;
    glGetTexEnvfv            : Access_Procedure_ := null;
    glGetTexEnviv            : Access_Procedure_ := null;
    glGetTexGendv            : Access_Procedure_ := null;
    glGetTexGenfv            : Access_Procedure_ := null;
    glGetTexGeniv            : Access_Procedure_ := null;
    glGetTexImage            : Access_Procedure_ := null;
    glGetTexLevelParameterfv : Access_Procedure_ := null;
    glGetTexLevelParameteriv : Access_Procedure_ := null;
    glGetTexParameterfv      : Access_Procedure_ := null;
    glGetTexParameteriv      : Access_Procedure_ := null;
    glHint                   : Access_Procedure_ := null;
    glIndexMask              : Access_Procedure_ := null;
    glIndexPointer           : Access_Procedure_ := null;
    glIndexd                 : Access_Procedure_ := null;
    glIndexdv                : Access_Procedure_ := null;
    glIndexf                 : Access_Procedure_ := null;
    glIndexfv                : Access_Procedure_ := null;
    glIndexi                 : Access_Procedure_ := null;
    glIndexiv                : Access_Procedure_ := null;
    glIndexs                 : Access_Procedure_ := null;
    glIndexsv                : Access_Procedure_ := null;
    glIndexub                : Access_Procedure_ := null;
    glIndexubv               : Access_Procedure_ := null;
    glInitNames              : Access_Procedure_ := null;
    glInterleavedArrays      : Access_Procedure_ := null;
    glIsEnabled              : Access_Procedure_ := null;
    glIsList                 : Access_Procedure_ := null;
    glIsTexture              : Access_Procedure_ := null;
    glLightModelf            : Access_Procedure_ := null;
    glLightModelfv           : Access_Procedure_ := null;
    glLightModeli            : Access_Procedure_ := null;
    glLightModeliv           : Access_Procedure_ := null;
    glLightf                 : Access_Procedure_ := null;
    glLightfv                : Access_Procedure_ := null;
    glLighti                 : Access_Procedure_ := null;
    glLightiv                : Access_Procedure_ := null;
    glLineStipple            : Access_Procedure_ := null;
    glLineWidth              : Access_Procedure_ := null;
    glListBase               : Access_Procedure_ := null;
    glLoadIdentity           : Access_Procedure_ := null;
    glLoadMatrixd            : Access_Procedure_ := null;
    glLoadMatrixf            : Access_Procedure_ := null;
    glLoadName               : Access_Procedure_ := null;
    glLogicOp                : Access_Procedure_ := null;
    glMap1D                  : Access_Procedure_ := null;
    glMap1f                  : Access_Procedure_ := null;
    glMap2D                  : Access_Procedure_ := null;
    glMap2f                  : Access_Procedure_ := null;
    glMapGrid1d              : Access_Procedure_ := null;
    glMapGrid1f              : Access_Procedure_ := null;
    glMapGrid2d              : Access_Procedure_ := null;
    glMapGrid2f              : Access_Procedure_ := null;
    glMaterialf              : Access_Procedure_ := null;
    glMaterialfv             : Access_Procedure_ := null;
    glMateriali              : Access_Procedure_ := null;
    glMaterialiv             : Access_Procedure_ := null;
    glMatrixMode             : Access_Procedure_ := null;
    glMultMatrixd            : Access_Procedure_ := null;
    glMultMatrixf            : Access_Procedure_ := null;
    glNewList                : Access_Procedure_ := null;
    glNormal3b               : Access_Procedure_ := null;
    glNormal3Bv              : Access_Procedure_ := null;
    glNormal3D               : Access_Procedure_ := null;
    glNormal3Dv              : Access_Procedure_ := null;
    glNormal3f               : Access_Procedure_ := null;
    glNormal3fv              : Access_Procedure_ := null;
    glNormal3i               : Access_Procedure_ := null;
    glNormal3iv              : Access_Procedure_ := null;
    glNormal3s               : Access_Procedure_ := null;
    glNormal3sv              : Access_Procedure_ := null;
    glNormalPointer          : Access_Procedure_ := null;
    glOrtho                  : Access_Procedure_ := null;
    glPassThrough            : Access_Procedure_ := null;
    glPixelMapfv             : Access_Procedure_ := null;
    glPixelMapuiv            : Access_Procedure_ := null;
    glPixelMapusv            : Access_Procedure_ := null;
    glPixelStoref            : Access_Procedure_ := null;
    glPixelStorei            : Access_Procedure_ := null;
    glPixelTransferf         : Access_Procedure_ := null;
    glPixelTransferi         : Access_Procedure_ := null;
    glPixelZoom              : Access_Procedure_ := null;
    glPointSize              : Access_Procedure_ := null;
    glPolygonMode            : Access_Procedure_ := null;
    glPolygonOffset          : Access_Procedure_ := null;
    glPolygonStipple         : Access_Procedure_ := null;
    glPopAttrib              : Access_Procedure_ := null;
    glPopClientAttrib        : Access_Procedure_ := null;
    glPopMatrix              : Access_Procedure_ := null;
    glPopName                : Access_Procedure_ := null;
    glPrioritizeTextures     : Access_Procedure_ := null;
    glPushAttrib             : Access_Procedure_ := null;
    glPushClientAttrib       : Access_Procedure_ := null;
    glPushMatrix             : Access_Procedure_ := null;
    glPushName               : Access_Procedure_ := null;
    glRasterPos2D            : Access_Procedure_ := null;
    glRasterPos2Dv           : Access_Procedure_ := null;
    glRasterPos2f            : Access_Procedure_ := null;
    glRasterPos2fv           : Access_Procedure_ := null;
    glRasterPos2i            : Access_Procedure_ := null;
    glRasterPos2iv           : Access_Procedure_ := null;
    glRasterPos2s            : Access_Procedure_ := null;
    glRasterPos2sv           : Access_Procedure_ := null;
    glRasterPos3d            : Access_Procedure_ := null;
    glRasterPos3dv           : Access_Procedure_ := null;
    glRasterPos3f            : Access_Procedure_ := null;
    glRasterPos3fv           : Access_Procedure_ := null;
    glRasterPos3i            : Access_Procedure_ := null;
    glRasterPos3iv           : Access_Procedure_ := null;
    glRasterPos3s            : Access_Procedure_ := null;
    glRasterPos3sv           : Access_Procedure_ := null;
    glRasterPos4d            : Access_Procedure_ := null;
    glRasterPos4dv           : Access_Procedure_ := null;
    glRasterPos4f            : Access_Procedure_ := null;
    glRasterPos4fv           : Access_Procedure_ := null;
    glRasterPos4i            : Access_Procedure_ := null;
    glRasterPos4iv           : Access_Procedure_ := null;
    glRasterPos4s            : Access_Procedure_ := null;
    glRasterPos4sv           : Access_Procedure_ := null;
    glReadBuffer             : Access_Procedure_ := null;
    glReadPixels             : Access_Procedure_ := null;
    glRectd                  : Access_Procedure_ := null;
    glRectdv                 : Access_Procedure_ := null;
    glRectf                  : Access_Procedure_ := null;
    glRectfv                 : Access_Procedure_ := null;
    glRecti                  : Access_Procedure_ := null;
    glRectiv                 : Access_Procedure_ := null;
    glRects                  : Access_Procedure_ := null;
    glRectsv                 : Access_Procedure_ := null;
    glRenderMode             : Access_Procedure_ := null;
    glRotated                : Access_Procedure_ := null;
    glRotatef                : Access_Procedure_ := null;
    glScaled                 : Access_Procedure_ := null;
    glScalef                 : Access_Procedure_ := null;
    glScissor                : Access_Procedure_ := null;
    glSelectBuffer           : Access_Procedure_ := null;
    glShadeModel             : Access_Procedure_ := null;
    glStencilFunc            : Access_Procedure_ := null;
    glStencilMask            : Access_Procedure_ := null;
    glStencilOp              : Access_Procedure_ := null;
    glTexCoord1d             : Access_Procedure_ := null;
    glTexCoord1dv            : Access_Procedure_ := null;
    glTexCoord1f             : Access_Procedure_ := null;
    glTexCoord1fv            : Access_Procedure_ := null;
    glTexCoord1i             : Access_Procedure_ := null;
    glTexCoord1iv            : Access_Procedure_ := null;
    glTexCoord1s             : Access_Procedure_ := null;
    glTexCoord1sv            : Access_Procedure_ := null;
    glTexCoord2d             : Access_Procedure_ := null;
    glTexCoord2dv            : Access_Procedure_ := null;
    glTexCoord2f             : Access_Procedure_ := null;
    glTexCoord2fv            : Access_Procedure_ := null;
    glTexCoord2i             : Access_Procedure_ := null;
    glTexCoord2iv            : Access_Procedure_ := null;
    glTexCoord2s             : Access_Procedure_ := null;
    glTexCoord2sv            : Access_Procedure_ := null;
    glTexCoord3d             : Access_Procedure_ := null;
    glTexCoord3dv            : Access_Procedure_ := null;
    glTexCoord3f             : Access_Procedure_ := null;
    glTexCoord3fv            : Access_Procedure_ := null;
    glTexCoord3i             : Access_Procedure_ := null;
    glTexCoord3iv            : Access_Procedure_ := null;
    glTexCoord3s             : Access_Procedure_ := null;
    glTexCoord3dv            : Access_Procedure_ := null;
    glTexCoord4d             : Access_Procedure_ := null;
    glTexCoord4dv            : Access_Procedure_ := null;
    glTexCoord4f             : Access_Procedure_ := null;
    glTexCoord4fv            : Access_Procedure_ := null;
    glTexCoord4i             : Access_Procedure_ := null;
    glTexCoord4iv            : Access_Procedure_ := null;
    glTexCoord4d             : Access_Procedure_ := null;
    glTexCoord4dv            : Access_Procedure_ := null;
    glTexCoordPointer        : Access_Procedure_ := null;
    glTexEnvf                : Access_Procedure_ := null;
    glTexEnvfv               : Access_Procedure_ := null;
    glTexEnvi                : Access_Procedure_ := null;
    glTexEnviv               : Access_Procedure_ := null;
    glTexGend                : Access_Procedure_ := null;
    glTexGendv               : Access_Procedure_ := null;
    glTexGenf                : Access_Procedure_ := null;
    glTexGenfv               : Access_Procedure_ := null;
    glTexGeni                : Access_Procedure_ := null;
    glTexGeniv               : Access_Procedure_ := null;
    glTexImage1D             : Access_Procedure_ := null;
    glTexImage2D             : Access_Procedure_ := null;
    glTexParameterf          : Access_Procedure_ := null;
    glTexParameterfv         : Access_Procedure_ := null;
    glTexParameteri          : Access_Procedure_ := null;
    glTexParameteriv         : Access_Procedure_ := null;
    glTexSubImage1D          : Access_Procedure_ := null;
    glTexSubImage2D          : Access_Procedure_ := null;
    glTranslated             : Access_Procedure_ := null;
    glTranslatef             : Access_Procedure_ := null;
    glVertex2d               : Access_Procedure_ := null;
    glVertex2dv              : Access_Procedure_ := null;
    glVertex2f               : Access_Procedure_ := null;
    glVertex2fv              : Access_Procedure_ := null;
    glVertex2i               : Access_Procedure_ := null;
    glVertex2iv              : Access_Procedure_ := null;
    glVertex2s               : Access_Procedure_ := null;
    glVertex2sv              : Access_Procedure_ := null;
    glVertex3d               : Access_Procedure_ := null;
    glVertex3dv              : Access_Procedure_ := null;
    glVertex3f               : Access_Procedure_ := null;
    glVertex3fv              : Access_Procedure_ := null;
    glVertex3i               : Access_Procedure_ := null;
    glVertex3iv              : Access_Procedure_ := null;
    glVertex3s               : Access_Procedure_ := null;
    glVertex3sv              : Access_Procedure_ := null;
    glVertex4d               : Access_Procedure_ := null;
    glVertex4dv              : Access_Procedure_ := null;
    glVertex4f               : Access_Procedure_ := null;
    glVertex4fv              : Access_Procedure_ := null;
    glVertex4i               : Access_Procedure_ := null;
    glVertex4iv              : Access_Procedure_ := null;
    glVertex4s               : Access_Procedure_ := null;
    glVertex4sv              : Access_Procedure_ := null;
    glVertexPointer          : Access_Procedure_ := null;
  end Neo.System.OpenGL;
