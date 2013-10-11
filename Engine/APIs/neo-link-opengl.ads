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
package Neo.Link.OpenGL
  is
  ---------------
  -- Constants --
  ---------------
    VERSION_1_1                     : constant Integer_4_Unsigned_C := 16#0000_0001#;
    ACCUM                           : constant Integer_4_Unsigned_C := 16#0000_0100#;
    LOAD                            : constant Integer_4_Unsigned_C := 16#0000_0101#;
    DO_RETURN                          : constant Integer_4_Unsigned_C := 16#0000_0102#;
    MULT                            : constant Integer_4_Unsigned_C := 16#0000_0103#;
    ADD                             : constant Integer_4_Unsigned_C := 16#0000_0104#;
    NEVER                           : constant Integer_4_Unsigned_C := 16#0000_0200#;
    LESS                            : constant Integer_4_Unsigned_C := 16#0000_0201#;
    EQUAL                           : constant Integer_4_Unsigned_C := 16#0000_0202#;
    LEQUAL                          : constant Integer_4_Unsigned_C := 16#0000_0203#;
    GREATER                         : constant Integer_4_Unsigned_C := 16#0000_0204#;
    NOT_EQUAL                        : constant Integer_4_Unsigned_C := 16#0000_0205#;
    GEQUAL                          : constant Integer_4_Unsigned_C := 16#0000_0206#;
    ALWAYS                          : constant Integer_4_Unsigned_C := 16#0000_0207#;
    CURRENT_BIT                     : constant Integer_4_Unsigned_C := 16#0000_0001#;
    POINT_BIT                       : constant Integer_4_Unsigned_C := 16#0000_0002#;
    LINE_BIT                        : constant Integer_4_Unsigned_C := 16#0000_0004#;
    POLYGON_BIT                     : constant Integer_4_Unsigned_C := 16#0000_0008#;
    POLYGON_STIPPLE_BIT             : constant Integer_4_Unsigned_C := 16#0000_0010#;
    PIXEL_MODE_BIT                  : constant Integer_4_Unsigned_C := 16#0000_0020#;
    LIGHTING_BIT                    : constant Integer_4_Unsigned_C := 16#0000_0040#;
    FOG_BIT                         : constant Integer_4_Unsigned_C := 16#0000_0080#;
    DEPTH_BUFFER_BIT                : constant Integer_4_Unsigned_C := 16#0000_0100#;
    ACCUM_BUFFER_BIT                : constant Integer_4_Unsigned_C := 16#0000_0200#;
    STENCIL_BUFFER_BIT              : constant Integer_4_Unsigned_C := 16#0000_0400#;
    VIEWPORT_BIT                    : constant Integer_4_Unsigned_C := 16#0000_0800#;
    TRANSFORM_BIT                   : constant Integer_4_Unsigned_C := 16#0000_1000#;
    ENABLE_BIT                      : constant Integer_4_Unsigned_C := 16#0000_2000#;
    COLOR_BUFFER_BIT                : constant Integer_4_Unsigned_C := 16#0000_4000#;
    HINT_BIT                        : constant Integer_4_Unsigned_C := 16#0000_8000#;
    EVAL_BIT                        : constant Integer_4_Unsigned_C := 16#0001_0000#;
    LIST_BIT                        : constant Integer_4_Unsigned_C := 16#0002_0000#;
    TEXTURE_BIT                     : constant Integer_4_Unsigned_C := 16#0004_0000#;
    SCISSOR_BIT                     : constant Integer_4_Unsigned_C := 16#0008_0000#;
    ALL_ATTRIBUTE_BITS                 : constant Integer_4_Unsigned_C := 16#000F_FFFF#;
    POINTS                          : constant Integer_4_Unsigned_C := 16#0000_0000#;
    LINES                           : constant Integer_4_Unsigned_C := 16#0000_0001#;
    LINE_LOOP                       : constant Integer_4_Unsigned_C := 16#0000_0002#;
    LINE_STRIP                      : constant Integer_4_Unsigned_C := 16#0000_0003#;
    TRIANGLES                       : constant Integer_4_Unsigned_C := 16#0000_0004#;
    TRIANGLE_STRIP                  : constant Integer_4_Unsigned_C := 16#0000_0005#;
    TRIANGLE_FAN                    : constant Integer_4_Unsigned_C := 16#0000_0006#;
    QUADS                           : constant Integer_4_Unsigned_C := 16#0000_0007#;
    QUAD_STRIP                      : constant Integer_4_Unsigned_C := 16#0000_0008#;
    POLYGON                         : constant Integer_4_Unsigned_C := 16#0000_0009#;
    ZERO                            : constant Integer_4_Unsigned_C := 16#0000_0000#;
    ONE                             : constant Integer_4_Unsigned_C := 16#0000_0001#;
    SOURCE_COLOR                       : constant Integer_4_Unsigned_C := 16#0000_0300#;
    ONE_MINUS_SOURCE_COLOR             : constant Integer_4_Unsigned_C := 16#0000_0301#;
    SOURCE_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_0302#;
    ONE_MINUS_SOURCE_ALPHA             : constant Integer_4_Unsigned_C := 16#0000_0303#;
    DESTINATION_ALPHA                       : constant Integer_4_Unsigned_C := 16#0000_0304#;
    ONE_MINUS_DESTINATION_ALPHA             : constant Integer_4_Unsigned_C := 16#0000_0305#;
    DESTINATION_COLOR                       : constant Integer_4_Unsigned_C := 16#0000_0306#;
    ONE_MINUS_DESTINATION_COLOR             : constant Integer_4_Unsigned_C := 16#0000_0307#;
    SOURCE_ALPHA_SATURATE              : constant Integer_4_Unsigned_C := 16#0000_0308#;
    CLIP_PLANE0                     : constant Integer_4_Unsigned_C := 16#0000_3000#;
    CLIP_PLANE1                     : constant Integer_4_Unsigned_C := 16#0000_3001#;
    CLIP_PLANE2                     : constant Integer_4_Unsigned_C := 16#0000_3002#;
    CLIP_PLANE3                     : constant Integer_4_Unsigned_C := 16#0000_3003#;
    CLIP_PLANE4                     : constant Integer_4_Unsigned_C := 16#0000_3004#;
    CLIP_PLANE5                     : constant Integer_4_Unsigned_C := 16#0000_3005#;
    BYTE                            : constant Integer_4_Unsigned_C := 16#0000_1400#;
    UNSIGNED_BYTE                   : constant Integer_4_Unsigned_C := 16#0000_1401#;
    SHORT                           : constant Integer_4_Unsigned_C := 16#0000_1402#;
    UNSIGNED_SHORT                  : constant Integer_4_Unsigned_C := 16#0000_1403#;
    INT                             : constant Integer_4_Unsigned_C := 16#0000_1404#;
    UNSIGNED_INT                    : constant Integer_4_Unsigned_C := 16#0000_1405#;
    FLOAT                           : constant Integer_4_Unsigned_C := 16#0000_1406#;
    2_BYTES                         : constant Integer_4_Unsigned_C := 16#0000_1407#;
    3_BYTES                         : constant Integer_4_Unsigned_C := 16#0000_1408#;
    4_BYTES                         : constant Integer_4_Unsigned_C := 16#0000_1409#;
    DOUBLE                          : constant Integer_4_Unsigned_C := 16#0000_140A#;
    NONE                            : constant Integer_4_Unsigned_C := 16#0000_0000#;
    FRONT_LEFT                      : constant Integer_4_Unsigned_C := 16#0000_0400#;
    FRONT_RIGHT                     : constant Integer_4_Unsigned_C := 16#0000_0401#;
    BACK_LEFT                       : constant Integer_4_Unsigned_C := 16#0000_0402#;
    BACK_RIGHT                      : constant Integer_4_Unsigned_C := 16#0000_0403#;
    FRONT                           : constant Integer_4_Unsigned_C := 16#0000_0404#;
    BACK                            : constant Integer_4_Unsigned_C := 16#0000_0405#;
    LEFT                            : constant Integer_4_Unsigned_C := 16#0000_0406#;
    RIGHT                           : constant Integer_4_Unsigned_C := 16#0000_0407#;
    FRONT_AND_BACK                  : constant Integer_4_Unsigned_C := 16#0000_0408#;
    AUX0                            : constant Integer_4_Unsigned_C := 16#0000_0409#;
    AUX1                            : constant Integer_4_Unsigned_C := 16#0000_040A#;
    AUX2                            : constant Integer_4_Unsigned_C := 16#0000_040B#;
    AUX3                            : constant Integer_4_Unsigned_C := 16#0000_040C#;
    NO_ERROR                        : constant Integer_4_Unsigned_C := 16#0000_0000#;
    INVALID_ENUMERATION                    : constant Integer_4_Unsigned_C := 16#0000_0500#;
    INVALID_VALUE                   : constant Integer_4_Unsigned_C := 16#0000_0501#;
    INVALID_OPERATION               : constant Integer_4_Unsigned_C := 16#0000_0502#;
    STACK_OVERFLOW                  : constant Integer_4_Unsigned_C := 16#0000_0503#;
    STACK_UNDERFLOW                 : constant Integer_4_Unsigned_C := 16#0000_0504#;
    OUT_OF_MEMORY                   : constant Integer_4_Unsigned_C := 16#0000_0505#;
    2D                              : constant Integer_4_Unsigned_C := 16#0000_0600#;
    3D                              : constant Integer_4_Unsigned_C := 16#0000_0601#;
    3D_COLOR                        : constant Integer_4_Unsigned_C := 16#0000_0602#;
    3D_COLOR_TEXTURE                : constant Integer_4_Unsigned_C := 16#0000_0603#;
    4D_COLOR_TEXTURE                : constant Integer_4_Unsigned_C := 16#0000_0604#;
    PASS_THROUGH_TOKEN              : constant Integer_4_Unsigned_C := 16#0000_0700#;
    POINT_TOKEN                     : constant Integer_4_Unsigned_C := 16#0000_0701#;
    LINE_TOKEN                      : constant Integer_4_Unsigned_C := 16#0000_0702#;
    POLYGON_TOKEN                   : constant Integer_4_Unsigned_C := 16#0000_0703#;
    BITMAP_TOKEN                    : constant Integer_4_Unsigned_C := 16#0000_0704#;
    DRAW_PIXEL_TOKEN                : constant Integer_4_Unsigned_C := 16#0000_0705#;
    COPY_PIXEL_TOKEN                : constant Integer_4_Unsigned_C := 16#0000_0706#;
    LINE_RESET_TOKEN                : constant Integer_4_Unsigned_C := 16#0000_0707#;
    EXP                             : constant Integer_4_Unsigned_C := 16#0000_0800#;
    EXP2                            : constant Integer_4_Unsigned_C := 16#0000_0801#;
    CW                              : constant Integer_4_Unsigned_C := 16#0000_0900#;
    CCW                             : constant Integer_4_Unsigned_C := 16#0000_0901#;
    COEFF                           : constant Integer_4_Unsigned_C := 16#0000_0A00#;
    ORDER                           : constant Integer_4_Unsigned_C := 16#0000_0A01#;
    DOMAIN                          : constant Integer_4_Unsigned_C := 16#0000_0A02#;
    CURRENT_COLOR                   : constant Integer_4_Unsigned_C := 16#0000_0B00#;
    CURRENT_INDEX                   : constant Integer_4_Unsigned_C := 16#0000_0B01#;
    CURRENT_NORMAL                  : constant Integer_4_Unsigned_C := 16#0000_0B02#;
    CURRENT_TEXTURE_COORDS          : constant Integer_4_Unsigned_C := 16#0000_0B03#;
    CURRENT_RASTER_COLOR            : constant Integer_4_Unsigned_C := 16#0000_0B04#;
    CURRENT_RASTER_INDEX            : constant Integer_4_Unsigned_C := 16#0000_0B05#;
    CURRENT_RASTER_TEXTURE_COORDS   : constant Integer_4_Unsigned_C := 16#0000_0B06#;
    CURRENT_RASTER_POSITION         : constant Integer_4_Unsigned_C := 16#0000_0B07#;
    CURRENT_RASTER_POSITION_VALID   : constant Integer_4_Unsigned_C := 16#0000_0B08#;
    CURRENT_RASTER_DISTANCE         : constant Integer_4_Unsigned_C := 16#0000_0B09#;
    POINT_SMOOTH                    : constant Integer_4_Unsigned_C := 16#0000_0B10#;
    POINT_SIZE                      : constant Integer_4_Unsigned_C := 16#0000_0B11#;
    POINT_SIZE_RANGE                : constant Integer_4_Unsigned_C := 16#0000_0B12#;
    POINT_SIZE_GRANULARITY          : constant Integer_4_Unsigned_C := 16#0000_0B13#;
    LINE_SMOOTH                     : constant Integer_4_Unsigned_C := 16#0000_0B20#;
    LINE_WIDTH                      : constant Integer_4_Unsigned_C := 16#0000_0B21#;
    LINE_WIDTH_RANGE                : constant Integer_4_Unsigned_C := 16#0000_0B22#;
    LINE_WIDTH_GRANULARITY          : constant Integer_4_Unsigned_C := 16#0000_0B23#;
    LINE_STIPPLE                    : constant Integer_4_Unsigned_C := 16#0000_0B24#;
    LINE_STIPPLE_PATTERN            : constant Integer_4_Unsigned_C := 16#0000_0B25#;
    LINE_STIPPLE_REPEAT             : constant Integer_4_Unsigned_C := 16#0000_0B26#;
    LIST_MODE                       : constant Integer_4_Unsigned_C := 16#0000_0B30#;
    MAX_LIST_NESTING                : constant Integer_4_Unsigned_C := 16#0000_0B31#;
    LIST_BASE                       : constant Integer_4_Unsigned_C := 16#0000_0B32#;
    LIST_INDEX                      : constant Integer_4_Unsigned_C := 16#0000_0B33#;
    POLYGON_MODE                    : constant Integer_4_Unsigned_C := 16#0000_0B40#;
    POLYGON_SMOOTH                  : constant Integer_4_Unsigned_C := 16#0000_0B41#;
    POLYGON_STIPPLE                 : constant Integer_4_Unsigned_C := 16#0000_0B42#;
    EDGE_FLAG                       : constant Integer_4_Unsigned_C := 16#0000_0B43#;
    CULL_FACE                       : constant Integer_4_Unsigned_C := 16#0000_0B44#;
    CULL_FACE_MODE                  : constant Integer_4_Unsigned_C := 16#0000_0B45#;
    FRONT_FACE                      : constant Integer_4_Unsigned_C := 16#0000_0B46#;
    LIGHTING                        : constant Integer_4_Unsigned_C := 16#0000_0B50#;
    LIGHT_MODEL_LOCAL_VIEWER        : constant Integer_4_Unsigned_C := 16#0000_0B51#;
    LIGHT_MODEL_TWO_SIDE            : constant Integer_4_Unsigned_C := 16#0000_0B52#;
    LIGHT_MODEL_AMBIENT             : constant Integer_4_Unsigned_C := 16#0000_0B53#;
    SHADE_MODEL                     : constant Integer_4_Unsigned_C := 16#0000_0B54#;
    COLOR_MATERIAL_FACE             : constant Integer_4_Unsigned_C := 16#0000_0B55#;
    COLOR_MATERIAL_PARAMETER        : constant Integer_4_Unsigned_C := 16#0000_0B56#;
    COLOR_MATERIAL                  : constant Integer_4_Unsigned_C := 16#0000_0B57#;
    FOG                             : constant Integer_4_Unsigned_C := 16#0000_0B60#;
    FOG_INDEX                       : constant Integer_4_Unsigned_C := 16#0000_0B61#;
    FOG_DENSITY                     : constant Integer_4_Unsigned_C := 16#0000_0B62#;
    FOG_START                       : constant Integer_4_Unsigned_C := 16#0000_0B63#;
    FOG_END                         : constant Integer_4_Unsigned_C := 16#0000_0B64#;
    FOG_MODE                        : constant Integer_4_Unsigned_C := 16#0000_0B65#;
    FOG_COLOR                       : constant Integer_4_Unsigned_C := 16#0000_0B66#;
    DEPTH_RANGE                     : constant Integer_4_Unsigned_C := 16#0000_0B70#;
    DEPTH_TEST                      : constant Integer_4_Unsigned_C := 16#0000_0B71#;
    DEPTH_WRITEMASK                 : constant Integer_4_Unsigned_C := 16#0000_0B72#;
    DEPTH_CLEAR_VALUE               : constant Integer_4_Unsigned_C := 16#0000_0B73#;
    DEPTH_FUNC                      : constant Integer_4_Unsigned_C := 16#0000_0B74#;
    ACCUM_CLEAR_VALUE               : constant Integer_4_Unsigned_C := 16#0000_0B80#;
    STENCIL_TEST                    : constant Integer_4_Unsigned_C := 16#0000_0B90#;
    STENCIL_CLEAR_VALUE             : constant Integer_4_Unsigned_C := 16#0000_0B91#;
    STENCIL_FUNC                    : constant Integer_4_Unsigned_C := 16#0000_0B92#;
    STENCIL_VALUE_MASK              : constant Integer_4_Unsigned_C := 16#0000_0B93#;
    STENCIL_FAIL                    : constant Integer_4_Unsigned_C := 16#0000_0B94#;
    STENCIL_PASS_DEPTH_FAIL         : constant Integer_4_Unsigned_C := 16#0000_0B95#;
    STENCIL_PASS_DEPTH_PASS         : constant Integer_4_Unsigned_C := 16#0000_0B96#;
    STENCIL_REF                     : constant Integer_4_Unsigned_C := 16#0000_0B97#;
    STENCIL_WRITEMASK               : constant Integer_4_Unsigned_C := 16#0000_0B98#;
    MATRIX_MODE                     : constant Integer_4_Unsigned_C := 16#0000_0BA0#;
    NORMALIZE                       : constant Integer_4_Unsigned_C := 16#0000_0BA1#;
    VIEWPORT                        : constant Integer_4_Unsigned_C := 16#0000_0BA2#;
    MODELVIEW_STACK_DEPTH           : constant Integer_4_Unsigned_C := 16#0000_0BA3#;
    PROJECTION_STACK_DEPTH          : constant Integer_4_Unsigned_C := 16#0000_0BA4#;
    TEXTURE_STACK_DEPTH             : constant Integer_4_Unsigned_C := 16#0000_0BA5#;
    MODELVIEW_MATRIX                : constant Integer_4_Unsigned_C := 16#0000_0BA6#;
    PROJECTION_MATRIX               : constant Integer_4_Unsigned_C := 16#0000_0BA7#;
    TEXTURE_MATRIX                  : constant Integer_4_Unsigned_C := 16#0000_0BA8#;
    ATTRIB_STACK_DEPTH              : constant Integer_4_Unsigned_C := 16#0000_0BB0#;
    CLIENT_ATTRIB_STACK_DEPTH       : constant Integer_4_Unsigned_C := 16#0000_0BB1#;
    ALPHA_TEST                      : constant Integer_4_Unsigned_C := 16#0000_0BC0#;
    ALPHA_TEST_FUNC                 : constant Integer_4_Unsigned_C := 16#0000_0BC1#;
    ALPHA_TEST_REF                  : constant Integer_4_Unsigned_C := 16#0000_0BC2#;
    DITHER                          : constant Integer_4_Unsigned_C := 16#0000_0BD0#;
    BLEND_DST                       : constant Integer_4_Unsigned_C := 16#0000_0BE0#;
    BLEND_SRC                       : constant Integer_4_Unsigned_C := 16#0000_0BE1#;
    BLEND                           : constant Integer_4_Unsigned_C := 16#0000_0BE2#;
    LOGIC_OP_MODE                   : constant Integer_4_Unsigned_C := 16#0000_0BF0#;
    INDEX_LOGIC_OP                  : constant Integer_4_Unsigned_C := 16#0000_0BF1#;
    COLOR_LOGIC_OP                  : constant Integer_4_Unsigned_C := 16#0000_0BF2#;
    AUX_BUFFERS                     : constant Integer_4_Unsigned_C := 16#0000_0C00#;
    DRAW_BUFFER                     : constant Integer_4_Unsigned_C := 16#0000_0C01#;
    READ_BUFFER                     : constant Integer_4_Unsigned_C := 16#0000_0C02#;
    SCISSOR_BOX                     : constant Integer_4_Unsigned_C := 16#0000_0C10#;
    SCISSOR_TEST                    : constant Integer_4_Unsigned_C := 16#0000_0C11#;
    INDEX_CLEAR_VALUE               : constant Integer_4_Unsigned_C := 16#0000_0C20#;
    INDEX_WRITEMASK                 : constant Integer_4_Unsigned_C := 16#0000_0C21#;
    COLOR_CLEAR_VALUE               : constant Integer_4_Unsigned_C := 16#0000_0C22#;
    COLOR_WRITEMASK                 : constant Integer_4_Unsigned_C := 16#0000_0C23#;
    INDEX_MODE                      : constant Integer_4_Unsigned_C := 16#0000_0C30#;
    RGBA_MODE                       : constant Integer_4_Unsigned_C := 16#0000_0C31#;
    DOUBLEBUFFER                    : constant Integer_4_Unsigned_C := 16#0000_0C32#;
    STEREO                          : constant Integer_4_Unsigned_C := 16#0000_0C33#;
    RENDER_MODE                     : constant Integer_4_Unsigned_C := 16#0000_0C40#;
    PERSPECTIVE_CORRECTION_HINT     : constant Integer_4_Unsigned_C := 16#0000_0C50#;
    POINT_SMOOTH_HINT               : constant Integer_4_Unsigned_C := 16#0000_0C51#;
    LINE_SMOOTH_HINT                : constant Integer_4_Unsigned_C := 16#0000_0C52#;
    POLYGON_SMOOTH_HINT             : constant Integer_4_Unsigned_C := 16#0000_0C53#;
    FOG_HINT                        : constant Integer_4_Unsigned_C := 16#0000_0C54#;
    TEXTURE_GEN_S                   : constant Integer_4_Unsigned_C := 16#0000_0C60#;
    TEXTURE_GEN_T                   : constant Integer_4_Unsigned_C := 16#0000_0C61#;
    TEXTURE_GEN_R                   : constant Integer_4_Unsigned_C := 16#0C62#;
    TEXTURE_GEN_Q                   : constant Integer_4_Unsigned_C := 16#0C63#;
    PIXEL_MAP_I_TO_I                : constant Integer_4_Unsigned_C := 16#0C70#;
    PIXEL_MAP_S_TO_S                : constant Integer_4_Unsigned_C := 16#0C71#;
    PIXEL_MAP_I_TO_R                : constant Integer_4_Unsigned_C := 16#0C72#;
    PIXEL_MAP_I_TO_G                : constant Integer_4_Unsigned_C := 16#0C73#;
    PIXEL_MAP_I_TO_B                : constant Integer_4_Unsigned_C := 16#0C74#;
    PIXEL_MAP_I_TO_A                : constant Integer_4_Unsigned_C := 16#0C75#;
    PIXEL_MAP_R_TO_R                : constant Integer_4_Unsigned_C := 16#0C76#;
    PIXEL_MAP_G_TO_G                : constant Integer_4_Unsigned_C := 16#0C77#;
    PIXEL_MAP_B_TO_B                : constant Integer_4_Unsigned_C := 16#0C78#;
    PIXEL_MAP_A_TO_A                : constant Integer_4_Unsigned_C := 16#0C79#;
    PIXEL_MAP_I_TO_I_SIZE           : constant Integer_4_Unsigned_C := 16#0CB0#;
    PIXEL_MAP_S_TO_S_SIZE           : constant Integer_4_Unsigned_C := 16#0CB1#;
    PIXEL_MAP_I_TO_R_SIZE           : constant Integer_4_Unsigned_C := 16#0CB2#;
    PIXEL_MAP_I_TO_G_SIZE           : constant Integer_4_Unsigned_C := 16#0CB3#;
    PIXEL_MAP_I_TO_B_SIZE           : constant Integer_4_Unsigned_C := 16#0CB4#;
    PIXEL_MAP_I_TO_A_SIZE           : constant Integer_4_Unsigned_C := 16#0CB5#;
    PIXEL_MAP_R_TO_R_SIZE           : constant Integer_4_Unsigned_C := 16#0CB6#;
    PIXEL_MAP_G_TO_G_SIZE           : constant Integer_4_Unsigned_C := 16#0CB7#;
    PIXEL_MAP_B_TO_B_SIZE           : constant Integer_4_Unsigned_C := 16#0CB8#;
    PIXEL_MAP_A_TO_A_SIZE           : constant Integer_4_Unsigned_C := 16#0CB9#;
    UNPACK_SWAP_BYTES               : constant Integer_4_Unsigned_C := 16#0CF0#;
    UNPACK_LSB_FIRST                : constant Integer_4_Unsigned_C := 16#0CF1#;
    UNPACK_ROW_LENGTH               : constant Integer_4_Unsigned_C := 16#0CF2#;
    UNPACK_SKIP_ROWS                : constant Integer_4_Unsigned_C := 16#0CF3#;
    UNPACK_SKIP_PIXELS              : constant Integer_4_Unsigned_C := 16#0CF4#;
    UNPACK_ALIGNMENT                : constant Integer_4_Unsigned_C := 16#0CF5#;
    PACK_SWAP_BYTES                 : constant Integer_4_Unsigned_C := 16#0D00#;
    PACK_LSB_FIRST                  : constant Integer_4_Unsigned_C := 16#0D01#;
    PACK_ROW_LENGTH                 : constant Integer_4_Unsigned_C := 16#0D02#;
    PACK_SKIP_ROWS                  : constant Integer_4_Unsigned_C := 16#0D03#;
    PACK_SKIP_PIXELS                : constant Integer_4_Unsigned_C := 16#0D04#;
    PACK_ALIGNMENT                  : constant Integer_4_Unsigned_C := 16#0D05#;
    MAP_COLOR                       : constant Integer_4_Unsigned_C := 16#0D10#;
    MAP_STENCIL                     : constant Integer_4_Unsigned_C := 16#0D11#;
    INDEX_SHIFT                     : constant Integer_4_Unsigned_C := 16#0D12#;
    INDEX_OFFSET                    : constant Integer_4_Unsigned_C := 16#0D13#;
    RED_SCALE                       : constant Integer_4_Unsigned_C := 16#0D14#;
    RED_BIAS                        : constant Integer_4_Unsigned_C := 16#0D15#;
    ZOOM_X                          : constant Integer_4_Unsigned_C := 16#0D16#;
    ZOOM_Y                          : constant Integer_4_Unsigned_C := 16#0D17#;
    GREEN_SCALE                     : constant Integer_4_Unsigned_C := 16#0D18#;
    GREEN_BIAS                      : constant Integer_4_Unsigned_C := 16#0D19#;
    BLUE_SCALE                      : constant Integer_4_Unsigned_C := 16#0D1A#;
    BLUE_BIAS                       : constant Integer_4_Unsigned_C := 16#0D1B#;
    ALPHA_SCALE                     : constant Integer_4_Unsigned_C := 16#0D1C#;
    ALPHA_BIAS                      : constant Integer_4_Unsigned_C := 16#0D1D#;
    DEPTH_SCALE                     : constant Integer_4_Unsigned_C := 16#0D1E#;
    DEPTH_BIAS                      : constant Integer_4_Unsigned_C := 16#0D1F#;
    MAX_EVAL_ORDER                  : constant Integer_4_Unsigned_C := 16#0D30#;
    MAX_LIGHTS                      : constant Integer_4_Unsigned_C := 16#0D31#;
    MAX_CLIP_PLANES                 : constant Integer_4_Unsigned_C := 16#0D32#;
    MAX_TEXTURE_SIZE                : constant Integer_4_Unsigned_C := 16#0D33#;
    MAX_PIXEL_MAP_TABLE             : constant Integer_4_Unsigned_C := 16#0D34#;
    MAX_ATTRIB_STACK_DEPTH          : constant Integer_4_Unsigned_C := 16#0D35#;
    MAX_MODELVIEW_STACK_DEPTH       : constant Integer_4_Unsigned_C := 16#0D36#;
    MAX_NAME_STACK_DEPTH            : constant Integer_4_Unsigned_C := 16#0D37#;
    MAX_PROJECTION_STACK_DEPTH      : constant Integer_4_Unsigned_C := 16#0D38#;
    MAX_TEXTURE_STACK_DEPTH         : constant Integer_4_Unsigned_C := 16#0D39#;
    MAX_VIEWPORT_DIMS               : constant Integer_4_Unsigned_C := 16#0D3A#;
    MAX_CLIENT_ATTRIB_STACK_DEPTH   : constant Integer_4_Unsigned_C := 16#0D3B#;
    SUBPIXEL_BITS                   : constant Integer_4_Unsigned_C := 16#0D50#;
    INDEX_BITS                      : constant Integer_4_Unsigned_C := 16#0D51#;
    RED_BITS                        : constant Integer_4_Unsigned_C := 16#0D52#;
    GREEN_BITS                      : constant Integer_4_Unsigned_C := 16#0D53#;
    BLUE_BITS                       : constant Integer_4_Unsigned_C := 16#0D54#;
    ALPHA_BITS                      : constant Integer_4_Unsigned_C := 16#0D55#;
    DEPTH_BITS                      : constant Integer_4_Unsigned_C := 16#0D56#;
    STENCIL_BITS                    : constant Integer_4_Unsigned_C := 16#0D57#;
    ACCUM_RED_BITS                  : constant Integer_4_Unsigned_C := 16#0D58#;
    ACCUM_GREEN_BITS                : constant Integer_4_Unsigned_C := 16#0D59#;
    ACCUM_BLUE_BITS                 : constant Integer_4_Unsigned_C := 16#0D5A#;
    ACCUM_ALPHA_BITS                : constant Integer_4_Unsigned_C := 16#0D5B#;
    NAME_STACK_DEPTH                : constant Integer_4_Unsigned_C := 16#0D70#;
    AUTO_NORMAL                     : constant Integer_4_Unsigned_C := 16#0D80#;
    MAP1_COLOR_4                    : constant Integer_4_Unsigned_C := 16#0D90#;
    MAP1_INDEX                      : constant Integer_4_Unsigned_C := 16#0D91#;
    MAP1_NORMAL                     : constant Integer_4_Unsigned_C := 16#0D92#;
    MAP1_TEXTURE_COORDINATE_1            : constant Integer_4_Unsigned_C := 16#0D93#;
    MAP1_TEXTURE_COORDINATE_2            : constant Integer_4_Unsigned_C := 16#0D94#;
    MAP1_TEXTURE_COORDINATE_3            : constant Integer_4_Unsigned_C := 16#0D95#;
    MAP1_TEXTURE_COORDINATE_4            : constant Integer_4_Unsigned_C := 16#0D96#;
    MAP1_VERTEX_3                   : constant Integer_4_Unsigned_C := 16#0D97#;
    MAP1_VERTEX_4                   : constant Integer_4_Unsigned_C := 16#0D98#;
    MAP2_COLOR_4                    : constant Integer_4_Unsigned_C := 16#0DB0#;
    MAP2_INDEX                      : constant Integer_4_Unsigned_C := 16#0DB1#;
    MAP2_NORMAL                     : constant Integer_4_Unsigned_C := 16#0DB2#;
    MAP2_TEXTURE_COORDINATE_1            : constant Integer_4_Unsigned_C := 16#0DB3#;
    MAP2_TEXTURE_COORDINATE_2            : constant Integer_4_Unsigned_C := 16#0DB4#;
    MAP2_TEXTURE_COORDINATE_3            : constant Integer_4_Unsigned_C := 16#0DB5#;
    MAP2_TEXTURE_COORDINATE_4            : constant Integer_4_Unsigned_C := 16#0DB6#;
    MAP2_VERTEX_3                   : constant Integer_4_Unsigned_C := 16#0DB7#;
    MAP2_VERTEX_4                   : constant Integer_4_Unsigned_C := 16#0DB8#;
    MAP1_GRID_DOMAIN                : constant Integer_4_Unsigned_C := 16#0DD0#;
    MAP1_GRID_SEGMENTS              : constant Integer_4_Unsigned_C := 16#0DD1#;
    MAP2_GRID_DOMAIN                : constant Integer_4_Unsigned_C := 16#0DD2#;
    MAP2_GRID_SEGMENTS              : constant Integer_4_Unsigned_C := 16#0DD3#;
    TEXTURE_1D                      : constant Integer_4_Unsigned_C := 16#0DE0#;
    TEXTURE_2D                      : constant Integer_4_Unsigned_C := 16#0DE1#;
    FEEDBACK_BUFFER_POINTER         : constant Integer_4_Unsigned_C := 16#0DF0#;
    FEEDBACK_BUFFER_SIZE            : constant Integer_4_Unsigned_C := 16#0DF1#;
    FEEDBACK_BUFFER_TYPE            : constant Integer_4_Unsigned_C := 16#0DF2#;
    SELECTION_BUFFER_POINTER        : constant Integer_4_Unsigned_C := 16#0DF3#;
    SELECTION_BUFFER_SIZE           : constant Integer_4_Unsigned_C := 16#0DF4#;
    TEXTURE_WIDTH                   : constant Integer_4_Unsigned_C := 16#1000#;
    TEXTURE_HEIGHT                  : constant Integer_4_Unsigned_C := 16#1001#;
    TEXTURE_INTERNAL_FORMAT         : constant Integer_4_Unsigned_C := 16#1003#;
    TEXTURE_BORDER_COLOR            : constant Integer_4_Unsigned_C := 16#1004#;
    TEXTURE_BORDER                  : constant Integer_4_Unsigned_C := 16#1005#;
    DONT_CARE                       : constant Integer_4_Unsigned_C := 16#1100#;
    FASTEST                         : constant Integer_4_Unsigned_C := 16#1101#;
    NICEST                          : constant Integer_4_Unsigned_C := 16#1102#;
    LIGHT0                          : constant Integer_4_Unsigned_C := 16#4000#;
    LIGHT1                          : constant Integer_4_Unsigned_C := 16#4001#;
    LIGHT2                          : constant Integer_4_Unsigned_C := 16#4002#;
    LIGHT3                          : constant Integer_4_Unsigned_C := 16#4003#;
    LIGHT4                          : constant Integer_4_Unsigned_C := 16#4004#;
    LIGHT5                          : constant Integer_4_Unsigned_C := 16#4005#;
    LIGHT6                          : constant Integer_4_Unsigned_C := 16#4006#;
    LIGHT7                          : constant Integer_4_Unsigned_C := 16#4007#;
    AMBIENT                         : constant Integer_4_Unsigned_C := 16#1200#;
    DIFFUSE                         : constant Integer_4_Unsigned_C := 16#1201#;
    SPECULAR                        : constant Integer_4_Unsigned_C := 16#1202#;
    POSITION                        : constant Integer_4_Unsigned_C := 16#1203#;
    SPOT_DIRECTION                  : constant Integer_4_Unsigned_C := 16#1204#;
    SPOT_EXPONENT                   : constant Integer_4_Unsigned_C := 16#1205#;
    SPOT_CUTOFF                     : constant Integer_4_Unsigned_C := 16#1206#;
    CONSTANT_ATTENUATION            : constant Integer_4_Unsigned_C := 16#1207#;
    LINEAR_ATTENUATION              : constant Integer_4_Unsigned_C := 16#1208#;
    QUADRATIC_ATTENUATION           : constant Integer_4_Unsigned_C := 16#1209#;
    COMPILE                         : constant Integer_4_Unsigned_C := 16#1300#;
    COMPILE_AND_EXECUTE             : constant Integer_4_Unsigned_C := 16#1301#;
    CLEAR                           : constant Integer_4_Unsigned_C := 16#1500#;
    AND                             : constant Integer_4_Unsigned_C := 16#1501#;
    AND_REVERSE                     : constant Integer_4_Unsigned_C := 16#1502#;
    COPY                            : constant Integer_4_Unsigned_C := 16#1503#;
    AND_INVERTED                    : constant Integer_4_Unsigned_C := 16#1504#;
    NOOP                            : constant Integer_4_Unsigned_C := 16#1505#;
    XOR                             : constant Integer_4_Unsigned_C := 16#1506#;
    OR                              : constant Integer_4_Unsigned_C := 16#1507#;
    NOR                             : constant Integer_4_Unsigned_C := 16#1508#;
    EQUIV                           : constant Integer_4_Unsigned_C := 16#1509#;
    INVERT                          : constant Integer_4_Unsigned_C := 16#150A#;
    OR_REVERSE                      : constant Integer_4_Unsigned_C := 16#150B#;
    COPY_INVERTED                   : constant Integer_4_Unsigned_C := 16#150C#;
    OR_INVERTED                     : constant Integer_4_Unsigned_C := 16#150D#;
    NAND                            : constant Integer_4_Unsigned_C := 16#150E#;
    SET                             : constant Integer_4_Unsigned_C := 16#150F#;
    EMISSION                        : constant Integer_4_Unsigned_C := 16#1600#;
    SHININESS                       : constant Integer_4_Unsigned_C := 16#1601#;
    AMBIENT_AND_DIFFUSE             : constant Integer_4_Unsigned_C := 16#1602#;
    COLOR_INDEXES                   : constant Integer_4_Unsigned_C := 16#1603#;
    MODELVIEW                       : constant Integer_4_Unsigned_C := 16#1700#;
    PROJECTION                      : constant Integer_4_Unsigned_C := 16#1701#;
    TEXTURE                         : constant Integer_4_Unsigned_C := 16#1702#;
    COLOR                           : constant Integer_4_Unsigned_C := 16#1800#;
    DEPTH                           : constant Integer_4_Unsigned_C := 16#1801#;
    STENCIL                         : constant Integer_4_Unsigned_C := 16#1802#;
    COLOR_INDEX                     : constant Integer_4_Unsigned_C := 16#1900#;
    STENCIL_INDEX                   : constant Integer_4_Unsigned_C := 16#1901#;
    DEPTH_COMPONENT                 : constant Integer_4_Unsigned_C := 16#1902#;
    RED                             : constant Integer_4_Unsigned_C := 16#1903#;
    GREEN                           : constant Integer_4_Unsigned_C := 16#1904#;
    BLUE                            : constant Integer_4_Unsigned_C := 16#1905#;
    ALPHA                           : constant Integer_4_Unsigned_C := 16#1906#;
    RGB                             : constant Integer_4_Unsigned_C := 16#1907#;
    RGBA                            : constant Integer_4_Unsigned_C := 16#1908#;
    LUMINANCE                       : constant Integer_4_Unsigned_C := 16#1909#;
    LUMINANCE_ALPHA                 : constant Integer_4_Unsigned_C := 16#190A#;
    BITMAP                          : constant Integer_4_Unsigned_C := 16#1A00#;
    POINT                           : constant Integer_4_Unsigned_C := 16#1B00#;
    LINE                            : constant Integer_4_Unsigned_C := 16#1B01#;
    FILL                            : constant Integer_4_Unsigned_C := 16#1B02#;
    RENDER                          : constant Integer_4_Unsigned_C := 16#1C00#;
    FEEDBACK                        : constant Integer_4_Unsigned_C := 16#1C01#;
    SELECT                          : constant Integer_4_Unsigned_C := 16#1C02#;
    FLAT                            : constant Integer_4_Unsigned_C := 16#1D00#;
    SMOOTH                          : constant Integer_4_Unsigned_C := 16#1D01#;
    KEEP                            : constant Integer_4_Unsigned_C := 16#1E00#;
    REPLACE                         : constant Integer_4_Unsigned_C := 16#1E01#;
    INCR                            : constant Integer_4_Unsigned_C := 16#1E02#;
    DECR                            : constant Integer_4_Unsigned_C := 16#1E03#;
    VENDOR                          : constant Integer_4_Unsigned_C := 16#1F00#;
    RENDERER                        : constant Integer_4_Unsigned_C := 16#1F01#;
    VERSION                         : constant Integer_4_Unsigned_C := 16#1F02#;
    EXTENSIONS                      : constant Integer_4_Unsigned_C := 16#1F03#;
    S                               : constant Integer_4_Unsigned_C := 16#2000#;
    T                               : constant Integer_4_Unsigned_C := 16#2001#;
    R                               : constant Integer_4_Unsigned_C := 16#2002#;
    Q                               : constant Integer_4_Unsigned_C := 16#2003#;
    MODULATE                        : constant Integer_4_Unsigned_C := 16#2100#;
    DECAL                           : constant Integer_4_Unsigned_C := 16#2101#;
    TEXTURE_ENV_MODE                : constant Integer_4_Unsigned_C := 16#2200#;
    TEXTURE_ENV_COLOR               : constant Integer_4_Unsigned_C := 16#2201#;
    TEXTURE_ENV                     : constant Integer_4_Unsigned_C := 16#2300#;
    EYE_LINEAR                      : constant Integer_4_Unsigned_C := 16#2400#;
    OBJECT_LINEAR                   : constant Integer_4_Unsigned_C := 16#2401#;
    SPHERE_MAP                      : constant Integer_4_Unsigned_C := 16#2402#;
    TEXTURE_GEN_MODE                : constant Integer_4_Unsigned_C := 16#2500#;
    OBJECT_PLANE                    : constant Integer_4_Unsigned_C := 16#2501#;
    EYE_PLANE                       : constant Integer_4_Unsigned_C := 16#2502#;
    NEAREST                         : constant Integer_4_Unsigned_C := 16#2600#;
    LINEAR                          : constant Integer_4_Unsigned_C := 16#2601#;
    NEAREST_MIPMAP_NEAREST          : constant Integer_4_Unsigned_C := 16#2700#;
    LINEAR_MIPMAP_NEAREST           : constant Integer_4_Unsigned_C := 16#2701#;
    NEAREST_MIPMAP_LINEAR           : constant Integer_4_Unsigned_C := 16#2702#;
    LINEAR_MIPMAP_LINEAR            : constant Integer_4_Unsigned_C := 16#2703#;
    TEXTURE_MAG_FILTER              : constant Integer_4_Unsigned_C := 16#2800#;
    TEXTURE_MIN_FILTER              : constant Integer_4_Unsigned_C := 16#2801#;
    TEXTURE_WRAP_S                  : constant Integer_4_Unsigned_C := 16#2802#;
    TEXTURE_WRAP_T                  : constant Integer_4_Unsigned_C := 16#2803#;
    CLAMP                           : constant Integer_4_Unsigned_C := 16#2900#;
    REPEAT                          : constant Integer_4_Unsigned_C := 16#2901#;
    CLIENT_PIXEL_STORE_BIT          : constant Integer_4_Unsigned_C := 16#0000_0001#;
    CLIENT_VERTEX_ARRAY_BIT         : constant Integer_4_Unsigned_C := 16#0000_0002#;
    CLIENT_ALL_ATTRIB_BITS          : constant Integer_4_Unsigned_C := 16#FFFF_FFFF#;
    POLYGON_OFFSET_FACTOR           : constant Integer_4_Unsigned_C := 16#8038#;
    POLYGON_OFFSET_UNITS            : constant Integer_4_Unsigned_C := 16#2A00#;
    POLYGON_OFFSET_POINT            : constant Integer_4_Unsigned_C := 16#2A01#;
    POLYGON_OFFSET_LINE             : constant Integer_4_Unsigned_C := 16#2A02#;
    POLYGON_OFFSET_FILL             : constant Integer_4_Unsigned_C := 16#8037#;
    ALPHA4                          : constant Integer_4_Unsigned_C := 16#803B#;
    ALPHA8                          : constant Integer_4_Unsigned_C := 16#803C#;
    ALPHA12                         : constant Integer_4_Unsigned_C := 16#803D#;
    ALPHA16                         : constant Integer_4_Unsigned_C := 16#803E#;
    LUMINANCE4                      : constant Integer_4_Unsigned_C := 16#803F#;
    LUMINANCE8                      : constant Integer_4_Unsigned_C := 16#8040#;
    LUMINANCE12                     : constant Integer_4_Unsigned_C := 16#8041#;
    LUMINANCE16                     : constant Integer_4_Unsigned_C := 16#8042#;
    LUMINANCE4_ALPHA4               : constant Integer_4_Unsigned_C := 16#8043#;
    LUMINANCE6_ALPHA2               : constant Integer_4_Unsigned_C := 16#8044#;
    LUMINANCE8_ALPHA8               : constant Integer_4_Unsigned_C := 16#8045#;
    LUMINANCE12_ALPHA4              : constant Integer_4_Unsigned_C := 16#8046#;
    LUMINANCE12_ALPHA12             : constant Integer_4_Unsigned_C := 16#8047#;
    LUMINANCE16_ALPHA16             : constant Integer_4_Unsigned_C := 16#8048#;
    INTENSITY                       : constant Integer_4_Unsigned_C := 16#8049#;
    INTENSITY4                      : constant Integer_4_Unsigned_C := 16#804A#;
    INTENSITY8                      : constant Integer_4_Unsigned_C := 16#804B#;
    INTENSITY12                     : constant Integer_4_Unsigned_C := 16#804C#;
    INTENSITY16                     : constant Integer_4_Unsigned_C := 16#804D#;
    R3_G3_B2                        : constant Integer_4_Unsigned_C := 16#2A10#;
    RGB4                            : constant Integer_4_Unsigned_C := 16#804F#;
    RGB5                            : constant Integer_4_Unsigned_C := 16#8050#;
    RGB8                            : constant Integer_4_Unsigned_C := 16#8051#;
    RGB10                           : constant Integer_4_Unsigned_C := 16#8052#;
    RGB12                           : constant Integer_4_Unsigned_C := 16#8053#;
    RGB16                           : constant Integer_4_Unsigned_C := 16#8054#;
    RGBA2                           : constant Integer_4_Unsigned_C := 16#8055#;
    RGBA4                           : constant Integer_4_Unsigned_C := 16#8056#;
    RGB5_A1                         : constant Integer_4_Unsigned_C := 16#8057#;
    RGBA8                           : constant Integer_4_Unsigned_C := 16#8058#;
    RGB10_A2                        : constant Integer_4_Unsigned_C := 16#8059#;
    RGBA12                          : constant Integer_4_Unsigned_C := 16#805A#;
    RGBA16                          : constant Integer_4_Unsigned_C := 16#805B#;
    TEXTURE_RED_SIZE                : constant Integer_4_Unsigned_C := 16#805C#;
    TEXTURE_GREEN_SIZE              : constant Integer_4_Unsigned_C := 16#805D#;
    TEXTURE_BLUE_SIZE               : constant Integer_4_Unsigned_C := 16#805E#;
    TEXTURE_ALPHA_SIZE              : constant Integer_4_Unsigned_C := 16#805F#;
    TEXTURE_LUMINANCE_SIZE          : constant Integer_4_Unsigned_C := 16#8060#;
    TEXTURE_INTENSITY_SIZE          : constant Integer_4_Unsigned_C := 16#8061#;
    PROXY_TEXTURE_1D                : constant Integer_4_Unsigned_C := 16#8063#;
    PROXY_TEXTURE_2D                : constant Integer_4_Unsigned_C := 16#8064#;
    TEXTURE_PRIORITY                : constant Integer_4_Unsigned_C := 16#8066#;
    TEXTURE_RESIDENT                : constant Integer_4_Unsigned_C := 16#8067#;
    TEXTURE_BINDING_1D              : constant Integer_4_Unsigned_C := 16#8068#;
    TEXTURE_BINDING_2D              : constant Integer_4_Unsigned_C := 16#8069#;
    VERTEX_ARRAY                    : constant Integer_4_Unsigned_C := 16#8074#;
    NORMAL_ARRAY                    : constant Integer_4_Unsigned_C := 16#8075#;
    COLOR_ARRAY                     : constant Integer_4_Unsigned_C := 16#8076#;
    INDEX_ARRAY                     : constant Integer_4_Unsigned_C := 16#8077#;
    TEXTURE_COORDINATE_ARRAY             : constant Integer_4_Unsigned_C := 16#8078#;
    EDGE_FLAG_ARRAY                 : constant Integer_4_Unsigned_C := 16#8079#;
    VERTEX_ARRAY_SIZE               : constant Integer_4_Unsigned_C := 16#807A#;
    VERTEX_ARRAY_TYPE               : constant Integer_4_Unsigned_C := 16#807B#;
    VERTEX_ARRAY_STRIDE             : constant Integer_4_Unsigned_C := 16#807C#;
    NORMAL_ARRAY_TYPE               : constant Integer_4_Unsigned_C := 16#807E#;
    NORMAL_ARRAY_STRIDE             : constant Integer_4_Unsigned_C := 16#807F#;
    COLOR_ARRAY_SIZE                : constant Integer_4_Unsigned_C := 16#8081#;
    COLOR_ARRAY_TYPE                : constant Integer_4_Unsigned_C := 16#8082#;
    COLOR_ARRAY_STRIDE              : constant Integer_4_Unsigned_C := 16#8083#;
    INDEX_ARRAY_TYPE                : constant Integer_4_Unsigned_C := 16#8085#;
    INDEX_ARRAY_STRIDE              : constant Integer_4_Unsigned_C := 16#8086#;
    TEXTURE_COORDINATE_ARRAY_SIZE        : constant Integer_4_Unsigned_C := 16#8088#;
    TEXTURE_COORDINATE_ARRAY_TYPE        : constant Integer_4_Unsigned_C := 16#8089#;
    TEXTURE_COORDINATE_ARRAY_STRIDE      : constant Integer_4_Unsigned_C := 16#808A#;
    EDGE_FLAG_ARRAY_STRIDE          : constant Integer_4_Unsigned_C := 16#808C#;
    VERTEX_ARRAY_POINTER            : constant Integer_4_Unsigned_C := 16#808E#;
    NORMAL_ARRAY_POINTER            : constant Integer_4_Unsigned_C := 16#808F#;
    COLOR_ARRAY_POINTER             : constant Integer_4_Unsigned_C := 16#8090#;
    INDEX_ARRAY_POINTER             : constant Integer_4_Unsigned_C := 16#8091#;
    TEXTURE_COORDINATE_ARRAY_POINTER     : constant Integer_4_Unsigned_C := 16#8092#;
    EDGE_FLAG_ARRAY_POINTER         : constant Integer_4_Unsigned_C := 16#8093#;
    V2F                             : constant Integer_4_Unsigned_C := 16#2A20#;
    V3F                             : constant Integer_4_Unsigned_C := 16#2A21#;
    C4UB_V2F                        : constant Integer_4_Unsigned_C := 16#2A22#;
    C4UB_V3F                        : constant Integer_4_Unsigned_C := 16#2A23#;
    C3F_V3F                         : constant Integer_4_Unsigned_C := 16#2A24#;
    N3F_V3F                         : constant Integer_4_Unsigned_C := 16#2A25#;
    C4F_N3F_V3F                     : constant Integer_4_Unsigned_C := 16#2A26#;
    T2F_V3F                         : constant Integer_4_Unsigned_C := 16#2A27#;
    T4F_V4F                         : constant Integer_4_Unsigned_C := 16#2A28#;
    T2F_C4UB_V3F                    : constant Integer_4_Unsigned_C := 16#2A29#;
    T2F_C3F_V3F                     : constant Integer_4_Unsigned_C := 16#2A2A#;
    T2F_N3F_V3F                     : constant Integer_4_Unsigned_C := 16#2A2B#;
    T2F_C4F_N3F_V3F                 : constant Integer_4_Unsigned_C := 16#2A2C#;
    T4F_C4F_N3F_V4F                 : constant Integer_4_Unsigned_C := 16#2A2D#;
    vertex_array                : constant Integer_4_Unsigned_C := 1;
    bgra                        : constant Integer_4_Unsigned_C := 1;
    paletted_texture            : constant Integer_4_Unsigned_C := 1;
    swap_hint                   : constant Integer_4_Unsigned_C := 1;
    draw_range_elements         : constant Integer_4_Unsigned_C := 1;
    VERTEX_ARRAY                : constant Integer_4_Unsigned_C := 16#8074#;
    NORMAL_ARRAY                : constant Integer_4_Unsigned_C := 16#8075#;
    COLOR_ARRAY                 : constant Integer_4_Unsigned_C := 16#8076#;
    INDEX_ARRAY                 : constant Integer_4_Unsigned_C := 16#8077#;
    TEXTURE_COORDINATE_ARRAY         : constant Integer_4_Unsigned_C := 16#8078#;
    EDGE_FLAG_ARRAY             : constant Integer_4_Unsigned_C := 16#8079#;
    VERTEX_ARRAY_SIZE           : constant Integer_4_Unsigned_C := 16#807A#;
    VERTEX_ARRAY_TYPE           : constant Integer_4_Unsigned_C := 16#807B#;
    VERTEX_ARRAY_STRIDE         : constant Integer_4_Unsigned_C := 16#807C#;
    VERTEX_ARRAY_COUNT          : constant Integer_4_Unsigned_C := 16#807D#;
    NORMAL_ARRAY_TYPE           : constant Integer_4_Unsigned_C := 16#807E#;
    NORMAL_ARRAY_STRIDE         : constant Integer_4_Unsigned_C := 16#807F#;
    NORMAL_ARRAY_COUNT          : constant Integer_4_Unsigned_C := 16#8080#;
    COLOR_ARRAY_SIZE            : constant Integer_4_Unsigned_C := 16#8081#;
    COLOR_ARRAY_TYPE            : constant Integer_4_Unsigned_C := 16#8082#;
    COLOR_ARRAY_STRIDE          : constant Integer_4_Unsigned_C := 16#8083#;
    COLOR_ARRAY_COUNT           : constant Integer_4_Unsigned_C := 16#8084#;
    INDEX_ARRAY_TYPE            : constant Integer_4_Unsigned_C := 16#8085#;
    INDEX_ARRAY_STRIDE          : constant Integer_4_Unsigned_C := 16#8086#;
    INDEX_ARRAY_COUNT           : constant Integer_4_Unsigned_C := 16#8087#;
    TEXTURE_COORDINATE_ARRAY_SIZE    : constant Integer_4_Unsigned_C := 16#8088#;
    TEXTURE_COORDINATE_ARRAY_TYPE    : constant Integer_4_Unsigned_C := 16#8089#;
    TEXTURE_COORDINATE_ARRAY_STRIDE  : constant Integer_4_Unsigned_C := 16#808A#;
    TEXTURE_COORDINATE_ARRAY_COUNT   : constant Integer_4_Unsigned_C := 16#808B#;
    EDGE_FLAG_ARRAY_STRIDE      : constant Integer_4_Unsigned_C := 16#808C#;
    EDGE_FLAG_ARRAY_COUNT       : constant Integer_4_Unsigned_C := 16#808D#;
    VERTEX_ARRAY_POINTER        : constant Integer_4_Unsigned_C := 16#808E#;
    NORMAL_ARRAY_POINTER        : constant Integer_4_Unsigned_C := 16#808F#;
    COLOR_ARRAY_POINTER         : constant Integer_4_Unsigned_C := 16#8090#;
    INDEX_ARRAY_POINTER         : constant Integer_4_Unsigned_C := 16#8091#;
    TEXTURE_COORDINATE_ARRAY_POINTER : constant Integer_4_Unsigned_C := 16#8092#;
    EDGE_FLAG_ARRAY_POINTER     : constant Integer_4_Unsigned_C := 16#8093#;
    DOUBLE                      : constant Integer_4_Unsigned_C := DOUBLE;
    BLUE_GREEN_RED                         : constant Integer_4_Unsigned_C := 16#80E0#;
    BLUE_GREEN_RED_ALPHA                        : constant Integer_4_Unsigned_C := 16#80E1#;
    COLOR_TABLE_FORMAT          : constant Integer_4_Unsigned_C := 16#80D8#;
    COLOR_TABLE_WIDTH           : constant Integer_4_Unsigned_C := 16#80D9#;
    COLOR_TABLE_RED_SIZE        : constant Integer_4_Unsigned_C := 16#80DA#;
    COLOR_TABLE_GREEN_SIZE      : constant Integer_4_Unsigned_C := 16#80DB#;
    COLOR_TABLE_BLUE_SIZE       : constant Integer_4_Unsigned_C := 16#80DC#;
    COLOR_TABLE_ALPHA_SIZE      : constant Integer_4_Unsigned_C := 16#80DD#;
    COLOR_TABLE_LUMINANCE_SIZE  : constant Integer_4_Unsigned_C := 16#80DE#;
    COLOR_TABLE_INTENSITY_SIZE  : constant Integer_4_Unsigned_C := 16#80DF#;
    COLOR_INDEX1                : constant Integer_4_Unsigned_C := 16#80E2#;
    COLOR_INDEX2                : constant Integer_4_Unsigned_C := 16#80E3#;
    COLOR_INDEX4                : constant Integer_4_Unsigned_C := 16#80E4#;
    COLOR_INDEX8                : constant Integer_4_Unsigned_C := 16#80E5#;
    COLOR_INDEX12               : constant Integer_4_Unsigned_C := 16#80E6#;
    COLOR_INDEX16               : constant Integer_4_Unsigned_C := 16#80E7#;
    MAXIMUM_ELEMENTS_VERTICES_WIN       : constant Integer_4_Unsigned_C := 16#80E8#;
    MAXIMUM_ELEMENTS_INDICES_WIN        : constant Integer_4_Unsigned_C := 16#80E9#;
    PHONG_WIN                       : constant Integer_4_Unsigned_C := 16#80EA#;
    PHONG_HINT_WIN                  : constant Integer_4_Unsigned_C := 16#80EB#;
    FOG_SPECULAR_TEXTURE_WIN        : constant Integer_4_Unsigned_C := 16#80EC#;
    LOGIC_OPERATION                        : constant Integer_4_Unsigned_C := INDEX_LOGIC_OP;
    TEXTURE_COMPONENTS              : constant Integer_4_Unsigned_C := TEXTURE_INTERNAL_FORMAT;
  -----------------
  -- Subprograms --
  -----------------

-- 1. Detect all hardware displays
-- 2. Setup for each one
-- 3. Call wglShareLists
-- 4. Set wglMakeCurrent to HDC and HGLRC for context 0 (wglMakeCurrent method)
-- 5. Create textures, VBOs, disp lists, frame buffers, etc.
-- 6. Start main rendering (for each monitor)
-- 7. End render loop
-- 8. Delete all textures, VBOs, then close contexts.

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
      Grouping : in Integer_4_Unsigned; -- Default: UNSIGNED_BYTE -- Others: 2_BYTES, 3_BYTES, 4_BYTES
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
  end Neo.Link.OpenGL;
