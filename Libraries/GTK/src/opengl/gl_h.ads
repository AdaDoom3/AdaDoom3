-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2001-2013, AdaCore                  --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------
--
--  Generated from GL/gl.h
--  Date: Wed Sep 22 12:47:08 1999
--

with Interfaces.C;
with Interfaces.C.Extensions;
with System;

package gl_h is
   pragma Warnings (Off);
   --  disable warnings for this file which was generated automatically

   GL_VERSION_1_1     : constant := 1;
   GL_VERSION_1_2     : constant := 1;

   --  Data types (may be architecture dependent in some cases)

   type Void is null record;
   subtype GLvoid          is Void;
   subtype GLboolean       is Interfaces.C.unsigned_char;
   subtype GLbyte          is Interfaces.C.signed_char;    --  1-byte signed
   subtype GLshort         is Short_Integer;               --  2-byte signed
   subtype GLint           is Integer;                     --  4-byte signed
   type    GLint_Vec_4     is array (0 .. 3) of GLint;
   subtype GLubyte         is Interfaces.C.unsigned_char;  --  1-byte unsigned
   type    GLubyte_Ptr     is access GLubyte;
   subtype GLushort        is Interfaces.C.unsigned_short;  --  2-byte unsigned
   subtype GLuint          is Interfaces.C.unsigned;        --  4-byte unsigned
   type    GLuint_Vec      is array (Natural range <>) of GLuint;
   subtype GLsizei         is Integer;                      --  4-byte signed
   subtype GLfloat         is Float;  --  single precision float
   type    GLfloat_Vec_16  is array (0 .. 15) of GLfloat;
   type    GLfloat_Vec_4   is array (0 .. 3)  of GLfloat;
   subtype GLclampf        is Float;       --  single precision float in [0;1]
   subtype GLdouble        is Long_Float;  --  double precision float
   type    GLdouble_Vec_3  is array (0 .. 2) of GLdouble;
   type    GLdouble_Vec_16 is array (0 .. 15) of GLdouble;
   subtype GLclampd        is Long_Float;  --  double precision float in [0;1]

   type GLenum is new Integer;
   for GLenum'Size use 32;

   --   Boolean values
   GL_FALSE          : constant GLenum := 0;
   GL_TRUE           : constant GLenum := 1;

   --   Data types
   GL_BYTE           : constant GLenum := 16#1400#;
   GL_UNSIGNED_BYTE  : constant GLenum := 16#1401#;
   GL_SHORT          : constant GLenum := 16#1402#;
   GL_UNSIGNED_SHORT : constant GLenum := 16#1403#;
   GL_INT            : constant GLenum := 16#1404#;
   GL_UNSIGNED_INT   : constant GLenum := 16#1405#;
   GL_FLOAT          : constant GLenum := 16#1406#;
   GL_2_BYTES        : constant GLenum := 16#1407#;
   GL_3_BYTES        : constant GLenum := 16#1408#;
   GL_4_BYTES        : constant GLenum := 16#1409#;
   GL_DOUBLE         : constant GLenum := 16#140A#;

   --   Primitives (see glBegin)
   --  GL_POINTS => each vertex set by glVertex* is one point to draw on the
   --               screen
   --  GL_LINES  => each pair of vertex is one line
   --  GL_TRIANGLES => each group of three vertices is a triangle
   --  GL_TRIANGLE_STRIP => first three vertices are a triangle, then each new
   --               vertex is combined with the previous two to build a new
   --               triangle. The triangles are [123], [234], [345],...
   --  GL_TRIANGLE_FAN => first vertex is the central vertex, then each vertex
   --               is combined with previous to make a triangle. So the
   --               triangles are [123], [134], [145],...
   --  GL_QUADS => Each group of four vertices is a quadrilateral
   --  GL_QUAD_STRIP => first four vertices are one polygon, then each pair of
   --               vertices are combined with the previous two.
   --  GL_POLYGON   => A single polygon

   GL_POINTS         : constant GLenum := 0;
   GL_LINES          : constant GLenum := 1;
   GL_LINE_LOOP      : constant GLenum := 2;
   GL_LINE_STRIP     : constant GLenum := 3;
   GL_TRIANGLES      : constant GLenum := 4;
   GL_TRIANGLE_STRIP : constant GLenum := 5;
   GL_TRIANGLE_FAN   : constant GLenum := 6;
   GL_QUADS          : constant GLenum := 7;
   GL_QUAD_STRIP     : constant GLenum := 8;
   GL_POLYGON        : constant GLenum := 9;

   --   Vertex Arrays
   GL_VERTEX_ARRAY                : constant GLenum := 16#8074#;
   GL_NORMAL_ARRAY                : constant GLenum := 16#8075#;
   GL_COLOR_ARRAY                 : constant GLenum := 16#8076#;
   GL_INDEX_ARRAY                 : constant GLenum := 16#8077#;
   GL_TEXTURE_COORD_ARRAY         : constant GLenum := 16#8078#;
   GL_EDGE_FLAG_ARRAY             : constant GLenum := 16#8079#;
   GL_VERTEX_ARRAY_SIZE           : constant GLenum := 16#807A#;
   GL_VERTEX_ARRAY_TYPE           : constant GLenum := 16#807B#;
   GL_VERTEX_ARRAY_STRIDE         : constant GLenum := 16#807C#;
   GL_NORMAL_ARRAY_TYPE           : constant GLenum := 16#807E#;
   GL_NORMAL_ARRAY_STRIDE         : constant GLenum := 16#807F#;
   GL_COLOR_ARRAY_SIZE            : constant GLenum := 16#8081#;
   GL_COLOR_ARRAY_TYPE            : constant GLenum := 16#8082#;
   GL_COLOR_ARRAY_STRIDE          : constant GLenum := 16#8083#;
   GL_INDEX_ARRAY_TYPE            : constant GLenum := 16#8085#;
   GL_INDEX_ARRAY_STRIDE          : constant GLenum := 16#8086#;
   GL_TEXTURE_COORD_ARRAY_SIZE    : constant GLenum := 16#8088#;
   GL_TEXTURE_COORD_ARRAY_TYPE    : constant GLenum := 16#8089#;
   GL_TEXTURE_COORD_ARRAY_STRIDE  : constant GLenum := 16#808A#;
   GL_EDGE_FLAG_ARRAY_STRIDE      : constant GLenum := 16#808C#;
   GL_VERTEX_ARRAY_POINTER        : constant GLenum := 16#808E#;
   GL_NORMAL_ARRAY_POINTER        : constant GLenum := 16#808F#;
   GL_COLOR_ARRAY_POINTER         : constant GLenum := 16#8090#;
   GL_INDEX_ARRAY_POINTER         : constant GLenum := 16#8091#;
   GL_TEXTURE_COORD_ARRAY_POINTER : constant GLenum := 16#8092#;
   GL_EDGE_FLAG_ARRAY_POINTER     : constant GLenum := 16#8093#;
   GL_V2F                         : constant GLenum := 16#2A20#;
   GL_V3F                         : constant GLenum := 16#2A21#;
   GL_C4UB_V2F                    : constant GLenum := 16#2A22#;
   GL_C4UB_V3F                    : constant GLenum := 16#2A23#;
   GL_C3F_V3F                     : constant GLenum := 16#2A24#;
   GL_N3F_V3F                     : constant GLenum := 16#2A25#;
   GL_C4F_N3F_V3F                 : constant GLenum := 16#2A26#;
   GL_T2F_V3F                     : constant GLenum := 16#2A27#;
   GL_T4F_V4F                     : constant GLenum := 16#2A28#;
   GL_T2F_C4UB_V3F                : constant GLenum := 16#2A29#;
   GL_T2F_C3F_V3F                 : constant GLenum := 16#2A2A#;
   GL_T2F_N3F_V3F                 : constant GLenum := 16#2A2B#;
   GL_T2F_C4F_N3F_V3F             : constant GLenum := 16#2A2C#;
   GL_T4F_C4F_N3F_V4F             : constant GLenum := 16#2A2D#;

   --   Matrix Mode
   GL_MATRIX_MODE : constant GLenum := 16#0BA0#;
   GL_MODELVIEW   : constant GLenum := 16#1700#;
   GL_PROJECTION  : constant GLenum := 16#1701#;
   GL_TEXTURE     : constant GLenum := 16#1702#;

   --   Points
   GL_POINT_SMOOTH           : constant GLenum := 2832;
   GL_POINT_SIZE             : constant GLenum := 2833;
   GL_POINT_SIZE_GRANULARITY : constant GLenum := 2835;
   GL_POINT_SIZE_RANGE       : constant GLenum := 2834;

   --   Lines
   GL_LINE_SMOOTH            : constant GLenum := 2848;
   GL_LINE_STIPPLE           : constant GLenum := 2852;
   GL_LINE_STIPPLE_PATTERN   : constant GLenum := 2853;
   GL_LINE_STIPPLE_REPEAT    : constant GLenum := 2854;
   GL_LINE_WIDTH             : constant GLenum := 2849;
   GL_LINE_WIDTH_GRANULARITY : constant GLenum := 2851;
   GL_LINE_WIDTH_RANGE       : constant GLenum := 2850;

   --   Polygons
   GL_POINT                 : constant GLenum := 6912;
   GL_LINE                  : constant GLenum := 6913;
   GL_FILL                  : constant GLenum := 6914;
   GL_CW                    : constant GLenum := 2304;
   GL_CCW                   : constant GLenum := 2305;
   GL_FRONT                 : constant GLenum := 1028;
   GL_BACK                  : constant GLenum := 1029;
   GL_POLYGON_MODE          : constant GLenum := 2880;
   GL_POLYGON_SMOOTH        : constant GLenum := 2881;
   GL_POLYGON_STIPPLE       : constant GLenum := 2882;
   GL_EDGE_FLAG             : constant GLenum := 2883;
   GL_CULL_FACE             : constant GLenum := 2884;
   GL_CULL_FACE_MODE        : constant GLenum := 2885;
   GL_FRONT_FACE            : constant GLenum := 2886;
   GL_POLYGON_OFFSET_FACTOR : constant GLenum := 32824;
   GL_POLYGON_OFFSET_UNITS  : constant GLenum := 10752;
   GL_POLYGON_OFFSET_POINT  : constant GLenum := 10753;
   GL_POLYGON_OFFSET_LINE   : constant GLenum := 10754;
   GL_POLYGON_OFFSET_FILL   : constant GLenum := 32823;

   --   Display Lists
   GL_COMPILE             : constant GLenum := 4864;
   GL_COMPILE_AND_EXECUTE : constant GLenum := 4865;
   GL_LIST_BASE           : constant GLenum := 2866;
   GL_LIST_INDEX          : constant GLenum := 2867;
   GL_LIST_MODE           : constant GLenum := 2864;

   --   Depth buffer
   GL_NEVER               : constant GLenum := 512;
   GL_LESS                : constant GLenum := 513;
   GL_GEQUAL              : constant GLenum := 518;
   GL_LEQUAL              : constant GLenum := 515;
   GL_GREATER             : constant GLenum := 516;
   GL_NOTEQUAL            : constant GLenum := 517;
   GL_EQUAL               : constant GLenum := 514;
   GL_ALWAYS              : constant GLenum := 519;
   GL_DEPTH_TEST          : constant GLenum := 2929;
   GL_DEPTH_BITS          : constant GLenum := 3414;
   GL_DEPTH_CLEAR_VALUE   : constant GLenum := 2931;
   GL_DEPTH_FUNC          : constant GLenum := 2932;
   GL_DEPTH_RANGE         : constant GLenum := 2928;
   GL_DEPTH_WRITEMASK     : constant GLenum := 2930;
   GL_DEPTH_COMPONENT     : constant GLenum := 6402;

   --   Lighting
   GL_LIGHTING                 : constant GLenum := 2896;
   GL_LIGHT0                   : constant GLenum := 16384;
   GL_LIGHT1                   : constant GLenum := 16385;
   GL_LIGHT2                   : constant GLenum := 16386;
   GL_LIGHT3                   : constant GLenum := 16387;
   GL_LIGHT4                   : constant GLenum := 16388;
   GL_LIGHT5                   : constant GLenum := 16389;
   GL_LIGHT6                   : constant GLenum := 16390;
   GL_LIGHT7                   : constant GLenum := 16391;
   GL_SPOT_EXPONENT            : constant GLenum := 4613;
   GL_SPOT_CUTOFF              : constant GLenum := 4614;
   GL_CONSTANT_ATTENUATION     : constant GLenum := 4615;
   GL_LINEAR_ATTENUATION       : constant GLenum := 4616;
   GL_QUADRATIC_ATTENUATION    : constant GLenum := 4617;
   GL_AMBIENT                  : constant GLenum := 4608;
   GL_DIFFUSE                  : constant GLenum := 4609;
   GL_SPECULAR                 : constant GLenum := 4610;
   GL_SHININESS                : constant GLenum := 5633;
   GL_EMISSION                 : constant GLenum := 5632;
   GL_POSITION                 : constant GLenum := 4611;
   GL_SPOT_DIRECTION           : constant GLenum := 4612;
   GL_AMBIENT_AND_DIFFUSE      : constant GLenum := 5634;
   GL_COLOR_INDEXES            : constant GLenum := 5635;
   GL_LIGHT_MODEL_TWO_SIDE     : constant GLenum := 2898;
   GL_LIGHT_MODEL_LOCAL_VIEWER : constant GLenum := 2897;
   GL_LIGHT_MODEL_AMBIENT      : constant GLenum := 2899;
   GL_FRONT_AND_BACK           : constant GLenum := 1032;

   GL_SHADE_MODEL              : constant GLenum := 2900;
   GL_FLAT                     : constant GLenum := 7424;
   GL_SMOOTH                   : constant GLenum := 7425;

   GL_COLOR_MATERIAL           : constant GLenum := 2903;
   GL_COLOR_MATERIAL_FACE      : constant GLenum := 2901;
   GL_COLOR_MATERIAL_PARAMETER : constant GLenum := 2902;
   GL_NORMALIZE                : constant GLenum := 2977;

   --   User clipping planes
   GL_CLIP_PLANE0 : constant GLenum := 12288;
   GL_CLIP_PLANE1 : constant GLenum := 12289;
   GL_CLIP_PLANE2 : constant GLenum := 12290;
   GL_CLIP_PLANE3 : constant GLenum := 12291;
   GL_CLIP_PLANE4 : constant GLenum := 12292;
   GL_CLIP_PLANE5 : constant GLenum := 12293;

   --   Accumulation buffer
   GL_ACCUM_RED_BITS    : constant GLenum := 3416;
   GL_ACCUM_GREEN_BITS  : constant GLenum := 3417;
   GL_ACCUM_BLUE_BITS   : constant GLenum := 3418;
   GL_ACCUM_ALPHA_BITS  : constant GLenum := 3419;
   GL_ACCUM_CLEAR_VALUE : constant GLenum := 2944;
   GL_ACCUM             : constant GLenum := 256;
   GL_ADD               : constant GLenum := 260;
   GL_LOAD              : constant GLenum := 257;
   GL_MULT              : constant GLenum := 259;
   GL_RETURN            : constant GLenum := 258;

   --   Alpha testing
   GL_ALPHA_TEST      : constant GLenum := 3008;
   GL_ALPHA_TEST_REF  : constant GLenum := 3010;
   GL_ALPHA_TEST_FUNC : constant GLenum := 3009;

   --   Blending
   GL_BLEND                    : constant GLenum := 3042;
   GL_BLEND_SRC                : constant GLenum := 3041;
   GL_BLEND_DST                : constant GLenum := 3040;
   GL_ZERO                     : constant GLenum := 0;
   GL_ONE                      : constant GLenum := 1;
   GL_SRC_COLOR                : constant GLenum := 768;
   GL_ONE_MINUS_SRC_COLOR      : constant GLenum := 769;
   GL_DST_COLOR                : constant GLenum := 774;
   GL_ONE_MINUS_DST_COLOR      : constant GLenum := 775;
   GL_SRC_ALPHA                : constant GLenum := 770;
   GL_ONE_MINUS_SRC_ALPHA      : constant GLenum := 771;
   GL_DST_ALPHA                : constant GLenum := 772;
   GL_ONE_MINUS_DST_ALPHA      : constant GLenum := 773;
   GL_SRC_ALPHA_SATURATE       : constant GLenum := 776;
   GL_CONSTANT_COLOR           : constant GLenum := 32769;
   GL_ONE_MINUS_CONSTANT_COLOR : constant GLenum := 32770;
   GL_CONSTANT_ALPHA           : constant GLenum := 32771;
   GL_ONE_MINUS_CONSTANT_ALPHA : constant GLenum := 32772;

   --   Render Mode
   GL_FEEDBACK : constant GLenum := 7169;
   GL_RENDER   : constant GLenum := 7168;
   GL_SELECT   : constant GLenum := 7170;

   --   Feedback
   GL_2D                      : constant GLenum := 1536;
   GL_3D                      : constant GLenum := 1537;
   GL_3D_COLOR                : constant GLenum := 1538;
   GL_3D_COLOR_TEXTURE        : constant GLenum := 1539;
   GL_4D_COLOR_TEXTURE        : constant GLenum := 1540;
   GL_POINT_TOKEN             : constant GLenum := 1793;
   GL_LINE_TOKEN              : constant GLenum := 1794;
   GL_LINE_RESET_TOKEN        : constant GLenum := 1799;
   GL_POLYGON_TOKEN           : constant GLenum := 1795;
   GL_BITMAP_TOKEN            : constant GLenum := 1796;
   GL_DRAW_PIXEL_TOKEN        : constant GLenum := 1797;
   GL_COPY_PIXEL_TOKEN        : constant GLenum := 1798;
   GL_PASS_THROUGH_TOKEN      : constant GLenum := 1792;
   GL_FEEDBACK_BUFFER_POINTER : constant GLenum := 3568;
   GL_FEEDBACK_BUFFER_SIZE    : constant GLenum := 3569;
   GL_FEEDBACK_BUFFER_TYPE    : constant GLenum := 3570;

   --   Selection
   GL_SELECTION_BUFFER_POINTER : constant GLenum := 3571;
   GL_SELECTION_BUFFER_SIZE    : constant GLenum := 3572;

   --   Fog
   GL_FOG         : constant GLenum := 2912;
   GL_FOG_MODE    : constant GLenum := 2917;
   GL_FOG_DENSITY : constant GLenum := 2914;
   GL_FOG_COLOR   : constant GLenum := 2918;
   GL_FOG_INDEX   : constant GLenum := 2913;
   GL_FOG_START   : constant GLenum := 2915;
   GL_FOG_END     : constant GLenum := 2916;
   GL_LINEAR      : constant GLenum := 9729;
   GL_EXP         : constant GLenum := 2048;
   GL_EXP2        : constant GLenum := 2049;

   --   Logic Ops
   GL_LOGIC_OP       : constant GLenum := 3057;
   GL_INDEX_LOGIC_OP : constant GLenum := 3057;
   GL_COLOR_LOGIC_OP : constant GLenum := 3058;
   GL_LOGIC_OP_MODE  : constant GLenum := 3056;
   GL_CLEAR          : constant GLenum := 5376;
   GL_SET            : constant GLenum := 5391;
   GL_COPY           : constant GLenum := 5379;
   GL_COPY_INVERTED  : constant GLenum := 5388;
   GL_NOOP           : constant GLenum := 5381;
   GL_INVERT         : constant GLenum := 5386;
   GL_AND            : constant GLenum := 5377;
   GL_NAND           : constant GLenum := 5390;
   GL_OR             : constant GLenum := 5383;
   GL_NOR            : constant GLenum := 5384;
   GL_XOR            : constant GLenum := 5382;
   GL_EQUIV          : constant GLenum := 5385;
   GL_AND_REVERSE    : constant GLenum := 5378;
   GL_AND_INVERTED   : constant GLenum := 5380;
   GL_OR_REVERSE     : constant GLenum := 5387;
   GL_OR_INVERTED    : constant GLenum := 5389;

   --   Stencil
   GL_STENCIL_TEST            : constant GLenum := 2960;
   GL_STENCIL_WRITEMASK       : constant GLenum := 2968;
   GL_STENCIL_BITS            : constant GLenum := 3415;
   GL_STENCIL_FUNC            : constant GLenum := 2962;
   GL_STENCIL_VALUE_MASK      : constant GLenum := 2963;
   GL_STENCIL_REF             : constant GLenum := 2967;
   GL_STENCIL_FAIL            : constant GLenum := 2964;
   GL_STENCIL_PASS_DEPTH_PASS : constant GLenum := 2966;
   GL_STENCIL_PASS_DEPTH_FAIL : constant GLenum := 2965;
   GL_STENCIL_CLEAR_VALUE     : constant GLenum := 2961;
   GL_STENCIL_INDEX           : constant GLenum := 6401;
   GL_KEEP                    : constant GLenum := 7680;
   GL_REPLACE                 : constant GLenum := 7681;
   GL_INCR                    : constant GLenum := 7682;
   GL_DECR                    : constant GLenum := 7683;

   --   Buffers, Pixel Drawing/Reading
   GL_NONE            : constant GLenum := 0;
   GL_LEFT            : constant GLenum := 1030;
   GL_RIGHT           : constant GLenum := 1031;
   --   GL_FRONT                      = 0x0404,
   --   GL_BACK                       = 0x0405,
   --   GL_FRONT_AND_BACK             = 0x0408,
   GL_FRONT_LEFT      : constant GLenum := 1024;
   GL_FRONT_RIGHT     : constant GLenum := 1025;
   GL_BACK_LEFT       : constant GLenum := 1026;
   GL_BACK_RIGHT      : constant GLenum := 1027;
   GL_AUX0            : constant GLenum := 1033;
   GL_AUX1            : constant GLenum := 1034;
   GL_AUX2            : constant GLenum := 1035;
   GL_AUX3            : constant GLenum := 1036;
   GL_COLOR_INDEX     : constant GLenum := 6400;
   GL_RED             : constant GLenum := 6403;
   GL_GREEN           : constant GLenum := 6404;
   GL_BLUE            : constant GLenum := 6405;
   GL_ALPHA           : constant GLenum := 6406;
   GL_LUMINANCE       : constant GLenum := 6409;
   GL_LUMINANCE_ALPHA : constant GLenum := 6410;
   GL_ALPHA_BITS      : constant GLenum := 3413;
   GL_RED_BITS        : constant GLenum := 3410;
   GL_GREEN_BITS      : constant GLenum := 3411;
   GL_BLUE_BITS       : constant GLenum := 3412;
   GL_INDEX_BITS      : constant GLenum := 3409;
   GL_SUBPIXEL_BITS   : constant GLenum := 3408;
   GL_AUX_BUFFERS     : constant GLenum := 3072;
   GL_READ_BUFFER     : constant GLenum := 3074;
   GL_DRAW_BUFFER     : constant GLenum := 3073;
   GL_DOUBLEBUFFER    : constant GLenum := 3122;
   GL_STEREO          : constant GLenum := 3123;
   GL_BITMAP          : constant GLenum := 6656;
   GL_COLOR           : constant GLenum := 6144;
   GL_DEPTH           : constant GLenum := 6145;
   GL_STENCIL         : constant GLenum := 6146;
   GL_DITHER          : constant GLenum := 3024;
   GL_RGB             : constant GLenum := 6407;
   GL_RGBA            : constant GLenum := 6408;

   --   Implementation limits
   GL_MAX_LIST_NESTING              : constant GLenum := 2865;
   GL_MAX_ATTRIB_STACK_DEPTH        : constant GLenum := 3381;
   GL_MAX_MODELVIEW_STACK_DEPTH     : constant GLenum := 3382;
   GL_MAX_NAME_STACK_DEPTH          : constant GLenum := 3383;
   GL_MAX_PROJECTION_STACK_DEPTH    : constant GLenum := 3384;
   GL_MAX_TEXTURE_STACK_DEPTH       : constant GLenum := 3385;
   GL_MAX_EVAL_ORDER                : constant GLenum := 3376;
   GL_MAX_LIGHTS                    : constant GLenum := 3377;
   GL_MAX_CLIP_PLANES               : constant GLenum := 3378;
   GL_MAX_TEXTURE_SIZE              : constant GLenum := 3379;
   GL_MAX_PIXEL_MAP_TABLE           : constant GLenum := 3380;
   GL_MAX_VIEWPORT_DIMS             : constant GLenum := 3386;
   GL_MAX_CLIENT_ATTRIB_STACK_DEPTH : constant GLenum := 3387;

   --   Gets
   GL_ATTRIB_STACK_DEPTH            : constant GLenum := 2992;
   GL_CLIENT_ATTRIB_STACK_DEPTH     : constant GLenum := 2993;
   GL_COLOR_CLEAR_VALUE             : constant GLenum := 3106;
   GL_COLOR_WRITEMASK               : constant GLenum := 3107;
   GL_CURRENT_INDEX                 : constant GLenum := 2817;
   GL_CURRENT_COLOR                 : constant GLenum := 2816;
   GL_CURRENT_NORMAL                : constant GLenum := 2818;
   GL_CURRENT_RASTER_COLOR          : constant GLenum := 2820;
   GL_CURRENT_RASTER_DISTANCE       : constant GLenum := 2825;
   GL_CURRENT_RASTER_INDEX          : constant GLenum := 2821;
   GL_CURRENT_RASTER_POSITION       : constant GLenum := 2823;
   GL_CURRENT_RASTER_TEXTURE_COORDS : constant GLenum := 2822;
   GL_CURRENT_RASTER_POSITION_VALID : constant GLenum := 2824;
   GL_CURRENT_TEXTURE_COORDS        : constant GLenum := 2819;
   GL_INDEX_CLEAR_VALUE             : constant GLenum := 3104;
   GL_INDEX_MODE                    : constant GLenum := 3120;
   GL_INDEX_WRITEMASK               : constant GLenum := 3105;
   GL_MODELVIEW_MATRIX              : constant GLenum := 2982;
   GL_MODELVIEW_STACK_DEPTH         : constant GLenum := 2979;
   GL_NAME_STACK_DEPTH              : constant GLenum := 3440;
   GL_PROJECTION_MATRIX             : constant GLenum := 2983;
   GL_PROJECTION_STACK_DEPTH        : constant GLenum := 2980;
   GL_RENDER_MODE                   : constant GLenum := 3136;
   GL_RGBA_MODE                     : constant GLenum := 3121;
   GL_TEXTURE_MATRIX                : constant GLenum := 2984;
   GL_TEXTURE_STACK_DEPTH           : constant GLenum := 2981;
   GL_VIEWPORT                      : constant GLenum := 2978;

   --   Evaluators
   GL_AUTO_NORMAL          : constant GLenum := 3456;
   GL_MAP1_COLOR_4         : constant GLenum := 3472;
   GL_MAP1_GRID_DOMAIN     : constant GLenum := 3536;
   GL_MAP1_GRID_SEGMENTS   : constant GLenum := 3537;
   GL_MAP1_INDEX           : constant GLenum := 3473;
   GL_MAP1_NORMAL          : constant GLenum := 3474;
   GL_MAP1_TEXTURE_COORD_1 : constant GLenum := 3475;
   GL_MAP1_TEXTURE_COORD_2 : constant GLenum := 3476;
   GL_MAP1_TEXTURE_COORD_3 : constant GLenum := 3477;
   GL_MAP1_TEXTURE_COORD_4 : constant GLenum := 3478;
   GL_MAP1_VERTEX_3        : constant GLenum := 3479;
   GL_MAP1_VERTEX_4        : constant GLenum := 3480;
   GL_MAP2_COLOR_4         : constant GLenum := 3504;
   GL_MAP2_GRID_DOMAIN     : constant GLenum := 3538;
   GL_MAP2_GRID_SEGMENTS   : constant GLenum := 3539;
   GL_MAP2_INDEX           : constant GLenum := 3505;
   GL_MAP2_NORMAL          : constant GLenum := 3506;
   GL_MAP2_TEXTURE_COORD_1 : constant GLenum := 3507;
   GL_MAP2_TEXTURE_COORD_2 : constant GLenum := 3508;
   GL_MAP2_TEXTURE_COORD_3 : constant GLenum := 3509;
   GL_MAP2_TEXTURE_COORD_4 : constant GLenum := 3510;
   GL_MAP2_VERTEX_3        : constant GLenum := 3511;
   GL_MAP2_VERTEX_4        : constant GLenum := 3512;
   GL_COEFF                : constant GLenum := 2560;
   GL_DOMAIN               : constant GLenum := 2562;
   GL_ORDER                : constant GLenum := 2561;

   --   Hints
   GL_FOG_HINT                    : constant GLenum := 3156;
   GL_LINE_SMOOTH_HINT            : constant GLenum := 3154;
   GL_PERSPECTIVE_CORRECTION_HINT : constant GLenum := 3152;
   GL_POINT_SMOOTH_HINT           : constant GLenum := 3153;
   GL_POLYGON_SMOOTH_HINT         : constant GLenum := 3155;
   GL_DONT_CARE                   : constant GLenum := 4352;
   GL_FASTEST                     : constant GLenum := 4353;
   GL_NICEST                      : constant GLenum := 4354;

   --   Scissor box
   GL_SCISSOR_TEST : constant GLenum := 3089;
   GL_SCISSOR_BOX  : constant GLenum := 3088;

   --   Pixel Mode / Transfer
   GL_MAP_COLOR             : constant GLenum := 3344;
   GL_MAP_STENCIL           : constant GLenum := 3345;
   GL_INDEX_SHIFT           : constant GLenum := 3346;
   GL_INDEX_OFFSET          : constant GLenum := 3347;
   GL_RED_SCALE             : constant GLenum := 3348;
   GL_RED_BIAS              : constant GLenum := 3349;
   GL_GREEN_SCALE           : constant GLenum := 3352;
   GL_GREEN_BIAS            : constant GLenum := 3353;
   GL_BLUE_SCALE            : constant GLenum := 3354;
   GL_BLUE_BIAS             : constant GLenum := 3355;
   GL_ALPHA_SCALE           : constant GLenum := 3356;
   GL_ALPHA_BIAS            : constant GLenum := 3357;
   GL_DEPTH_SCALE           : constant GLenum := 3358;
   GL_DEPTH_BIAS            : constant GLenum := 3359;
   GL_PIXEL_MAP_S_TO_S_SIZE : constant GLenum := 3249;
   GL_PIXEL_MAP_I_TO_I_SIZE : constant GLenum := 3248;
   GL_PIXEL_MAP_I_TO_R_SIZE : constant GLenum := 3250;
   GL_PIXEL_MAP_I_TO_G_SIZE : constant GLenum := 3251;
   GL_PIXEL_MAP_I_TO_B_SIZE : constant GLenum := 3252;
   GL_PIXEL_MAP_I_TO_A_SIZE : constant GLenum := 3253;
   GL_PIXEL_MAP_R_TO_R_SIZE : constant GLenum := 3254;
   GL_PIXEL_MAP_G_TO_G_SIZE : constant GLenum := 3255;
   GL_PIXEL_MAP_B_TO_B_SIZE : constant GLenum := 3256;
   GL_PIXEL_MAP_A_TO_A_SIZE : constant GLenum := 3257;
   GL_PIXEL_MAP_S_TO_S      : constant GLenum := 3185;
   GL_PIXEL_MAP_I_TO_I      : constant GLenum := 3184;
   GL_PIXEL_MAP_I_TO_R      : constant GLenum := 3186;
   GL_PIXEL_MAP_I_TO_G      : constant GLenum := 3187;
   GL_PIXEL_MAP_I_TO_B      : constant GLenum := 3188;
   GL_PIXEL_MAP_I_TO_A      : constant GLenum := 3189;
   GL_PIXEL_MAP_R_TO_R      : constant GLenum := 3190;
   GL_PIXEL_MAP_G_TO_G      : constant GLenum := 3191;
   GL_PIXEL_MAP_B_TO_B      : constant GLenum := 3192;
   GL_PIXEL_MAP_A_TO_A      : constant GLenum := 3193;
   GL_PACK_ALIGNMENT        : constant GLenum := 3333;
   GL_PACK_LSB_FIRST        : constant GLenum := 3329;
   GL_PACK_ROW_LENGTH       : constant GLenum := 3330;
   GL_PACK_SKIP_PIXELS      : constant GLenum := 3332;
   GL_PACK_SKIP_ROWS        : constant GLenum := 3331;
   GL_PACK_SWAP_BYTES       : constant GLenum := 3328;
   GL_UNPACK_ALIGNMENT      : constant GLenum := 3317;
   GL_UNPACK_LSB_FIRST      : constant GLenum := 3313;
   GL_UNPACK_ROW_LENGTH     : constant GLenum := 3314;
   GL_UNPACK_SKIP_PIXELS    : constant GLenum := 3316;
   GL_UNPACK_SKIP_ROWS      : constant GLenum := 3315;
   GL_UNPACK_SWAP_BYTES     : constant GLenum := 3312;
   GL_ZOOM_X                : constant GLenum := 3350;
   GL_ZOOM_Y                : constant GLenum := 3351;

   --   Texture mapping
   GL_TEXTURE_ENV            : constant GLenum := 8960;
   GL_TEXTURE_ENV_MODE       : constant GLenum := 8704;
   GL_TEXTURE_1D             : constant GLenum := 3552;
   GL_TEXTURE_2D             : constant GLenum := 3553;
   GL_TEXTURE_WRAP_S         : constant GLenum := 10242;
   GL_TEXTURE_WRAP_T         : constant GLenum := 10243;
   GL_TEXTURE_MAG_FILTER     : constant GLenum := 10240;
   GL_TEXTURE_MIN_FILTER     : constant GLenum := 10241;
   GL_TEXTURE_ENV_COLOR      : constant GLenum := 8705;
   GL_TEXTURE_GEN_S          : constant GLenum := 3168;
   GL_TEXTURE_GEN_T          : constant GLenum := 3169;
   GL_TEXTURE_GEN_MODE       : constant GLenum := 9472;
   GL_TEXTURE_BORDER_COLOR   : constant GLenum := 4100;
   GL_TEXTURE_WIDTH          : constant GLenum := 4096;
   GL_TEXTURE_HEIGHT         : constant GLenum := 4097;
   GL_TEXTURE_BORDER         : constant GLenum := 4101;
   GL_TEXTURE_COMPONENTS     : constant GLenum := 4099;
   GL_TEXTURE_RED_SIZE       : constant GLenum := 32860;
   GL_TEXTURE_GREEN_SIZE     : constant GLenum := 32861;
   GL_TEXTURE_BLUE_SIZE      : constant GLenum := 32862;
   GL_TEXTURE_ALPHA_SIZE     : constant GLenum := 32863;
   GL_TEXTURE_LUMINANCE_SIZE : constant GLenum := 32864;
   GL_TEXTURE_INTENSITY_SIZE : constant GLenum := 32865;
   GL_NEAREST_MIPMAP_NEAREST : constant GLenum := 9984;
   GL_NEAREST_MIPMAP_LINEAR  : constant GLenum := 9986;
   GL_LINEAR_MIPMAP_NEAREST  : constant GLenum := 9985;
   GL_LINEAR_MIPMAP_LINEAR   : constant GLenum := 9987;
   GL_OBJECT_LINEAR          : constant GLenum := 9217;
   GL_OBJECT_PLANE           : constant GLenum := 9473;
   GL_EYE_LINEAR             : constant GLenum := 9216;
   GL_EYE_PLANE              : constant GLenum := 9474;
   GL_SPHERE_MAP             : constant GLenum := 9218;
   GL_DECAL                  : constant GLenum := 8449;
   GL_MODULATE               : constant GLenum := 8448;
   GL_NEAREST                : constant GLenum := 9728;
   GL_REPEAT                 : constant GLenum := 10497;
   GL_CLAMP                  : constant GLenum := 10496;
   GL_S                      : constant GLenum := 8192;
   GL_T                      : constant GLenum := 8193;
   GL_R                      : constant GLenum := 8194;
   GL_Q                      : constant GLenum := 8195;
   GL_TEXTURE_GEN_R          : constant GLenum := 3170;
   GL_TEXTURE_GEN_Q          : constant GLenum := 3171;

   --   Utility
   GL_VENDOR     : constant GLenum := 7936;
   GL_RENDERER   : constant GLenum := 7937;
   GL_VERSION    : constant GLenum := 7938;
   GL_EXTENSIONS : constant GLenum := 7939;

   --   Errors
   GL_NO_ERROR          : constant := 0;
   GL_INVALID_VALUE     : constant GLenum := 1281;
   GL_INVALID_ENUM      : constant GLenum := 1280;
   GL_INVALID_OPERATION : constant GLenum := 1282;
   GL_STACK_OVERFLOW    : constant GLenum := 1283;
   GL_STACK_UNDERFLOW   : constant GLenum := 1284;
   GL_OUT_OF_MEMORY     : constant GLenum := 1285;

   --  glPush/PopAttrib bits
   type GLbitfield is mod 16#100000#;
   GL_CURRENT_BIT         : constant GLbitfield := 16#00000001#;
   GL_POINT_BIT           : constant GLbitfield := 16#00000002#;
   GL_LINE_BIT            : constant GLbitfield := 16#00000004#;
   GL_POLYGON_BIT         : constant GLbitfield := 16#00000008#;
   GL_POLYGON_STIPPLE_BIT : constant GLbitfield := 16#00000010#;
   GL_PIXEL_MODE_BIT      : constant GLbitfield := 16#00000020#;
   GL_LIGHTING_BIT        : constant GLbitfield := 16#00000040#;
   GL_FOG_BIT             : constant GLbitfield := 16#00000080#;
   GL_DEPTH_BUFFER_BIT    : constant GLbitfield := 16#00000100#;
   GL_ACCUM_BUFFER_BIT    : constant GLbitfield := 16#00000200#;
   GL_STENCIL_BUFFER_BIT  : constant GLbitfield := 16#00000400#;
   GL_VIEWPORT_BIT        : constant GLbitfield := 16#00000800#;
   GL_TRANSFORM_BIT       : constant GLbitfield := 16#00001000#;
   GL_ENABLE_BIT          : constant GLbitfield := 16#00002000#;
   GL_COLOR_BUFFER_BIT    : constant GLbitfield := 16#00004000#;
   GL_HINT_BIT            : constant GLbitfield := 16#00008000#;
   GL_EVAL_BIT            : constant GLbitfield := 16#00010000#;
   GL_LIST_BIT            : constant GLbitfield := 16#00020000#;
   GL_TEXTURE_BIT         : constant GLbitfield := 16#00040000#;
   GL_SCISSOR_BIT         : constant GLbitfield := 16#00080000#;
   GL_ALL_ATTRIB_BITS     : constant GLbitfield := 16#000fffff#;

   --   GL 1.1 texturing
   GL_PROXY_TEXTURE_1D        : constant GLenum := 32867;
   GL_PROXY_TEXTURE_2D        : constant GLenum := 32868;
   GL_TEXTURE_PRIORITY        : constant GLenum := 32870;
   GL_TEXTURE_RESIDENT        : constant GLenum := 32871;
   GL_TEXTURE_BINDING_1D      : constant GLenum := 32872;
   GL_TEXTURE_BINDING_2D      : constant GLenum := 32873;
   GL_TEXTURE_INTERNAL_FORMAT : constant GLenum := 4099;
   GL_ALPHA4                  : constant GLenum := 32827;
   GL_ALPHA8                  : constant GLenum := 32828;
   GL_ALPHA12                 : constant GLenum := 32829;
   GL_ALPHA16                 : constant GLenum := 32830;
   GL_LUMINANCE4              : constant GLenum := 32831;
   GL_LUMINANCE8              : constant GLenum := 32832;
   GL_LUMINANCE12             : constant GLenum := 32833;
   GL_LUMINANCE16             : constant GLenum := 32834;
   GL_LUMINANCE4_ALPHA4       : constant GLenum := 32835;
   GL_LUMINANCE6_ALPHA2       : constant GLenum := 32836;
   GL_LUMINANCE8_ALPHA8       : constant GLenum := 32837;
   GL_LUMINANCE12_ALPHA4      : constant GLenum := 32838;
   GL_LUMINANCE12_ALPHA12     : constant GLenum := 32839;
   GL_LUMINANCE16_ALPHA16     : constant GLenum := 32840;
   GL_INTENSITY               : constant GLenum := 32841;
   GL_INTENSITY4              : constant GLenum := 32842;
   GL_INTENSITY8              : constant GLenum := 32843;
   GL_INTENSITY12             : constant GLenum := 32844;
   GL_INTENSITY16             : constant GLenum := 32845;
   GL_R3_G3_B2                : constant GLenum := 10768;
   GL_RGB4                    : constant GLenum := 32847;
   GL_RGB5                    : constant GLenum := 32848;
   GL_RGB8                    : constant GLenum := 32849;
   GL_RGB10                   : constant GLenum := 32850;
   GL_RGB12                   : constant GLenum := 32851;
   GL_RGB16                   : constant GLenum := 32852;
   GL_RGBA2                   : constant GLenum := 32853;
   GL_RGBA4                   : constant GLenum := 32854;
   GL_RGB5_A1                 : constant GLenum := 32855;
   GL_RGBA8                   : constant GLenum := 32856;
   GL_RGB10_A2                : constant GLenum := 32857;
   GL_RGBA12                  : constant GLenum := 32858;
   GL_RGBA16                  : constant GLenum := 32859;
   GL_CLIENT_PIXEL_STORE_BIT  : constant GLenum := 16#00000001#;
   GL_CLIENT_VERTEX_ARRAY_BIT : constant GLenum := 16#00000002#;

   --   GL 1.2 texturing
   GL_RESCALE_NORMAL                : constant GLenum := 16#803A#;
   GL_CLAMP_TO_EDGE                 :  constant GLenum := 16#812F#;
   GL_MAX_ELEMENTS_VERTICES         : constant GLenum := 16#80E8#;
   GL_MAX_ELEMENTS_INDICES          : constant GLenum := 16#80E9#;
   GL_BGR                           : constant GLenum := 32992;
   GL_BGRA                          : constant GLenum := 32993;
   GL_UNSIGNED_BYTE_3_3_2           : constant GLenum := 32818;
   GL_UNSIGNED_BYTE_2_3_3_REV       : constant GLenum := 33634;
   GL_UNSIGNED_SHORT_5_6_5          : constant GLenum := 33635;
   GL_UNSIGNED_SHORT_5_6_5_REV      : constant GLenum := 33636;
   GL_UNSIGNED_SHORT_4_4_4_4        : constant GLenum := 32819;
   GL_UNSIGNED_SHORT_4_4_4_4_REV    : constant GLenum := 33637;
   GL_UNSIGNED_SHORT_5_5_5_1        : constant GLenum := 32820;
   GL_UNSIGNED_SHORT_1_5_5_5_REV    : constant GLenum := 33638;
   GL_UNSIGNED_INT_8_8_8_8          : constant GLenum := 32821;
   GL_UNSIGNED_INT_8_8_8_8_REV      : constant GLenum := 33639;
   GL_UNSIGNED_INT_10_10_10_2       : constant GLenum := 32822;
   GL_UNSIGNED_INT_2_10_10_10_REV   : constant GLenum := 33640;
   GL_LIGHT_MODEL_COLOR_CONTROL     : constant GLenum := 33272;
   GL_SINGLE_COLOR                  : constant GLenum := 33273;
   GL_SEPARATE_SPECULAR_COLOR       : constant GLenum := 33274;
   GL_TEXTURE_MIN_LOD               : constant GLenum := 33082;
   GL_TEXTURE_MAX_LOD               : constant GLenum := 33083;
   GL_TEXTURE_BASE_LEVEL            : constant GLenum := 33084;
   GL_TEXTURE_MAX_LEVEL             : constant GLenum := 33085;
   GL_SMOOTH_POINT_SIZE_RANGE       : constant GLenum := 16#0B12#;
   GL_SMOOTH_POINT_SIZE_GRANULARITY : constant GLenum := 16#0B13#;
   GL_SMOOTH_LINE_WIDTH_RANGE       : constant GLenum := 16#0B22#;
   GL_SMOOTH_LINE_WIDTH_GRANULARITY : constant GLenum := 16#0B23#;
   GL_ALIASED_POINT_SIZE_RANGE      : constant GLenum := 16#846D#;
   GL_ALIASED_LINE_WIDTH_RANGE      : constant GLenum := 16#846E#;
   GL_PACK_SKIP_IMAGES              : constant GLenum := 16#806B#;
   GL_PACK_IMAGE_HEIGHT             : constant GLenum := 16#806C#;
   GL_UNPACK_SKIP_IMAGES            : constant GLenum := 16#806D#;
   GL_UNPACK_IMAGE_HEIGHT           : constant GLenum := 16#806E#;
   GL_TEXTURE_3D                    : constant GLenum := 16#806F#;
   GL_PROXY_TEXTURE_3D              : constant GLenum := 16#8070#;
   GL_TEXTURE_DEPTH                 : constant GLenum := 16#8071#;
   GL_TEXTURE_WRAP_R                : constant GLenum := 16#8072#;
   GL_MAX_3D_TEXTURE_SIZE           : constant GLenum := 16#8073#;
   GL_TEXTURE_BINDING_3D            : constant GLenum := 16#806A#;

   --  Extensions

            --   GL_EXT_blend_minmax and GL_EXT_blend_color
            --   GL_EXT_polygon_offset
            --   GL_EXT_vertex_array
            --   GL_EXT_texture_object
            --   GL_EXT_texture3D
            --   GL_EXT_paletted_texture
            --   GL_EXT_shared_texture_palette
            --   GL_EXT_point_parameters
            --   GL_EXT_rescale_normal
            --   GL_EXT_abgr
            --   GL_SGIS_multitexture
            --   GL_EXT_multitexture
            --   GL_SGIS_texture_edge_clamp
            --   OpenGL 1.2

   GL_CONSTANT_COLOR_EXT : constant GLenum := 32769;
   GL_ONE_MINUS_CONSTANT_COLOR_EXT : constant GLenum := 32770;
   GL_CONSTANT_ALPHA_EXT : constant GLenum := 32771;
   GL_ONE_MINUS_CONSTANT_ALPHA_EXT : constant GLenum := 32772;
   GL_BLEND_EQUATION_EXT : constant GLenum := 32777;
   GL_MIN_EXT : constant GLenum := 32775;
   GL_MAX_EXT : constant GLenum := 32776;
   GL_FUNC_ADD_EXT : constant GLenum := 32774;
   GL_FUNC_SUBTRACT_EXT : constant GLenum := 32778;
   GL_FUNC_REVERSE_SUBTRACT_EXT : constant GLenum := 32779;
   GL_BLEND_COLOR_EXT : constant GLenum := 32773;
   GL_POLYGON_OFFSET_EXT : constant GLenum := 32823;
   GL_POLYGON_OFFSET_FACTOR_EXT : constant GLenum := 32824;
   GL_POLYGON_OFFSET_BIAS_EXT : constant GLenum := 32825;
   GL_VERTEX_ARRAY_EXT : constant GLenum := 32884;
   GL_NORMAL_ARRAY_EXT : constant GLenum := 32885;
   GL_COLOR_ARRAY_EXT : constant GLenum := 32886;
   GL_INDEX_ARRAY_EXT : constant GLenum := 32887;
   GL_TEXTURE_COORD_ARRAY_EXT : constant GLenum := 32888;
   GL_EDGE_FLAG_ARRAY_EXT : constant GLenum := 32889;
   GL_VERTEX_ARRAY_SIZE_EXT : constant GLenum := 32890;
   GL_VERTEX_ARRAY_TYPE_EXT : constant GLenum := 32891;
   GL_VERTEX_ARRAY_STRIDE_EXT : constant GLenum := 32892;
   GL_VERTEX_ARRAY_COUNT_EXT : constant GLenum := 32893;
   GL_NORMAL_ARRAY_TYPE_EXT : constant GLenum := 32894;
   GL_NORMAL_ARRAY_STRIDE_EXT : constant GLenum := 32895;
   GL_NORMAL_ARRAY_COUNT_EXT : constant GLenum := 32896;
   GL_COLOR_ARRAY_SIZE_EXT : constant GLenum := 32897;
   GL_COLOR_ARRAY_TYPE_EXT : constant GLenum := 32898;
   GL_COLOR_ARRAY_STRIDE_EXT : constant GLenum := 32899;
   GL_COLOR_ARRAY_COUNT_EXT : constant GLenum := 32900;
   GL_INDEX_ARRAY_TYPE_EXT : constant GLenum := 32901;
   GL_INDEX_ARRAY_STRIDE_EXT : constant GLenum := 32902;
   GL_INDEX_ARRAY_COUNT_EXT : constant GLenum := 32903;
   GL_TEXTURE_COORD_ARRAY_SIZE_EXT : constant GLenum := 32904;
   GL_TEXTURE_COORD_ARRAY_TYPE_EXT : constant GLenum := 32905;
   GL_TEXTURE_COORD_ARRAY_STRIDE_EXT : constant GLenum := 32906;
   GL_TEXTURE_COORD_ARRAY_COUNT_EXT : constant GLenum := 32907;
   GL_EDGE_FLAG_ARRAY_STRIDE_EXT : constant GLenum := 32908;
   GL_EDGE_FLAG_ARRAY_COUNT_EXT : constant GLenum := 32909;
   GL_VERTEX_ARRAY_POINTER_EXT : constant GLenum := 32910;
   GL_NORMAL_ARRAY_POINTER_EXT : constant GLenum := 32911;
   GL_COLOR_ARRAY_POINTER_EXT : constant GLenum := 32912;
   GL_INDEX_ARRAY_POINTER_EXT : constant GLenum := 32913;
   GL_TEXTURE_COORD_ARRAY_POINTER_EXT : constant GLenum := 32914;
   GL_EDGE_FLAG_ARRAY_POINTER_EXT : constant GLenum := 32915;
   GL_TEXTURE_PRIORITY_EXT : constant GLenum := 32870;
   GL_TEXTURE_RESIDENT_EXT : constant GLenum := 32871;
   GL_TEXTURE_1D_BINDING_EXT : constant GLenum := 32872;
   GL_TEXTURE_2D_BINDING_EXT : constant GLenum := 32873;
   GL_PACK_SKIP_IMAGES_EXT : constant GLenum := 32875;
   GL_PACK_IMAGE_HEIGHT_EXT : constant GLenum := 32876;
   GL_UNPACK_SKIP_IMAGES_EXT : constant GLenum := 32877;
   GL_UNPACK_IMAGE_HEIGHT_EXT : constant GLenum := 32878;
   GL_TEXTURE_3D_EXT : constant GLenum := 32879;
   GL_PROXY_TEXTURE_3D_EXT : constant GLenum := 32880;
   GL_TEXTURE_DEPTH_EXT : constant GLenum := 32881;
   GL_TEXTURE_WRAP_R_EXT : constant GLenum := 32882;
   GL_MAX_3D_TEXTURE_SIZE_EXT : constant GLenum := 32883;
   GL_TEXTURE_3D_BINDING_EXT : constant GLenum := 32874;
   GL_TABLE_TOO_LARGE_EXT : constant GLenum := 32817;
   GL_COLOR_TABLE_FORMAT_EXT : constant GLenum := 32984;
   GL_COLOR_TABLE_WIDTH_EXT : constant GLenum := 32985;
   GL_COLOR_TABLE_RED_SIZE_EXT : constant GLenum := 32986;
   GL_COLOR_TABLE_GREEN_SIZE_EXT : constant GLenum := 32987;
   GL_COLOR_TABLE_BLUE_SIZE_EXT : constant GLenum := 32988;
   GL_COLOR_TABLE_ALPHA_SIZE_EXT : constant GLenum := 32989;
   GL_COLOR_TABLE_LUMINANCE_SIZE_EXT : constant GLenum := 32990;
   GL_COLOR_TABLE_INTENSITY_SIZE_EXT : constant GLenum := 32991;
   GL_TEXTURE_INDEX_SIZE_EXT : constant GLenum := 33005;
   GL_COLOR_INDEX1_EXT : constant GLenum := 32994;
   GL_COLOR_INDEX2_EXT : constant GLenum := 32995;
   GL_COLOR_INDEX4_EXT : constant GLenum := 32996;
   GL_COLOR_INDEX8_EXT : constant GLenum := 32997;
   GL_COLOR_INDEX12_EXT : constant GLenum := 32998;
   GL_COLOR_INDEX16_EXT : constant GLenum := 32999;
   GL_SHARED_TEXTURE_PALETTE_EXT : constant GLenum := 33275;
   GL_POINT_SIZE_MIN_EXT : constant GLenum := 33062;
   GL_POINT_SIZE_MAX_EXT : constant GLenum := 33063;
   GL_POINT_FADE_THRESHOLD_SIZE_EXT : constant GLenum := 33064;
   GL_DISTANCE_ATTENUATION_EXT : constant GLenum := 33065;

   GL_ABGR_EXT : constant GLenum := 32768;
   GL_SELECTED_TEXTURE_SGIS : constant GLenum := 33628;
   GL_SELECTED_TEXTURE_COORD_SET_SGIS : constant GLenum := 33629;
   GL_MAX_TEXTURES_SGIS : constant GLenum := 33630;
   GL_TEXTURE0_SGIS : constant GLenum := 33631;
   GL_TEXTURE1_SGIS : constant GLenum := 33632;
   GL_TEXTURE2_SGIS : constant GLenum := 33633;
   GL_TEXTURE3_SGIS : constant GLenum := 33634;
   GL_TEXTURE_COORD_SET_SOURCE_SGIS : constant GLenum := 33635;
   GL_SELECTED_TEXTURE_EXT : constant GLenum := 33728;
   GL_SELECTED_TEXTURE_COORD_SET_EXT : constant GLenum := 33729;
   GL_SELECTED_TEXTURE_TRANSFORM_EXT : constant GLenum := 33730;
   GL_MAX_TEXTURES_EXT : constant GLenum := 33731;
   GL_MAX_TEXTURE_COORD_SETS_EXT : constant GLenum := 33732;
   GL_TEXTURE_ENV_COORD_SET_EXT : constant GLenum := 33733;
   GL_TEXTURE0_EXT : constant GLenum := 33734;
   GL_TEXTURE1_EXT : constant GLenum := 33735;
   GL_TEXTURE2_EXT : constant GLenum := 33736;
   GL_TEXTURE3_EXT : constant GLenum := 33737;
   GL_CLAMP_TO_EDGE_SGIS : constant GLenum := 33071;

   --
   --  Miscellaneous
   --

   procedure glClearIndex (c : GLfloat);
   procedure glClearColor (red   : GLclampf;
                           green : GLclampf;
                           blue  : GLclampf;
                           alpha : GLclampf);
   procedure glClear (mask : GLbitfield);
   procedure glIndexMask (mask : GLuint);
   procedure glColorMask (red   : GLboolean;
                          green : GLboolean;
                          blue  : GLboolean;
                          alpha : GLboolean);
   procedure glAlphaFunc (func : GLenum; ref : GLclampf);
   procedure glBlendFunc (sfactor : GLenum; dfactor : GLenum);
   procedure glLogicOp (opcode : GLenum);
   procedure glCullFace (mode : GLenum);
   procedure glFrontFace (mode : GLenum);
   procedure glPointSize (size : GLfloat);
   procedure glLineWidth (width : GLfloat);
   procedure glLineStipple (factor : GLint; pattern : GLushort);
   procedure glPolygonMode (face : GLenum; mode : GLenum);
   procedure glPolygonOffset (factor : GLfloat; units : GLfloat);
   procedure glPolygonStipple (mask : access GLubyte);
   procedure glGetPolygonStipple (mask : access GLubyte);
   procedure glEdgeFlag (flag : GLboolean);
   procedure glEdgeFlagv (flag : access GLboolean);
   procedure glScissor (x      : GLint;
                        y      : GLint;
                        width  : GLsizei;
                        height : GLsizei);
   procedure glClipPlane (plane : GLenum; equation : access GLdouble);
   procedure glGetClipPlane (plane : GLenum; equation : access GLdouble);
   procedure glDrawBuffer (mode : GLenum);
   procedure glReadBuffer (mode : GLenum);
   procedure glEnable (cap : GLenum);
   procedure glDisable (cap : GLenum);
   function glIsEnabled (cap : GLenum) return GLboolean;
   procedure glEnableClientState (cap : GLenum  --  1.1
         );
   procedure glDisableClientState (cap : GLenum  --  1.1
         );
   procedure glGetBooleanv (pname : GLenum; params : access GLboolean);
   procedure glGetDoublev (pname : GLenum; params : access GLdouble);
   procedure glGetFloatv (pname : GLenum; params : access GLfloat);
   procedure glGetIntegerv (pname : GLenum; params : access GLint);
   procedure glPushAttrib (mask : GLbitfield);
   procedure glPopAttrib;
   procedure glPushClientAttrib (mask : GLbitfield  --  1.1
         );
   procedure glPopClientAttrib;    --  1.1
   function glRenderMode (mode : GLenum) return GLint;
   function glGetError return GLenum;
   function glGetString (name : GLenum) return GLubyte_Ptr;
   procedure glFinish;
   procedure glFlush;
   procedure glHint (target : GLenum; mode : GLenum);

   --
   --  Depth Buffer
   --

   procedure glClearDepth (depth : GLclampd);
   procedure glDepthFunc (func : GLenum);
   procedure glDepthMask (flag : GLboolean);
   procedure glDepthRange (near_val : GLclampd; far_val : GLclampd);

   --
   --  Accumulation Buffer
   --

   procedure glClearAccum (red   : GLfloat;
                           green : GLfloat;
                           blue  : GLfloat;
                           alpha : GLfloat);
   procedure glAccum (op : GLenum; value : GLfloat);

   --
   --  Transformation
   --

   procedure glMatrixMode (mode : GLenum);
   procedure glOrtho (left     : GLdouble;
                      right    : GLdouble;
                      bottom   : GLdouble;
                      top      : GLdouble;
                      near_val : GLdouble;
                      far_val  : GLdouble);
   procedure glFrustum (left     : GLdouble;
                        right    : GLdouble;
                        bottom   : GLdouble;
                        top      : GLdouble;
                        near_val : GLdouble;
                        far_val  : GLdouble);
   procedure glViewport (x      : GLint;
                         y      : GLint;
                         width  : GLsizei;
                         height : GLsizei);
   procedure glPushMatrix;
   procedure glPopMatrix;
   procedure glLoadIdentity;
   procedure glLoadMatrixd (m : access GLdouble);
   procedure glLoadMatrixf (m : access GLfloat);
   procedure glMultMatrixd (m : access GLdouble);
   procedure glMultMatrixf (m : access GLfloat);

   procedure glRotated (angle : GLdouble;
                        x     : GLdouble;
                        y     : GLdouble;
                        z     : GLdouble);
   procedure glRotatef (angle : GLfloat;
                        x     : GLfloat;
                        y     : GLfloat;
                        z     : GLfloat);
   --  The angle is specified in degrees, counter-clockwise.

   procedure glScaled (x : GLdouble; y : GLdouble; z : GLdouble);
   procedure glScalef (x : GLfloat; y : GLfloat; z : GLfloat);
   procedure glTranslated (x : GLdouble; y : GLdouble; z : GLdouble);
   procedure glTranslatef (x : GLfloat; y : GLfloat; z : GLfloat);

   --
   --  Display Lists
   --

   function glIsList (list : GLuint) return GLboolean;
   procedure glDeleteLists (list : GLuint; range_Id : GLsizei);
   function glGenLists (range_Id : GLsizei) return GLuint;
   procedure glNewList (list : GLuint; mode : GLenum);
   procedure glEndList;
   procedure glCallList (list : GLuint);
   procedure glCallLists (n       : GLsizei;
                          type_Id : GLenum;
                          lists   : access GLvoid);
   procedure glListBase (base : GLuint);

   --
   --  Drawing Functions
   --

   procedure glBegin (mode : GLenum);
   procedure glEnd;
   procedure glVertex2d (x : GLdouble; y : GLdouble);
   procedure glVertex2f (x : GLfloat; y : GLfloat);
   procedure glVertex2i (x : GLint; y : GLint);
   procedure glVertex2s (x : GLshort; y : GLshort);
   procedure glVertex3d (x : GLdouble; y : GLdouble; z : GLdouble);
   procedure glVertex3f (x : GLfloat; y : GLfloat; z : GLfloat);
   procedure glVertex3i (x : GLint; y : GLint; z : GLint);
   procedure glVertex3s (x : GLshort; y : GLshort; z : GLshort);
   procedure glVertex4d (x : GLdouble;
                         y : GLdouble;
                         z : GLdouble;
                         w : GLdouble);
   procedure glVertex4f (x : GLfloat; y : GLfloat; z : GLfloat; w : GLfloat);
   procedure glVertex4i (x : GLint; y : GLint; z : GLint; w : GLint);
   procedure glVertex4s (x : GLshort; y : GLshort; z : GLshort; w : GLshort);
   procedure glVertex2dv (v : access GLdouble);
   procedure glVertex2fv (v : access GLfloat);
   procedure glVertex2iv (v : access GLint);
   procedure glVertex2sv (v : access GLshort);
   procedure glVertex3dv (v : access GLdouble);
   procedure glVertex3fv (v : access GLfloat);
   procedure glVertex3iv (v : access GLint);
   procedure glVertex3sv (v : access GLshort);
   procedure glVertex4dv (v : access GLdouble);
   procedure glVertex4fv (v : access GLfloat);
   procedure glVertex4iv (v : access GLint);
   procedure glVertex4sv (v : access GLshort);
   procedure glNormal3b (nx : GLbyte; ny : GLbyte; nz : GLbyte);
   procedure glNormal3d (nx : GLdouble; ny : GLdouble; nz : GLdouble);
   procedure glNormal3f (nx : GLfloat; ny : GLfloat; nz : GLfloat);
   procedure glNormal3i (nx : GLint; ny : GLint; nz : GLint);
   procedure glNormal3s (nx : GLshort; ny : GLshort; nz : GLshort);
   procedure glNormal3bv (v : access GLbyte);
   procedure glNormal3dv (v : access GLdouble);
   procedure glNormal3fv (v : access GLfloat);
   procedure glNormal3iv (v : access GLint);
   procedure glNormal3sv (v : access GLshort);
   procedure glIndexd (c : GLdouble);
   procedure glIndexf (c : GLfloat);
   procedure glIndexi (c : GLint);
   procedure glIndexs (c : GLshort);
   procedure glIndexub (c : GLubyte  --  1.1
         );
   procedure glIndexdv (c : access GLdouble);
   procedure glIndexfv (c : access GLfloat);
   procedure glIndexiv (c : access GLint);
   procedure glIndexsv (c : access GLshort);
   procedure glIndexubv (c : access GLubyte  --  1.1
         );
   procedure glColor3b (red : GLbyte; green : GLbyte; blue : GLbyte);
   procedure glColor3d (red : GLdouble; green : GLdouble; blue : GLdouble);
   procedure glColor3f (red : GLfloat; green : GLfloat; blue : GLfloat);
   procedure glColor3i (red : GLint; green : GLint; blue : GLint);
   procedure glColor3s (red : GLshort; green : GLshort; blue : GLshort);
   procedure glColor3ub (red : GLubyte; green : GLubyte; blue : GLubyte);
   procedure glColor3ui (red : GLuint; green : GLuint; blue : GLuint);
   procedure glColor3us (red : GLushort; green : GLushort; blue : GLushort);
   procedure glColor4b (red   : GLbyte;
                        green : GLbyte;
                        blue  : GLbyte;
                        alpha : GLbyte);
   procedure glColor4d (red   : GLdouble;
                        green : GLdouble;
                        blue  : GLdouble;
                        alpha : GLdouble);
   procedure glColor4f (red   : GLfloat;
                        green : GLfloat;
                        blue  : GLfloat;
                        alpha : GLfloat);
   procedure glColor4i (red   : GLint;
                        green : GLint;
                        blue  : GLint;
                        alpha : GLint);
   procedure glColor4s (red   : GLshort;
                        green : GLshort;
                        blue  : GLshort;
                        alpha : GLshort);
   procedure glColor4ub (red   : GLubyte;
                         green : GLubyte;
                         blue  : GLubyte;
                         alpha : GLubyte);
   procedure glColor4ui (red   : GLuint;
                         green : GLuint;
                         blue  : GLuint;
                         alpha : GLuint);
   procedure glColor4us (red   : GLushort;
                         green : GLushort;
                         blue  : GLushort;
                         alpha : GLushort);
   procedure glColor3bv (v : access GLbyte);
   procedure glColor3dv (v : access GLdouble);
   procedure glColor3fv (v : access GLfloat);
   procedure glColor3iv (v : access GLint);
   procedure glColor3sv (v : access GLshort);
   procedure glColor3ubv (v : access GLubyte);
   procedure glColor3uiv (v : access GLuint);
   procedure glColor3usv (v : access GLushort);
   procedure glColor4bv (v : access GLbyte);
   procedure glColor4dv (v : access GLdouble);
   procedure glColor4fv (v : access GLfloat);
   procedure glColor4iv (v : access GLint);
   procedure glColor4sv (v : access GLshort);
   procedure glColor4ubv (v : access GLubyte);
   procedure glColor4uiv (v : access GLuint);
   procedure glColor4usv (v : access GLushort);
   procedure glTexCoord1d (s : GLdouble);
   procedure glTexCoord1f (s : GLfloat);
   procedure glTexCoord1i (s : GLint);
   procedure glTexCoord1s (s : GLshort);
   procedure glTexCoord2d (s : GLdouble; t : GLdouble);
   procedure glTexCoord2f (s : GLfloat; t : GLfloat);
   procedure glTexCoord2i (s : GLint; t : GLint);
   procedure glTexCoord2s (s : GLshort; t : GLshort);
   procedure glTexCoord3d (s : GLdouble; t : GLdouble; r : GLdouble);
   procedure glTexCoord3f (s : GLfloat; t : GLfloat; r : GLfloat);
   procedure glTexCoord3i (s : GLint; t : GLint; r : GLint);
   procedure glTexCoord3s (s : GLshort; t : GLshort; r : GLshort);
   procedure glTexCoord4d (s : GLdouble;
                           t : GLdouble;
                           r : GLdouble;
                           q : GLdouble);
   procedure glTexCoord4f
      (s : GLfloat; t : GLfloat; r : GLfloat; q : GLfloat);
   procedure glTexCoord4i (s : GLint; t : GLint; r : GLint; q : GLint);
   procedure glTexCoord4s
      (s : GLshort; t : GLshort; r : GLshort; q : GLshort);
   procedure glTexCoord1dv (v : access GLdouble);
   procedure glTexCoord1fv (v : access GLfloat);
   procedure glTexCoord1iv (v : access GLint);
   procedure glTexCoord1sv (v : access GLshort);
   procedure glTexCoord2dv (v : access GLdouble);
   procedure glTexCoord2fv (v : access GLfloat);
   procedure glTexCoord2iv (v : access GLint);
   procedure glTexCoord2sv (v : access GLshort);
   procedure glTexCoord3dv (v : access GLdouble);
   procedure glTexCoord3fv (v : access GLfloat);
   procedure glTexCoord3iv (v : access GLint);
   procedure glTexCoord3sv (v : access GLshort);
   procedure glTexCoord4dv (v : access GLdouble);
   procedure glTexCoord4fv (v : access GLfloat);
   procedure glTexCoord4iv (v : access GLint);
   procedure glTexCoord4sv (v : access GLshort);
   procedure glRasterPos2d (x : GLdouble; y : GLdouble);
   procedure glRasterPos2f (x : GLfloat; y : GLfloat);
   procedure glRasterPos2i (x : GLint; y : GLint);
   procedure glRasterPos2s (x : GLshort; y : GLshort);
   procedure glRasterPos3d (x : GLdouble; y : GLdouble; z : GLdouble);
   procedure glRasterPos3f (x : GLfloat; y : GLfloat; z : GLfloat);
   procedure glRasterPos3i (x : GLint; y : GLint; z : GLint);
   procedure glRasterPos3s (x : GLshort; y : GLshort; z : GLshort);
   procedure glRasterPos4d (x : GLdouble;
                            y : GLdouble;
                            z : GLdouble;
                            w : GLdouble);
   procedure glRasterPos4f (x : GLfloat;
                            y : GLfloat;
                            z : GLfloat;
                            w : GLfloat);
   procedure glRasterPos4i (x : GLint; y : GLint; z : GLint; w : GLint);
   procedure glRasterPos4s (x : GLshort;
                            y : GLshort;
                            z : GLshort;
                            w : GLshort);
   procedure glRasterPos2dv (v : access GLdouble);
   procedure glRasterPos2fv (v : access GLfloat);
   procedure glRasterPos2iv (v : access GLint);
   procedure glRasterPos2sv (v : access GLshort);
   procedure glRasterPos3dv (v : access GLdouble);
   procedure glRasterPos3fv (v : access GLfloat);
   procedure glRasterPos3iv (v : access GLint);
   procedure glRasterPos3sv (v : access GLshort);
   procedure glRasterPos4dv (v : access GLdouble);
   procedure glRasterPos4fv (v : access GLfloat);
   procedure glRasterPos4iv (v : access GLint);
   procedure glRasterPos4sv (v : access GLshort);
   procedure glRectd (x1 : GLdouble;
                      y1 : GLdouble;
                      x2 : GLdouble;
                      y2 : GLdouble);
   procedure glRectf (x1 : GLfloat; y1 : GLfloat; x2 : GLfloat; y2 : GLfloat);
   procedure glRecti (x1 : GLint; y1 : GLint; x2 : GLint; y2 : GLint);
   procedure glRects (x1 : GLshort; y1 : GLshort; x2 : GLshort; y2 : GLshort);
   procedure glRectdv (v1 : access GLdouble; v2 : access GLdouble);
   procedure glRectfv (v1 : access GLfloat; v2 : access GLfloat);
   procedure glRectiv (v1 : access GLint; v2 : access GLint);
   procedure glRectsv (v1 : access GLshort; v2 : access GLshort);

   --
   --  Vertex Arrays  (1.1)
   --

   procedure glVertexPointer (size    : GLint;
                              type_Id : GLenum;
                              stride  : GLsizei;
                              ptr     : access GLvoid);
   procedure glNormalPointer (type_Id : GLenum;
                              stride  : GLsizei;
                              ptr     : access GLvoid);
   procedure glColorPointer (size    : GLint;
                             type_Id : GLenum;
                             stride  : GLsizei;
                             ptr     : access GLvoid);
   procedure glIndexPointer (type_Id : GLenum;
                             stride  : GLsizei;
                             ptr     : access GLvoid);
   procedure glTexCoordPointer (size    : GLint;
                                type_Id : GLenum;
                                stride  : GLsizei;
                                ptr     : access GLvoid);
   procedure glEdgeFlagPointer (stride : GLsizei; ptr : access GLboolean);
   procedure glGetPointerv (pname  : GLenum;
                            params : access Interfaces.C.Extensions.void_ptr);
   procedure glArrayElement (i : GLint);
   procedure glDrawArrays (mode : GLenum; first : GLint; count : GLsizei);
   procedure glDrawElements (mode    : GLenum;
                             count   : GLsizei;
                             type_Id : GLenum;
                             indices : access GLvoid);
   procedure glInterleavedArrays (format  : GLenum;
                                  stride  : GLsizei;
                                  pointer : access GLvoid);

   --
   --  Lighting
   --

   procedure glShadeModel (mode : GLenum);
   procedure glLightf (light : GLenum; pname : GLenum; param : GLfloat);
   procedure glLighti (light : GLenum; pname : GLenum; param : GLint);
   procedure glLightfv (light  : GLenum;
                        pname  : GLenum;
                        params : GLfloat_Vec_4);
   procedure glLightiv
      (light : GLenum; pname : GLenum; params : access GLint);
   procedure glGetLightfv (light  : GLenum;
                           pname  : GLenum;
                           params : access GLfloat);
   procedure glGetLightiv (light  : GLenum;
                           pname  : GLenum;
                           params : access GLint);
   procedure glLightModelf (pname : GLenum; param : GLfloat);
   procedure glLightModeli (pname : GLenum; param : GLint);
   procedure glLightModelfv (pname : GLenum; params : access GLfloat);
   procedure glLightModeliv (pname : GLenum; params : access GLint);
   procedure glMaterialf (face : GLenum; pname : GLenum; param : GLfloat);
   procedure glMateriali (face : GLenum; pname : GLenum; param : GLint);
   procedure glMaterialfv (face   : GLenum;
                           pname  : GLenum;
                           params : GLfloat_Vec_4);
   procedure glMaterialiv (face   : GLenum;
                           pname  : GLenum;
                           params : access GLint);
   procedure glGetMaterialfv (face   : GLenum;
                              pname  : GLenum;
                              params : access GLfloat);
   procedure glGetMaterialiv (face   : GLenum;
                              pname  : GLenum;
                              params : access GLint);
   procedure glColorMaterial (face : GLenum; mode : GLenum);

   --
   --  Raster functions
   --

   procedure glPixelZoom (xfactor : GLfloat; yfactor : GLfloat);
   procedure glPixelStoref (pname : GLenum; param : GLfloat);
   procedure glPixelStorei (pname : GLenum; param : GLint);
   procedure glPixelTransferf (pname : GLenum; param : GLfloat);
   procedure glPixelTransferi (pname : GLenum; param : GLint);
   procedure glPixelMapfv (map     : GLenum;
                           mapsize : GLint;
                           values  : access GLfloat);
   procedure glPixelMapuiv (map     : GLenum;
                            mapsize : GLint;
                            values  : access GLuint);
   procedure glPixelMapusv (map     : GLenum;
                            mapsize : GLint;
                            values  : access GLushort);
   procedure glGetPixelMapfv (map : GLenum; values : access GLfloat);
   procedure glGetPixelMapuiv (map : GLenum; values : access GLuint);
   procedure glGetPixelMapusv (map : GLenum; values : access GLushort);
   procedure glBitmap (width  : GLsizei;
                       height : GLsizei;
                       xorig  : GLfloat;
                       yorig  : GLfloat;
                       xmove  : GLfloat;
                       ymove  : GLfloat;
                       bitmap : access GLubyte);
   procedure glReadPixels (x       : GLint;
                           y       : GLint;
                           width   : GLsizei;
                           height  : GLsizei;
                           format  : GLenum;
                           type_Id : GLenum;
                           pixels  : access GLvoid);
   procedure glDrawPixels (width   : GLsizei;
                           height  : GLsizei;
                           format  : GLenum;
                           type_Id : GLenum;
                           pixels  : access GLvoid);
   procedure glCopyPixels (x       : GLint;
                           y       : GLint;
                           width   : GLsizei;
                           height  : GLsizei;
                           type_Id : GLenum);

   --
   --  Stenciling
   --

   procedure glStencilFunc (func : GLenum; ref : GLint; mask : GLuint);
   procedure glStencilMask (mask : GLuint);
   procedure glStencilOp (fail : GLenum; zfail : GLenum; zpass : GLenum);
   procedure glClearStencil (s : GLint);

   --
   --  Texture mapping
   --

   procedure glTexGend (coord : GLenum; pname : GLenum; param : GLdouble);
   procedure glTexGenf (coord : GLenum; pname : GLenum; param : GLfloat);
   procedure glTexGeni (coord : GLenum; pname : GLenum; param : GLint);
   procedure glTexGendv (coord  : GLenum;
                         pname  : GLenum;
                         params : access GLdouble);
   procedure glTexGenfv (coord  : GLenum;
                         pname  : GLenum;
                         params : access GLfloat);
   procedure glTexGeniv (coord  : GLenum;
                         pname  : GLenum;
                         params : access GLint);
   procedure glGetTexGendv (coord  : GLenum;
                            pname  : GLenum;
                            params : access GLdouble);
   procedure glGetTexGenfv (coord  : GLenum;
                            pname  : GLenum;
                            params : access GLfloat);
   procedure glGetTexGeniv (coord  : GLenum;
                            pname  : GLenum;
                            params : access GLint);
   procedure glTexEnvf (target : GLenum; pname : GLenum; param : GLfloat);
   procedure glTexEnvi (target : GLenum; pname : GLenum; param : GLint);
   procedure glTexEnvfv (target : GLenum;
                         pname  : GLenum;
                         params : access GLfloat);
   procedure glTexEnviv (target : GLenum;
                         pname  : GLenum;
                         params : access GLint);
   procedure glGetTexEnvfv (target : GLenum;
                            pname  : GLenum;
                            params : access GLfloat);
   procedure glGetTexEnviv (target : GLenum;
                            pname  : GLenum;
                            params : access GLint);
   procedure glTexParameterf (target : GLenum;
                              pname  : GLenum;
                              param  : GLfloat);
   procedure glTexParameteri (target : GLenum; pname : GLenum; param : GLenum);
   procedure glTexParameterfv (target : GLenum;
                               pname  : GLenum;
                               params : access GLfloat);
   procedure glTexParameteriv (target : GLenum;
                               pname  : GLenum;
                               params : access GLint);
   procedure glGetTexParameterfv (target : GLenum;
                                  pname  : GLenum;
                                  params : access GLfloat);
   procedure glGetTexParameteriv (target : GLenum;
                                  pname  : GLenum;
                                  params : access GLint);
   procedure glGetTexLevelParameterfv (target : GLenum;
                                       level  : GLint;
                                       pname  : GLenum;
                                       params : access GLfloat);
   procedure glGetTexLevelParameteriv (target : GLenum;
                                       level  : GLint;
                                       pname  : GLenum;
                                       params : access GLint);
   procedure glTexImage1D (target         : GLenum;
                           level          : GLint;
                           internalFormat : GLint;
                           width          : GLsizei;
                           border         : GLint;
                           format         : GLenum;
                           type_Id        : GLenum;
                           pixels         : System.Address);
   procedure glTexImage2D (target         : GLenum;
                           level          : GLint;
                           internalFormat : GLint;
                           width          : GLsizei;
                           height         : GLsizei;
                           border         : GLint;
                           format         : GLenum;
                           type_Id        : GLenum;
                           pixels         : System.Address);
   procedure glGetTexImage (target  : GLenum;
                            level   : GLint;
                            format  : GLenum;
                            type_Id : GLenum;
                            pixels  : access GLvoid);

   --   1.1 functions

   procedure glGenTextures (n : GLsizei; textures : in out GLuint_Vec);
   procedure glDeleteTextures (n : GLsizei; textures : access GLuint);
   procedure glBindTexture (target : GLenum; texture : GLuint);
   procedure glPrioritizeTextures (n          : GLsizei;
                                   textures   : access GLuint;
                                   priorities : access GLclampf);
   function glAreTexturesResident
      (n : GLsizei;  textures : access GLuint;  residences : access GLboolean)
      return GLboolean;
   function glIsTexture (texture : GLuint) return GLboolean;
   procedure glTexSubImage1D (target  : GLenum;
                              level   : GLint;
                              xoffset : GLint;
                              width   : GLsizei;
                              format  : GLenum;
                              type_Id : GLenum;
                              pixels  : access GLvoid);
   procedure glTexSubImage2D (target  : GLenum;
                              level   : GLint;
                              xoffset : GLint;
                              yoffset : GLint;
                              width   : GLsizei;
                              height  : GLsizei;
                              format  : GLenum;
                              type_Id : GLenum;
                              pixels  : access GLvoid);
   procedure glCopyTexImage1D (target         : GLenum;
                               level          : GLint;
                               internalformat : GLenum;
                               x              : GLint;
                               y              : GLint;
                               width          : GLsizei;
                               border         : GLint);
   procedure glCopyTexImage2D (target         : GLenum;
                               level          : GLint;
                               internalformat : GLenum;
                               x              : GLint;
                               y              : GLint;
                               width          : GLsizei;
                               height         : GLsizei;
                               border         : GLint);
   procedure glCopyTexSubImage1D (target  : GLenum;
                                  level   : GLint;
                                  xoffset : GLint;
                                  x       : GLint;
                                  y       : GLint;
                                  width   : GLsizei);
   procedure glCopyTexSubImage2D (target  : GLenum;
                                  level   : GLint;
                                  xoffset : GLint;
                                  yoffset : GLint;
                                  x       : GLint;
                                  y       : GLint;
                                  width   : GLsizei;
                                  height  : GLsizei);

   --
   --  Evaluators
   --

   procedure glMap1d (target : GLenum;
                      u1     : GLdouble;
                      u2     : GLdouble;
                      stride : GLint;
                      order  : GLint;
                      points : access GLdouble);
   procedure glMap1f (target : GLenum;
                      u1     : GLfloat;
                      u2     : GLfloat;
                      stride : GLint;
                      order  : GLint;
                      points : access GLfloat);
   procedure glMap2d (target  : GLenum;
                      u1      : GLdouble;
                      u2      : GLdouble;
                      ustride : GLint;
                      uorder  : GLint;
                      v1      : GLdouble;
                      v2      : GLdouble;
                      vstride : GLint;
                      vorder  : GLint;
                      points  : access GLdouble);
   procedure glMap2f (target  : GLenum;
                      u1      : GLfloat;
                      u2      : GLfloat;
                      ustride : GLint;
                      uorder  : GLint;
                      v1      : GLfloat;
                      v2      : GLfloat;
                      vstride : GLint;
                      vorder  : GLint;
                      points  : access GLfloat);
   procedure glGetMapdv
      (target : GLenum; query : GLenum; v : access GLdouble);
   procedure glGetMapfv (target : GLenum; query : GLenum; v : access GLfloat);
   procedure glGetMapiv (target : GLenum; query : GLenum; v : access GLint);
   procedure glEvalCoord1d (u : GLdouble);
   procedure glEvalCoord1f (u : GLfloat);
   procedure glEvalCoord1dv (u : access GLdouble);
   procedure glEvalCoord1fv (u : access GLfloat);
   procedure glEvalCoord2d (u : GLdouble; v : GLdouble);
   procedure glEvalCoord2f (u : GLfloat; v : GLfloat);
   procedure glEvalCoord2dv (u : access GLdouble);
   procedure glEvalCoord2fv (u : access GLfloat);
   procedure glMapGrid1d (un : GLint; u1 : GLdouble; u2 : GLdouble);
   procedure glMapGrid1f (un : GLint; u1 : GLfloat; u2 : GLfloat);
   procedure glMapGrid2d (un : GLint;
                          u1 : GLdouble;
                          u2 : GLdouble;
                          vn : GLint;
                          v1 : GLdouble;
                          v2 : GLdouble);
   procedure glMapGrid2f (un : GLint;
                          u1 : GLfloat;
                          u2 : GLfloat;
                          vn : GLint;
                          v1 : GLfloat;
                          v2 : GLfloat);
   procedure glEvalPoint1 (i : GLint);
   procedure glEvalPoint2 (i : GLint; j : GLint);
   procedure glEvalMesh1 (mode : GLenum; i1 : GLint; i2 : GLint);
   procedure glEvalMesh2 (mode : GLenum;
                          i1   : GLint;
                          i2   : GLint;
                          j1   : GLint;
                          j2   : GLint);

   --
   --  Fog
   --

   procedure glFogf (pname : GLenum; param : GLfloat);
   procedure glFogi (pname : GLenum; param : GLenum);
   procedure glFogfv (pname : GLenum; params : GLfloat_Vec_4);
   procedure glFogiv (pname : GLenum; params : access GLint);

   --
   --  Selection and Feedback
   --

   procedure glFeedbackBuffer (size    : GLsizei;
                               type_Id : GLenum;
                               buffer  : access GLfloat);
   procedure glPassThrough (token : GLfloat);
   procedure glSelectBuffer (size : GLsizei; buffer : access GLuint);
   procedure glInitNames;
   procedure glLoadName (name : GLuint);
   procedure glPushName (name : GLuint);
   procedure glPopName;

   --
   --  1.0 Extensions
   --

   --   GL_EXT_blend_minmax

   procedure glBlendEquationEXT (mode : GLenum);

   --   GL_EXT_blend_color

   procedure glBlendColorEXT (red   : GLclampf;
                              green : GLclampf;
                              blue  : GLclampf;
                              alpha : GLclampf);

   --   GL_EXT_polygon_offset

   procedure glPolygonOffsetEXT (factor : GLfloat; bias : GLfloat);

   --   GL_EXT_vertex_array

   procedure glVertexPointerEXT (size    : GLint;
                                 type_Id : GLenum;
                                 stride  : GLsizei;
                                 count   : GLsizei;
                                 ptr     : access GLvoid);
   procedure glNormalPointerEXT (type_Id : GLenum;
                                 stride  : GLsizei;
                                 count   : GLsizei;
                                 ptr     : access GLvoid);
   procedure glColorPointerEXT (size    : GLint;
                                type_Id : GLenum;
                                stride  : GLsizei;
                                count   : GLsizei;
                                ptr     : access GLvoid);
   procedure glIndexPointerEXT (type_Id : GLenum;
                                stride  : GLsizei;
                                count   : GLsizei;
                                ptr     : access GLvoid);
   procedure glTexCoordPointerEXT (size    : GLint;
                                   type_Id : GLenum;
                                   stride  : GLsizei;
                                   count   : GLsizei;
                                   ptr     : access GLvoid);
   procedure glEdgeFlagPointerEXT (stride : GLsizei;
                                   count  : GLsizei;
                                   ptr    : access GLboolean);
   procedure glGetPointervEXT
      (pname    : GLenum;
         params : access Interfaces.C.Extensions.void_ptr);
   procedure glArrayElementEXT (i : GLint);
   procedure glDrawArraysEXT (mode : GLenum; first : GLint; count : GLsizei);

   --   GL_EXT_texture_object

   procedure glGenTexturesEXT (n : GLsizei; textures : access GLuint);
   procedure glDeleteTexturesEXT (n : GLsizei; textures : access GLuint);
   procedure glBindTextureEXT (target : GLenum; texture : GLuint);
   procedure glPrioritizeTexturesEXT (n          : GLsizei;
                                      textures   : access GLuint;
                                      priorities : access GLclampf);
   function glAreTexturesResidentEXT
      (n : GLsizei;  textures : access GLuint;  residences : access GLboolean)
      return GLboolean;
   function glIsTextureEXT (texture : GLuint) return GLboolean;

   --   GL_EXT_texture3D

   procedure glTexImage3DEXT (target         : GLenum;
                              level          : GLint;
                              internalFormat : GLenum;
                              width          : GLsizei;
                              height         : GLsizei;
                              depth          : GLsizei;
                              border         : GLint;
                              format         : GLenum;
                              type_Id        : GLenum;
                              pixels         : access GLvoid);
   procedure glTexSubImage3DEXT (target  : GLenum;
                                 level   : GLint;
                                 xoffset : GLint;
                                 yoffset : GLint;
                                 zoffset : GLint;
                                 width   : GLsizei;
                                 height  : GLsizei;
                                 depth   : GLsizei;
                                 format  : GLenum;
                                 type_Id : GLenum;
                                 pixels  : access GLvoid);
   procedure glCopyTexSubImage3DEXT (target  : GLenum;
                                     level   : GLint;
                                     xoffset : GLint;
                                     yoffset : GLint;
                                     zoffset : GLint;
                                     x       : GLint;
                                     y       : GLint;
                                     width   : GLsizei;
                                     height  : GLsizei);

   --   GL_EXT_color_table

   procedure glColorTableEXT (target         : GLenum;
                              internalformat : GLenum;
                              width          : GLsizei;
                              format         : GLenum;
                              type_Id        : GLenum;
                              table          : access GLvoid);
   procedure glColorSubTableEXT (target  : GLenum;
                                 start   : GLsizei;
                                 count   : GLsizei;
                                 format  : GLenum;
                                 type_Id : GLenum;
                                 data    : access GLvoid);
   procedure glGetColorTableEXT (target  : GLenum;
                                 format  : GLenum;
                                 type_Id : GLenum;
                                 table   : access GLvoid);
   procedure glGetColorTableParameterfvEXT (target : GLenum;
                                            pname  : GLenum;
                                            params : access GLfloat);
   procedure glGetColorTableParameterivEXT (target : GLenum;
                                            pname  : GLenum;
                                            params : access GLint);

   --   GL_SGIS_multitexture

   procedure glMultiTexCoord1dSGIS (target : GLenum; s : GLdouble);
   procedure glMultiTexCoord1dvSGIS (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord1fSGIS (target : GLenum; s : GLfloat);
   procedure glMultiTexCoord1fvSGIS (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord1iSGIS (target : GLenum; s : GLint);
   procedure glMultiTexCoord1ivSGIS (target : GLenum; v : access GLint);
   procedure glMultiTexCoord1sSGIS (target : GLenum; s : GLshort);
   procedure glMultiTexCoord1svSGIS (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord2dSGIS (target : GLenum;
                                    s      : GLdouble;
                                    t      : GLdouble);
   procedure glMultiTexCoord2dvSGIS (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord2fSGIS
      (target : GLenum; s : GLfloat; t : GLfloat);
   procedure glMultiTexCoord2fvSGIS (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord2iSGIS (target : GLenum; s : GLint; t : GLint);
   procedure glMultiTexCoord2ivSGIS (target : GLenum; v : access GLint);
   procedure glMultiTexCoord2sSGIS
      (target : GLenum; s : GLshort; t : GLshort);
   procedure glMultiTexCoord2svSGIS (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord3dSGIS (target : GLenum;
                                    s      : GLdouble;
                                    t      : GLdouble;
                                    r      : GLdouble);
   procedure glMultiTexCoord3dvSGIS (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord3fSGIS (target : GLenum;
                                    s      : GLfloat;
                                    t      : GLfloat;
                                    r      : GLfloat);
   procedure glMultiTexCoord3fvSGIS (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord3iSGIS (target : GLenum;
                                    s      : GLint;
                                    t      : GLint;
                                    r      : GLint);
   procedure glMultiTexCoord3ivSGIS (target : GLenum; v : access GLint);
   procedure glMultiTexCoord3sSGIS (target : GLenum;
                                    s      : GLshort;
                                    t      : GLshort;
                                    r      : GLshort);
   procedure glMultiTexCoord3svSGIS (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord4dSGIS (target : GLenum;
                                    s      : GLdouble;
                                    t      : GLdouble;
                                    r      : GLdouble;
                                    q      : GLdouble);
   procedure glMultiTexCoord4dvSGIS (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord4fSGIS (target : GLenum;
                                    s      : GLfloat;
                                    t      : GLfloat;
                                    r      : GLfloat;
                                    q      : GLfloat);
   procedure glMultiTexCoord4fvSGIS (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord4iSGIS (target : GLenum;
                                    s      : GLint;
                                    t      : GLint;
                                    r      : GLint;
                                    q      : GLint);
   procedure glMultiTexCoord4ivSGIS (target : GLenum; v : access GLint);
   procedure glMultiTexCoord4sSGIS (target : GLenum;
                                    s      : GLshort;
                                    t      : GLshort;
                                    r      : GLshort;
                                    q      : GLshort);
   procedure glMultiTexCoord4svSGIS (target : GLenum; v : access GLshort);
   procedure glMultiTexCoordPointerSGIS (target  : GLenum;
                                         size    : GLint;
                                         type_Id : GLenum;
                                         stride  : GLsizei;
                                         pointer : access GLvoid);
   procedure glSelectTextureSGIS (target : GLenum);
   procedure glSelectTextureCoordSetSGIS (target : GLenum);

   --   GL_EXT_multitexture

   procedure glMultiTexCoord1dEXT (target : GLenum; s : GLdouble);
   procedure glMultiTexCoord1dvEXT (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord1fEXT (target : GLenum; s : GLfloat);
   procedure glMultiTexCoord1fvEXT (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord1iEXT (target : GLenum; s : GLint);
   procedure glMultiTexCoord1ivEXT (target : GLenum; v : access GLint);
   procedure glMultiTexCoord1sEXT (target : GLenum; s : GLshort);
   procedure glMultiTexCoord1svEXT (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord2dEXT (target : GLenum;
                                   s      : GLdouble;
                                   t      : GLdouble);
   procedure glMultiTexCoord2dvEXT (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord2fEXT (target : GLenum; s : GLfloat; t : GLfloat);
   procedure glMultiTexCoord2fvEXT (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord2iEXT (target : GLenum; s : GLint; t : GLint);
   procedure glMultiTexCoord2ivEXT (target : GLenum; v : access GLint);
   procedure glMultiTexCoord2sEXT (target : GLenum; s : GLshort; t : GLshort);
   procedure glMultiTexCoord2svEXT (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord3dEXT (target : GLenum;
                                   s      : GLdouble;
                                   t      : GLdouble;
                                   r      : GLdouble);
   procedure glMultiTexCoord3dvEXT (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord3fEXT (target : GLenum;
                                   s      : GLfloat;
                                   t      : GLfloat;
                                   r      : GLfloat);
   procedure glMultiTexCoord3fvEXT (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord3iEXT (target : GLenum;
                                   s      : GLint;
                                   t      : GLint;
                                   r      : GLint);
   procedure glMultiTexCoord3ivEXT (target : GLenum; v : access GLint);
   procedure glMultiTexCoord3sEXT (target : GLenum;
                                   s      : GLshort;
                                   t      : GLshort;
                                   r      : GLshort);
   procedure glMultiTexCoord3svEXT (target : GLenum; v : access GLshort);
   procedure glMultiTexCoord4dEXT (target : GLenum;
                                   s      : GLdouble;
                                   t      : GLdouble;
                                   r      : GLdouble;
                                   q      : GLdouble);
   procedure glMultiTexCoord4dvEXT (target : GLenum; v : access GLdouble);
   procedure glMultiTexCoord4fEXT (target : GLenum;
                                   s      : GLfloat;
                                   t      : GLfloat;
                                   r      : GLfloat;
                                   q      : GLfloat);
   procedure glMultiTexCoord4fvEXT (target : GLenum; v : access GLfloat);
   procedure glMultiTexCoord4iEXT (target : GLenum;
                                   s      : GLint;
                                   t      : GLint;
                                   r      : GLint;
                                   q      : GLint);
   procedure glMultiTexCoord4ivEXT (target : GLenum; v : access GLint);
   procedure glMultiTexCoord4sEXT (target : GLenum;
                                   s      : GLshort;
                                   t      : GLshort;
                                   r      : GLshort;
                                   q      : GLshort);
   procedure glMultiTexCoord4svEXT (target : GLenum; v : access GLshort);
   procedure glInterleavedTextureCoordSetsEXT (factor : GLint);
   procedure glSelectTextureEXT (target : GLenum);
   procedure glSelectTextureCoordSetEXT (target : GLenum);
   procedure glSelectTextureTransformEXT (target : GLenum);

   --   GL_EXT_point_parameters

   procedure glPointParameterfEXT (pname : GLenum; param : GLfloat);
   procedure glPointParameterfvEXT (pname : GLenum; params : access GLfloat);

   --   GL_MESA_window_pos

   procedure glWindowPos2iMESA (x : GLint; y : GLint);
   procedure glWindowPos2sMESA (x : GLshort; y : GLshort);
   procedure glWindowPos2fMESA (x : GLfloat; y : GLfloat);
   procedure glWindowPos2dMESA (x : GLdouble; y : GLdouble);
   procedure glWindowPos2ivMESA (p : access GLint);
   procedure glWindowPos2svMESA (p : access GLshort);
   procedure glWindowPos2fvMESA (p : access GLfloat);
   procedure glWindowPos2dvMESA (p : access GLdouble);
   procedure glWindowPos3iMESA (x : GLint; y : GLint; z : GLint);
   procedure glWindowPos3sMESA (x : GLshort; y : GLshort; z : GLshort);
   procedure glWindowPos3fMESA (x : GLfloat; y : GLfloat; z : GLfloat);
   procedure glWindowPos3dMESA (x : GLdouble; y : GLdouble; z : GLdouble);
   procedure glWindowPos3ivMESA (p : access GLint);
   procedure glWindowPos3svMESA (p : access GLshort);
   procedure glWindowPos3fvMESA (p : access GLfloat);
   procedure glWindowPos3dvMESA (p : access GLdouble);
   procedure glWindowPos4iMESA (x : GLint; y : GLint; z : GLint; w : GLint);
   procedure glWindowPos4sMESA (x : GLshort;
                                y : GLshort;
                                z : GLshort;
                                w : GLshort);
   procedure glWindowPos4fMESA (x : GLfloat;
                                y : GLfloat;
                                z : GLfloat;
                                w : GLfloat);
   procedure glWindowPos4dMESA (x : GLdouble;
                                y : GLdouble;
                                z : GLdouble;
                                w : GLdouble);
   procedure glWindowPos4ivMESA (p : access GLint);
   procedure glWindowPos4svMESA (p : access GLshort);
   procedure glWindowPos4fvMESA (p : access GLfloat);
   procedure glWindowPos4dvMESA (p : access GLdouble);

   --   GL_MESA_resize_buffers

   procedure glResizeBuffersMESA;

   --   1.2 functions

   procedure glDrawRangeElements (mode    : GLenum;
                                  start   : GLuint;
                                  end_Id  : GLuint;
                                  count   : GLsizei;
                                  type_Id : GLenum;
                                  indices : access GLvoid);
   procedure glTexImage3D (target         : GLenum;
                           level          : GLint;
                           internalFormat : GLenum;
                           width          : GLsizei;
                           height         : GLsizei;
                           depth          : GLsizei;
                           border         : GLint;
                           format         : GLenum;
                           type_Id        : GLenum;
                           pixels         : access GLvoid);
   procedure glTexSubImage3D (target  : GLenum;
                              level   : GLint;
                              xoffset : GLint;
                              yoffset : GLint;
                              zoffset : GLint;
                              width   : GLsizei;
                              height  : GLsizei;
                              depth   : GLsizei;
                              format  : GLenum;
                              type_Id : GLenum;
                              pixels  : access GLvoid);
   procedure glCopyTexSubImage3D (target  : GLenum;
                                  level   : GLint;
                                  xoffset : GLint;
                                  yoffset : GLint;
                                  zoffset : GLint;
                                  x       : GLint;
                                  y       : GLint;
                                  width   : GLsizei;
                                  height  : GLsizei);

   --
   --  Compile-time tests for extensions:
   --

   GL_EXT_blend_color            : constant := 1;
   GL_EXT_blend_logic_op         : constant := 1;
   GL_EXT_blend_minmax           : constant := 1;
   GL_EXT_blend_subtract         : constant := 1;
   GL_EXT_polygon_offset         : constant := 1;
   GL_EXT_vertex_array           : constant := 1;
   GL_EXT_texture_object         : constant := 1;
   GL_EXT_texture3D              : constant := 1;
   GL_EXT_paletted_texture       : constant := 1;
   GL_EXT_shared_texture_palette : constant := 1;
   GL_EXT_point_parameters       : constant := 1;
   GL_EXT_rescale_normal         : constant := 1;
   GL_EXT_abgr                   : constant := 1;
   GL_EXT_multitexture           : constant := 1;
   GL_MESA_window_pos            : constant := 1;
   GL_MESA_resize_buffers        : constant := 1;
   GL_SGIS_multitexture          : constant := 1;
   GL_SGIS_texture_edge_clamp    : constant := 1;

private

   pragma Import (Stdcall, glClearIndex, "glClearIndex");
   pragma Import (Stdcall, glClearColor, "glClearColor");
   pragma Import (Stdcall, glClear, "glClear");
   pragma Import (Stdcall, glIndexMask, "glIndexMask");
   pragma Import (Stdcall, glColorMask, "glColorMask");
   pragma Import (Stdcall, glAlphaFunc, "glAlphaFunc");
   pragma Import (Stdcall, glBlendFunc, "glBlendFunc");
   pragma Import (Stdcall, glLogicOp, "glLogicOp");
   pragma Import (Stdcall, glCullFace, "glCullFace");
   pragma Import (Stdcall, glFrontFace, "glFrontFace");
   pragma Import (Stdcall, glPointSize, "glPointSize");
   pragma Import (Stdcall, glLineWidth, "glLineWidth");
   pragma Import (Stdcall, glLineStipple, "glLineStipple");
   pragma Import (Stdcall, glPolygonMode, "glPolygonMode");
   pragma Import (Stdcall, glPolygonOffset, "glPolygonOffset");
   pragma Import (Stdcall, glPolygonStipple, "glPolygonStipple");
   pragma Import (Stdcall, glGetPolygonStipple, "glGetPolygonStipple");
   pragma Import (Stdcall, glEdgeFlag, "glEdgeFlag");
   pragma Import (Stdcall, glEdgeFlagv, "glEdgeFlagv");
   pragma Import (Stdcall, glScissor, "glScissor");
   pragma Import (Stdcall, glClipPlane, "glClipPlane");
   pragma Import (Stdcall, glGetClipPlane, "glGetClipPlane");
   pragma Import (Stdcall, glDrawBuffer, "glDrawBuffer");
   pragma Import (Stdcall, glReadBuffer, "glReadBuffer");
   pragma Import (Stdcall, glEnable, "glEnable");
   pragma Import (Stdcall, glDisable, "glDisable");
   pragma Import (Stdcall, glIsEnabled, "glIsEnabled");
   pragma Import (Stdcall, glEnableClientState, "glEnableClientState");
   pragma Import (Stdcall, glDisableClientState, "glDisableClientState");
   pragma Import (Stdcall, glGetBooleanv, "glGetBooleanv");
   pragma Import (Stdcall, glGetDoublev, "glGetDoublev");
   pragma Import (Stdcall, glGetFloatv, "glGetFloatv");
   pragma Import (Stdcall, glGetIntegerv, "glGetIntegerv");
   pragma Import (Stdcall, glPushAttrib, "glPushAttrib");
   pragma Import (Stdcall, glPopAttrib, "glPopAttrib");
   pragma Import (Stdcall, glPushClientAttrib, "glPushClientAttrib");
   pragma Import (Stdcall, glPopClientAttrib, "glPopClientAttrib");
   pragma Import (Stdcall, glRenderMode, "glRenderMode");
   pragma Import (Stdcall, glGetError, "glGetError");
   pragma Import (Stdcall, glGetString, "glGetString");
   pragma Import (Stdcall, glFinish, "glFinish");
   pragma Import (Stdcall, glFlush, "glFlush");
   pragma Import (Stdcall, glHint, "glHint");
   pragma Import (Stdcall, glClearDepth, "glClearDepth");
   pragma Import (Stdcall, glDepthFunc, "glDepthFunc");
   pragma Import (Stdcall, glDepthMask, "glDepthMask");
   pragma Import (Stdcall, glDepthRange, "glDepthRange");
   pragma Import (Stdcall, glClearAccum, "glClearAccum");
   pragma Import (Stdcall, glAccum, "glAccum");
   pragma Import (Stdcall, glMatrixMode, "glMatrixMode");
   pragma Import (Stdcall, glOrtho, "glOrtho");
   pragma Import (Stdcall, glFrustum, "glFrustum");
   pragma Import (Stdcall, glViewport, "glViewport");
   pragma Import (Stdcall, glPushMatrix, "glPushMatrix");
   pragma Import (Stdcall, glPopMatrix, "glPopMatrix");
   pragma Import (Stdcall, glLoadIdentity, "glLoadIdentity");
   pragma Import (Stdcall, glLoadMatrixd, "glLoadMatrixd");
   pragma Import (Stdcall, glLoadMatrixf, "glLoadMatrixf");
   pragma Import (Stdcall, glMultMatrixd, "glMultMatrixd");
   pragma Import (Stdcall, glMultMatrixf, "glMultMatrixf");
   pragma Import (Stdcall, glRotated, "glRotated");
   pragma Import (Stdcall, glRotatef, "glRotatef");
   pragma Import (Stdcall, glScaled, "glScaled");
   pragma Import (Stdcall, glScalef, "glScalef");
   pragma Import (Stdcall, glTranslated, "glTranslated");
   pragma Import (Stdcall, glTranslatef, "glTranslatef");
   pragma Import (Stdcall, glIsList, "glIsList");
   pragma Import (Stdcall, glDeleteLists, "glDeleteLists");
   pragma Import (Stdcall, glGenLists, "glGenLists");
   pragma Import (Stdcall, glNewList, "glNewList");
   pragma Import (Stdcall, glEndList, "glEndList");
   pragma Import (Stdcall, glCallList, "glCallList");
   pragma Import (Stdcall, glCallLists, "glCallLists");
   pragma Import (Stdcall, glListBase, "glListBase");
   pragma Import (Stdcall, glBegin, "glBegin");
   pragma Import (Stdcall, glEnd, "glEnd");
   pragma Import (Stdcall, glVertex2d, "glVertex2d");
   pragma Import (Stdcall, glVertex2f, "glVertex2f");
   pragma Import (Stdcall, glVertex2i, "glVertex2i");
   pragma Import (Stdcall, glVertex2s, "glVertex2s");
   pragma Import (Stdcall, glVertex3d, "glVertex3d");
   pragma Import (Stdcall, glVertex3f, "glVertex3f");
   pragma Import (Stdcall, glVertex3i, "glVertex3i");
   pragma Import (Stdcall, glVertex3s, "glVertex3s");
   pragma Import (Stdcall, glVertex4d, "glVertex4d");
   pragma Import (Stdcall, glVertex4f, "glVertex4f");
   pragma Import (Stdcall, glVertex4i, "glVertex4i");
   pragma Import (Stdcall, glVertex4s, "glVertex4s");
   pragma Import (Stdcall, glVertex2dv, "glVertex2dv");
   pragma Import (Stdcall, glVertex2fv, "glVertex2fv");
   pragma Import (Stdcall, glVertex2iv, "glVertex2iv");
   pragma Import (Stdcall, glVertex2sv, "glVertex2sv");
   pragma Import (Stdcall, glVertex3dv, "glVertex3dv");
   pragma Import (Stdcall, glVertex3fv, "glVertex3fv");
   pragma Import (Stdcall, glVertex3iv, "glVertex3iv");
   pragma Import (Stdcall, glVertex3sv, "glVertex3sv");
   pragma Import (Stdcall, glVertex4dv, "glVertex4dv");
   pragma Import (Stdcall, glVertex4fv, "glVertex4fv");
   pragma Import (Stdcall, glVertex4iv, "glVertex4iv");
   pragma Import (Stdcall, glVertex4sv, "glVertex4sv");
   pragma Import (Stdcall, glNormal3b, "glNormal3b");
   pragma Import (Stdcall, glNormal3d, "glNormal3d");
   pragma Import (Stdcall, glNormal3f, "glNormal3f");
   pragma Import (Stdcall, glNormal3i, "glNormal3i");
   pragma Import (Stdcall, glNormal3s, "glNormal3s");
   pragma Import (Stdcall, glNormal3bv, "glNormal3bv");
   pragma Import (Stdcall, glNormal3dv, "glNormal3dv");
   pragma Import (Stdcall, glNormal3fv, "glNormal3fv");
   pragma Import (Stdcall, glNormal3iv, "glNormal3iv");
   pragma Import (Stdcall, glNormal3sv, "glNormal3sv");
   pragma Import (Stdcall, glIndexd, "glIndexd");
   pragma Import (Stdcall, glIndexf, "glIndexf");
   pragma Import (Stdcall, glIndexi, "glIndexi");
   pragma Import (Stdcall, glIndexs, "glIndexs");
   pragma Import (Stdcall, glIndexub, "glIndexub");
   pragma Import (Stdcall, glIndexdv, "glIndexdv");
   pragma Import (Stdcall, glIndexfv, "glIndexfv");
   pragma Import (Stdcall, glIndexiv, "glIndexiv");
   pragma Import (Stdcall, glIndexsv, "glIndexsv");
   pragma Import (Stdcall, glIndexubv, "glIndexubv");
   pragma Import (Stdcall, glColor3b, "glColor3b");
   pragma Import (Stdcall, glColor3d, "glColor3d");
   pragma Import (Stdcall, glColor3f, "glColor3f");
   pragma Import (Stdcall, glColor3i, "glColor3i");
   pragma Import (Stdcall, glColor3s, "glColor3s");
   pragma Import (Stdcall, glColor3ub, "glColor3ub");
   pragma Import (Stdcall, glColor3ui, "glColor3ui");
   pragma Import (Stdcall, glColor3us, "glColor3us");
   pragma Import (Stdcall, glColor4b, "glColor4b");
   pragma Import (Stdcall, glColor4d, "glColor4d");
   pragma Import (Stdcall, glColor4f, "glColor4f");
   pragma Import (Stdcall, glColor4i, "glColor4i");
   pragma Import (Stdcall, glColor4s, "glColor4s");
   pragma Import (Stdcall, glColor4ub, "glColor4ub");
   pragma Import (Stdcall, glColor4ui, "glColor4ui");
   pragma Import (Stdcall, glColor4us, "glColor4us");
   pragma Import (Stdcall, glColor3bv, "glColor3bv");
   pragma Import (Stdcall, glColor3dv, "glColor3dv");
   pragma Import (Stdcall, glColor3fv, "glColor3fv");
   pragma Import (Stdcall, glColor3iv, "glColor3iv");
   pragma Import (Stdcall, glColor3sv, "glColor3sv");
   pragma Import (Stdcall, glColor3ubv, "glColor3ubv");
   pragma Import (Stdcall, glColor3uiv, "glColor3uiv");
   pragma Import (Stdcall, glColor3usv, "glColor3usv");
   pragma Import (Stdcall, glColor4bv, "glColor4bv");
   pragma Import (Stdcall, glColor4dv, "glColor4dv");
   pragma Import (Stdcall, glColor4fv, "glColor4fv");
   pragma Import (Stdcall, glColor4iv, "glColor4iv");
   pragma Import (Stdcall, glColor4sv, "glColor4sv");
   pragma Import (Stdcall, glColor4ubv, "glColor4ubv");
   pragma Import (Stdcall, glColor4uiv, "glColor4uiv");
   pragma Import (Stdcall, glColor4usv, "glColor4usv");
   pragma Import (Stdcall, glTexCoord1d, "glTexCoord1d");
   pragma Import (Stdcall, glTexCoord1f, "glTexCoord1f");
   pragma Import (Stdcall, glTexCoord1i, "glTexCoord1i");
   pragma Import (Stdcall, glTexCoord1s, "glTexCoord1s");
   pragma Import (Stdcall, glTexCoord2d, "glTexCoord2d");
   pragma Import (Stdcall, glTexCoord2f, "glTexCoord2f");
   pragma Import (Stdcall, glTexCoord2i, "glTexCoord2i");
   pragma Import (Stdcall, glTexCoord2s, "glTexCoord2s");
   pragma Import (Stdcall, glTexCoord3d, "glTexCoord3d");
   pragma Import (Stdcall, glTexCoord3f, "glTexCoord3f");
   pragma Import (Stdcall, glTexCoord3i, "glTexCoord3i");
   pragma Import (Stdcall, glTexCoord3s, "glTexCoord3s");
   pragma Import (Stdcall, glTexCoord4d, "glTexCoord4d");
   pragma Import (Stdcall, glTexCoord4f, "glTexCoord4f");
   pragma Import (Stdcall, glTexCoord4i, "glTexCoord4i");
   pragma Import (Stdcall, glTexCoord4s, "glTexCoord4s");
   pragma Import (Stdcall, glTexCoord1dv, "glTexCoord1dv");
   pragma Import (Stdcall, glTexCoord1fv, "glTexCoord1fv");
   pragma Import (Stdcall, glTexCoord1iv, "glTexCoord1iv");
   pragma Import (Stdcall, glTexCoord1sv, "glTexCoord1sv");
   pragma Import (Stdcall, glTexCoord2dv, "glTexCoord2dv");
   pragma Import (Stdcall, glTexCoord2fv, "glTexCoord2fv");
   pragma Import (Stdcall, glTexCoord2iv, "glTexCoord2iv");
   pragma Import (Stdcall, glTexCoord2sv, "glTexCoord2sv");
   pragma Import (Stdcall, glTexCoord3dv, "glTexCoord3dv");
   pragma Import (Stdcall, glTexCoord3fv, "glTexCoord3fv");
   pragma Import (Stdcall, glTexCoord3iv, "glTexCoord3iv");
   pragma Import (Stdcall, glTexCoord3sv, "glTexCoord3sv");
   pragma Import (Stdcall, glTexCoord4dv, "glTexCoord4dv");
   pragma Import (Stdcall, glTexCoord4fv, "glTexCoord4fv");
   pragma Import (Stdcall, glTexCoord4iv, "glTexCoord4iv");
   pragma Import (Stdcall, glTexCoord4sv, "glTexCoord4sv");
   pragma Import (Stdcall, glRasterPos2d, "glRasterPos2d");
   pragma Import (Stdcall, glRasterPos2f, "glRasterPos2f");
   pragma Import (Stdcall, glRasterPos2i, "glRasterPos2i");
   pragma Import (Stdcall, glRasterPos2s, "glRasterPos2s");
   pragma Import (Stdcall, glRasterPos3d, "glRasterPos3d");
   pragma Import (Stdcall, glRasterPos3f, "glRasterPos3f");
   pragma Import (Stdcall, glRasterPos3i, "glRasterPos3i");
   pragma Import (Stdcall, glRasterPos3s, "glRasterPos3s");
   pragma Import (Stdcall, glRasterPos4d, "glRasterPos4d");
   pragma Import (Stdcall, glRasterPos4f, "glRasterPos4f");
   pragma Import (Stdcall, glRasterPos4i, "glRasterPos4i");
   pragma Import (Stdcall, glRasterPos4s, "glRasterPos4s");
   pragma Import (Stdcall, glRasterPos2dv, "glRasterPos2dv");
   pragma Import (Stdcall, glRasterPos2fv, "glRasterPos2fv");
   pragma Import (Stdcall, glRasterPos2iv, "glRasterPos2iv");
   pragma Import (Stdcall, glRasterPos2sv, "glRasterPos2sv");
   pragma Import (Stdcall, glRasterPos3dv, "glRasterPos3dv");
   pragma Import (Stdcall, glRasterPos3fv, "glRasterPos3fv");
   pragma Import (Stdcall, glRasterPos3iv, "glRasterPos3iv");
   pragma Import (Stdcall, glRasterPos3sv, "glRasterPos3sv");
   pragma Import (Stdcall, glRasterPos4dv, "glRasterPos4dv");
   pragma Import (Stdcall, glRasterPos4fv, "glRasterPos4fv");
   pragma Import (Stdcall, glRasterPos4iv, "glRasterPos4iv");
   pragma Import (Stdcall, glRasterPos4sv, "glRasterPos4sv");
   pragma Import (Stdcall, glRectd, "glRectd");
   pragma Import (Stdcall, glRectf, "glRectf");
   pragma Import (Stdcall, glRecti, "glRecti");
   pragma Import (Stdcall, glRects, "glRects");
   pragma Import (Stdcall, glRectdv, "glRectdv");
   pragma Import (Stdcall, glRectfv, "glRectfv");
   pragma Import (Stdcall, glRectiv, "glRectiv");
   pragma Import (Stdcall, glRectsv, "glRectsv");
   pragma Import (Stdcall, glVertexPointer, "glVertexPointer");
   pragma Import (Stdcall, glNormalPointer, "glNormalPointer");
   pragma Import (Stdcall, glColorPointer, "glColorPointer");
   pragma Import (Stdcall, glIndexPointer, "glIndexPointer");
   pragma Import (Stdcall, glTexCoordPointer, "glTexCoordPointer");
   pragma Import (Stdcall, glEdgeFlagPointer, "glEdgeFlagPointer");
   pragma Import (Stdcall, glGetPointerv, "glGetPointerv");
   pragma Import (Stdcall, glArrayElement, "glArrayElement");
   pragma Import (Stdcall, glDrawArrays, "glDrawArrays");
   pragma Import (Stdcall, glDrawElements, "glDrawElements");
   pragma Import (Stdcall, glInterleavedArrays, "glInterleavedArrays");
   pragma Import (Stdcall, glShadeModel, "glShadeModel");
   pragma Import (Stdcall, glLightf, "glLightf");
   pragma Import (Stdcall, glLighti, "glLighti");
   pragma Import (Stdcall, glLightiv, "glLightiv");
   pragma Import (Stdcall, glGetLightfv, "glGetLightfv");
   pragma Import (Stdcall, glGetLightiv, "glGetLightiv");
   pragma Import (Stdcall, glLightModelf, "glLightModelf");
   pragma Import (Stdcall, glLightModeli, "glLightModeli");
   pragma Import (Stdcall, glLightModelfv, "glLightModelfv");
   pragma Import (Stdcall, glLightModeliv, "glLightModeliv");
   pragma Import (Stdcall, glMaterialf, "glMaterialf");
   pragma Import (Stdcall, glMateriali, "glMateriali");
   pragma Import (Stdcall, glMaterialiv, "glMaterialiv");
   pragma Import (Stdcall, glGetMaterialfv, "glGetMaterialfv");
   pragma Import (Stdcall, glGetMaterialiv, "glGetMaterialiv");
   pragma Import (Stdcall, glColorMaterial, "glColorMaterial");
   pragma Import (Stdcall, glPixelZoom, "glPixelZoom");
   pragma Import (Stdcall, glPixelStoref, "glPixelStoref");
   pragma Import (Stdcall, glPixelStorei, "glPixelStorei");
   pragma Import (Stdcall, glPixelTransferf, "glPixelTransferf");
   pragma Import (Stdcall, glPixelTransferi, "glPixelTransferi");
   pragma Import (Stdcall, glPixelMapfv, "glPixelMapfv");
   pragma Import (Stdcall, glPixelMapuiv, "glPixelMapuiv");
   pragma Import (Stdcall, glPixelMapusv, "glPixelMapusv");
   pragma Import (Stdcall, glGetPixelMapfv, "glGetPixelMapfv");
   pragma Import (Stdcall, glGetPixelMapuiv, "glGetPixelMapuiv");
   pragma Import (Stdcall, glGetPixelMapusv, "glGetPixelMapusv");
   pragma Import (Stdcall, glBitmap, "glBitmap");
   pragma Import (Stdcall, glReadPixels, "glReadPixels");
   pragma Import (Stdcall, glDrawPixels, "glDrawPixels");
   pragma Import (Stdcall, glCopyPixels, "glCopyPixels");
   pragma Import (Stdcall, glStencilFunc, "glStencilFunc");
   pragma Import (Stdcall, glStencilMask, "glStencilMask");
   pragma Import (Stdcall, glStencilOp, "glStencilOp");
   pragma Import (Stdcall, glClearStencil, "glClearStencil");
   pragma Import (Stdcall, glTexGend, "glTexGend");
   pragma Import (Stdcall, glTexGenf, "glTexGenf");
   pragma Import (Stdcall, glTexGeni, "glTexGeni");
   pragma Import (Stdcall, glTexGendv, "glTexGendv");
   pragma Import (Stdcall, glTexGenfv, "glTexGenfv");
   pragma Import (Stdcall, glTexGeniv, "glTexGeniv");
   pragma Import (Stdcall, glGetTexGendv, "glGetTexGendv");
   pragma Import (Stdcall, glGetTexGenfv, "glGetTexGenfv");
   pragma Import (Stdcall, glGetTexGeniv, "glGetTexGeniv");
   pragma Import (Stdcall, glTexEnvf, "glTexEnvf");
   pragma Import (Stdcall, glTexEnvi, "glTexEnvi");
   pragma Import (Stdcall, glTexEnvfv, "glTexEnvfv");
   pragma Import (Stdcall, glTexEnviv, "glTexEnviv");
   pragma Import (Stdcall, glGetTexEnvfv, "glGetTexEnvfv");
   pragma Import (Stdcall, glGetTexEnviv, "glGetTexEnviv");
   pragma Import (Stdcall, glTexParameterf, "glTexParameterf");
   pragma Import (Stdcall, glTexParameteri, "glTexParameteri");
   pragma Import (Stdcall, glTexParameterfv, "glTexParameterfv");
   pragma Import (Stdcall, glTexParameteriv, "glTexParameteriv");
   pragma Import (Stdcall, glGetTexParameterfv, "glGetTexParameterfv");
   pragma Import (Stdcall, glGetTexParameteriv, "glGetTexParameteriv");
   pragma Import (Stdcall,
                  glGetTexLevelParameterfv,
                  "glGetTexLevelParameterfv");
   pragma Import (Stdcall,
                  glGetTexLevelParameteriv,
                  "glGetTexLevelParameteriv");
   pragma Import (Stdcall, glTexImage1D, "glTexImage1D");
   pragma Import (Stdcall, glTexImage2D, "glTexImage2D");
   pragma Import (Stdcall, glGetTexImage, "glGetTexImage");
   pragma Import (Stdcall, glDeleteTextures, "glDeleteTextures");
   pragma Import (Stdcall, glBindTexture, "glBindTexture");
   pragma Import (Stdcall, glPrioritizeTextures, "glPrioritizeTextures");
   pragma Import (Stdcall, glAreTexturesResident, "glAreTexturesResident");
   pragma Import (Stdcall, glIsTexture, "glIsTexture");
   pragma Import (Stdcall, glTexSubImage1D, "glTexSubImage1D");
   pragma Import (Stdcall, glTexSubImage2D, "glTexSubImage2D");
   pragma Import (Stdcall, glCopyTexImage1D, "glCopyTexImage1D");
   pragma Import (Stdcall, glCopyTexImage2D, "glCopyTexImage2D");
   pragma Import (Stdcall, glCopyTexSubImage1D, "glCopyTexSubImage1D");
   pragma Import (Stdcall, glCopyTexSubImage2D, "glCopyTexSubImage2D");
   pragma Import (Stdcall, glMap1d, "glMap1d");
   pragma Import (Stdcall, glMap1f, "glMap1f");
   pragma Import (Stdcall, glMap2d, "glMap2d");
   pragma Import (Stdcall, glMap2f, "glMap2f");
   pragma Import (Stdcall, glGetMapdv, "glGetMapdv");
   pragma Import (Stdcall, glGetMapfv, "glGetMapfv");
   pragma Import (Stdcall, glGetMapiv, "glGetMapiv");
   pragma Import (Stdcall, glEvalCoord1d, "glEvalCoord1d");
   pragma Import (Stdcall, glEvalCoord1f, "glEvalCoord1f");
   pragma Import (Stdcall, glEvalCoord1dv, "glEvalCoord1dv");
   pragma Import (Stdcall, glEvalCoord1fv, "glEvalCoord1fv");
   pragma Import (Stdcall, glEvalCoord2d, "glEvalCoord2d");
   pragma Import (Stdcall, glEvalCoord2f, "glEvalCoord2f");
   pragma Import (Stdcall, glEvalCoord2dv, "glEvalCoord2dv");
   pragma Import (Stdcall, glEvalCoord2fv, "glEvalCoord2fv");
   pragma Import (Stdcall, glMapGrid1d, "glMapGrid1d");
   pragma Import (Stdcall, glMapGrid1f, "glMapGrid1f");
   pragma Import (Stdcall, glMapGrid2d, "glMapGrid2d");
   pragma Import (Stdcall, glMapGrid2f, "glMapGrid2f");
   pragma Import (Stdcall, glEvalPoint1, "glEvalPoint1");
   pragma Import (Stdcall, glEvalPoint2, "glEvalPoint2");
   pragma Import (Stdcall, glEvalMesh1, "glEvalMesh1");
   pragma Import (Stdcall, glEvalMesh2, "glEvalMesh2");
   pragma Import (Stdcall, glFogf, "glFogf");
   pragma Import (Stdcall, glFogi, "glFogi");
   pragma Import (Stdcall, glFogiv, "glFogiv");
   pragma Import (Stdcall, glFeedbackBuffer, "glFeedbackBuffer");
   pragma Import (Stdcall, glPassThrough, "glPassThrough");
   pragma Import (Stdcall, glSelectBuffer, "glSelectBuffer");
   pragma Import (Stdcall, glInitNames, "glInitNames");
   pragma Import (Stdcall, glLoadName, "glLoadName");
   pragma Import (Stdcall, glPushName, "glPushName");
   pragma Import (Stdcall, glPopName, "glPopName");
   pragma Import (Stdcall, glBlendEquationEXT, "glBlendEquationEXT");
   pragma Import (Stdcall, glBlendColorEXT, "glBlendColorEXT");
   pragma Import (Stdcall, glPolygonOffsetEXT, "glPolygonOffsetEXT");
   pragma Import (Stdcall, glVertexPointerEXT, "glVertexPointerEXT");
   pragma Import (Stdcall, glNormalPointerEXT, "glNormalPointerEXT");
   pragma Import (Stdcall, glColorPointerEXT, "glColorPointerEXT");
   pragma Import (Stdcall, glIndexPointerEXT, "glIndexPointerEXT");
   pragma Import (Stdcall, glTexCoordPointerEXT, "glTexCoordPointerEXT");
   pragma Import (Stdcall, glEdgeFlagPointerEXT, "glEdgeFlagPointerEXT");
   pragma Import (Stdcall, glGetPointervEXT, "glGetPointervEXT");
   pragma Import (Stdcall, glArrayElementEXT, "glArrayElementEXT");
   pragma Import (Stdcall, glDrawArraysEXT, "glDrawArraysEXT");
   pragma Import (Stdcall, glGenTexturesEXT, "glGenTexturesEXT");
   pragma Import (Stdcall, glDeleteTexturesEXT, "glDeleteTexturesEXT");
   pragma Import (Stdcall, glBindTextureEXT, "glBindTextureEXT");
   pragma Import (Stdcall, glPrioritizeTexturesEXT, "glPrioritizeTexturesEXT");
   pragma Import (Stdcall,
                  glAreTexturesResidentEXT,
                  "glAreTexturesResidentEXT");
   pragma Import (Stdcall, glIsTextureEXT, "glIsTextureEXT");
   pragma Import (Stdcall, glTexImage3DEXT, "glTexImage3DEXT");
   pragma Import (Stdcall, glTexSubImage3DEXT, "glTexSubImage3DEXT");
   pragma Import (Stdcall, glCopyTexSubImage3DEXT, "glCopyTexSubImage3DEXT");
   pragma Import (Stdcall, glColorTableEXT, "glColorTableEXT");
   pragma Import (Stdcall, glColorSubTableEXT, "glColorSubTableEXT");
   pragma Import (Stdcall, glGetColorTableEXT, "glGetColorTableEXT");
   pragma Import (Stdcall,
                  glGetColorTableParameterfvEXT,
                  "glGetColorTableParameterfvEXT");
   pragma Import (Stdcall,
                  glGetColorTableParameterivEXT,
                  "glGetColorTableParameterivEXT");
   pragma Import (Stdcall, glMultiTexCoord1dSGIS, "glMultiTexCoord1dSGIS");
   pragma Import (Stdcall, glMultiTexCoord1dvSGIS, "glMultiTexCoord1dvSGIS");
   pragma Import (Stdcall, glMultiTexCoord1fSGIS, "glMultiTexCoord1fSGIS");
   pragma Import (Stdcall, glMultiTexCoord1fvSGIS, "glMultiTexCoord1fvSGIS");
   pragma Import (Stdcall, glMultiTexCoord1iSGIS, "glMultiTexCoord1iSGIS");
   pragma Import (Stdcall, glMultiTexCoord1ivSGIS, "glMultiTexCoord1ivSGIS");
   pragma Import (Stdcall, glMultiTexCoord1sSGIS, "glMultiTexCoord1sSGIS");
   pragma Import (Stdcall, glMultiTexCoord1svSGIS, "glMultiTexCoord1svSGIS");
   pragma Import (Stdcall, glMultiTexCoord2dSGIS, "glMultiTexCoord2dSGIS");
   pragma Import (Stdcall, glMultiTexCoord2dvSGIS, "glMultiTexCoord2dvSGIS");
   pragma Import (Stdcall, glMultiTexCoord2fSGIS, "glMultiTexCoord2fSGIS");
   pragma Import (Stdcall, glMultiTexCoord2fvSGIS, "glMultiTexCoord2fvSGIS");
   pragma Import (Stdcall, glMultiTexCoord2iSGIS, "glMultiTexCoord2iSGIS");
   pragma Import (Stdcall, glMultiTexCoord2ivSGIS, "glMultiTexCoord2ivSGIS");
   pragma Import (Stdcall, glMultiTexCoord2sSGIS, "glMultiTexCoord2sSGIS");
   pragma Import (Stdcall, glMultiTexCoord2svSGIS, "glMultiTexCoord2svSGIS");
   pragma Import (Stdcall, glMultiTexCoord3dSGIS, "glMultiTexCoord3dSGIS");
   pragma Import (Stdcall, glMultiTexCoord3dvSGIS, "glMultiTexCoord3dvSGIS");
   pragma Import (Stdcall, glMultiTexCoord3fSGIS, "glMultiTexCoord3fSGIS");
   pragma Import (Stdcall, glMultiTexCoord3fvSGIS, "glMultiTexCoord3fvSGIS");
   pragma Import (Stdcall, glMultiTexCoord3iSGIS, "glMultiTexCoord3iSGIS");
   pragma Import (Stdcall, glMultiTexCoord3ivSGIS, "glMultiTexCoord3ivSGIS");
   pragma Import (Stdcall, glMultiTexCoord3sSGIS, "glMultiTexCoord3sSGIS");
   pragma Import (Stdcall, glMultiTexCoord3svSGIS, "glMultiTexCoord3svSGIS");
   pragma Import (Stdcall, glMultiTexCoord4dSGIS, "glMultiTexCoord4dSGIS");
   pragma Import (Stdcall, glMultiTexCoord4dvSGIS, "glMultiTexCoord4dvSGIS");
   pragma Import (Stdcall, glMultiTexCoord4fSGIS, "glMultiTexCoord4fSGIS");
   pragma Import (Stdcall, glMultiTexCoord4fvSGIS, "glMultiTexCoord4fvSGIS");
   pragma Import (Stdcall, glMultiTexCoord4iSGIS, "glMultiTexCoord4iSGIS");
   pragma Import (Stdcall, glMultiTexCoord4ivSGIS, "glMultiTexCoord4ivSGIS");
   pragma Import (Stdcall, glMultiTexCoord4sSGIS, "glMultiTexCoord4sSGIS");
   pragma Import (Stdcall, glMultiTexCoord4svSGIS, "glMultiTexCoord4svSGIS");
   pragma Import (Stdcall,
                  glMultiTexCoordPointerSGIS,
                  "glMultiTexCoordPointerSGIS");
   pragma Import (Stdcall, glSelectTextureSGIS, "glSelectTextureSGIS");
   pragma Import (Stdcall,
                  glSelectTextureCoordSetSGIS,
                  "glSelectTextureCoordSetSGIS");
   pragma Import (Stdcall, glMultiTexCoord1dEXT, "glMultiTexCoord1dEXT");
   pragma Import (Stdcall, glMultiTexCoord1dvEXT, "glMultiTexCoord1dvEXT");
   pragma Import (Stdcall, glMultiTexCoord1fEXT, "glMultiTexCoord1fEXT");
   pragma Import (Stdcall, glMultiTexCoord1fvEXT, "glMultiTexCoord1fvEXT");
   pragma Import (Stdcall, glMultiTexCoord1iEXT, "glMultiTexCoord1iEXT");
   pragma Import (Stdcall, glMultiTexCoord1ivEXT, "glMultiTexCoord1ivEXT");
   pragma Import (Stdcall, glMultiTexCoord1sEXT, "glMultiTexCoord1sEXT");
   pragma Import (Stdcall, glMultiTexCoord1svEXT, "glMultiTexCoord1svEXT");
   pragma Import (Stdcall, glMultiTexCoord2dEXT, "glMultiTexCoord2dEXT");
   pragma Import (Stdcall, glMultiTexCoord2dvEXT, "glMultiTexCoord2dvEXT");
   pragma Import (Stdcall, glMultiTexCoord2fEXT, "glMultiTexCoord2fEXT");
   pragma Import (Stdcall, glMultiTexCoord2fvEXT, "glMultiTexCoord2fvEXT");
   pragma Import (Stdcall, glMultiTexCoord2iEXT, "glMultiTexCoord2iEXT");
   pragma Import (Stdcall, glMultiTexCoord2ivEXT, "glMultiTexCoord2ivEXT");
   pragma Import (Stdcall, glMultiTexCoord2sEXT, "glMultiTexCoord2sEXT");
   pragma Import (Stdcall, glMultiTexCoord2svEXT, "glMultiTexCoord2svEXT");
   pragma Import (Stdcall, glMultiTexCoord3dEXT, "glMultiTexCoord3dEXT");
   pragma Import (Stdcall, glMultiTexCoord3dvEXT, "glMultiTexCoord3dvEXT");
   pragma Import (Stdcall, glMultiTexCoord3fEXT, "glMultiTexCoord3fEXT");
   pragma Import (Stdcall, glMultiTexCoord3fvEXT, "glMultiTexCoord3fvEXT");
   pragma Import (Stdcall, glMultiTexCoord3iEXT, "glMultiTexCoord3iEXT");
   pragma Import (Stdcall, glMultiTexCoord3ivEXT, "glMultiTexCoord3ivEXT");
   pragma Import (Stdcall, glMultiTexCoord3sEXT, "glMultiTexCoord3sEXT");
   pragma Import (Stdcall, glMultiTexCoord3svEXT, "glMultiTexCoord3svEXT");
   pragma Import (Stdcall, glMultiTexCoord4dEXT, "glMultiTexCoord4dEXT");
   pragma Import (Stdcall, glMultiTexCoord4dvEXT, "glMultiTexCoord4dvEXT");
   pragma Import (Stdcall, glMultiTexCoord4fEXT, "glMultiTexCoord4fEXT");
   pragma Import (Stdcall, glMultiTexCoord4fvEXT, "glMultiTexCoord4fvEXT");
   pragma Import (Stdcall, glMultiTexCoord4iEXT, "glMultiTexCoord4iEXT");
   pragma Import (Stdcall, glMultiTexCoord4ivEXT, "glMultiTexCoord4ivEXT");
   pragma Import (Stdcall, glMultiTexCoord4sEXT, "glMultiTexCoord4sEXT");
   pragma Import (Stdcall, glMultiTexCoord4svEXT, "glMultiTexCoord4svEXT");
   pragma Import (Stdcall,
                  glInterleavedTextureCoordSetsEXT,
                  "glInterleavedTextureCoordSetsEXT");
   pragma Import (Stdcall, glSelectTextureEXT, "glSelectTextureEXT");
   pragma Import (Stdcall,
                  glSelectTextureCoordSetEXT,
                  "glSelectTextureCoordSetEXT");
   pragma Import (Stdcall,
                  glSelectTextureTransformEXT,
                  "glSelectTextureTransformEXT");
   pragma Import (Stdcall, glPointParameterfEXT, "glPointParameterfEXT");
   pragma Import (Stdcall, glPointParameterfvEXT, "glPointParameterfvEXT");
   pragma Import (Stdcall, glWindowPos2iMESA, "glWindowPos2iMESA");
   pragma Import (Stdcall, glWindowPos2sMESA, "glWindowPos2sMESA");
   pragma Import (Stdcall, glWindowPos2fMESA, "glWindowPos2fMESA");
   pragma Import (Stdcall, glWindowPos2dMESA, "glWindowPos2dMESA");
   pragma Import (Stdcall, glWindowPos2ivMESA, "glWindowPos2ivMESA");
   pragma Import (Stdcall, glWindowPos2svMESA, "glWindowPos2svMESA");
   pragma Import (Stdcall, glWindowPos2fvMESA, "glWindowPos2fvMESA");
   pragma Import (Stdcall, glWindowPos2dvMESA, "glWindowPos2dvMESA");
   pragma Import (Stdcall, glWindowPos3iMESA, "glWindowPos3iMESA");
   pragma Import (Stdcall, glWindowPos3sMESA, "glWindowPos3sMESA");
   pragma Import (Stdcall, glWindowPos3fMESA, "glWindowPos3fMESA");
   pragma Import (Stdcall, glWindowPos3dMESA, "glWindowPos3dMESA");
   pragma Import (Stdcall, glWindowPos3ivMESA, "glWindowPos3ivMESA");
   pragma Import (Stdcall, glWindowPos3svMESA, "glWindowPos3svMESA");
   pragma Import (Stdcall, glWindowPos3fvMESA, "glWindowPos3fvMESA");
   pragma Import (Stdcall, glWindowPos3dvMESA, "glWindowPos3dvMESA");
   pragma Import (Stdcall, glWindowPos4iMESA, "glWindowPos4iMESA");
   pragma Import (Stdcall, glWindowPos4sMESA, "glWindowPos4sMESA");
   pragma Import (Stdcall, glWindowPos4fMESA, "glWindowPos4fMESA");
   pragma Import (Stdcall, glWindowPos4dMESA, "glWindowPos4dMESA");
   pragma Import (Stdcall, glWindowPos4ivMESA, "glWindowPos4ivMESA");
   pragma Import (Stdcall, glWindowPos4svMESA, "glWindowPos4svMESA");
   pragma Import (Stdcall, glWindowPos4fvMESA, "glWindowPos4fvMESA");
   pragma Import (Stdcall, glWindowPos4dvMESA, "glWindowPos4dvMESA");
   pragma Import (Stdcall, glResizeBuffersMESA, "glResizeBuffersMESA");
   pragma Import (Stdcall, glDrawRangeElements, "glDrawRangeElements");
   pragma Import (Stdcall, glTexImage3D, "glTexImage3D");
   pragma Import (Stdcall, glTexSubImage3D, "glTexSubImage3D");
   pragma Import (Stdcall, glCopyTexSubImage3D, "glCopyTexSubImage3D");

end gl_h;
