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
package Neo.System.Graphics
  is
  ----------------
  -- Exceptions --
  ----------------
    Invalid_Enumeration : Exception;
    Invalid_Value       : Exception;
    Stack_Overflow      : Exception;
    Stack_Underflow     : Exception;
    Out_Of_Memory       : Exception;
    Unsupported         : Exception;
  ---------------
  -- Constants --
  ---------------
    VARIABLE_PREFIX : constant String_2 := "r_";
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_API
      is(
      OpenGL_API,
      --
      -- 1992 Silicon Graphics
      --      http://web.archive.org/web/20130722222817/http://www.opengl.org/registry/doc/glspec43.core.20130214.pdf
      --
      Direct3D_API,
      --
      -- 1995 Microsoft
      --      http://web.archive.org/web/20120819174252/http://msdn.microsoft.com/en-US/library/hh309466(v=vs.85).aspx
      --
      GCM_API);
      --
      -- 2006 Sony
      --      http://web.archive.org/web/20130715050830/http://research.ncl.ac.uk/game/mastersdegree/workshops/ps3introductiontogcm/
      --
    type Enumerated_Shading_Language
      is(
      Architecture_Review_Board_Language,
      --
      -- 2002 OpenGL Architecture Review Board
      --      http://web.archive.org/web/20120802192218/http://www.renderguild.com/gpuguide.pdf
      --      http://web.archive.org/web/20130512010221/http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_program.txt
      --
      C_For_Graphics_Language,
      --
      -- 2002 Nvidia
      --      http://web.archive.org/web/20040720052349/http://download.nvidia.com/developer/cg/Cg_Specification.pdf
      --
      High_Level_Shading_Language,
      --
      -- 2003 Microsoft
      --      http://web.archive.org/web/20130701061644/http://msdn.microsoft.com/en-us/library/windows/desktop/bb509638(v=vs.85).aspx
      --
      OpenGL_Shading_Language,
      --
      -- 2004 OpenGL Architecture Review Board
      --      http://web.archive.org/web/20130807130424/http://www.opengl.org/registry/doc/GLSLangSpec.4.40.diff.pdf
      --
      Playstation_Shading_Languge);
      --
      -- ???? Sony
      --      http://web.archive.org/web/20130827033837/http://www.examiner.com/article/here-are-most-of-the-technical-specifications-for-the-playstation-4
      --
    type Enumerated_Stereo_Depth
      is(
      No_Depth,
      Near_Depth,
      Middle_Depth,
      Far_Depth);
    type Enumerated_Stereo_3D
      is(
      No_Stereo_3D,
      Side_By_Side_Stereo_3D,
      Side_By_Side_Compressed_Stereo_3D,
      Top_And_Bottom_Compressed_Stereo_3D,
      Interlaced_Stereo_3D,
      Quad_Buffer_Stereo_3D,
      HDMI_720_Stereo_3D);
    type Enumerated_Blend
      is(
      One_Blend,
      Zero_Blend,
      Destination_Color_Blend,
      One_Minus_Destination_Color_Blend,
      Source_Alpha_Blend,
      One_Minus_Source_Alpha_Blend,
      Destination_Alpha_Blend,
      One_Minus_Destination_Alpha_Blend);
    type Enumerated_Blend_Operation
      is(
      Add_Blend_Operation,
      Subtract_Blend_Operation,
      Minimum_Blend_Operation,
      Maximum_Blend_Operation,
      Bits_Blend_Operation);
    type Enumerated_Stencil
      is(
      Reference_Shift_Stencil,
      Reference_Bits_Stencil,
      Mask_Shift_Stencil,
      Mask_Bits_Stencil);
    type Enumerated_Stencil_Operation
      is(
      Keep_Stencil_Operation,
      Zero_Stencil_Operation,
      Replace_Stencil_Operation,
      Increment_Stencil_Operation,
      Decrement_Stencil_Operation,
      Invert_Stencil_Operation,
      Increment_Wrap_Stencil_Operation,
      Decrement_Wrap_Stencil_Operation,
      Bits_Stencil_Operation);
    type Enumerated_Stencil
      is(
      Always_Stencil,
      Less_Stencil,
      Less_Than_Or_Equal_To_Stencil,
      Greater_Than_Stencil,
      Equal_Stencil,
      Not_Equal_Stencil,
      Never_Stencil);
    type Enumerated_Depth
      is(
      Less_Depth,
      Always_Depth,
      Greater_Depth,
      Equal_Depth,
      Bits_Depth);
  -------------
  -- Records --
  -------------
    type Record_Specifics(
      API : Enumerated_API := OpenGL_API)
      is record
        Shading_Language                 : Enumerated_Shading_Language := OpenGL_Shading_Language;
        Version                          : Float_4_Real                := 0.0;
        Frequency                        : Integer_4_Natural           := 0;
        Maximum_Texture_Size             : Integer_4_Natural           := 0;
        Has_Multitexture                 : Boolean                     := False;
        Has_Direct_State_Access          : Boolean                     := False;
        Has_Texture_Compression          : Boolean                     := False;
        Has_Anisotropic_Filter           : Boolean                     := False;
        Has_Texture_Level_Of_Detail_Bias : Boolean                     := False;
        Has_Seamless_Cube_Map            : Boolean                     := False;
        Has_RGB_Color_Framebuffer        : Boolean                     := False;
        Has_Vertex_Buffer_Object         : Boolean                     := False;
        Has_Map_Buffer_Range             : Boolean                     := False;
        Has_Vertex_Array_Object          : Boolean                     := False;
        Has_Draw_Elements_Base_Vertex    : Boolean                     := False;
        Has_Uniform_Buffer               : Boolean                     := False;
        Has_Two_Sided_Stencil            : Boolean                     := False;
        Has_Depth_Bounds_Test            : Boolean                     := False;
        Has_Sync                         : Boolean                     := False;
        Has_Timer_Query                  : Boolean                     := False;
        Has_Occlusion_Query              : Boolean                     := False;
        Has_Swap_Control_Tear            : Boolean                     := False;
        Has_Stereo_Pixel_Format          : Boolean                     := False;
        case API is
          when Direct3D_API => null;
          when GCM_API      => null;
          when OpenGL_API   => null;
        end case;
      end record;
    type Record_Texture(
      Path   : String_2;
      Width  : Integer_4_Positive := 1;
      Height : Integer_4_Positive := 1)
      is new Ada.Finalization.Controlled
      with private;
    type Record_Texture(
      Path   : String_2;
      Width  : Integer_4_Positive := 1;
      Height : Integer_4_Positive := 1)
      is new Ada.Finalization.Controlled
      with private;
  -----------------
  -- Subprograms --
  -----------------
    function Get_Specifics
      return Record_Specifics;
    procedure Check_Exceptions;
    procedure Begin_Depth_Pass(
      Screen : in Record_Rectangle);
    procedure End_Depth_Pass;
    function Get_Depth_Pass_Rectangle(
    procedure Select_Texture(
      Texture : in Record_Texture);
    procedure Color(
      Red   : in Float_4_Real;
      Green : in Float_4_Real;
      Blue  : in Float_4_Real;
      Alpha : in Float_4_Real := 1.0);
    procedure Depth_Bounds_Test(
      Z_Minimum : in Float_4_Real;
      Z_Maximum : in Float_4_Real);
      with Pre => Z_Minimum < Z_Maximum;
    procedure Polygon_Offset(
      Scale : in Float_4_Real;
      Bias  : in Float_4_Real);
    procedure Clear(
      Do_Clear_Color : in Boolean;
      Do_Clear_Depth : in Boolean;
      Stencil_Value  : in Integer_1_Unsigned;
      Red            : in Float_4_Real;
      Green          : in Float_4_Real;
      Blue           : in Float_4_Real;
      Alpha          : in Float_4_Real);
    procedure Viewport(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed);
    procedure Clear(
      Do_Clear_Color : in Boolean;
      Do_Clear_Color : in Boolean;
      Do_Clear_Color : in Boolean;
      Stencil_Value  : in Integer_1_Unsigned;
      Pixel          : in Record_);
    function Get_Depth_Pass
      return Record_Rectangle;
    procedure Cull(
      Kind : in Enumerated_Cull);
    procedure Scissor(
      Rectangle : in Record_Rectangle);
    procedure Upload_Subimage(
      int mipLevel,
      int x,
      int y,
      int z,
      int width,
      int height,
      const void * pic,
      int pixelPitch)
      assert( x >= 0 && y >= 0 && mipLevel >= 0 && width >= 0 && height >= 0 && mipLevel < opts.numLevels );
  --------------
  -- Packages --
  --------------
    package API : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "api"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package  : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warp"),
      Description => To_String_2_Unbounded("use the optical warping renderprog instead of stereoDeGhost: " & Item_Boolean.Get_Possible_Values),"0"
    package Stereo_warpStrength : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpStrength"),
      Description => To_String_2_Unbounded("amount of pre-distortion: " & Item_Boolean.Get_Possible_Values),"1.45"
    package Stereo_Warp_X_Center : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpCenterX"),
      Description => To_String_2_Unbounded("center for left eye, right eye will be 1.0: " & Item_Boolean.Get_Possible_Values), "0.5"
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpCenterY"),
      Description => To_String_2_Unbounded("center for both eyes: " & Item_Boolean.Get_Possible_Values), "0.5"
    package Stereo_Warp_Z : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpParmZ"),
      Description => To_String_2_Unbounded("development parm: " & Item_Boolean.Get_Possible_Values), "0"
    package Stereo_Warp_W : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpParmW"),
      Description => To_String_2_Unbounded("development parm: " & Item_Boolean.Get_Possible_Values), "0"
    package Stereo_Warp_Target_Fraction : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "warpTargetFraction"),
      Description => To_String_2_Unbounded("fraction of half-width the through-lens view covers: " & Item_Boolean.Get_Possible_Values), "1.0"
    package Do_Sync_Frames : Item_.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "syncframes"),
      Description => To_String_2_Unbounded("Don't let the GPU buffer execution past swapbuffers: " & Item_Boolean.Get_Possible_Values),
      Initial     => True);
    package Stereo_3D : Item_Stereo_3D.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Polygon_Offset_Factor : Item_Real.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Polygon_Offset_Units : Item_Real.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Texture_Anisotropy : Item_Real.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Texture_Level_Of_Detail_Bias : Item_Real.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Texture_Minimum_Filter : Item_Integer.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Texture_Maximum_Filter : Item_Integer.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Texture_MIP_Filter : Item_Integer.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Disable_State_Caching : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Lazy_Bind_Programs : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Lazy_Bind_Parameters : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Lazy_Bind_Textures : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Strip_Fragment_Branches : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Skip_Detail_Triangles : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Do_Use_Single_Trinagle : Item_Boolean.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
    package Stereo_Depth : Item_Stereo_Depth.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Stereo_3D : Item_Stereo_3D.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Blend : Item_Blend.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Blend_Operation : Item_Blend_Operation.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Stencil : Item_Stencil.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Stencil_Operation : Item_Stencil_Operation.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "menumode"),
      Description => To_String_2_Unbounded("Hide the mouse cursor: " & Item_Boolean.Get_Possible_Values),
      Initial     => True) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Stencil_Function : Item_Stencil_Function.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Depth_Function : Item_Depth_Function.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Alpha_Test_Function : Item_Alpha_Test_Function.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "stereodepth"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
    package Mask : Item_Mask.Variable(
      Name        => To_String_2_Unbounded(VARIABLE_PREFIX & "mask"),
      Description => To_String_2_Unbounded(""),
      Initial     => ) :=(
        Is_Logged        => False,
        Is_User_Settable => False,
        others           => <>);
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    MAXIMUM_NUMBER_OF_OCCLUSION_QUERIES : constant Integer_4_Positive := 16#0000_1000#;
  ---------------
  -- Variables --
  ---------------
  -----------------
  -- Subprograms --
  -----------------
  ------------
  -- Import --
  ------------
    package OpenGL
      is
....
package Direct3D
  is
      end Direct3D;
package GCM
  is
end GCM;
end Neo.System.Graphics;
