package Neo.System.Graphics is -- Essentially a private pag
  pragma Suppress(Elaboration_Check);
  Stack_Underflow : Exception;
  Stack_Overflow  : Exception;
  Uninitialized   : Exception;
  Out_Of_Memory   : Exception;
  Unsupported     : Exception;
  Inturrupted     : Exception;
  type Enumerated_API is(
    OpenGL_API,
    -- 1992 Silicon Graphics
    --      http://web.archive.org/web/20130722222817/http://www.opengl.org/registry/doc/glspec43.core.20130214.pdf
    Direct3D_API,
    -- 1995 Microsoft
    --      http://web.archive.org/web/20120819174252/http://msdn.microsoft.com/en-US/library/hh309466(v=vs.85).aspx
    GCM_API);
    -- 2006 Sony
    --      http://web.archive.org/web/20130715050830/http://research.ncl.ac.uk/game/mastersdegree/workshops/ps3introductiontogcm/
  type Enumerated_Shading_Language is(
    Architecture_Review_Board_Language,
    -- 2002 OpenGL Architecture Review Board
    --      http://web.archive.org/web/20120802192218/http://www.renderguild.com/gpuguide.pdf
    --      http://web.archive.org/web/20130512010221/http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_program.txt
    C_For_Graphics_Language,
    -- 2002 Nvidia
    --      http://web.archive.org/web/20040720052349/http://download.nvidia.com/developer/cg/Cg_Specification.pdf
    High_Level_Shading_Language,
    -- 2003 Microsoft
    --      http://web.archive.org/web/20130701061644/http://msdn.microsoft.com/en-us/library/windows/desktop/bb509638(v=vs.85).aspx
    OpenGL_Shading_Language,
    -- 2004 OpenGL Architecture Review Board
    --      http://web.archive.org/web/20130807130424/http://www.opengl.org/registry/doc/GLSLangSpec.4.40.diff.pdf
    Playstation_Shading_Languge);
    -- ???? Sony
    --      http://web.archive.org/web/20130827033837/http://www.examiner.com/article/here-are-most-of-the-technical-specifications-for-the-playstation-4  
  type Record_Specifics is record
      Is_Supported                     : Boolean                     := False;
      Shading_Language                 : Enumerated_Shading_Language := OpenGL_Shading_Language;
      Version                          : Float_4_Real                := 0.0;
      --Frequency                        : Integer_4_Natural           := 0;
      Maximum_Texture_Size             : Integer_4_Natural           := 0;
      Maximum_Texture_Units            : Integer_4_Natural           := 0;
      Color_Bits                       : Integer_4_Positive          := 1;
      Depth_Bits                       : Integer_4_Positive          := 1;
      Stencil_Bits                     : Integer_4_Positive          := 1;
      Bits_Per_Pixel                   : Integer_4_Positive          := 1;
      Multisamples                     : Integer_4_Natural           := 0;
      Has_Swap_Control_Tear            : Boolean                     := False;
      Has_Stereo_Pixel_Format          : Boolean                     := False;
      Has_Multitexture                 : Boolean                     := False;
      Has_Direct_State_Access          : Boolean                     := False;
      Has_Texture_Compression          : Boolean                     := False;
      Has_Anisotropic_Filter           : Boolean                     := False;
      --Has_Texture_Level_Of_Detail_Bias : Boolean                     := False;
      Has_Seamless_Cube_Map            : Boolean                     := False;
      Has_RGB_Color_Framebuffer        : Boolean                     := False;
      Has_Vertex_Buffer_Object         : Boolean                     := False;
      Has_Map_Buffer_Range             : Boolean                     := False;
      Has_Vertex_Array_Object          : Boolean                     := False;
      Has_Draw_Elements_Base_Vertex    : Boolean                     := False;
      Has_Uniform_Buffer               : Boolean                     := False;
      --Has_Two_Sided_Stencil            : Boolean                     := False;
      Has_Depth_Bounds_Test            : Boolean                     := False;
      Has_Sync                         : Boolean                     := False;
      Has_Timer_Query                  : Boolean                     := False;
      Has_Occlusion_Query              : Boolean                     := False;
    end record;
--     procedure Draw;
--     procedure Post_Process;
--     function Is_Drawing  return Boolean; -- If the graphics are drawing no other operations can be done, Inturrupted will be raised
--     function Get_Frame   return Record_Graphic;
  function Get_Specifics return Record_Specifics;
  procedure Initialize (Monitor : in Integer_4_Positive);
  procedure Finalize   (Monitor : in Integer_4_Positive);
  VARIABLE_PREFIX : constant String_2 := "r_";
  package Width                      is new Variable(VARIABLE_PREFIX & "width",         "Windowed mode width",                    Integer_4_Positive, 640);
  package Height                     is new Variable(VARIABLE_PREFIX & "height",        "Windowed mode height",                   Integer_4_Positive, 480);
  package Antialiasing_Samples       is new Variable(VARIABLE_PREFIX & "multisamples" , "Number of antialiasing samples", Integer_4_Natural, 0);
  package API                        is new Variable(VARIABLE_PREFIX & "api",           "", Enumerated_API, OpenGL_API);
  package Warp                       is new Variable(VARIABLE_PREFIX & "warp",          "Use the optical warping renderprog instead of stereoDeGhost", Boolean, False);
  package Stereo_Warp_Power          is new Variable(VARIABLE_PREFIX & "warppower",     "Amount of pre-distortion", Integer_4_Positive, 145);
  package Stereo_Warp_X_Center       is new Variable(VARIABLE_PREFIX & "warpcenterz",   "Center for left eye, right eye will be 100", Integer_1_Percent, 50);
  package Stereo_Warp_Y_Center       is new Variable(VARIABLE_PREFIX & "warpcentery",   "Center for both eyes", Integer_1_Percent, 50);
  package Stereo_Warp_Z              is new Variable(VARIABLE_PREFIX & "warpz",         "", Integer_4_Positive, 1);
  package Stereo_Warp_W              is new Variable(VARIABLE_PREFIX & "warpw",         "", Integer_4_Positive, 1);
  package Stereo_Warp_Fraction       is new Variable(VARIABLE_PREFIX & "warpfraction",  "Fraction of half-width the through-lens view covers", Integer_1_Percent, 100);
  package Do_Sync_Frames             is new Variable(VARIABLE_PREFIX & "syncframes",    "Don't let the GPU buffer execution past swapbuffers", Boolean, True);
  package Stereo_3D                  is new Variable(VARIABLE_PREFIX & "stereo3d",      "", Boolean, False);
  package Polygon_Offset_Factor      is new Variable(VARIABLE_PREFIX & "polyfactor",    "", Boolean, False);
  package Polygon_Offset_Units       is new Variable(VARIABLE_PREFIX & "polyunits",     "", Boolean, False);
  package Texture_Anisotropy         is new Variable(VARIABLE_PREFIX & "anisotropy",    "", Boolean, False);
  package Texture_Detail_Bias        is new Variable(VARIABLE_PREFIX & "bias",          "", Boolean, False);
  package Texture_Minimum_Filter     is new Variable(VARIABLE_PREFIX & "minfilter",     "", Boolean, False);
  package Texture_Maximum_Filter     is new Variable(VARIABLE_PREFIX & "maxfilter",     "", Boolean, False);
  package Texture_MIP_Filter         is new Variable(VARIABLE_PREFIX & "mipfilter",     "", Boolean, False);
  package Do_State_Cache             is new Variable(VARIABLE_PREFIX & "cache",         "", Boolean, False);
  package Do_Lazy_Bind_Programs      is new Variable(VARIABLE_PREFIX & "lazyprog",      "", Boolean, False);
  package Do_Lazy_Bind_Parameters    is new Variable(VARIABLE_PREFIX & "lazyparam",     "", Boolean, False);
  package Do_Lazy_Bind_Textures      is new Variable(VARIABLE_PREFIX & "lazytex",       "", Boolean, False);
  package Do_Strip_Fragment_Branches is new Variable(VARIABLE_PREFIX & "stripbranches", "Strip fragment branches", Boolean, False);
  package Do_Skip_Detail_Triangles   is new Variable(VARIABLE_PREFIX & "skipdtri",      "Skip detail triangles", Boolean, False);
  package Do_Use_Single_Trinagle     is new Variable(VARIABLE_PREFIX & "singletri",     "", Boolean, False);
  package Stereo_Depth               is new Variable(VARIABLE_PREFIX & "stereodepth",   "", Boolean, False);
  package Blend                      is new Variable(VARIABLE_PREFIX & "blend",         "", Boolean, False);
  package Blend_Operation            is new Variable(VARIABLE_PREFIX & "blendop",       "", Boolean, False);
  package Stencil                    is new Variable(VARIABLE_PREFIX & "stencil",       "", Boolean, False);
  package Stencil_Operation          is new Variable(VARIABLE_PREFIX & "stencilop",     "", Boolean, False);
  package Stencil_Function           is new Variable(VARIABLE_PREFIX & "stencilfun",    "", Boolean, False);
  package Depth_Function             is new Variable(VARIABLE_PREFIX & "depthfun",      "", Boolean, False);
  package Alpha_Test_Function        is new Variable(VARIABLE_PREFIX & "alphatest",     "", Boolean, False);
  package Mask                       is new Variable(VARIABLE_PREFIX & "mask",          "", Boolean, False);   
private
  MAXIMUM_CLIP_PLANES                 : constant Integer_4_Positive := 1;
  MAXIMUM_NUMBER_OF_OCCLUSION_QUERIES : constant Integer_4_Positive := 16#0000_1000#;
  type Enumerated_Depth             is (Less_Depth,                  Always_Depth,                 Greater_Depth,                     Equal_Depth);
  type Enumerated_Depth_Stereo      is (No_Stereo,                   Near_Stereo,                  Middle_Stereo,                     Far_Stereo);
  type Enumerated_Stereo_3D         is (No_Stereo_3D,                Side_By_Side_Stereo_3D,       Side_By_Side_Compressed_Stereo_3D, Top_And_Bottom_Compressed_Stereo_3D,
                                        Interlaced_Stereo_3D,        Quad_Buffer_Stereo_3D,        HDMI_720_Stereo_3D);
  type Enumerated_Stencil           is (Reference_Shift_Stencil,     Reference_Bits_Stencil,       Mask_Shift_Stencil);
  type Enumerated_Stencil_Operation is (Keep_Stencil_Operation,      Zero_Stencil_Operation,       Replace_Stencil_Operation,         Increment_Stencil_Operation,
                                        Decrement_Stencil_Operation, Invert_Stencil_Operation,     Increment_Wrap_Stencil_Operation,  Decrement_Wrap_Stencil_Operation);
  type Enumerated_Stencil_Function  is (Always_Stencil,              Not_Equal_Stencil,            Less_Than_Or_Equal_To_Stencil,     Greater_Than_Stencil,
                                        Equal_Stencil,               Never_Stencil,                Less_Stencil);
  type Enumerated_Blend             is (Zero_Blend,                  One_Blend,                    Destination_Color_Blend,           One_Minus_Destination_Color_Blend,
                                        Source_Alpha_Blend,          One_Minus_Source_Alpha_Blend, Destination_Alpha_Blend,           One_Minus_Destination_Alpha_Blend);
  type Enumerated_Blend_Operation   is (Add_Blend_Operation,         Subtract_Blend_Operation,     Minimum_Blend_Operation,           Maximum_Blend_Operation);
  type Record_Driver is record
      Initialize            : not null access procedure(Monitor : in Integer_4_Positive);
      Finalize              : not null access procedure(Monitor : in Integer_4_Positive);
--        Upload_Subimage       : not null access procedure
--        Upload_Subimage       : not null access procedure(mipLevel, x, y, z, width, height, const void * pic, int pixelPitch) ;
--        Finalize_Texture      : not null access procedure(Texture : in out Record_Texture) ;
--        Initialize_Texture    : not null access procedure(Texture : in out Record_Texture) ;
--        Render_Headset        : not null access procedure( ;
--        Make_Stereo_Image     : not null access procedure(Graphic : in Record_Graphic) ;
--        Set_Buffer            : not null access procedure(const void *data ) ;
--        Set_Polygon_Offset    : not null access procedure(Do_Enable : in Boolean) ;
--        Set_Polymode_Line     : not null access procedure(;
--        Set_Depth_Mask        : not null access procedure(;
--        Set_Mask              : not null access procedure(Do_Mask_Red, Do_Mask_Green, Do_Mask_Blue, Do_Mask_Alpha : in Boolean) ;
--        Set_Depth_Function    : not null access procedure(Value : in Enumerated_Depth_Function) ;
--        Set_Stencil_Function  : not null access procedure(Stencil : in Enumerated_Stencil_Function) ;
--        Set_Stencil_Operation : not null access procedure(Fail, Fail_Z, Pass : in Enumerated_Stencil_Operation) ;
--        Set_Stencil           : not null access procedure(Stencil : in Enumerated_Stencil) ;
--        Set_Blend_Operation   : not null access procedure(Blend_Operation : in Enumerated_Blend_Operation) ;
--        Set_Blend             : not null access procedure(Source, Destination : in Enumerated_Blend) ;
--        Set_Stereo_3D         : not null access procedure(Stereo_3D : in Enumerated_Stereo_3D) ;
--        Set_Stereo_Depth      : not null access procedure(Stereo_Depth : in Item_Stereo_Depth.Variable) ;
--        Clear                 : not null access procedure(Color : in Record_Pixel; Stencil_Value : in Integer_1_Unsigned; Do_Clear_Depth : in Boolean := False) is
--        Clear                 : not null access procedure(Stencil_Value : in Integer_1_Unsigned; Do_Clear_Depth : in Boolean := False) ;
--        Clear                 : not null access procedure(Color : in Record_Pixel; Do_Clear_Depth : in Boolean := False) ;
--        Clear                 : not null access procedure(;
--        Color                 : not null access procedure(Color : in Record_Color) ;
--        Color                 : not null access procedure(Pixel : in Record_Pixel) ;
--        Get_Depth_Pass        : not null access procedure(Rectane : in out Record_Rectane) ;
--        Finish_Depth_Pass     : not null access procedure(Rectane : in Record_Rectane) ;
--        Start_Depth_Pass      : not null access procedure(Rectane : in Record_Rectane) ;
--        Depth_Bounds_Test     : not null access procedure(Z_Minimum, Z_Maximum : in Float_4_Real := 0.0) ;
--        Polygon_Offset        : not null access procedure(Scale, Bias : in Float_4_Real) ;
--        View_Port             : not null access procedure(X, Y, Width, Height : in Integer_4_Signed) ;
--        Cull                  : not null access procedure(Kind : in Enumerated_Cull; Is_Mirror : in Boolean := False) ;
--        Check_Exceptions      : not null access procedure(;
--        Reset                 : not null access procedure(;
    end record;
  package GCM      is function Get_Driver return Record_Driver; end GCM;
  package OpenGL   is function Get_Driver return Record_Driver; end OpenGL;
  package Direct3D is function Get_Driver return Record_Driver; end Direct3D;
  Current_API       : Enumerated_API     := API.Get;
  Current_Width     : Integer_4_Positive := 1;
  Current_Height    : Integer_4_Positive := 1;
  Current_Specifics : Record_Specifics   := (others => <>);
  Drivers           : array(Enumerated_API'range) of Record_Driver := (OpenGL.Get_Driver, Direct3D.Get_Driver, GCM.Get_Driver);
end Neo.System.Graphics;
