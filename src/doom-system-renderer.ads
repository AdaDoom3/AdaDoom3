
  --------------
  -- Graphics --
  --------------

  type Enumerated_Material is (Bump_Material,   Diffuse_Material, Specular_Material,  Coverage_Material, Ambient_Material, 
                               Mirror_Material, Xray_Material,    Cube_Material);
  type Enumerated_Light    is (Fog_Light,       Blend_Light,      Normal_Light);
  package Vector_String_2_Unbounded is new Vectors(String_2_Unbounded);
  package Vector_Record_Graphic     is new Vectors(Record_Graphic);
  package Map_Record_Mesh           is new Hashed_Maps(Record_Mesh);
  --package Map_Record_Camera         is new Hashed_Maps(Record_Camera);
  package Map_Record_Animation      is new Hashed_Maps(Record_Animation);
  --package Map_Vector_Record_Graphic is new Hashed_Maps(Vector_Record_Graphic);
  type Record_Material is record
--      Scale             : Array_Matrix_2x3   := (others => (others => 0.0));
      Color             : Record_Pixel       := (others => <>);
      Opacity_Color     : Float_4_Percent    := 100.0;
      Opacity_General   : Float_4_Percent    := 100.0;
      Offset            : Float_4_Real       := 0.0;
      Frame             : Integer_4_Positive := 1;
      Frame_Rate        : Integer_4_Natural  := 0; -- Non-animated materials have a zero frame rate
      Texture           : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Fragment_Program  : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Vertex_Program    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Fragment_Images   : Vector_String_2_Unbounded.Unprotected.Vector;
--      Vertex_Parameters : Vector_Vertex_Parameter.Unprotected.Vector;
    end record;
--  package Ordered_Map_Record_Material_Stage                 is new Ordered_Maps(Enumerated_Material_Stage, Record_Material);
--  package Map_Ordered_Map_Record_Material_Stage_Unprotected is new Hashed_Maps(Ordered_Map_Record_Material_Stages.Unprotected.Map);
  type Record_Light is record
      Axis   : Array_Record_Coordinate_3D(1..3) := (others => (others => <>));
      Origin : Record_Coordinate_3D             := (others => <>);
      -- Frustum definitions for project lights - should have real plane equations here though...
      Target : Record_Coordinate_3D             := (others => <>);
      Right  : Record_Coordinate_3D             := (others => <>);
      Up     : Record_Coordinate_3D             := (others => <>);
      Start  : Record_Coordinate_3D             := (others => <>);
      Ending : Record_Coordinate_3D             := (others => <>);
    end record;
  type Record_View is record
      Field_Of_View : Record_Coordinate_2D_Degree := (others => <>);
      Origin        : Record_Coordinate_3D        := (others => <>);
      Axis          : Record_Coordinate_3D        := (others => <>);
      Time          : Duration                    := 0.0;
    end record;
--  type Record_Texture is new Controlled with record
--      Is_Compressed : Boolean := False;
--      case Is_Compressed is
--        when True => 
--    end record;
--  type Record_Element(Kind : Enumerated_Element := Scene_Element) is record
--      X      : Integer_4_Natural  := 0;
--      Y      : Integer_4_Natural  := 0;
--      Width  : Integer_4_Positive := 640;
--      Height : Integer_4_Positive := 480;
--      case Kind is
--        when Text_Element =>
--          Size    : Integer_4_Positive := 12;
--          Font    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
--          Text    : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
--          Color   : Record_Color       := (others => <>);
--          Opacity : Float_4_Percent    := 100.0;
--        when Scene_Element => null;
--          --Surfaces : ; 
--          --Lights   : ;
--          --Views    : ;
--        when Interface_Element =>
--          Material : Record_Material := (others => <>);
--      end case;
--    end record;
  type Record_Specifics is record
      Is_Supported                     : Boolean                     := False;
      Shading_Language                 : Enumerated_Shading_Language := OpenGL_Shading_Language;
      Version                          : Float_4_Real                := 0.0;
      Color_Bits                       : Integer_4_Positive          := 1;
      Depth_Bits                       : Integer_4_Positive          := 1;
      Stencil_Bits                     : Integer_4_Positive          := 1;
      Bits_Per_Pixel                   : Integer_4_Positive          := 1;
      Maximum_Texture_Size             : Integer_4_Natural           := 0;
      Multisamples                     : Integer_4_Natural           := 0;
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
  procedure Render_Frame;
  procedure Initialize (Monitor : in Integer_4_Positive);
  procedure Finalize   (Monitor : in Integer_4_Positive);
  function Get_Specifics return Record_Specifics;
--  Elements  : Vector_Record_Element.Protected_Vector;
--  Textures  : Map_Vector_Record_Graphics_Unprotected.Protected_Map;
--  Materials : Map_Ordered_Map_Record_Material_Stage_Unprotected.Protected_Map;
  MAXIMUM_CLIP_PLANES                 : constant Integer_4_Positive := 1;
  MAXIMUM_NUMBER_OF_OCCLUSION_QUERIES : constant Integer_4_Positive := 16#0000_1000#;
  VARIABLE_PREFIX                     : constant String_2           := "r_";
  package API           is new Variable(VARIABLE_PREFIX & "api",    "", Enumerated_API, OpenGL_API);
private
  type Enumerated_Condition          is (Always_Condition,            Not_Equal_Condition,          Greater_Than_Condition,            Less_Than_Or_Equal_To_Condition,  
                                         Never_Condition,             Equal_Condition,              Less_Than_Condition,               Greater_Than_Or_Equal_To_Condition);
  type Enumerated_Cull               is (Face_Culling,                Two_Sided_Cull,               Back_Sided_Cull);
  type Enumerated_Stereo             is (No_Stereo,                   Near_Stereo,                  Middle_Stereo,                     Far_Stereo);
  type Enumerated_Stereo_3D          is (No_Stereo_3D,                Side_By_Side_Stereo_3D,       Side_By_Side_Compressed_Stereo_3D, Top_And_Bottom_Compressed_Stereo_3D,
                                         Interlaced_Stereo_3D,        Quad_Buffer_Stereo_3D,        HDMI_720_Stereo_3D);
  type Enumerated_Stencil            is (Reference_Shift_Stencil,     Reference_Bits_Stencil,       Mask_Shift_Stencil);
  type Enumerated_Stencil_Operation  is (Keep_Stencil_Operation,      Zero_Stencil_Operation,       Replace_Stencil_Operation,         Increment_Stencil_Operation,
                                         Decrement_Stencil_Operation, Invert_Stencil_Operation,     Increment_Wrap_Stencil_Operation,  Decrement_Wrap_Stencil_Operation);
  type Enumerated_Blend              is (Zero_Blend,                  One_Blend,                    Destination_Color_Blend,           One_Minus_Destination_Color_Blend,
                                         Source_Alpha_Blend,          One_Minus_Source_Alpha_Blend, Destination_Alpha_Blend,           One_Minus_Destination_Alpha_Blend);
  type Enumerated_Blend_Operation    is (Add_Blend_Operation,         Subtract_Blend_Operation,     Minimum_Blend_Operation,           Maximum_Blend_Operation);
  type Record_Driver is record
      Reset                 : not null access procedure;
      Initialize            : not null access procedure(Monitor : in Integer_4_Positive);
      Finalize              : not null access procedure(Monitor : in Integer_4_Positive);
--      Upload_Subimage       : not null access procedure
--      Upload_Subimage       : not null access procedure(mipLevel, x, y, z, width, height, const void * pic, int pixelPitch) ;
--      Finalize_Texture      : not null access procedure(Texture : in out Record_Texture) ;
--      Initialize_Texture    : not null access procedure(Texture : in out Record_Texture) ;
--      Render_Headset        : not null access procedure( ;
--      Make_Stereo_Image     : not null access procedure(Graphic : in Record_Graphic) ;
--      Set_Buffer            : not null access procedure(const void *data ) ;
--      Set_Polygon_Offset    : not null access procedure(Do_Enable : in Boolean) ;
--      Set_Polymode_Line     : not null access procedure(;
--      Set_Depth_Mask        : not null access procedure(;
      Set_Color_Mask        : not null access procedure(Do_Mask_Red, Do_Mask_Green, Do_Mask_Blue, Do_Mask_Alpha : in Boolean := True);
--      Set_Depth_Function    : not null access procedure(Value : in Enumerated_Depth_Function) ;
--      Set_Stencil_Function  : not null access procedure(Stencil : in Enumerated_Stencil_Function) ;
--      Set_Stencil_Operation : not null access procedure(Fail, Fail_Z, Pass : in Enumerated_Stencil_Operation) ;
--      Set_Stencil           : not null access procedure(Stencil : in Enumerated_Stencil) ;
--      Set_Blend_Operation   : not null access procedure(Blend_Operation : in Enumerated_Blend_Operation) ;
--      Set_Blend             : not null access procedure(Source, Destination : in Enumerated_Blend) ;
--      Set_Stereo_3D         : not null access procedure(Stereo_3D : in Enumerated_Stereo_3D) ;
--      Set_Stereo_Depth      : not null access procedure(Stereo_Depth : in Item_Stereo_Depth.Variable) ;
--      Clear                 : not null access procedure(Color : in Record_Pixel; Stencil_Value : in Integer_1_Unsigned; Do_Clear_Depth : in Boolean := False) is
--      Clear                 : not null access procedure(Stencil_Value : in Integer_1_Unsigned; Do_Clear_Depth : in Boolean := False) ;
--      Clear                 : not null access procedure(Color : in Record_Pixel; Do_Clear_Depth : in Boolean := False) ;
--      Clear                 : not null access procedure(;
--      Color                 : not null access procedure(Color : in Record_Color) ;
--      Color                 : not null access procedure(Pixel : in Record_Pixel) ;
--      Get_Depth_Pass        : not null access procedure(Rectane : in out Record_Rectane) ;
--      Finish_Depth_Pass     : not null access procedure(Rectane : in Record_Rectane) ;
--      Start_Depth_Pass      : not null access procedure(Rectane : in Record_Rectane) ;
--      Depth_Bounds_Test     : not null access procedure(Z_Minimum, Z_Maximum : in Float_4_Real := 0.0) ;
--      Polygon_Offset        : not null access procedure(Scale, Bias : in Float_4_Real) ;
--      View_Port             : not null access procedure(X, Y, Width, Height : in Integer_4_Signed) ;
--      Cull                  : not null access procedure(Kind : in Enumerated_Cull; Is_Mirror : in Boolean := False) ;
--      Check_Exceptions      : not null access procedure(;
    end record;
  package GCM      is function Get_Driver return Record_Driver; end GCM;
  package OpenGL   is function Get_Driver return Record_Driver; end OpenGL;
  package Direct3D is function Get_Driver return Record_Driver; end Direct3D;
  Current_API       : Enumerated_API   := API.Get;
  Current_Specifics : Record_Specifics := (others => <>);
  Drivers           : array(Enumerated_API'range) of Record_Driver := (OpenGL.Get_Driver, Direct3D.Get_Driver, GCM.Get_Driver);