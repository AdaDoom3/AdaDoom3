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
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.System.OpenGL)
package Implementation
  is
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Driver : in String_2)
      is
      ------------------
      function Callback(
      ------------------
        Window        : Address;
        Message       : Integer_4_Unsigned_C;
        Data_Unsigned : Integer_4_Unsigned_C;
        Data_Signed   : Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        Device_Context    : Record_Device_Context    := 0;
        Rendering_Context : Record_Rendering_Context := hGLRC;
        Pixel_Format      : Record_Pixel_Format      :=(
          Version      => Integer_2_Unsigned_C(PIXEL_VERSION), --1
          Flags        => Integer_4_Unsigned_C(PIXEL_FLAGS), --
          Pixel_Type   => Integer_1_Unsigned_C(PIXEL_TYPE), --
          Color_Bits   => Integer_1_Unsigned_C(COLOR_DEPTH), --32
          Alpha_Bits   => Integer_1_Unsigned_C(DESTINATION_ALPHA), --8
          Depth_Bits   => Integer_1_Unsigned_C(Z_BUFFER), --24
          Stencil_Bits => Integer_1_Unsigned_C(STENCIL_BUFFER), -- 8
          Layer_Type   => Integer_1_Unsigned_C(PIXEL_LAYER), -- PFD_MAIN_PLANE
          others       => <>);
        begin
          if Message = EVENT_DESTROY then
            Post_Quit_Message(0);
          end if;
          if Message /= EVENT_CREATE then
            return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
          end if;
          Device_Context := Get_Device_Context(Window);
          Set_Pixel_Format(
            Device_Context => Device_Context,
            Format         => Choose_Pixel_Format(Device_Context, Pixel_Format'address),
            Descriptor     => Pixel_Format'address);
          Rendering_Context := OpenGL_Create_Context(Device_Context);
          Make_Current(Device_Context, Rendering_Context);
          Make_Current(NULL_ADDRESS, NULL_ADDRESS);
          Delete_Context(Rendering_Context);
          Release_Device_Context(Window, Device_Context);
          return 1;
        end Faux_Callback;
        pragma Convention(Stdcall, Callback);
      Message           : Record_Message := NULL_RECORD_MESSAGE;
      Window            : Address        := NULL_ADDRESS;
      Device_Context    : Address        := NULL_ADDRESS;
      Rendering_Context : Address        := NULL_ADDRESS;
      begin
        if Create_Class(Faux_Callback'address, Faux_Name) = FAILED
        if OpenGL_Instance /= NULL_ADDRESS then
          Unload_OpenGL_Driver;
        end if;
        Put_Line("Initializing OpenGL" );
        Put("Calling LoadLibrary(" & Driver_Name & "): ");
        OpenGL_Instance := Load_Library(OpenGL_Driver);
        if OpenGL_Instance = 0 then
          Put_Line("Failed");
          raise System_Call_Failure;
        end if;
        Accum                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glAccum"));
        Alpha_Func                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glAlphaFunc"));
        Are_Textures_Resident      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glAreTexturesResident"));
        Array_Element              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glArrayElement"));
        Begin                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glBegin"));
        Bind_Texture               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glBindTexture"));
        Bitmap                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glBitmap"));
        Blend_Func                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glBlendFunc"));
        Call_List                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCallList"));
        Call_Lists                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCallLists"));
        Clear                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClear"));
        Clear_Accum                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClearAccum"));
        Clear_Color                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClearColor"));
        Clear_Depth                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClearDepth"));
        Clear_Index                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClearIndex"));
        Clear_Stencil              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClearStencil"));
        Clip_Plane                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glClipPlane"));
        Color_3B                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3b"));
        Color_3BV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3bv"));
        Color_3D                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3d"));
        Color_3DV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3dv"));
        Color_3F                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3f"));
        Color_3FV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3fv"));
        Color_3I                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3i"));
        Color_3IV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3iv"));
        Color_3S                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3s"));
        Color_3SV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3sv"));
        Color_3UB                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3ub"));
        Color_3UBV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3ubv"));
        Color_3UI                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3ui"));
        Color_3UIV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3uiv"));
        Color_3US                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3us"));
        Color_3USV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor3usv"));
        Color_4B                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4b"));
        Color_4BV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4bv"));
        Color_4D                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4d"));
        Color_4DV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4dv"));
        Color_4F                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4f"));
        Color_4FV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4fv"));
        Color_4I                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4i"));
        Color_4IV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4iv"));
        Color_4S                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4s"));
        Color_4SV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4sv"));
        Color_4UB                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4ub"));
        Color_4UBV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4ubv"));
        Color_4UI                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4ui"));
        Color_4UIV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4uiv"));
        Color_4US                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4us"));
        Color_4USV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColor4usv"));
        Color_Mask                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColorMask"));
        Color_Material             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColorMaterial"));
        Color_Pointer              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glColorPointer"));
        Copy_Pixels                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCopyPixels"));
        Copy_Tex_Image_1D          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCopyTexImage1D"));
        Copy_Tex_Image_2D          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCopyTexImage2D"));
        Copy_Tex_Sub_Image_1D      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCopyTexSubImage1D"));
        Copy_Tex_Sub_Image_2D      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCopyTexSubImage2D"));
        Cull_Face                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glCullFace"));
        Delete_Lists               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDeleteLists"));
        Delete_Textures            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDeleteTextures"));
        Depth_Func                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDepthFunc"));
        Depth_Mask                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDepthMask"));
        Depth_Range                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDepthRange"));
        Disable                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDisable"));
        Disable_Client_State       := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDisableClientState"));
        Draw_Arrays                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDrawArrays"));
        Draw_Buffer                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDrawBuffer"));
        Draw_Elements              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDrawElements"));
        Draw_Pixels                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glDrawPixels"));
        Edge_Flag                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEdgeFlag"));
        Edge_Flag_Pointer          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEdgeFlagPointer"));
        Edge_Flag_V                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEdgeFlagv"));
        Enable                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEnable"));
        Enable_Client_State        := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEnableClientState"));
        End                        := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEnd"));
        End_List                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEndList"));
        Eval_Coord_1D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord1d"));
        Eval_Coord_1DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord1dv"));
        Eval_Coord_1F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord1f"));
        Eval_Coord_1FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord1fv"));
        Eval_Coord_2D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord2d"));
        Eval_Coord_2DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord2dv"));
        Eval_Coord_2F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord2f"));
        Eval_Coord_2FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalCoord2fv"));
        Eval_Mesh_1                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalMesh1"));
        Eval_Mesh_2                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalMesh2"));
        Eval_Point_1               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalPoint1"));
        Eval_Point_2               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glEvalPoint2"));
        Feedback_Buffer            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFeedbackBuffer"));
        Finish                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFinish"));
        Flush                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFlush"));
        Fog_F                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFogf"));
        Fog_FV                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFogfv"));
        Fog_I                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFogi"));
        Fog_IV                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFogiv"));
        Front_Face                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFrontFace"));
        Frustum                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glFrustum"));
        Gen_Lists                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGenLists"));
        Gen_Textures               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGenTextures"));
        Get_Boolean_V              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetBooleanv"));
        Get_Clip_Plane             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetClipPlane"));
        Get_Double_V               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetDoublev"));
        Get_Error                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetError"));
        Get_Float_V                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetFloatv"));
        Get_Integer_V              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetIntegerv"));
        Get_Light_FV               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetLightfv"));
        Get_Light_IV               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetLightiv"));
        Get_Map_DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetMapdv"));
        Get_Map_FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetMapfv"));
        Get_Map_IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetMapiv"));
        Get_Material_FV            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetMaterialfv"));
        Get_Material_IV            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetMaterialiv"));
        Get_Pixel_Map_FV           := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetPixelMapfv"));
        Get_Pixel_Map_UIV          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetPixelMapuiv"));
        Get_Pixel_Map_USV          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetPixelMapusv"));
        Get_Pointer_B              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetPointerv"));
        Get_Polygon_Stipple        := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetPolygonStipple"));
        Get_String                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetString"));
        Get_Tex_Env_FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexEnvfv"));
        Get_Tex_Env_IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexEnviv"));
        Get_Tex_Gen_DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexGendv"));
        Get_Tex_Gen_FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexGenfv"));
        Get_Tex_Gen_IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexGeniv"));
        Get_Tex_Image              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexImage"));
        Get_Tex_Level_Parameter_FV := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexLevelParameterfv"));
        Get_Tex_Level_Parameter_IV := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexLevelParameteriv"));
        Get_Tex_Parameter_FV       := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexParameterfv"));
        Get_Tex_Parameter_IV       := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glGetTexParameteriv"));
        Hint                       := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glHint"));
        Index_Mask                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexMask"));
        Index_Pointer              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexPointer"));
        Index_D                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexd"));
        Index_DV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexdv"));
        Index_F                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexf"));
        Index_FV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexfv"));
        Index_I                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexi"));
        Index_IV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexiv"));
        Index_S                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexs"));
        Index_SV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexsv"));
        Index_UB                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexub"));
        Index_UBV                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIndexubv"));
        Init_Names                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glInitNames"));
        Interleaved_Arrays         := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glInterleavedArrays"));
        Is_Enabled                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIsEnabled"));
        Is_List                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIsList"));
        Is_Texture                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glIsTexture"));
        Light_Model_F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightModelf"));
        Light_Model_FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightModelfv"));
        Light_Model_I              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightModeli"));
        Light_Model_IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightModeliv"));
        Light_F                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightf"));
        Light_FV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightfv"));
        Light_I                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLighti"));
        Light_IV                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLightiv"));
        Line_Stipple               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLineStipple"));
        Line_Width                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLineWidth"));
        List_Base                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glListBase"));
        Load_Identity              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLoadIdentity"));
        Load_Matrix_D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLoadMatrixd"));
        Load_Matrix_F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLoadMatrixf"));
        Load_Name                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLoadName"));
        Logic_Op                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glLogicOp"));
        Map_1D                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMap1d"));
        Map_1F                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMap1f"));
        Map_2D                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMap2d"));
        Map_2F                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMap2f"));
        Map_Grid_1D                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMapGrid1d"));
        Map_Grid_1F                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMapGrid1f"));
        Map_Grid_2D                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMapGrid2d"));
        Map_Grid_2F                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMapGrid2f"));
        Material_F                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMaterialf"));
        Material_FV                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMaterialfv"));
        Material_I                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMateriali"));
        Material_IV                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMaterialiv"));
        Matrix_Mode                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMatrixMode"));
        Mult_Matrix_D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMultMatrixd"));
        Mult_Matrix_F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glMultMatrixf"));
        New_List                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNewList"));
        Normal_3B                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3b"));
        Normal_3BV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3bv"));
        Normal_3D                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3d"));
        Normal_3DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3dv"));
        Normal_3F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3f"));
        Normal_3FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3fv"));
        Normal_3I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3i"));
        Normal_3IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3iv"));
        Normal_3s                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3s"));
        Normal_3sv                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormal3sv"));
        Normal_Pointer             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glNormalPointer"));
        Ortho                      := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glOrtho"));
        Pass_Through               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPassThrough"));
        Pixel_Map_FV               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelMapfv"));
        Pixel_Map_UIV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelMapuiv"));
        Pixel_Map_USV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelMapusv"));
        Pixel_Store_F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelStoref"));
        Pixel_Store_I              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelStorei"));
        Pixel_Transfer_F           := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelTransferf"));
        Pixel_Transfer_I           := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelTransferi"));
        Pixel_Zoom                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPixelZoom"));
        Point_Size                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPointSize"));
        Polygon_Mode               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPolygonMode"));
        Polygon_Offset             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPolygonOffset"));
        Polygon_Stipple            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPolygonStipple"));
        Pop_Attrib                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPopAttrib"));
        Pop_Client_Attrib          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPopClientAttrib"));
        Pop_Matrix                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPopMatrix"));
        Pop_Name                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPopName"));
        Prioritize_Textures        := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPrioritizeTextures"));
        Push_Attrib                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPushAttrib"));
        Push_Client_Attrib         := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPushClientAttrib"));
        Push_Matrix                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPushMatrix"));
        Push_Name                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glPushName"));
        Raster_Pos_2D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2d"));
        Raster_Pos_2DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2dv"));
        Raster_Pos_2F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2f"));
        Raster_Pos_2FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2fv"));
        Raster_Pos_2I              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2i"));
        Raster_Pos_2IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2iv"));
        Raster_Pos_2S              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2s"));
        Raster_Pos_2SV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos2sv"));
        Raster_Pos_3D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3d"));
        Raster_Pos_3DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3dv"));
        Raster_Pos_3F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3f"));
        Raster_Pos_3FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3fv"));
        Raster_Pos_3I              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3i"));
        Raster_Pos_3IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3iv"));
        Raster_Pos_3S              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3s"));
        Raster_Pos_3SV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos3sv"));
        Raster_Pos_4D              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4d"));
        Raster_Pos_4DV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4dv"));
        Raster_Pos_4F              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4f"));
        Raster_Pos_4FV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4fv"));
        Raster_Pos_4I              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4i"));
        Raster_Pos_4IV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4iv"));
        Raster_Pos_4S              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4s"));
        Raster_Pos_4SV             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRasterPos4sv"));
        Read_Buffer                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glReadBuffer"));
        Read_Pixels                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glReadPixels"));
        Rect_D                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectd"));
        Rect_DV                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectdv"));
        Rect_F                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectf"));
        Rect_FV                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectfv"));
        Rect_I                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRecti"));
        Rect_IV                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectiv"));
        Rect_S                     := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRects"));
        Rect_SV                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRectsv"));
        Render_Mode                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRenderMode"));
        Rotate_D                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRotated"));
        Rotate_F                   := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glRotatef"));
        Scale_D                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glScaled"));
        Scale_F                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glScalef"));
        Scissor                    := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glScissor"));
        Select_Buffer              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glSelectBuffer"));
        Shade_Model                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glShadeModel"));
        Stencil_Func               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glStencilFunc"));
        Stencil_Mask               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glStencilMask"));
        Stencil_Op                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glStencilOp"));
        Tex_Coord_1D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1d"));
        Tex_Coord_1DV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1dv"));
        Tex_Coord_1F               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1f"));
        Tex_Coord_1FV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1fv"));
        Tex_Coord_1I               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1i"));
        Tex_Coord_1IV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1iv"));
        Tex_Coord_1S               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1s"));
        Tex_Coord_1SV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord1sv"));
        Tex_Coord_2D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2d"));
        Tex_Coord_2DV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2dv"));
        Tex_Coord_2F               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2f"));
        Tex_Coord_2FV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2fv"));
        Tex_Coord_2I               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2i"));
        Tex_Coord_2IV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2iv"));
        Tex_Coord_2S               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2s"));
        Tex_Coord_2SV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord2sv"));
        Tex_Coord_3D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3d"));
        Tex_Coord_3DV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3dv"));
        Tex_Coord_3F               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3f"));
        Tex_Coord_3FV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3fv"));
        Tex_Coord_3I               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3i"));
        Tex_Coord_3IV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3iv"));
        Tex_Coord_3S               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3s"));
        Tex_Coord_3DV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord3sv"));
        Tex_Coord_4D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4d"));
        Tex_Coord_4DV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4dv"));
        Tex_Coord_4F               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4f"));
        Tex_Coord_4FV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4fv"));
        Tex_Coord_4I               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4i"));
        Tex_Coord_4IV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4iv"));
        Tex_Coord_4S               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4s"));
        Tex_Coord_4SV              := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoord4sv"));
        Tex_Coord_Pointer          := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexCoordPointer"));
        Tex_Env_F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexEnvf"));
        Tex_Env_FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexEnvfv"));
        Tex_Env_I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexEnvi"));
        Tex_Env_IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexEnviv"));
        Tex_Gen_D                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexGend"));
        Tex_Gen_DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexGendv"));
        Tex_Gen_F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexGenf"));
        Tex_Gen_FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "lTexGenfv"));
        Tex_Gen_I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexGeni"));
        Tex_Gen_IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexGeniv"));
        Tex_Image_1D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexImage1D"));
        Tex_Image_2D               := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexImage2D"));
        Tex_Parameter_F            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexParameterf"));
        Tex_Parameter_FV           := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexParameterfv"));
        Tex_Parameter_I            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexParameteri"));
        Tex_Parameter_IV           := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexParameteriv"));
        Tex_SubImage_1D            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexSubImage1D"));
        Tex_SubImage_2D            := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTexSubImage2D"));
        Translate_D                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTranslated"));
        Translate_F                := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glTranslatef"));
        Vertex_2D                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2d"));
        Vertex_2DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2dv"));
        Vertex_2F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2f"));
        Vertex_2FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2fv"));
        Vertex_2I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2i"));
        Vertex_2IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2iv"));
        Vertex_2S                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2s"));
        Vertex_2SV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex2sv"));
        Vertex_3D                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3d"));
        Vertex_3DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3dv"));
        Vertex_3F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3f"));
        Vertex_3FV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3fv"));
        Vertex_3I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3i"));
        Vertex_3IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3iv"));
        Vertex_3S                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3s"));
        Vertex_3SV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex3sv"));
        Vertex_4D                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4d"));
        Vertex_4DV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4dv"));
        Vertex_4F                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4f"));
        Vertex_4Fv                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4fv"));
        Vertex_4I                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4i"));
        Vertex_4IV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4iv"));
        Vertex_4S                  := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4s"));
        Vertex_4SV                 := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertex4sv"));
        Vertex_Pointer             := To_Access_(Get_Procedure_Address(OpenGL_Instance, "glVertexPointer"));
        Viewport                   := vglViewport;
        Copy_Context               := wglCopyContext;
        Create_Context             := wglCreateContext;
        Create_Layer_Context       := wglCreateLayerContext;
        Delete_Context             := wglDeleteContext;
        Describe_Layer_Plane       := wglDescribeLayerPlane;
        Get_Current_Context        := wglGetCurrentContext;
        Get_Current_DC             := wglGetCurrentDC;
        Get_Layer_Palette_Entries  := wglGetLayerPaletteEntries;
        Get_Proc_Address           := wglGetProcAddress;
        Make_Current               := wglMakeCurrent;
        Realize_Layer_Palette      := wglRealizeLayerPalette;
        Set_Layer_Palette_Entries  := wglSetLayerPaletteEntries;
        Share_Lists                := wglShareLists;
        Swap_Layer_Buffers         := wglSwapLayerBuffers;
        Use_Font_Bitmaps           := wglUseFontBitmapsA;
        Use_Font_Outlines          := wglUseFontOutlinesA;
        Choose_Pixel_Format        := ChoosePixelFormat;
        Describe_Pixel_Format      := DescribePixelFormat;
        Get_Pixel_Format           := GetPixelFormat;
        Set_Pixel_Format           := SetPixelFormat;
        Swap_Buffers               := SwapBuffers;
        Active_Texture_ARB         := null;
        Client_Active_Texture_ARB  := null;
        Multi_Tex_Coord_2F_ARB     := null;
        Window := Create_Window(
          Class_Name  => WIN32_FAKE_WINDOW_CLASS_NAME,
          Window_Name => GAME_NAME,
          Style       => WS_OVERLAPPEDWINDOW);
        if Window = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        Device_Context    := Get_Device_Context(Window);
        Rendering_Context := OpenGL.Create_Rendering_Context(Device_Context);
        OpenGL.Make_Current(Device_Context, Rendering_Context);
        Release_Device_Context(Window, Device_Context);
        Destroy_Window(Window);
        while Get_Message(Message'address, NULL_ADDRESS, 0, 0) loop
          Result := Translate_Message(Message'address);
          Result := Dispatch_Message(Message'address);
        end loop;
        Put_Line("Succeeded");
        if Device_Context = NULL_ADDRESS then
          Device_Context := Get_Device_Context(Window);
          if Device_Context = NULL_ADDRESS then
            raise Initialization_Error;
          end if;
        end if;
        if OpenGL.Choose_Pixel_Format /= null and Multi_Samples > 1 then
          ---------------
          Preferred_Path:
          ---------------
            declare
            Number_Of_Formats : Integer_4_Signed_C  := 0;
            begin
              OpenGL.Choose_Pixel_Format(
                Device_Context     => Device_Context,
                Attributes_Integer =>(
                  WGL_SAMPLE_BUFFERS_ARB, 1,
                  WGL_SAMPLES_ARB,        Multi_Samples,
                  WGL_DOUBLE_BUFFER_ARB,  1,
                  WGL_STENCIL_BITS_ARB,   8,
                  WGL_DEPTH_BITS_ARB,     24,
                  WGL_RED_BITS_ARB,       8,
                  WGL_BLUE_BITS_ARB,      8,
                  WGL_GREEN_BITS_ARB,     8,
                  WGL_ALPHA_BITS_ARB,     8, 0, 0),
                Attributes_Float  => (0.0, 0.0),
                Pixel_Format      => Pixel_Format'address,
                Number_Of_Formats => Number_Of_Formats'address);
            end Preferred_Path;
        else
          -------------
          Classic_Path:
          -------------
            declare
             Source : aliased Record_Pixel_Format :=(
                Size                   => Record_Pixel_Formal'size / Byte'size,
                Version                => PIXEL_VERSION, --1
                Flags                  => PIXEL_FLAGS, --
                Pixel_Type             => PIXEL_TYPE, --
                Color_Bits             => COLOR_DEPTH, --32
                Red_Bits               => 0,
                Red_Shift              => 0,
                Green_Bits             => 0,
                Green_Shift            => 0,
                Blue_Bits              => 0,
                Blue_Shift             => 0,
                Alpha_Bits             => DESTINATION_ALPHA, --8
                Alpha_Shift            => 0,
                Accumulated_Bits       => 0,
                Accumulated_Red_Bits   => 0,
                Accumulated_Green_Bits => 0,
                Accumulated_Blue_Bits  => 0,
                Accumulated_Alpha_Bits => 0,
                Depth_Bits             => Z_BUFFER, --24
                Stencil_Bits           => STENCIL_BUFFER, -- 8
                Aux_Buffers            => 0,
                Layer_Type             => PIXEL_LAYER, -- PFD_MAIN_PLANE
                Reserved               => 0,
                Layer_Mask             => 0,
                Visible_Mask           => 0,
                Damage_Mask            => 0);
            begin
              -- We need to have more fallbacks, but for now, ask for everything
              if Do_Use_Stereo then
                Pixel_Descriptor.Flags := Pixel_Descriptor.Flags or PFD_STEREO;
              end if;
              Pixel_Format := Choose_Pixel_Format(Device_Context, Source'address)
              if Pixel_Format = 0 then
                raise Initialization_Error;
              end if;
            end Classic_Path;
        end if;
        Describe_Pixel_Format(Device_Context, Pixel_Format, Pixel_Descriptor'size / Byte'size, Pixel_Descriptor'address);
        Color_Bits   := Pixel_Descriptor.Color_Bits;
        Depth_Bits   := Pixel_Descriptor.Depth_Bits;
        Stencil_Bits := Pixel_Descriptor.Stencil_Bits;
        if Stencil_Bits = 0 then -- XP seems to set this incorrectly
          Stencil_Bits = 8;
        end if;
        if Set_Pixel_Format(Device_Context, Pixel_Format, &win32.pfd) = 0 then
          raise Initialization_Error;
        end if;
        Rendering_Context := OpenGL.Create_Context(Device_Context);
        if Rendering_Context = 0 then
          raise Initialization_Error;
        end if;
        if not OpenGL.Make_Current(Device_Context, Rendering_Context) then
          OpenGL.Delete_Context(Rendering_Context);
          Rendering_Context := NULL_Address;
          raise Initialization_Error;
        end if;
        if Set_Foreground_Window(Window) = 0 then
          raise System_Call_Failure;
        end if;
        if Set_Focus(Window) = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        return true;
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Unload_OpenGL_Driver
      is
      begin
        Put_Line("Shutting down..." );
        if OpenGL_Instance /= NULL_ADDRESS then
          Put_Line("Unloading OpenGL DLL...");
          Free_Library(OpenGL_Instance);
          OpenGL_Instance := NULL_ADDRESS;
        end if;
        Accum                     := NULL_ADDRESS;
        AlphaFunc                 := NULL_ADDRESS;
        AreTexturesResident       := NULL_ADDRESS;
        ArrayElement              := NULL_ADDRESS;
        Begin                     := NULL_ADDRESS;
        BindTexture               := NULL_ADDRESS;
        Bitmap                    := NULL_ADDRESS;
        BlendFunc                 := NULL_ADDRESS;
        CallList                  := NULL_ADDRESS;
        CallLists                 := NULL_ADDRESS;
        Clear                     := NULL_ADDRESS;
        ClearAccum                := NULL_ADDRESS;
        ClearColor                := NULL_ADDRESS;
        ClearDepth                := NULL_ADDRESS;
        ClearIndex                := NULL_ADDRESS;
        ClearStencil              := NULL_ADDRESS;
        ClipPlane                 := NULL_ADDRESS;
        Color3b                   := NULL_ADDRESS;
        Color3bv                  := NULL_ADDRESS;
        Color3d                   := NULL_ADDRESS;
        Color3dv                  := NULL_ADDRESS;
        Color3f                   := NULL_ADDRESS;
        Color3fv                  := NULL_ADDRESS;
        Color3i                   := NULL_ADDRESS;
        Color3iv                  := NULL_ADDRESS;
        Color3s                   := NULL_ADDRESS;
        Color3sv                  := NULL_ADDRESS;
        Color3ub                  := NULL_ADDRESS;
        Color3ubv                 := NULL_ADDRESS;
        Color3ui                  := NULL_ADDRESS;
        Color3uiv                 := NULL_ADDRESS;
        Color3us                  := NULL_ADDRESS;
        Color3usv                 := NULL_ADDRESS;
        Color4b                   := NULL_ADDRESS;
        Color4bv                  := NULL_ADDRESS;
        Color4d                   := NULL_ADDRESS;
        Color4dv                  := NULL_ADDRESS;
        Color4f                   := NULL_ADDRESS;
        Color4fv                  := NULL_ADDRESS;
        Color4i                   := NULL_ADDRESS;
        Color4iv                  := NULL_ADDRESS;
        Color4s                   := NULL_ADDRESS;
        Color4sv                  := NULL_ADDRESS;
        Color4ub                  := NULL_ADDRESS;
        Color4ubv                 := NULL_ADDRESS;
        Color4ui                  := NULL_ADDRESS;
        Color4uiv                 := NULL_ADDRESS;
        Color4us                  := NULL_ADDRESS;
        Color4usv                 := NULL_ADDRESS;
        ColorMask                 := NULL_ADDRESS;
        ColorMaterial             := NULL_ADDRESS;
        ColorPointer              := NULL_ADDRESS;
        CopyPixels                := NULL_ADDRESS;
        CopyTexImage1D            := NULL_ADDRESS;
        CopyTexImage2D            := NULL_ADDRESS;
        CopyTexSubImage1D         := NULL_ADDRESS;
        CopyTexSubImage2D         := NULL_ADDRESS;
        CullFace                  := NULL_ADDRESS;
        DeleteLists               := NULL_ADDRESS;
        DeleteTextures            := NULL_ADDRESS;
        DepthFunc                 := NULL_ADDRESS;
        DepthMask                 := NULL_ADDRESS;
        DepthRange                := NULL_ADDRESS;
        Disable                   := NULL_ADDRESS;
        DisableClientState        := NULL_ADDRESS;
        DrawArrays                := NULL_ADDRESS;
        DrawBuffer                := NULL_ADDRESS;
        DrawElements              := NULL_ADDRESS;
        DrawPixels                := NULL_ADDRESS;
        EdgeFlag                  := NULL_ADDRESS;
        EdgeFlagPointer           := NULL_ADDRESS;
        EdgeFlagv                 := NULL_ADDRESS;
        Enable                    := NULL_ADDRESS;
        EnableClientState         := NULL_ADDRESS;
        End                       := NULL_ADDRESS;
        EndList                   := NULL_ADDRESS;
        EvalCoord1d               := NULL_ADDRESS;
        EvalCoord1dv              := NULL_ADDRESS;
        EvalCoord1f               := NULL_ADDRESS;
        EvalCoord1fv              := NULL_ADDRESS;
        EvalCoord2d               := NULL_ADDRESS;
        EvalCoord2dv              := NULL_ADDRESS;
        EvalCoord2f               := NULL_ADDRESS;
        EvalCoord2fv              := NULL_ADDRESS;
        EvalMesh1                 := NULL_ADDRESS;
        EvalMesh2                 := NULL_ADDRESS;
        EvalPoint1                := NULL_ADDRESS;
        EvalPoint2                := NULL_ADDRESS;
        FeedbackBuffer            := NULL_ADDRESS;
        Finish                    := NULL_ADDRESS;
        Flush                     := NULL_ADDRESS;
        Fogf                      := NULL_ADDRESS;
        Fogfv                     := NULL_ADDRESS;
        Fogi                      := NULL_ADDRESS;
        Fogiv                     := NULL_ADDRESS;
        FrontFace                 := NULL_ADDRESS;
        Frustum                   := NULL_ADDRESS;
        GenLists                  := NULL_ADDRESS;
        GenTextures               := NULL_ADDRESS;
        GetBooleanv               := NULL_ADDRESS;
        GetClipPlane              := NULL_ADDRESS;
        GetDoublev                := NULL_ADDRESS;
        GetError                  := NULL_ADDRESS;
        GetFloatv                 := NULL_ADDRESS;
        GetIntegerv               := NULL_ADDRESS;
        GetLightfv                := NULL_ADDRESS;
        GetLightiv                := NULL_ADDRESS;
        GetMapdv                  := NULL_ADDRESS;
        GetMapfv                  := NULL_ADDRESS;
        GetMapiv                  := NULL_ADDRESS;
        GetMaterialfv             := NULL_ADDRESS;
        GetMaterialiv             := NULL_ADDRESS;
        GetPixelMapfv             := NULL_ADDRESS;
        GetPixelMapuiv            := NULL_ADDRESS;
        GetPixelMapusv            := NULL_ADDRESS;
        GetPointerv               := NULL_ADDRESS;
        GetPolygonStipple         := NULL_ADDRESS;
        GetString                 := NULL_ADDRESS;
        GetTexEnvfv               := NULL_ADDRESS;
        GetTexEnviv               := NULL_ADDRESS;
        GetTexGendv               := NULL_ADDRESS;
        GetTexGenfv               := NULL_ADDRESS;
        GetTexGeniv               := NULL_ADDRESS;
        GetTexImage               := NULL_ADDRESS;
        GetTexLevelParameterfv    := NULL_ADDRESS;
        GetTexLevelParameteriv    := NULL_ADDRESS;
        GetTexParameterfv         := NULL_ADDRESS;
        GetTexParameteriv         := NULL_ADDRESS;
        Hint                      := NULL_ADDRESS;
        IndexMask                 := NULL_ADDRESS;
        IndexPointer              := NULL_ADDRESS;
        Indexd                    := NULL_ADDRESS;
        Indexdv                   := NULL_ADDRESS;
        Indexf                    := NULL_ADDRESS;
        Indexfv                   := NULL_ADDRESS;
        Indexi                    := NULL_ADDRESS;
        Indexiv                   := NULL_ADDRESS;
        Indexs                    := NULL_ADDRESS;
        Indexsv                   := NULL_ADDRESS;
        Indexub                   := NULL_ADDRESS;
        Indexubv                  := NULL_ADDRESS;
        InitNames                 := NULL_ADDRESS;
        InterleavedArrays         := NULL_ADDRESS;
        IsEnabled                 := NULL_ADDRESS;
        IsList                    := NULL_ADDRESS;
        IsTexture                 := NULL_ADDRESS;
        LightModelf               := NULL_ADDRESS;
        LightModelfv              := NULL_ADDRESS;
        LightModeli               := NULL_ADDRESS;
        LightModeliv              := NULL_ADDRESS;
        Lightf                    := NULL_ADDRESS;
        Lightfv                   := NULL_ADDRESS;
        Lighti                    := NULL_ADDRESS;
        Lightiv                   := NULL_ADDRESS;
        LineStipple               := NULL_ADDRESS;
        LineWidth                 := NULL_ADDRESS;
        ListBase                  := NULL_ADDRESS;
        LoadIdentity              := NULL_ADDRESS;
        LoadMatrixd               := NULL_ADDRESS;
        LoadMatrixf               := NULL_ADDRESS;
        LoadName                  := NULL_ADDRESS;
        LogicOp                   := NULL_ADDRESS;
        Map1d                     := NULL_ADDRESS;
        Map1f                     := NULL_ADDRESS;
        Map2d                     := NULL_ADDRESS;
        Map2f                     := NULL_ADDRESS;
        MapGrid1d                 := NULL_ADDRESS;
        MapGrid1f                 := NULL_ADDRESS;
        MapGrid2d                 := NULL_ADDRESS;
        MapGrid2f                 := NULL_ADDRESS;
        Materialf                 := NULL_ADDRESS;
        Materialfv                := NULL_ADDRESS;
        Materiali                 := NULL_ADDRESS;
        Materialiv                := NULL_ADDRESS;
        MatrixMode                := NULL_ADDRESS;
        MultMatrixd               := NULL_ADDRESS;
        MultMatrixf               := NULL_ADDRESS;
        NewList                   := NULL_ADDRESS;
        Normal3b                  := NULL_ADDRESS;
        Normal3bv                 := NULL_ADDRESS;
        Normal3d                  := NULL_ADDRESS;
        Normal3dv                 := NULL_ADDRESS;
        Normal3f                  := NULL_ADDRESS;
        Normal3fv                 := NULL_ADDRESS;
        Normal3i                  := NULL_ADDRESS;
        Normal3iv                 := NULL_ADDRESS;
        Normal3s                  := NULL_ADDRESS;
        Normal3sv                 := NULL_ADDRESS;
        NormalPointer             := NULL_ADDRESS;
        Ortho                     := NULL_ADDRESS;
        PassThrough               := NULL_ADDRESS;
        PixelMapfv                := NULL_ADDRESS;
        PixelMapuiv               := NULL_ADDRESS;
        PixelMapusv               := NULL_ADDRESS;
        PixelStoref               := NULL_ADDRESS;
        PixelStorei               := NULL_ADDRESS;
        PixelTransferf            := NULL_ADDRESS;
        PixelTransferi            := NULL_ADDRESS;
        PixelZoom                 := NULL_ADDRESS;
        PointSize                 := NULL_ADDRESS;
        PolygonMode               := NULL_ADDRESS;
        PolygonOffset             := NULL_ADDRESS;
        PolygonStipple            := NULL_ADDRESS;
        PopAttrib                 := NULL_ADDRESS;
        PopClientAttrib           := NULL_ADDRESS;
        PopMatrix                 := NULL_ADDRESS;
        PopName                   := NULL_ADDRESS;
        PrioritizeTextures        := NULL_ADDRESS;
        PushAttrib                := NULL_ADDRESS;
        PushClientAttrib          := NULL_ADDRESS;
        PushMatrix                := NULL_ADDRESS;
        PushName                  := NULL_ADDRESS;
        RasterPos2d               := NULL_ADDRESS;
        RasterPos2dv              := NULL_ADDRESS;
        RasterPos2f               := NULL_ADDRESS;
        RasterPos2fv              := NULL_ADDRESS;
        RasterPos2i               := NULL_ADDRESS;
        RasterPos2iv              := NULL_ADDRESS;
        RasterPos2s               := NULL_ADDRESS;
        RasterPos2sv              := NULL_ADDRESS;
        RasterPos3d               := NULL_ADDRESS;
        RasterPos3dv              := NULL_ADDRESS;
        RasterPos3f               := NULL_ADDRESS;
        RasterPos3fv              := NULL_ADDRESS;
        RasterPos3i               := NULL_ADDRESS;
        RasterPos3iv              := NULL_ADDRESS;
        RasterPos3s               := NULL_ADDRESS;
        RasterPos3sv              := NULL_ADDRESS;
        RasterPos4d               := NULL_ADDRESS;
        RasterPos4dv              := NULL_ADDRESS;
        RasterPos4f               := NULL_ADDRESS;
        RasterPos4fv              := NULL_ADDRESS;
        RasterPos4i               := NULL_ADDRESS;
        RasterPos4iv              := NULL_ADDRESS;
        RasterPos4s               := NULL_ADDRESS;
        RasterPos4sv              := NULL_ADDRESS;
        ReadBuffer                := NULL_ADDRESS;
        ReadPixels                := NULL_ADDRESS;
        Rectd                     := NULL_ADDRESS;
        Rectdv                    := NULL_ADDRESS;
        Rectf                     := NULL_ADDRESS;
        Rectfv                    := NULL_ADDRESS;
        Recti                     := NULL_ADDRESS;
        Rectiv                    := NULL_ADDRESS;
        Rects                     := NULL_ADDRESS;
        Rectsv                    := NULL_ADDRESS;
        RenderMode                := NULL_ADDRESS;
        Rotated                   := NULL_ADDRESS;
        Rotatef                   := NULL_ADDRESS;
        Scaled                    := NULL_ADDRESS;
        Scalef                    := NULL_ADDRESS;
        Scissor                   := NULL_ADDRESS;
        SelectBuffer              := NULL_ADDRESS;
        ShadeModel                := NULL_ADDRESS;
        StencilFunc               := NULL_ADDRESS;
        StencilMask               := NULL_ADDRESS;
        StencilOp                 := NULL_ADDRESS;
        TexCoord1d                := NULL_ADDRESS;
        TexCoord1dv               := NULL_ADDRESS;
        TexCoord1f                := NULL_ADDRESS;
        TexCoord1fv               := NULL_ADDRESS;
        TexCoord1i                := NULL_ADDRESS;
        TexCoord1iv               := NULL_ADDRESS;
        TexCoord1s                := NULL_ADDRESS;
        TexCoord1sv               := NULL_ADDRESS;
        TexCoord2d                := NULL_ADDRESS;
        TexCoord2dv               := NULL_ADDRESS;
        TexCoord2f                := NULL_ADDRESS;
        TexCoord2fv               := NULL_ADDRESS;
        TexCoord2i                := NULL_ADDRESS;
        TexCoord2iv               := NULL_ADDRESS;
        TexCoord2s                := NULL_ADDRESS;
        TexCoord2sv               := NULL_ADDRESS;
        TexCoord3d                := NULL_ADDRESS;
        TexCoord3dv               := NULL_ADDRESS;
        TexCoord3f                := NULL_ADDRESS;
        TexCoord3fv               := NULL_ADDRESS;
        TexCoord3i                := NULL_ADDRESS;
        TexCoord3iv               := NULL_ADDRESS;
        TexCoord3s                := NULL_ADDRESS;
        TexCoord3sv               := NULL_ADDRESS;
        TexCoord4d                := NULL_ADDRESS;
        TexCoord4dv               := NULL_ADDRESS;
        TexCoord4f                := NULL_ADDRESS;
        TexCoord4fv               := NULL_ADDRESS;
        TexCoord4i                := NULL_ADDRESS;
        TexCoord4iv               := NULL_ADDRESS;
        TexCoord4s                := NULL_ADDRESS;
        TexCoord4sv               := NULL_ADDRESS;
        TexCoordPointer           := NULL_ADDRESS;
        TexEnvf                   := NULL_ADDRESS;
        TexEnvfv                  := NULL_ADDRESS;
        TexEnvi                   := NULL_ADDRESS;
        TexEnviv                  := NULL_ADDRESS;
        TexGend                   := NULL_ADDRESS;
        TexGendv                  := NULL_ADDRESS;
        TexGenf                   := NULL_ADDRESS;
        TexGenfv                  := NULL_ADDRESS;
        TexGeni                   := NULL_ADDRESS;
        TexGeniv                  := NULL_ADDRESS;
        TexImage1D                := NULL_ADDRESS;
        TexImage2D                := NULL_ADDRESS;
        TexParameterf             := NULL_ADDRESS;
        TexParameterfv            := NULL_ADDRESS;
        TexParameteri             := NULL_ADDRESS;
        TexParameteriv            := NULL_ADDRESS;
        TexSubImage1D             := NULL_ADDRESS;
        TexSubImage2D             := NULL_ADDRESS;
        Translated                := NULL_ADDRESS;
        Translatef                := NULL_ADDRESS;
        Vertex2d                  := NULL_ADDRESS;
        Vertex2dv                 := NULL_ADDRESS;
        Vertex2f                  := NULL_ADDRESS;
        Vertex2fv                 := NULL_ADDRESS;
        Vertex2i                  := NULL_ADDRESS;
        Vertex2iv                 := NULL_ADDRESS;
        Vertex2s                  := NULL_ADDRESS;
        Vertex2sv                 := NULL_ADDRESS;
        Vertex3d                  := NULL_ADDRESS;
        Vertex3dv                 := NULL_ADDRESS;
        Vertex3f                  := NULL_ADDRESS;
        Vertex3fv                 := NULL_ADDRESS;
        Vertex3i                  := NULL_ADDRESS;
        Vertex3iv                 := NULL_ADDRESS;
        Vertex3s                  := NULL_ADDRESS;
        Vertex3sv                 := NULL_ADDRESS;
        Vertex4d                  := NULL_ADDRESS;
        Vertex4dv                 := NULL_ADDRESS;
        Vertex4f                  := NULL_ADDRESS;
        Vertex4fv                 := NULL_ADDRESS;
        Vertex4i                  := NULL_ADDRESS;
        Vertex4iv                 := NULL_ADDRESS;
        Vertex4s                  := NULL_ADDRESS;
        Vertex4sv                 := NULL_ADDRESS;
        VertexPointer             := NULL_ADDRESS;
        Viewport                  := NULL_ADDRESS;
        ------------------------------------
        CopyContext              := NULL_ADDRESS;
        CreateContext            := NULL_ADDRESS;
        CreateLayerContext       := NULL_ADDRESS;
        DeleteContext            := NULL_ADDRESS;
        Describe_Layer_Plane       := NULL_ADDRESS;
        Get_Current_Context        := NULL_ADDRESS;
        Get_Current_DC             := NULL_ADDRESS;
        Get_LayerPalette_Entries   := NULL_ADDRESS;
        Get_Proc_Address           := NULL_ADDRESS;
        Make_Current              := NULL_ADDRESS;
        Realize_Layer_Palette      := NULL_ADDRESS;
        Set_Layer_Palette_Entries   := NULL_ADDRESS;
        Share_Lists               := NULL_ADDRESS;
        Swap_Layer_Buffers         := NULL_ADDRESS;
        Use_Font_Bitmaps           := NULL_ADDRESS;
        Use_Font_Out_lines          := NULL_ADDRESS;
        ------------------------------------
        Choose_Pixel_Format        := NULL_ADDRESS;
        Describe_Pixel_Format      := NULL_ADDRESS;
        Get_Pixel_Format           := NULL_ADDRESS;
        Set_Pixel_Format           := NULL_ADDRESS;
        Swap_Buffers              := NULL_ADDRESS;
      end Finalize;
  end Implementation;
