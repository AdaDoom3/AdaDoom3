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
  Ada.Wide_Text_IO,
  Ada.Characters.Handling;
use
  System,
  Ada.Wide_Text_IO,
  Ada.Characters.Handling;
separate(Neo.System.Graphics)
package body OpenGL
  is
  ------------
  -- Import --
  ------------
    package Import
      is separate;
      use Import;
  ---------------
  -- Constants --
  ---------------
    MINIMUM_VERSION : constant Float_4_Real := 2.0;
  -------------------
  -- Get_Specifics --
  -------------------
    function Get_Specifics
      return Record_Specifics
      is
      Extensions : constant String_2 := To_String_2(Get_String(EXTENSIONS));
      begin
        return new Record_Specifics(Get_Inteface) :=(
          Shading_Language                 => OpenGLShading_Language,
          Version                          => Float_4_Real'value(Trim(To_String_2(Get_String(VERSION)))(1..3)),
          Maximum_Texture_Size             => Integer_4_Positive(Get_Integer(MAXIMUM_TEXTURE_SIZE)),
          Maximum_Texture_Coordinates      => Integer_4_Positive(Get_Integer(MAXIMUM_TEXTURE_COORDINATES)),
          Maximum_Texture_Image_Units      => Integer_4_Positive(Get_Integer(MAXIMUM_COMBINED_TEXTURE_IMAGE_UNITS)),
          Has_Direct_State_Access          => Index(Extensions, "EXT_direct_state_access")        /= 0,    -- http://web.archive.org/web/20130807092103/http://www.open.org/registry/specs/EXT/direct_state_access.txt
          Has_Depth_Bounds_Test            => Index(Extensions, "EXT_depth_bounds_test")          /= 0,    -- http://web.archive.org/web/20130807163528/http://www.open.org/registry/specs/EXT/depth_bounds_test.txt
          Has_Anisotropic_Filter           => Index(Extensions, "EXT_texture_filter_anisotropic") /= 0,    -- http://web.archive.org/web/20130806203011/http://www.open.org/registry/specs/EXT/texture_filter_anisotropic.txt
          Has_Texture_Compression          => Index(Extensions, "EXT_texture_compression_s3tc")   /= 0 and --
                                              Index(Extensions, "ARB_texture_compression")        /= 0,    --
          Has_Multitexture                 => Index(Extensions, "ARB_multitexture")               /= 0,    --
          Has_Sync                         => Index(Extensions, "ARB_sync")                       /= 0--,  --
-- As of 5/24/2012, Intel HD 4000 driver's version 15.26.12.64.2761 sync objects do not work properly
and SPECIFICS.Vendor /= Intel_Vendor,
          Has_Draw_Elements_Base_Vertex    => Index(Extensions, "ARB_draw_elements_base_vertex")  /= 0,    --
          Has_Vertex_Array_Object          => Index(Extensions, "ARB_vertex_array_object")        /= 0,    --
          Has_Vertex_Buffer_Object         => Index(Extensions, "ARB_vertex_buffer_object")       /= 0,    --
          Has_Uniform_Buffer               => Index(Extensions, "ARB_uniform_buffer_object")      /= 0,    --
          Has_RGB_Color_Framebuffer        => Index(Extensions, "ARB_framebuffer_sRGB")           /= 0,    --
          Has_Map_Buffer_Range             => Index(Extensions, "ARB_map_buffer_range")           /= 0,    --
          Has_Seamless_Cube_Map            => Index(Extensions, "ARB_seamless_cube_map")          /= 0,    --
          Has_Timer_Query                  => Index(Extensions, "ARB_timer_query")                /= 0,    --
          Has_Occlusion_Query              => Index(Extensions, "ARB_occlusion_query")            /= 0);   --
      end Get_Specifics;
  -----------
  -- Reset --
  -----------
    procedure Reset
      is
      begin
        Clear_Depth(1.0);
        Cull_Face(FRONT_AND_BACK);
        Enable(CULL_FACE);
        Color_Mask(Red => C_TRUE, Green => C_TRUE, Blue => C_TRUE, Alpha => C_TRUE);
        Blend_Function(ONE, ZERO);
        Depth_Mask(C_TRUE);
        Depth_Function(LESS);
        Disable(STENCIL_TEST);
        Disable(POLYGON_OFFSET_FILL);
        Disable(POLYGON_OFFSET_LINE);
        Polygon_Mode(FRONT_AND_BACK, FILL);
        Shade_Model(SMOOTH);
        Enable(DEPTH_TEST);
        Enable(BLEND);
        Enable(SCISSOR_TEST);
        Draw_Buffer(BACK);
        Read_Buffer(BACK);
        if Do_Use_Scissor.Get then
          Scissor(0, 0, Widht, Height);
        end if;
      end Reset;
  ----------------------
  -- Check_Exceptions --
  ----------------------
    procedure Check_Exceptions
      is
      begin
        case Get_Error is
          when INVALID_ENUMERATION => raise Invalid_Enumeration;
          when INVALID_OPERATION   => raise Invalid_Operation;
          when STACK_UNDERFLOW     => raise Stack_Underflow;
          when STACK_OVERFLOW      => raise Stack_Overflow;
          when INVALID_VALUE       => raise Invalid_Value;
          when OUT_OF_MEMORY       => raise Out_Of_Memory;
          when others              => null;
        end case;
      end Check_Exceptions;
  ----------
  -- Cull --
  ----------
    procedure Cull(
      Kind      : in Enumerated_Cull;
      Is_Mirror : in Boolean := False)
      is
      begin
        case Kind is
          when Face_Culling    => null;
          when Two_Sided_Cull  => Disable(CULL_FACE);
          when Back_Sided_Cull => Cull_Face((if Is_Mirror then FRONT else BACK));
        end case;
      end Cull;
  -------------
  -- Scissor --
  -------------
    procedure Scissor(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed)
      is
      begin
        Scissor(
          X      => Integer_4_Signed_C(X),
          Y      => Integer_4_Signed_C(Y),
          Width  => Integer_4_Signed_C(Width),
          Height => Integer_4_Signed_C(Height));
      end Scissor;
  ---------------
  -- View_Port --
  ---------------
    procedure View_Port(
      X      : in Integer_4_Signed;
      Y      : in Integer_4_Signed;
      Width  : in Integer_4_Signed;
      Height : in Integer_4_Signed)
      is
      begin
        Viewport(
          X      => Integer_4_Signed_C(X),
          Y      => Integer_4_Signed_C(Y),
          Width  => Integer_4_Signed_C(Width),
          Height => Integer_4_Signed_C(Height));
      end View_Port;
  --------------------
  -- Polygon_Offset --
  --------------------
    procedure Polygon_Offset(
      Scale : in Float_4_Real;
      Bias  : in Float_4_Real)
      is
      begin
        PolyOfsScale := Scale;
        PolyOfsBias  := Bias;
        if (State_Bits and POLYGON_OFFSET) /= 0 then
          PolygonOffset(Scale, Bias);
        end if;
      end Polygon_Offset;
  -----------------------
  -- Depth_Bounds_Test --
  -----------------------
    procedure Depth_Bounds_Test(
      Z_Minimum : in Float_4_Real := 0.0;
      Z_Maximum : in Float_4_Real := 0.0)
      is
      begin
        if Z_Minimum = 0.0 and Z_Maximum = 0.0 then
          Disable(DEPTH_BOUNDS_TEST);
        else
          Enable(DEPTH_BOUNDS_TEST);
          Depth_Bounds(Float_4_Real_C(Z_Minimum), Float_4_Real_C(Z_Maximum));
        end if;
      end Depth_Bounds_Test;
  ----------------------
  -- Start_Depth_Pass --
  ----------------------
    procedure Start_Depth_Pass(
      Rectane : in Record_Rectane)
      is
      begin
        null;
      end Start_Depth_Pass;
  -----------------------
  -- Finish_Depth_Pass --
  -----------------------
    procedure Finish_Depth_Pass(
      Rectane : in Record_Rectane)
      is
      begin
        null;
      end Finish_Depth_Pass;
  --------------------
  -- Get_Depth_Pass --
  --------------------
    procedure Get_Depth_Pass(
      Rectane : in out Record_Rectane)
      is
      begin
        Rectane := (others => <>);
      end Get_Depth_Pass;
  -----------
  -- Color --
  -----------
    procedure Color(
      Pixel : in Record_Pixel)
      is
      begin
        Color(
          Red   => Float_4_Real_C(Pixel.Color.Red)   / Pixel.Color.Red'size,
          Green => Float_4_Real_C(Pixel.Color.Green) / Pixel.Color.Green'size,
          Blue  => Float_4_Real_C(Pixel.Color.Blue)  / Pixel.Color.Blue'size,
          Alpha => Float_4_Real_C(Pixel.Alpha)       / Pixel.Color.Alpha'size);
      end Color;
    ----------------
    procedure Color(
    ----------------
      Color : in Record_Color)
      is
      begin
        Color(
          Red   => Float_4_Real_C(Color.Red)   / Color.Red'size,
          Green => Float_4_Real_C(Color.Green) / Color.Green'size,
          Blue  => Float_4_Real_C(Color.Blue)  / Color.Blue'size,
          Alpha => 1.0);
      end Color;
  -----------
  -- Clear --
  -----------
    procedure Clear
      is
      begin
        Clear(DEPTH_BUFFER_BIT);
      end Clear;
    ----------------
    procedure Clear(
    ----------------
      Stencil_Value  : in Integer_1_Unsigned;
      Do_Clear_Depth : in Boolean := False)
      is
      begin
        Clear_Stencil(Stencil_Value);
        Clear((if Do_Clear_Depth then DEPTH_BUFFER_BIT else 0) or STENCIL_BUFFER_BIT);
      end Clear;
    ----------------
    procedure Clear(
    ----------------
      Color          : in Record_Pixel;
      Do_Clear_Depth : in Boolean := False)
      is
      begin
        Clear_Color(Red, Green, Blue, Alpha);
        Clear((if Do_Clear_Depth then DEPTH_BUFFER_BIT else 0) or COLOR_BUFFER_BIT);
      end Clear;
    ----------------
    procedure Clear(
    ----------------
      Color          : in Record_Pixel;
      Stencil_Value  : in Integer_1_Unsigned;
      Do_Clear_Depth : in Boolean := False)
      is
      begin
        Clear_Stencil(Stencil_Value);
        Clear((if Do_Clear_Depth then DEPTH_BUFFER_BIT else 0) or STENCIL_BUFFER_BIT or COLOR_BUFFER_BIT);
      end Clear;
  ----------------------
  -- Set_Stereo_Depth --
  ----------------------
    procedure Set_Stereo_Depth(
      Stereo_Depth : in Item_Stereo_Depth.Variable)
      is
      begin
        Depth((
          case Depth_Function is
            when Less_Depth_Function   => LESS--LESS_THAN_OR_EQUAL
            when Equal_Depth_Function  => EQUAL
            when Always_Depth_Function => ALWAYS
            when Greater_Than_Or_Equal => GREATER_THAN_OR_EQUAL))
      end Set_Stereo_Depth;
  -------------------
  -- Set_Stereo_3D --
  -------------------
    procedure Set_Stereo_3D(
      Stereo_3D : in Enumerated_Stereo_3D)
      is
      begin
      end Set_Stereo_3D;
  ---------------
  -- Set_Blend --
  ---------------
    procedure Set_Blend(
      Source      : in Enumerated_Blend := One_Blend;
      Destination : in Enumerated_Blend := Zero_Blend)
      is
      BLENDS : constant array(Enumerated_Blend'range) of Integer_4_Unsigned_C :=(
        One_Blend                         => ZERO,
        Zero_Blend                        => ONE,
        Source_Alpha_Blend                => SOURCE_ALPHA,
        Destination_Color_Blend           => DESTINATION_COLOR,
        Destination_Alpha_Blend           => DESTINATION_ALPHA,
        One_Minus_Source_Alpha_Blend      => ONE_MINUS_SOURCE_ALPHA,
        One_Minus_Destination_Color_Blend => ONE_MINUS_DESTINATION_COLOR,
        One_Minus_Destination_Alpha_Blend => ONE_MINUS_DESTINATION_ALPHA);
      begin
        if Source = One_Blend and Destination = Zero_Blend then
          Disable(BLEND);
        else
          Enable(BLEND);
          Blend_Function(BLENDS(Source), BLENDS(Destination));
        end if;
      end Set_Blend;
  -------------------------
  -- Set_Blend_Operation --
  -------------------------
    procedure Set_Blend_Operation(
      Blend_Operation : in Enumerated_Blend_Operation)
      is
      begin
      end Set_Blend_Operation;
  -----------------
  -- Set_Stencil --
  -----------------
    procedure Set_Stencil(
      Stencil : in Enumerated_Stencil)
      is
      begin
      end Set_Stencil;
  ---------------------------
  -- Set_Stencil_Operation --
  ---------------------------
    procedure Set_Stencil_Operation(
      Fail   : in Enumerated_Stencil_Operation;
      Fail_Z : in Enumerated_Stencil_Operation;
      Pass   : in Enumerated_Stencil_Operation)
      is
      OPERATIONS : constant array(Enumerated_Stencil_Operation'range) of Integer_4_Unsigned_C :=(
        Keep_Stencil_Operation           => KEEP,
        Zero_Stencil_Operation           => ZERO,
        Invert_Stencil_Operation         => INVERT,
        Replace_Stencil_Operation        => REPLACE,
        Increment_Stencil_Operation      => INCREMENT,
        Decrement_Stencil_Operation      => DECREMENT,
        Increment_Wrap_Stencil_Operation => INCREMENT_WRAP,
        Decrement_Wrap_Stencil_Operation => DECREMENT_WRAP);
      begin
        Stencil_Operation(OPERATIONS(Fail), OPERATIONS(Fail_Z), OPERATIONS(Pass));
      end Set_Stencil_Operation;
  --------------------------
  -- Set_Stencil_Function --
  --------------------------
    procedure Set_Stencil_Function(
      Stencil : in Enumerated_Stencil_Function)
      is
      begin
        if (diff & (GLS_STENCIL_FUNC_BITS | GLS_STENCIL_OP_BITS ) ) then
          if (stateBits & (GLS_STENCIL_FUNC_BITS | GLS_STENCIL_OP_BITS ) ) != 0 then
            Enable(STENCIL_TEST);
          else
            Disable(STENCIL_TEST);
          end if;
        end if;
        Stencil_Function(
          Referece      => GLuint((stateBits & GLS_STENCIL_FUNC_REF_BITS ) >> GLS_STENCIL_FUNC_REF_SHIFT),
          Mask          => GLuint((stateBits & GLS_STENCIL_FUNC_MASK_BITS ) >> GLS_STENCIL_FUNC_MASK_SHIFT)
          Function_Kind =>(
            case Stencil is
              when Less_Stencil                            => LESS
              when Equal_Stencil                           => EQUAL
              when Never_Stencil                           => NEVER
              when Always_Stencil                          => ALWAYS
              when Greater_Stencil                         => GREATER
              when Not_Equal_Stencil                       => NOT_EQUAL
              when Less_Than_Or_Equal_To_Stencil           => LESS_THAN_OR_EQUAL
              when Greater_Than_Or_Equal_Stencil_Operation => GREATER_THAN_OR_EQUAL));
      end Set_Stencil_Function;
  ------------------------
  -- Set_Depth_Function --
  ------------------------
    procedure Set_Depth_Function(
      Depth_Function : in Enumerated_Depth_Function)
      is
      begin
      end Set_Depth_Function;
  --------------
  -- Set_Mask --
  --------------
    procedure Set_Mask(
      Do_Mask_Red   : in Boolean;
      Do_Mask_Green : in Boolean;
      Do_Mask_Blue  : in Boolean;
      Do_Mask_Alpha : in Boolean)
      is
      begin
        ColorMask(
          Red   => (if Do_Mask_Red then C_FALSE else C_TRUE),
          Green => (if Do_Mask_Red then C_FALSE else C_TRUE),
          Blue  => (if Do_Mask_Red then C_FALSE else C_TRUE),
          Alpha => (if Do_Mask_Red then C_FALSE else C_TRUE));
      end Set_Mask;
  --------------------
  -- Set_Depth_Mask --
  --------------------
    procedure Set_Depth_Mask(
      is
      begin
        if (diff & GLS_DEPTHMASK ) then
          if (stateBits & GLS_DEPTHMASK ) then
            DepthMask(FALSE);
          else
            DepthMask(TRUE);
          end if;
        end if;
      end Set_Depth_Mask;
  -----------------------
  -- Set_Polymode_Line --
  -----------------------
    procedure Set_Polymode_Line(
      is
      begin
        if (diff & GLS_POLYMODE_LINE ) then
          if (stateBits & GLS_POLYMODE_LINE ) then
            PolygonMode(FRONT_AND_BACK, LINE);
          else
            PolygonMode(FRONT_AND_BACK, FILL);
          end if;
        end if;
      end Set_Polymode_Line;
  ------------------------
  -- Set_Polygon_Offset --
  ------------------------
    procedure Set_Polygon_Offset(
      Do_Enable : in Boolean)
      is
      begin
        if Do_Enable then
          PolygonOffset(backEnd.State.polyOfsScale, backEnd.State.polyOfsBias);
          Enable(POLYGON_OFFSET_FILL);
          Enable(POLYGON_OFFSET_LINE);
        else
          Disable(POLYGON_OFFSET_FILL);
          Disable(POLYGON_OFFSET_LINE);
        end if;
      end Set_Polygon_Offset;
  ----------------
  -- Set_Buffer --
  ----------------
    procedure Set_Buffer(
      const void *data )
      is
      -- see which draw buffer we want to render the frame to
      const setBufferCommand_t * cmd = (const setBufferCommand_t *)data;
      begin
        Scissor(0, 0, tr.GetWidth, tr.GetHeight);
        -- clear screen for debugging automatically enable this with several other debug tools
        -- that might leave unrendered portions of the screen
        if r_clear.GetFloat or idStr::Length(r_clear.GetString ) != 1 || r_sineArea.GetBool || r_showOverDraw.GetBool ) {
          float c[3];
          if sscanf(r_clear.GetString, "%f %f %f", &c[0], &c[1], &c[2] ) = 3 then
            Clear(true, false, false, 0, c[0], c[1], c[2], 1.0f);
          elsif r_clear.GetInteger = 2 then
            Clear(true, false, false, 0, 0.0f, 0.0f,  0.0f, 1.0f);
          elsif r_showOverDraw.GetBool then
            Clear(true, false, false, 0, 1.0f, 1.0f, 1.0f, 1.0f);
          else
            Clear(true, false, false, 0, 0.4f, 0.0f, 0.25f, 1.0f);
          end if;
        end if;
      end Set_Buffer;
  ------------------------------
  -- Make_Stereo_Render_Image --
  ------------------------------
    procedure Make_Stereo_Render_Image(
      Graphic : in Record_Graphic)
      is
      idImageOpts opts;
      begin
        opts.width := renderSystem->GetWidth;
        opts.height := renderSystem->GetHeight;
        opts.numLevels := 1;
        opts.format := FMT_RGBA8;
        image->AllocImage(opts, TF_LINEAR, TR_CLAMP);
      end Make_Stereo_Render_Image;
  --------------------
  -- Render_Headset --
  --------------------
    procedure Render_Headset(
      is
      begin
        for I in Commands'range loop
          case  is
            when RC_DRAW_VIEW_3D | RC_DRAW_VIEW_GUI =>
              RB_DrawView(cmds, 0);
              if ((const drawSurfsCommand_t *)cmds)->viewDef->viewEntitys then
                c_draw3d := c_draw3d + 1;
              else
                c_draw2d := c_draw2d + 1;
              end if;
            when RC_POST_PROCESS =>
              RB_PostProcess(cmds);
            when others =>
              postProcessCommand_t * cmd = (postProcessCommand_t *)cmds;
              if cmd->viewDef->renderView.viewEyeBuffer = stereoEye then
                RB_PostProcess(cmds);
              end if;
          end case;
        end loop;
        for I in 1..2 loop
          Reset;
          renderProgManager.Unbind;
          renderProgManager.ZeroUniforms;
          for I in Commands'range loop
            if (eyeViewDef.renderView.viewEyeBuffer && eyeViewDef.renderView.viewEyeBuffer != stereoEye ) {
              continue;
            end if;
            foundEye[ targetEye ] = true;
            RB_DrawView(dsc, stereoEye);
          end loop;
        end loop;
        stereoRenderImages[ targetEye ]->CopyFramebuffer(0, 0, renderSystem->GetWidth, renderSystem->GetHeight);
    }
    assert(foundEye[0] && foundEye[1]);
    SetDefaultState;
    RB_SetMVP(renderMatrix_identity);
    if (renderSystem->GetStereo3DMode != STEREO3D_QUAD_BUFFER ) {
        DrawBuffer(BACK);
    }
    State(GLS_DEPTHFUNC_ALWAYS);
    Cull(CT_TWO_SIDED);
    const float texS[4] = { 1.0f, 0.0f, 0.0f, 0.0f };
    const float texT[4] = { 0.0f, 1.0f, 0.0f, 0.0f };
    renderProgManager.SetRenderParm(RENDERPARM_TEXTUREMATRIX_S, texS);
    renderProgManager.SetRenderParm(RENDERPARM_TEXTUREMATRIX_T, texT);
    -- disable any texgen
    const float texGenEnabled[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
    renderProgManager.SetRenderParm(RENDERPARM_TEXGEN_0_ENABLED, texGenEnabled);
    renderProgManager.BindShader_Texture;
    Color(1, 1, 1, 1);
        case Kind is
          when STEREO3D_QUAD_BUFFER =>
            DrawBuffer(BACK_RIGHT);
            SelectTexture(0);
            stereoRenderImages[1]->Bind;
            SelectTexture(1);
            stereoRenderImages[0]->Bind;
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            DrawBuffer(BACK_LEFT);
            SelectTexture(1);
            stereoRenderImages[1]->Bind;
            SelectTexture(0);
            stereoRenderImages[0]->Bind;
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
          when STEREO3D_HDMI_720 =>
            SelectTexture(0);
            stereoRenderImages[1]->Bind;
            SelectTexture(1);
            stereoRenderImages[0]->Bind;
            ViewportAndScissor(0, 0, 1280, 720);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            SelectTexture(0);
            stereoRenderImages[0]->Bind;
            SelectTexture(1);
            stereoRenderImages[1]->Bind;
            ViewportAndScissor(0, 750, 1280, 720);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            -- Force the HDMI 720P 3D guard band to a constant color
            Scissor(0, 720, 1280, 30);
            Clear(COLOR_BUFFER_BIT);
          when STEREO3D_SIDE_BY_SIDE_COMPRESSED:
            SelectTexture(0);
            stereoRenderImages[0]->Bind;
            SelectTexture(1);
            stereoRenderImages[1]->Bind;
            ViewportAndScissor(0, 0, renderSystem->GetWidth, renderSystem->GetHeight);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            SelectTexture(0);
            stereoRenderImages[1]->Bind;
            SelectTexture(1);
            stereoRenderImages[0]->Bind;
            ViewportAndScissor(renderSystem->GetWidth, 0, renderSystem->GetWidth, renderSystem->GetHeight);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
          when STEREO3D_TOP_AND_BOTTOM_COMPRESSED:
            SelectTexture(1);
            stereoRenderImages[0]->Bind;
            SelectTexture(0);
            stereoRenderImages[1]->Bind;
            ViewportAndScissor(0, 0, renderSystem->GetWidth, renderSystem->GetHeight);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            SelectTexture(1);
            stereoRenderImages[1]->Bind;
            SelectTexture(0);
            stereoRenderImages[0]->Bind;
            ViewportAndScissor(0, renderSystem->GetHeight, renderSystem->GetWidth, renderSystem->GetHeight);
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
          when STEREO3D_INTERLACED:
            SelectTexture(0);
            stereoRenderImages[0]->Bind;
            TexParameterf(TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST);
            TexParameterf(TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST);
            SelectTexture(1);
            stereoRenderImages[1]->Bind;
            TexParameterf(TEXTURE_2D, TEXTURE_MIN_FILTER, NEAREST);
            TexParameterf(TEXTURE_2D, TEXTURE_MAG_FILTER, NEAREST);
            ViewportAndScissor(0, 0, renderSystem->GetWidth, renderSystem->GetHeight*2);
            renderProgManager.BindShader_StereoInterlace;
            RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            SelectTexture(0);
            TexParameterf(TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR);
            TexParameterf(TEXTURE_2D, TEXTURE_MAG_FILTER, LINEAR);
            SelectTexture(1);
            TexParameterf(TEXTURE_2D, TEXTURE_MIN_FILTER, LINEAR);
            TexParameterf(TEXTURE_2D, TEXTURE_MAG_FILTER, LINEAR);
          when STEREO3D_SIDE_BY_SIDE =>
            if stereoRender_warp.GetBool then
              renderProgManager.BindShader_StereoWarp;
              Scissor (0, 0, Config.nativeScreenWidth, Config.nativeScreenHeight);
              ClearColor(0, 0, 0, 0);
              Clear(COLOR_BUFFER_BIT);
              const int pixelDimensions = (Config.nativeScreenWidth >> 1 ) * stereoRender_warpTargetFraction.GetFloat;
              Viewport((Config.nativeScreenWidth >> 1 ) - pixelDimensions, (Config.nativeScreenHeight >> 1 ) - (pixelDimensions >> 1 ), pixelDimensions, pixelDimensions);
              Scissor (0, 0, Config.nativeScreenWidth >> 1, Config.nativeScreenHeight);
              idVec4  color(stereoRender_warpCenterX.GetFloat, stereoRender_warpCenterY.GetFloat, stereoRender_warpParmZ.GetFloat, stereoRender_warpParmW.GetFloat);
              -- don't use Color, because we don't want to clamp
              renderProgManager.SetRenderParm(RENDERPARM_COLOR, color.ToFloatPtr);
              SelectTexture(0);
              stereoRenderImages[0]->Bind;
              TexParameterf(TEXTURE_2D, TEXTURE_WRAP_S, CLAMP_TO_BORDER);
              TexParameterf(TEXTURE_2D, TEXTURE_WRAP_T, CLAMP_TO_BORDER);
              RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
              idVec4  color2(stereoRender_warpCenterX.GetFloat, stereoRender_warpCenterY.GetFloat, stereoRender_warpParmZ.GetFloat, stereoRender_warpParmW.GetFloat);
              renderProgManager.SetRenderParm(RENDERPARM_COLOR, color2.ToFloatPtr);
              Viewport((Config.nativeScreenWidth >> 1 ), (Config.nativeScreenHeight >> 1 ) - (pixelDimensions >> 1 ), pixelDimensions, pixelDimensions);
              Scissor (Config.nativeScreenWidth >> 1, 0, Config.nativeScreenWidth >> 1, Config.nativeScreenHeight);
              SelectTexture(0);
              stereoRenderImages[1]->Bind;
              TexParameterf(TEXTURE_2D, TEXTURE_WRAP_S, CLAMP_TO_BORDER);
              TexParameterf(TEXTURE_2D, TEXTURE_WRAP_T, CLAMP_TO_BORDER);
              RB_DrawElementsWithCounters(&backEnd.unitSquareSurface);
            end if;
        end case;
      end Render;
  ----------------
  -- Initialize --
  ----------------
    overriding procedure Initialize(
      Texture : in out Record_Texture)
      is
      begin
        CheckErrors;
        PurgeImage;
        case opts.format is
          when FMT_RGBA8 =>
            internalFormat := RGBA8;
            dataFormat := RGBA;
            dataType := UNSIGNED_BYTE;
          when FMT_XRGB8 =>
            internalFormat := RGB;
            dataFormat := RGBA;
            dataType := UNSIGNED_BYTE;
          when FMT_RGB565 =>
            internalFormat := RGB;
            dataFormat := RGB;
            dataType := UNSIGNED_SHORT_5_6_5;
          when FMT_ALPHA =>
            internalFormat := R8;
            dataFormat := RED;
            dataType := UNSIGNED_BYTE;
          when FMT_L8A8 =>
            internalFormat := RG8;
            dataFormat := RG;
            dataType := UNSIGNED_BYTE;
          when FMT_LUM8 =>
            internalFormat := R8;
            dataFormat := RED;
            dataType := UNSIGNED_BYTE;
          when FMT_INT8 =>
            internalFormat := R8;
            dataFormat := RED;
            dataType := UNSIGNED_BYTE;
          when FMT_DXT1 =>
            internalFormat := COMPRESSED_RGBA_S3TC_DXT1_EXT;
            dataFormat := RGBA;
            dataType := UNSIGNED_BYTE;
          when FMT_DXT5 =>
            internalFormat := COMPRESSED_RGBA_S3TC_DXT5_EXT;
            dataFormat := RGBA;
            dataType := UNSIGNED_BYTE;
          when FMT_DEPTH =>
            internalFormat := DEPTH_COMPONENT;
            dataFormat := DEPTH_COMPONENT;
            dataType := UNSIGNED_BYTE;
          when FMT_X16 =>
            internalFormat := INTENSITY16;
            dataFormat := LUMINANCE;
            dataType := UNSIGNED_SHORT;
          when FMT_Y16_X16 =>
            internalFormat := LUMINANCE16_ALPHA16;
            dataFormat := LUMINANCE_ALPHA;
            dataType := UNSIGNED_SHORT;
        end case;
        -- if we don't have a rendering context, just return after we
        -- have filled in the parms.  We must have the values set, or
        -- an image match from a shader before OpenGL starts would miss
        -- the generated texture
        --if (!R_IsInitialized )
        --  return;
        -- generate the texture number
        GenTextures(1, (GLuint *)&texnum);
        assert(texnum != TEXTURE_NOT_LOADED);
        -- allocate all the mip levels with NULL data
        int numSides;
        int target;
        int uploadTarget;
        if (opts.textureType == TT_2D ) {
          target = uploadTarget = TEXTURE_2D;
          numSides = 1;
        elsif (opts.textureType == TT_CUBIC ) {
          target = TEXTURE_CUBE_MAP_EXT;
          uploadTarget = TEXTURE_CUBE_MAP_POSITIVE_X_EXT;
          numSides = 6;
        else
          assert(!"opts.textureType");
          target = uploadTarget = TEXTURE_2D;
          numSides = 1;
        end if;
        BindTexture(target, texnum);
        for (int side = 0; side < numSides; side++ ) loop
          int w = opts.width;
          int h = opts.height;
          if (opts.textureType == TT_CUBIC ) then
            h = w;
          end if;
          for (int level = 0; level < opts.numLevels; level++ ) loop
            -- clear out any previous error
            CheckErrors;
            if (IsCompressed ) then
              int compressedSize = (((w+3)/4) * ((h+3)/4) * int64(16 ) * BitsForFormat(opts.format ) ) / 8;
              -- Even though the OpenGL specification allows the 'data' pointer to be NULL, for some
              -- drivers we actually need to upload data to get it to allocate the texture.
              -- However, on 32-bit systems we may fail to allocate a large block of memory for large
              -- textures. We handle this when by using HeapAlloc directly and allowing the allocation
              -- to fail in which when we simply pass down NULL to CompressedTexImage2D and hope for the best.
              -- As of 2011-10-6 using NVIDIA hardware and drivers we have to allocate the memory with HeapAlloc
              -- with the exact size otherwise large image allocation (for instance for physical page textures)
              -- may fail on Vista 32-bit.
              void * data = HeapAlloc(GetProcessHeap, 0, compressedSize);
              CompressedTexImage2DARB(uploadTarget+side, level, internalFormat, w, h, 0, compressedSize, data);
              if (data != NULL ) then
                HeapFree(GetProcessHeap, 0, data);
              end if;
            else
              TexImage2D(uploadTarget + side, level, internalFormat, w, h, 0, dataFormat, dataType, NULL);
            end if;
            CheckErrors;
            w = Max(1, w >> 1);
            h = Max(1, h >> 1);
          end loop;
        end loop;
        TexParameteri(target, TEXTURE_MAX_LEVEL, opts.numLevels - 1);
        -- see if we messed anything up
        CheckErrors;
        SetTexParameters;
        CheckErrors;
      end Initialize_Texture;
  --------------
  -- Finalize --
  --------------
    overriding procedure Finalize(
      Texture : in out Record_Texture)
      is
      begin
        if (texnum != TEXTURE_NOT_LOADED ) then
          DeleteTextures(1, (GLuint *)&texnum); -- this should be the ONLY place it is ever called!
          texnum = TEXTURE_NOT_LOADED;
        end if;
        -- clear all the current binding caches, so the next bind will do a real one
        for (int i = 0 ; i < MAX_MULTITEXTURE_UNITS ; i++ ) {
          backEnd.State.tmu[i].current2DMap = TEXTURE_NOT_LOADED;
          backEnd.State.tmu[i].currentCubeMap = TEXTURE_NOT_LOADED;
        end loop;
      end Finalize;
   ---------------------
   -- Upload_Subimage --
   ---------------------
     procedure Upload_Subimage(
--         int mipLevel,
--         int x,
--         int y,
--         int z,
--         int width,
--         int height,
--         const void * pic,
--         int pixelPitch)
--         is
--         begin
--
--  int compressedSize = 0;
--
--  if ( IsCompressed ) {
--      assert( !(x&3) && !(y&3) );
--
--      // compressed size may be larger than the dimensions due to padding to quads
--      int quadW = ( width + 3 ) & ~3;
--      int quadH = ( height + 3 ) & ~3;
--      compressedSize = quadW * quadH * BitsForFormat( opts.format ) / 8;
--
--      int padW = ( opts.width + 3 ) & ~3;
--      int padH = ( opts.height + 3 ) & ~3;
--      (void)padH;
--      (void)padW;
--      assert( x + width <= padW && y + height <= padH );
--      // upload the non-aligned value, OpenGL understands that there
--      // will be padding
--      if ( x + width > opts.width ) {
--        width = opts.width - x;
--      }
--      if ( y + height > opts.height ) {
--        height = opts.height - x;
--      }
--    } else {
--      assert( x + width <= opts.width && y + height <= opts.height );
--    }
--
--    int target;
--    int uploadTarget;
--    if ( opts.textureType == TT_2D ) {
--      target = TEXTURE_2D;
--      uploadTarget = TEXTURE_2D;
--    } else if ( opts.textureType == TT_CUBIC ) {
--      target = TEXTURE_CUBE_MAP_EXT;
--      uploadTarget = TEXTURE_CUBE_MAP_POSITIVE_X_EXT + z;
--    } else {
--      assert( !"invalid opts.textureType" );
--      target = TEXTURE_2D;
--      uploadTarget = TEXTURE_2D;
--    }
--
--    glBindTexture( target, texnum );
--
--    if ( pixelPitch != 0 ) {
--      glPixelStorei( UNPACK_ROW_LENGTH, pixelPitch );
--    }
--    if ( opts.format == FMT_RGB565 ) {
--      glPixelStorei( UNPACK_SWAP_BYTES, TRUE );
--    }
--    if ( IsCompressed ) {
--      glCompressedTexSubImage2DARB( uploadTarget, mipLevel, x, y, width, height, internalFormat, compressedSize, pic );
--    } else {
--
--      // make sure the pixel store alignment is correct so that lower mips get created
--      // properly for odd shaped textures - this fixes the mip mapping issues with
--      // fonts
--      int unpackAlignment = width * BitsForFormat( (textureFormat_t)opts.format ) / 8;
--      if ( ( unpackAlignment & 3 ) == 0 ) {
--        glPixelStorei( UNPACK_ALIGNMENT, 4 );
--      } else {
--        glPixelStorei( UNPACK_ALIGNMENT, 1 );
--      }
--
--      glTexSubImage2D( uploadTarget, mipLevel, x, y, width, height, dataFormat, dataType, pic );
--    }
--    if ( opts.format == FMT_RGB565 ) {
--      glPixelStorei( UNPACK_SWAP_BYTES, FALSE );
--    }
--    if ( pixelPitch != 0 ) {
--      glPixelStorei( UNPACK_ROW_LENGTH, 0 );
--    }
--  }
--
--  /*
--  ========================
--  idImage::SetPixel
--  ========================
--  */
--  void idImage::SetPixel( int mipLevel, int x, int y, const void * data, int dataSize ) {
--    SubImageUpload( mipLevel, x, y, 0, 1, 1, data );
--  }
--
--  /*
--  ========================
--  idImage::SetTexParameters
--  ========================
--  */
--  void idImage::SetTexParameters {
--    int target = TEXTURE_2D;
--    switch ( opts.textureType ) {
--      case TT_2D:
--        target = TEXTURE_2D;
--        break;
--      case TT_CUBIC:
--        target = TEXTURE_CUBE_MAP_EXT;
--        break;
--      default:
--        idLib::FatalError( "%s: bad texture type %d", GetName, opts.textureType );
--        return;
--    }
--
--    -- ALPHA, LUMINANCE, LUMINANCE_ALPHA, and INTENSITY have been removed
--    -- in OpenGL 3.2. In order to mimic those modes, we use the swizzle operators
--
--    if ( opts.colorFormat == CFM_GREEN_ALPHA ) {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, GREEN );
--    } else if ( opts.format == FMT_LUM8 ) {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, ONE );
--    } else if ( opts.format == FMT_L8A8 ) {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, GREEN );
--    } else if ( opts.format == FMT_ALPHA ) {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, ONE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, RED );
--    } else if ( opts.format == FMT_INT8 ) {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, RED );
--    } else {
--      glTexParameteri( target, TEXTURE_SWIZZLE_R, RED );
--      glTexParameteri( target, TEXTURE_SWIZZLE_G, GREEN );
--      glTexParameteri( target, TEXTURE_SWIZZLE_B, BLUE );
--      glTexParameteri( target, TEXTURE_SWIZZLE_A, ALPHA );
--    }
--
--    switch( filter ) {
--      case TF_DEFAULT:
--        if ( r_useTrilinearFiltering.GetBool ) {
--          glTexParameterf( target, TEXTURE_MIN_FILTER, LINEAR_MIPMAP_LINEAR );
--        } else {
--          glTexParameterf( target, TEXTURE_MIN_FILTER, LINEAR_MIPMAP_NEAREST );
--        }
--        glTexParameterf( target, TEXTURE_MAG_FILTER, LINEAR );
--        break;
--      case TF_LINEAR:
--        glTexParameterf( target, TEXTURE_MIN_FILTER, LINEAR );
--        glTexParameterf( target, TEXTURE_MAG_FILTER, LINEAR );
--        break;
--      case TF_NEAREST:
--        glTexParameterf( target, TEXTURE_MIN_FILTER, NEAREST );
--        glTexParameterf( target, TEXTURE_MAG_FILTER, NEAREST );
--        break;
--      default:
--        common->FatalError( "%s: bad texture filter %d", GetName, filter );
--    }
--    glDebugMessageControlARB
--    if ( glConfig.anisotropicFilterAvailable ) {
--      // only do aniso filtering on mip mapped images
--      if ( filter == TF_DEFAULT ) {
--        int aniso = r_maxAnisotropicFiltering.GetInteger;
--        if ( aniso > glConfig.maxTextureAnisotropy ) {
--          aniso = glConfig.maxTextureAnisotropy;
--        }
--        if ( aniso < 0 ) {
--          aniso = 0;
--        }
--        glTexParameterf(target, TEXTURE_MAX_ANISOTROPY_EXT, aniso );
--      } else {
--        glTexParameterf(target, TEXTURE_MAX_ANISOTROPY_EXT, 1 );
--      }
--    }
--    if ( glConfig.textureLODBiasAvailable && ( usage != TD_FONT ) ) {
--      // use a blurring LOD bias in combination with high anisotropy to fix our aliasing grate textures...
--      glTexParameterf(target, TEXTURE_LOD_BIAS_EXT, r_lodBias.GetFloat );
--    }
--
--    // set the wrap/clamp modes
--    switch( repeat ) {
--      case TR_REPEAT:
--        glTexParameterf( target, TEXTURE_WRAP_S, REPEAT );
--        glTexParameterf( target, TEXTURE_WRAP_T, REPEAT );
--        break;
--      case TR_CLAMP_TO_ZERO: {
--        float color[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
--        glTexParameterfv(target, TEXTURE_BORDER_COLOR, color );
--        glTexParameterf( target, TEXTURE_WRAP_S, CLAMP_TO_BORDER );
--        glTexParameterf( target, TEXTURE_WRAP_T, CLAMP_TO_BORDER );
--        }
--        break;
--      case TR_CLAMP_TO_ZERO_ALPHA: {
--        float color[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
--        glTexParameterfv(target, TEXTURE_BORDER_COLOR, color );
--        glTexParameterf( target, TEXTURE_WRAP_S, CLAMP_TO_BORDER );
--        glTexParameterf( target, TEXTURE_WRAP_T, CLAMP_TO_BORDER );
--        }
--        break;
--      case TR_CLAMP:
--        glTexParameterf( target, TEXTURE_WRAP_S, CLAMP_TO_EDGE );
--        glTexParameterf( target, TEXTURE_WRAP_T, CLAMP_TO_EDGE );
--        break;
--      default:
--        common->FatalError( "%s: bad texture repeat %d", GetName, repeat );
--    }
--  }
--      if renderZPass then
--        StencilOpSeparate(FRONT, KEEP, KEEP, INCREMENT);
--        StencilOpSeparate(BACK, KEEP, KEEP, DECREMENT);
--      elsif r_useStencilShadowPreload.GetBool then
--        // preload + Z-pass
--        StencilOpSeparate(FRONT, KEEP, DECREMENT, DECREMENT);
--        StencilOpSeparate(BACK, KEEP, INCREMENT, INCREMENT);
--      else
--        null; -- Z-fail
--      end if;
--
--  /*
--  ================
--  RB_DrawElementsWithCounters
--  ================
--  */
--  void RB_DrawElementsWithCounters( const drawSurf_t *surf ) {
--    // get vertex buffer
--    const vertCacheHandle_t vbHandle = surf->ambientCache;
--    idVertexBuffer * vertexBuffer;
--    if ( vertexCache.CacheIsStatic( vbHandle ) ) {
--      vertexBuffer = &vertexCache.staticData.vertexBuffer;
--    } else {
--      const uint64 frameNum = (int)( vbHandle >> VERTCACHE_FRAME_SHIFT ) & VERTCACHE_FRAME_MASK;
--      if ( frameNum != ( ( vertexCache.currentFrame - 1 ) & VERTCACHE_FRAME_MASK ) ) {
--        idLib::Warning( "RB_DrawElementsWithCounters, vertexBuffer == NULL" );
--        return;
--      }
--      vertexBuffer = &vertexCache.frameData[vertexCache.drawListNum].vertexBuffer;
--    }
--    const int vertOffset = (int)( vbHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK;
--
--    // get index buffer
--    const vertCacheHandle_t ibHandle = surf->indexCache;
--    idIndexBuffer * indexBuffer;
--    if ( vertexCache.CacheIsStatic( ibHandle ) ) {
--      indexBuffer = &vertexCache.staticData.indexBuffer;
--    } else {
--      const uint64 frameNum = (int)( ibHandle >> VERTCACHE_FRAME_SHIFT ) & VERTCACHE_FRAME_MASK;
--      if ( frameNum != ( ( vertexCache.currentFrame - 1 ) & VERTCACHE_FRAME_MASK ) ) {
--        idLib::Warning( "RB_DrawElementsWithCounters, indexBuffer == NULL" );
--        return;
--      }
--      indexBuffer = &vertexCache.frameData[vertexCache.drawListNum].indexBuffer;
--    }
--    const int indexOffset = (int)( ibHandle >> VERTCACHE_OFFSET_SHIFT ) & VERTCACHE_OFFSET_MASK;
--
--    RENDERLOG_PRINTF( "Binding Buffers: %p:%i %p:%i\n", vertexBuffer, vertOffset, indexBuffer, indexOffset );
--
--    if ( surf->jointCache ) {
--      if ( !verify( renderProgManager.ShaderUsesJoints ) ) {
--        return;
--      }
--    } else {
--      if ( !verify( !renderProgManager.ShaderUsesJoints || renderProgManager.ShaderHasOptionalSkinning ) ) {
--        return;
--      }
--    }
--
--
--    if ( surf->jointCache ) {
--      idJointBuffer jointBuffer;
--      if ( !vertexCache.GetJointBuffer( surf->jointCache, &jointBuffer ) ) {
--        idLib::Warning( "RB_DrawElementsWithCounters, jointBuffer == NULL" );
--        return;
--      }
--      assert( ( jointBuffer.GetOffset & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) == 0 );
--
--      const GLuint ubo = reinterpret_cast< GLuint >( jointBuffer.GetAPIObject );
--      glBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset, jointBuffer.GetNumJoints * sizeof( idJointMat ) );
--    }
--
--    renderProgManager.CommitUniforms;
--
--    if ( backEnd.glState.currentIndexBuffer != (GLuint)indexBuffer->GetAPIObject || !r_useStateCaching.GetBool ) {
--      glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer->GetAPIObject );
--      backEnd.glState.currentIndexBuffer = (GLuint)indexBuffer->GetAPIObject;
--    }
--
--    if ( ( backEnd.glState.vertexLayout != LAYOUT_DRAW_VERT ) || ( backEnd.glState.currentVertexBuffer != (GLuint)vertexBuffer->GetAPIObject ) || !r_useStateCaching.GetBool ) {
--      glBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer->GetAPIObject );
--      backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer->GetAPIObject;
--
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX );
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL );
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR );
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2 );
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST );
--      glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT );
--
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 3, GL_FLOAT, GL_FALSE, sizeof( idDrawVert ), (void *)( DRAWVERT_XYZ_OFFSET ) );
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_NORMAL, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_NORMAL_OFFSET ) );
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR_OFFSET ) );
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_COLOR2_OFFSET ) );
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_ST, 2, GL_HALF_FLOAT, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_ST_OFFSET ) );
--      glVertexAttribPointerARB( PC_ATTRIB_INDEX_TANGENT, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idDrawVert ), (void *)( DRAWVERT_TANGENT_OFFSET ) );
--
--      backEnd.glState.vertexLayout = LAYOUT_DRAW_VERT;
--    }
--
--    glDrawElementsBaseVertex( GL_TRIANGLES,
--                  r_singleTriangle.GetBool ? 3 : surf->numIndexes,
--                  GL_INDEX_TYPE,
--                  (triIndex_t *)indexOffset,
--                  vertOffset / sizeof ( idDrawVert ) );
--
--
--  }
--
--
--      RENDERLOG_PRINTF( "Binding Buffers: %p %p\n", vertexBuffer, indexBuffer );
--
--
--      if ( backEnd.glState.currentIndexBuffer != (GLuint)indexBuffer->GetAPIObject || !r_useStateCaching.GetBool ) {
--        glBindBufferARB( GL_ELEMENT_ARRAY_BUFFER_ARB, (GLuint)indexBuffer->GetAPIObject );
--        backEnd.glState.currentIndexBuffer = (GLuint)indexBuffer->GetAPIObject;
--      }
--
--      if ( drawSurf->jointCache ) {
--        assert( renderProgManager.ShaderUsesJoints );
--
--        idJointBuffer jointBuffer;
--        if ( !vertexCache.GetJointBuffer( drawSurf->jointCache, &jointBuffer ) ) {
--          idLib::Warning( "RB_DrawElementsWithCounters, jointBuffer == NULL" );
--          continue;
--        }
--        assert( ( jointBuffer.GetOffset & ( glConfig.uniformBufferOffsetAlignment - 1 ) ) == 0 );
--
--        const GLuint ubo = reinterpret_cast< GLuint >( jointBuffer.GetAPIObject );
--        glBindBufferRange( GL_UNIFORM_BUFFER, 0, ubo, jointBuffer.GetOffset, jointBuffer.GetNumJoints * sizeof( idJointMat ) );
--
--        if ( ( backEnd.glState.vertexLayout != LAYOUT_DRAW_SHADOW_VERT_SKINNED) || ( backEnd.glState.currentVertexBuffer != (GLuint)vertexBuffer->GetAPIObject ) || !r_useStateCaching.GetBool ) {
--          glBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer->GetAPIObject );
--          backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer->GetAPIObject;
--
--          glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL );
--          glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR );
--          glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2 );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT );
--
--          glVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_XYZW_OFFSET ) );
--          glVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_COLOR_OFFSET ) );
--          glVertexAttribPointerARB( PC_ATTRIB_INDEX_COLOR2, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof( idShadowVertSkinned ), (void *)( SHADOWVERTSKINNED_COLOR2_OFFSET ) );
--
--          backEnd.glState.vertexLayout = LAYOUT_DRAW_SHADOW_VERT_SKINNED;
--        }
--
--      } else {
--
--        if ( ( backEnd.glState.vertexLayout != LAYOUT_DRAW_SHADOW_VERT ) || ( backEnd.glState.currentVertexBuffer != (GLuint)vertexBuffer->GetAPIObject ) || !r_useStateCaching.GetBool ) {
--          glBindBufferARB( GL_ARRAY_BUFFER_ARB, (GLuint)vertexBuffer->GetAPIObject );
--          backEnd.glState.currentVertexBuffer = (GLuint)vertexBuffer->GetAPIObject;
--
--          glEnableVertexAttribArrayARB( PC_ATTRIB_INDEX_VERTEX );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_NORMAL );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_COLOR2 );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_ST );
--          glDisableVertexAttribArrayARB( PC_ATTRIB_INDEX_TANGENT );
--
--          glVertexAttribPointerARB( PC_ATTRIB_INDEX_VERTEX, 4, GL_FLOAT, GL_FALSE, sizeof( idShadowVert ), (void *)( SHADOWVERT_XYZW_OFFSET ) );
--
--          backEnd.glState.vertexLayout = LAYOUT_DRAW_SHADOW_VERT;
--        }
--      }
--
--      renderProgManager.CommitUniforms;
--
--      if ( drawSurf->jointCache ) {
--        glDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVertSkinned ) );
--      } else {
--        glDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof( idShadowVert ) );
--      }
--
--      if ( !renderZPass && r_useStencilShadowPreload.GetBool ) {
--        // render again with Z-pass
--        glStencilOpSeparate( GL_FRONT, GL_KEEP, GL_KEEP, GL_INCR );
--        glStencilOpSeparate( GL_BACK, GL_KEEP, GL_KEEP, GL_DECR );
--
--        if ( drawSurf->jointCache ) {
--          glDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof ( idShadowVertSkinned ) );
--        } else {
--          glDrawElementsBaseVertex( GL_TRIANGLES, r_singleTriangle.GetBool ? 3 : drawSurf->numIndexes, GL_INDEX_TYPE, (triIndex_t *)indexOffset, vertOffset / sizeof ( idShadowVert ) );
--        }
--      }
--    }
--
--    // cleanup the shadow specific rendering state
--
--    GL_Cull( CT_FRONT_SIDED );
--
--    // reset depth bounds
--    if ( r_useShadowDepthBounds.GetBool ) {
--      if ( r_useLightDepthBounds.GetBool ) {
--        GL_DepthBoundsTest( vLight->scissorRect.zmin, vLight->scissorRect.zmax );
--      } else {
--        GL_DepthBoundsTest( 0.0f, 0.0f );
--      }
--    }
--
--    // two-sided stencil test
--    glStencilOpSeparate( GL_FRONT, GL_KEEP, GL_REPLACE, GL_ZERO );
--    glStencilOpSeparate( GL_BACK, GL_KEEP, GL_ZERO, GL_REPLACE );
  -----
  begin
  -----
    if SPECIFICS.Version < MINIMUM_VERSION then
      raise Unsupported;
    end if;
  end OpenGL
