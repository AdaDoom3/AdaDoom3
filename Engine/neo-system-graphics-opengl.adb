with Neo.OpenGL;              use Neo.OpenGL;
with Ada.Wide_Text_IO;        use Ada.Wide_Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
separate(Neo.System.Graphics) package body OpenGL is
  Invalid_Enumeration : Exception;
  Invalid_Operation   : Exception;
  Stack_Underflow     : Exception;
  Stack_Overflow      : Exception;
  Invalid_Value       : Exception;
  package Import is
      function Get_Extensions return String_1;
      function Load_Function  (Name : in String_1) return Address;
      procedure Initialize    (Monitor : in Integer_4_Positive);
      procedure Finalize      (Monitor : in Integer_4_Positive);
      procedure Swap_Buffers;
    end Import;
  package body Import is separate; use Import;
  procedure Set_Color_Mask(Do_Mask_Red, Do_Mask_Green, Do_Mask_Blue, Do_Mask_Alpha : in Boolean := True) is
    begin
      Color_Mask(
        Red   => (if Do_Mask_Red   then GL_FALSE else GL_TRUE),
        Green => (if Do_Mask_Green then GL_FALSE else GL_TRUE),
        Blue  => (if Do_Mask_Blue  then GL_FALSE else GL_TRUE),
        Alpha => (if Do_Mask_Alpha then GL_FALSE else GL_TRUE));
    end Set_Color_Mask;
  procedure Scissor(X, Y, Width, Height : in Integer_4_Signed) is
    begin
      Scissor(
        X      => Integer_4_Signed_C(X),
        Y      => Integer_4_Signed_C(Y),
        Width  => Integer_4_Signed_C(Width),
        Height => Integer_4_Signed_C(Height));
    end Scissor;
  procedure Reset is
    begin
      Clear_Depth(1.0);
      Cull_Face(GL_FRONT_AND_BACK);
      Enable(GL_CULL_FACE);
      Set_Color_Mask;
      Blend_Function(GL_ONE, GL_ZERO);
      Depth_Mask(GL_TRUE);
      Depth_Function(GL_LESS);
      Disable(GL_STENCIL_TEST);
      Disable(GL_POLYGON_OFFSET_FILL);
      Disable(GL_POLYGON_OFFSET_LINE);
      Polygon_Mode(GL_FRONT_AND_BACK, GL_FILL);
      Shade_Model(GL_SMOOTH);
      Enable(GL_DEPTH_TEST);
      Enable(GL_BLEND);
      Enable(GL_SCISSOR_TEST);
      Draw_Buffer(GL_BACK);
      Read_Buffer(GL_BACK);
      if Do_Scissor.Get then Scissor(0, 0, Width.Get, Height.Get); end if;
    end Reset;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    Extensions           :         String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
    Number_Of_Extensions : aliased Integer_4_Signed_C := 0;
    Maximum_Texture_Size : aliased Float_4_Real_C     := 0.0;
    begin
      Import.Initialize(Monitor);
      Neo.OpenGL.Initialize(Import.Load_Function'access);
      if Monitor = 1 then
        Get_Integer_Value(GL_NUM_EXTENSIONS, Number_Of_Extensions'unchecked_access);
        Get_Float_Value(GL_MAX_TEXTURE_SIZE, Maximum_Texture_Size'unchecked_access);
        declare
        function Pure_Evil return String_2 is begin return "666"; end Pure_Evil; -- This needs to be here. Why? Your guess is as good as mine.
        begin
          for I in 0..Number_Of_Extensions - 1 loop Extensions := Extensions & To_String_2_Unbounded(Pure_Evil & To_String_2(Get_String_Index(GL_EXTENSIONS, Integer_4_Unsigned_C(I)))); end loop;
        end;
        Current_Specifics.Is_Supported                  := True;
        Current_Specifics.Shading_Language              := OpenGL_Shading_Language;
        Current_Specifics.Maximum_Texture_Size          := Integer_4_Positive(Maximum_Texture_Size);
        Current_Specifics.Version                       := Float_4_Real'value(Trim(To_String_1(Get_String(GL_VERSION)), Both)(1..3));
        Current_Specifics.Has_Depth_Bounds_Test         := Index(Extensions, "EXT_depth_bounds_test")          /= 0;
        Current_Specifics.Has_Anisotropic_Filter        := Index(Extensions, "EXT_texture_filter_anisotropic") /= 0;
        Current_Specifics.Has_Direct_State_Access       := Index(Extensions, "EXT_direct_state_access")        /= 0;
        Current_Specifics.Has_Texture_Compression       := Index(Extensions, "EXT_texture_compression_s3tc")   /= 0 and Index(Import.Get_Extensions, "ARB_texture_compression") /= 0;
        Current_Specifics.Has_Sync                      := Index(Extensions, "ARB_sync")                       /= 0;
        Current_Specifics.Has_Timer_Query               := Index(Extensions, "ARB_timer_query")                /= 0;
        Current_Specifics.Has_Multitexture              := Index(Extensions, "ARB_multitexture")               /= 0;
        Current_Specifics.Has_Uniform_Buffer            := Index(Extensions, "ARB_uniform_buffer_object")      /= 0;
        Current_Specifics.Has_Occlusion_Query           := Index(Extensions, "ARB_occlusion_query")            /= 0;
        Current_Specifics.Has_Map_Buffer_Range          := Index(Extensions, "ARB_map_buffer_range")           /= 0;
        Current_Specifics.Has_Seamless_Cube_Map         := Index(Extensions, "ARB_seamless_cube_map")          /= 0;
        Current_Specifics.Has_Vertex_Array_Object       := Index(Extensions, "ARB_vertex_array_object")        /= 0;
        Current_Specifics.Has_Vertex_Buffer_Object      := Index(Extensions, "ARB_vertex_buffer_object")       /= 0;
        Current_Specifics.Has_RGB_Color_Framebuffer     := Index(Extensions, "ARB_framebuffer_sRGB")           /= 0;
        Current_Specifics.Has_Draw_Elements_Base_Vertex := Index(Extensions, "ARB_draw_elements_base_vertex")  /= 0;
        if Current_Specifics.Version < 2.0 then raise Unsupported; end if;
      end if;
      Reset;
    end Initialize;
  procedure Finalize(Monitor : in Integer_4_Positive) is
    begin
      Import.Finalize(Monitor);
    end Finalize;
  procedure Clear(Stencil_Value : in Integer_4_Signed; Do_Clear_Depth : in Boolean := False) is
    begin
      Clear_Stencil(Integer_4_Signed_C(Stencil_Value));
      Clear((if Do_Clear_Depth then GL_DEPTH_BUFFER_BIT else 0) or GL_STENCIL_BUFFER_BIT);
    end Clear;
  procedure Check_Exceptions is
    begin
      case Get_Error is
        when GL_INVALID_ENUM => raise Invalid_Enumeration;
        when GL_INVALID_OPERATION   => raise Invalid_Operation;
        when GL_STACK_UNDERFLOW     => raise Stack_Underflow;
        when GL_STACK_OVERFLOW      => raise Stack_Overflow;
        when GL_INVALID_VALUE       => raise Invalid_Value;
        when GL_OUT_OF_MEMORY       => raise Out_Of_Memory;
        when others                 => null;
      end case;
    end Check_Exceptions;
  function Get_Driver return Record_Driver is
    begin
      return(
        Set_Color_Mask => Set_Color_Mask'access,
        Reset          => Reset'access,
        Initialize     => Initialize'access,
        Finalize       => Finalize'access);
    end Get_Driver;
--     procedure Cull(Kind : in Enumerated_Cull; Is_Mirror : in Boolean := False) is
--       begin
--         case Kind is
--           when Face_Culling    => null;
--           when Two_Sided_Cull  => Disable(CULL_FACE);
--           when Back_Sided_Cull => Cull_Face((if Is_Mirror then FRONT else BACK));
--         end case;
--       end Cull;
--     procedure View_Port(X, Y, Width, Height : in Integer_4_Signed) is
--       begin
--         Viewport(
--           X      => Integer_4_Signed_C(X),
--           Y      => Integer_4_Signed_C(Y),
--           Width  => Integer_4_Signed_C(Width),
--           Height => Integer_4_Signed_C(Height));
--       end View_Port;
--     procedure Polygon_Offset(Scale, Bias : in Float_4_Real) is
--       begin
--         PolygonOffset(
--           Scale => Integer_4_Signed_C(Scale),
--           Bias  => Integer_4_Signed_C(Bias));
--       end Polygon_Offset;
--     procedure Depth_Bounds_Test(Z_Minimum, Z_Maximum : in Float_4_Real := 0.0) is
--       begin
--         if Z_Minimum = 0.0 and Z_Maximum = 0.0 then
--           Disable(DEPTH_BOUNDS_TEST);
--         else
--           Enable(DEPTH_BOUNDS_TEST);
--           Depth_Bounds(Float_4_Real_C(Z_Minimum), Float_4_Real_C(Z_Maximum));
--         end if;
--       end Depth_Bounds_Test;
--     procedure Start_Depth_Pass(Rectane : in Record_Rectane) is
--       begin
--         null;
--       end Start_Depth_Pass;
--     procedure Finish_Depth_Pass(Rectane : in Record_Rectane)  is
--       begin
--         null;
--       end Finish_Depth_Pass;
--     procedure Get_Depth_Pass(Rectane : in out Record_Rectane) is
--       begin
--         Rectane := (others => <>);
--       end Get_Depth_Pass;
--     procedure Color(Pixel : in Record_Pixel)  is
--       begin
--         Color(
--           Red   => Float_4_Real_C(Pixel.Color.Red)   / Pixel.Color.Red'size,
--           Green => Float_4_Real_C(Pixel.Color.Green) / Pixel.Color.Green'size,
--           Blue  => Float_4_Real_C(Pixel.Color.Blue)  / Pixel.Color.Blue'size,
--           Alpha => Float_4_Real_C(Pixel.Alpha)       / Pixel.Color.Alpha'size);
--       end Color;
--     procedure Color(Color : in Record_Color) is
--       begin
--         Color(
--           Red   => Float_4_Real_C(Color.Red)   / Color.Red'size,
--           Green => Float_4_Real_C(Color.Green) / Color.Green'size,
--           Blue  => Float_4_Real_C(Color.Blue)  / Color.Blue'size,
--           Alpha => 1.0);
--       end Color;
--     procedure Clear is
--       begin
--         Clear(DEPTH_BUFFER_BIT);
--       end Clear;
--     procedure Clear(Color : in Record_Pixel; Do_Clear_Depth : in Boolean := False) is
--       begin
--         Clear_Color(Red, Green, Blue, Alpha);
--         Clear((if Do_Clear_Depth then DEPTH_BUFFER_BIT else 0) or COLOR_BUFFER_BIT);
--       end Clear;
--     procedure Clear(Color : in Record_Pixel; Stencil_Value : in Integer_1_Unsigned; Do_Clear_Depth : in Boolean := False) is
--       begin
--         Clear_Stencil(Stencil_Value);
--         Clear((if Do_Clear_Depth then DEPTH_BUFFER_BIT else 0) or STENCIL_BUFFER_BIT or COLOR_BUFFER_BIT);
--       end Clear;
    --procedure Set_Stereo_Depth(Stereo_Depth : in Item_Stereo_Depth.Variable) is
    --  begin
    --    Depth_Function((
    --      case Depth_Function is
    --        when Less_Depth_Function   => GL_LESS--LESS_THAN_OR_EQUAL
    --        when Equal_Depth_Function  => GL_EQUAL
    --        when Always_Depth_Function => GL_ALWAYS
    --        when Greater_Than_Or_Equal => GL_GREATER_THAN_OR_EQUAL));
    --  end Set_Stereo_Depth;
--     procedure Set_Stereo_3D(Stereo_3D : in Enumerated_Stereo_3D) is
--       begin
--       end Set_Stereo_3D;
--     procedure Set_Blend(Source, Destination : in Enumerated_Blend) is
--       BLENDS : constant array(Enumerated_Blend'range) of Integer_4_Unsigned_C :=(
--         One_Blend                         => ZERO,
--         Zero_Blend                        => ONE,
--         Source_Alpha_Blend                => SOURCE_ALPHA,
--         Destination_Color_Blend           => DESTINATION_COLOR,
--         Destination_Alpha_Blend           => DESTINATION_ALPHA,
--         One_Minus_Source_Alpha_Blend      => ONE_MINUS_SOURCE_ALPHA,
--         One_Minus_Destination_Color_Blend => ONE_MINUS_DESTINATION_COLOR,
--         One_Minus_Destination_Alpha_Blend => ONE_MINUS_DESTINATION_ALPHA);
--       begin
--         if Source = One_Blend and Destination = Zero_Blend then
--           Disable(BLEND);
--         else
--           Enable(BLEND);
--           Blend_Function(BLENDS(Source), BLENDS(Destination));
--         end if;
--       end Set_Blend;
--     procedure Set_Blend_Operation(Blend_Operation : in Enumerated_Blend_Operation) is
--       begin
--       end Set_Blend_Operation;
--     procedure Set_Stencil(Stencil : in Enumerated_Stencil) is
--       begin
--       end Set_Stencil;
--     procedure Set_Stencil_Operation(Fail, Fail_Z, Pass : in Enumerated_Stencil_Operation) is
--       OPERATIONS : constant array(Enumerated_Stencil_Operation'range) of Integer_4_Unsigned_C :=(
--         Keep_Stencil_Operation           => KEEP,
--         Zero_Stencil_Operation           => ZERO,
--         Invert_Stencil_Operation         => INVERT,
--         Replace_Stencil_Operation        => REPLACE,
--         Increment_Stencil_Operation      => INCREMENT,
--         Decrement_Stencil_Operation      => DECREMENT,
--         Increment_Wrap_Stencil_Operation => INCREMENT_WRAP,
--         Decrement_Wrap_Stencil_Operation => DECREMENT_WRAP);
--       begin
--         Stencil_Operation(OPERATIONS(Fail), OPERATIONS(Fail_Z), OPERATIONS(Pass));
--       end Set_Stencil_Operation;
--     procedure Set_Stencil_Function(Stencil : in Enumerated_Stencil_Function) is
--       begin
--         if (diff & (GLS_STENCIL_FUNC_BITS | GLS_STENCIL_OP_BITS ) ) then
--           if (stateBits & (GLS_STENCIL_FUNC_BITS | GLS_STENCIL_OP_BITS ) ) != 0 then
--             Enable(STENCIL_TEST);
--           else
--             Disable(STENCIL_TEST);
--           end if;
--         end if;
--         Stencil_Function(
--           Referece      => GLuint((stateBits & GLS_STENCIL_FUNC_REF_BITS ) >> GLS_STENCIL_FUNC_REF_SHIFT),
--           Mask          => GLuint((stateBits & GLS_STENCIL_FUNC_MASK_BITS ) >> GLS_STENCIL_FUNC_MASK_SHIFT)
--           Function_Kind =>(
--             case Stencil is
--               when Less_Stencil                            => LESS
--               when Equal_Stencil                           => EQUAL
--               when Never_Stencil                           => NEVER
--               when Always_Stencil                          => ALWAYS
--               when Greater_Stencil                         => GREATER
--               when Not_Equal_Stencil                       => NOT_EQUAL
--               when Less_Than_Or_Equal_To_Stencil           => LESS_THAN_OR_EQUAL
--               when Greater_Than_Or_Equal_Stencil_Operation => GREATER_THAN_OR_EQUAL));
--       end Set_Stencil_Function;
--     procedure Set_Depth_Function(Value : in Enumerated_Depth_Function) is
--       begin
--       end Set_Depth_Function;
--     procedure Set_Depth_Mask( is
--       begin
--         if (diff & GLS_DEPTHMASK ) then
--           if (stateBits & GLS_DEPTHMASK ) then
--             DepthMask(FALSE);
--           else
--             DepthMask(TRUE);
--           end if;
--         end if;
--       end Set_Depth_Mask;
--     procedure Set_Polymode_Line( is
--       begin
--         if (diff & GLS_POLYMODE_LINE ) then
--           if (stateBits & GLS_POLYMODE_LINE ) then
--             PolygonMode(FRONT_AND_BACK, LINE);
--           else
--             PolygonMode(FRONT_AND_BACK, FILL);
--           end if;
--         end if;
--       end Set_Polymode_Line;
--     procedure Set_Polygon_Offset(Do_Enable : in Boolean) is
--       begin
--         if Do_Enable then
--           PolygonOffset(backEnd.State.polyOfsScale, backEnd.State.polyOfsBias);
--           Enable(POLYGON_OFFSET_FILL);
--           Enable(POLYGON_OFFSET_LINE);
--         else
--           Disable(POLYGON_OFFSET_FILL);
--           Disable(POLYGON_OFFSET_LINE);
--         end if;
--       end Set_Polygon_Offset;
--     procedure Set_Buffer(const void *data ) is
--       -- see which draw buffer we want to render the frame to
--       const setBufferCommand_t * cmd = (const setBufferCommand_t *)data;
--       begin
--         Scissor(0, 0, tr.GetWidth, tr.GetHeight);
--         -- clear screen for debugging automatically enable this with several other debug tools
--         -- that might leave unrendered portions of the screen
--         if r_clear.GetFloat or idStr::Length(r_clear.GetString ) != 1 || r_sineArea.GetBool || r_showOverDraw.GetBool ) {
--           float c[3];
--           if sscanf(r_clear.GetString, "%f %f %f", &c[0], &c[1], &c[2] ) = 3 then
--             Clear(true, false, false, 0, c[0], c[1], c[2], 1.0f);
--           elsif r_clear.GetInteger = 2 then
--             Clear(true, false, false, 0, 0.0, 0.0, 0.0, 1.0);
--           elsif r_showOverDraw.GetBool then
--             Clear(true, false, false, 0, 1.0, 1.0, 1.0, 1.0);
--           else
--             Clear(true, false, false, 0, 0.4, 0.0, 0.25, 1.0);
--           end if;
--         end if;
--       end Set_Buffer;
--     procedure Make_Stereo_Render_Image(Graphic : in Record_Graphic) is
--       idImageOpts opts;
--       begin
--         opts.width := renderSystem->GetWidth;
--         opts.height := renderSystem->GetHeight;
--         opts.numLevels := 1;
--         opts.format := FMT_RGBA8;
--         image->AllocImage(opts, TF_LINEAR, TR_CLAMP);
--       end Make_Stereo_Render_Image;
--     procedure Render_Headset( is
--       begin
--       end Render;
--     procedure Initialize_Texture(Texture : in out Record_Texture) is
--       int numSides;
--       int target;
--       int uploadTarget;
--       begin
--         CheckErrors;
--         PurgeImage;
--         case opts.format is
--           when FMT_RGBA8 =>
--             internalFormat := RGBA8;
--             dataFormat := RGBA;
--             dataType := UNSIGNED_BYTE;
--           when FMT_XRGB8 =>
--             internalFormat := RGB;
--             dataFormat := RGBA;
--             dataType := UNSIGNED_BYTE;
--           when FMT_RGB565 =>
--             internalFormat := RGB;
--             dataFormat := RGB;
--             dataType := UNSIGNED_SHORT_5_6_5;
--           when FMT_ALPHA =>
--             internalFormat := R8;
--             dataFormat := RED;
--             dataType := UNSIGNED_BYTE;
--           when FMT_L8A8 =>
--             internalFormat := RG8;
--             dataFormat := RG;
--             dataType := UNSIGNED_BYTE;
--           when FMT_LUM8 =>
--             internalFormat := R8;
--             dataFormat := RED;
--             dataType := UNSIGNED_BYTE;
--           when FMT_INT8 =>
--             internalFormat := R8;
--             dataFormat := RED;
--             dataType := UNSIGNED_BYTE;
--           when FMT_DXT1 =>
--             internalFormat := COMPRESSED_RGBA_S3TC_DXT1_EXT;
--             dataFormat := RGBA;
--             dataType := UNSIGNED_BYTE;
--           when FMT_DXT5 =>
--             internalFormat := COMPRESSED_RGBA_S3TC_DXT5_EXT;
--             dataFormat := RGBA;
--             dataType := UNSIGNED_BYTE;
--           when FMT_DEPTH =>
--             internalFormat := DEPTH_COMPONENT;
--             dataFormat := DEPTH_COMPONENT;
--             dataType := UNSIGNED_BYTE;
--           when FMT_X16 =>
--             internalFormat := INTENSITY16;
--             dataFormat := LUMINANCE;
--             dataType := UNSIGNED_SHORT;
--           when FMT_Y16_X16 =>
--             internalFormat := LUMINANCE16_ALPHA16;
--             dataFormat := LUMINANCE_ALPHA;
--             dataType := UNSIGNED_SHORT;
--         end case;
--         -- generate the texture number
--         GenTextures(1, (GLuint *)&texnum);
--         if texnum != TEXTURE_NOT_LOADED then raise Texture_Load_Failure; end if;
--         -- allocate all the mip levels with NULL data
--         if (opts.textureType == TT_2D ) {
--           target = uploadTarget = TEXTURE_2D;
--           numSides = 1;
--         elsif (opts.textureType == TT_CUBIC ) {
--           target = TEXTURE_CUBE_MAP_EXT;
--           uploadTarget = TEXTURE_CUBE_MAP_POSITIVE_X_EXT;
--           numSides = 6;
--         else
--           assert(!"opts.textureType");
--           target = uploadTarget = TEXTURE_2D;
--           numSides = 1;
--         end if;
--         BindTexture(target, texnum);
--         for (int side = 0; side < numSides; side++ ) loop
--           int w = opts.width;
--           int h = opts.height;
--           if (opts.textureType == TT_CUBIC ) then
--             h = w;
--           end if;
--           for (int level = 0; level < opts.numLevels; level++ ) loop
--             -- clear out any previous error
--             CheckErrors;
--             if (IsCompressed ) then
--               int compressedSize = (((w+3)/4) * ((h+3)/4) * int64(16 ) * BitsForFormat(opts.format ) ) / 8;
--               -- Even though the OpenGL specification allows the 'data' pointer to be NULL, for some
--               -- drivers we actually need to upload data to get it to allocate the texture.
--               -- However, on 32-bit systems we may fail to allocate a large block of memory for large
--               -- textures. We handle this when by using HeapAlloc directly and allowing the allocation
--               -- to fail in which when we simply pass down NULL to CompressedTexImage2D and hope for the best.
--               -- As of 2011-10-6 using NVIDIA hardware and drivers we have to allocate the memory with HeapAlloc
--               -- with the exact size otherwise large image allocation (for instance for physical page textures)
--               -- may fail on Vista 32-bit.
--               void * data = HeapAlloc(GetProcessHeap, 0, compressedSize);
--               CompressedTexImage2DARB(uploadTarget+side, level, internalFormat, w, h, 0, compressedSize, data);
--               if (data != NULL ) then
--                 HeapFree(GetProcessHeap, 0, data);
--               end if;
--             else
--               TexImage2D(uploadTarget + side, level, internalFormat, w, h, 0, dataFormat, dataType, NULL);
--             end if;
--             CheckErrors;
--             w = Max(1, w >> 1);
--             h = Max(1, h >> 1);
--           end loop;
--         end loop;
--         TexParameteri(target, TEXTURE_MAX_LEVEL, opts.numLevels - 1);
--         -- see if we messed anything up
--         CheckErrors;
--         SetTexParameters;
--         CheckErrors;
--       end Initialize_Texture;
--     procedure Finalize_Texture(Texture : in out Record_Texture) is
--       begin
--         if (texnum != TEXTURE_NOT_LOADED ) then
--           DeleteTextures(1, (GLuint *)&texnum); -- this should be the ONLY place it is ever called!
--           texnum = TEXTURE_NOT_LOADED;
--         end if;
--         -- clear all the current binding caches, so the next bind will do a real one
--         for (int i = 0 ; i < MAX_MULTITEXTURE_UNITS ; i++ ) {
--           backEnd.State.tmu[i].current2DMap = TEXTURE_NOT_LOADED;
--           backEnd.State.tmu[i].currentCubeMap = TEXTURE_NOT_LOADED;
--         end loop;
--       end Finalize;
--     procedure Upload_Subimage (mipLevel, x, y, z, width, height, const void * pic, int pixelPitch) is
--       int compressedSize = 0;
--       begin
--         if Is_Compressed then
--           assert( !(x&3) && !(y&3) );
--           -- The compressed size may be larger than the dimensions due to padding to quads
--           compressedSize = ( width + 3 ) & ~3 * ( height + 3 ) & ~3 * BitsForFormat( opts.format ) / 8;
--           assert( x + width <= ( opts.width + 3 ) & ~3 && y + height <= ( opts.height + 3 ) & ~3);
--           -- OpenGL understands that there will be padding
--           if x + width > opts.width then width = opts.width - x; end if;
--           if y + height > opts.height then height = opts.height - x; end if;
--         else
--           assert( x + width <= opts.width && y + height <= opts.height );
--         end if;
--         int target;
--         int uploadTarget;
--         if ( opts.textureType == TT_2D ) {
--           target = TEXTURE_2D;
--           uploadTarget = TEXTURE_2D;
--         } else if ( opts.textureType == TT_CUBIC ) {
--           target = TEXTURE_CUBE_MAP_EXT;
--           uploadTarget = TEXTURE_CUBE_MAP_POSITIVE_X_EXT + z;
--         } else {
--           assert( !"invalid opts.textureType" );
--           target = TEXTURE_2D;
--           uploadTarget = TEXTURE_2D;
--         }
--         glBindTexture( target, texnum );
--         if ( pixelPitch != 0 ) {
--           glPixelStorei( UNPACK_ROW_LENGTH, pixelPitch );
--         }
--         if ( opts.format == FMT_RGB565 ) {
--           glPixelStorei( UNPACK_SWAP_BYTES, TRUE );
--         }
--         if ( IsCompressed ) {
--           glCompressedTexSubImage2DARB( uploadTarget, mipLevel, x, y, width, height, internalFormat, compressedSize, pic );
--         } else {
--           // make sure the pixel store alignment is correct so that lower mips get created
--           // properly for odd shaped textures - this fixes the mip mapping issues with
--           // fonts
--           int unpackAlignment = width * BitsForFormat( (textureFormat_t)opts.format ) / 8;
--           if ( ( unpackAlignment & 3 ) == 0 ) {
--             glPixelStorei( UNPACK_ALIGNMENT, 4 );
--           } else {
--             glPixelStorei( UNPACK_ALIGNMENT, 1 );
--           }
--           glTexSubImage2D( uploadTarget, mipLevel, x, y, width, height, dataFormat, dataType, pic );
--         }
--         if ( opts.format == FMT_RGB565 ) {
--           glPixelStorei( UNPACK_SWAP_BYTES, FALSE );
--         }
--         if ( pixelPitch != 0 ) {
--           glPixelStorei( UNPACK_ROW_LENGTH, 0 );
--         }
--       end Upload_Subimage;
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
--    -- ALPHA, LUMINANCE, LUMINANCE_ALPHA, and INTENSITY have been removed in OpenGL 3.2. In order to mimic those modes, we use the swizzle operators
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
end OpenGL;
