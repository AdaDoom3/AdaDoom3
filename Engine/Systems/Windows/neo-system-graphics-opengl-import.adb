
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
  Neo.Link.Windows;
use
  Neo.Link.Windows;
separate(Neo.System.Graphics.OpenGL)
package Import
  is
  ---------------
  -- Constants --
  ---------------
    Pixel_Format : constant aliased Record_Pixel_Format :=(
      Version      => 1,
      Flags        => PIXEL_FLAGS, --
      Color_Bits   => 32,
      Alpha_Bits   => 8,
      Depth_Bits   => 24,
      Stencil_Bits => 8,
      Layer_Type   => PIXEL_LAYER, -- PFD_MAIN_PLANE
      Pixel_Type   => PIXEL_TYPE or(if Do_Use_Stereo then PFD_STEREO else 0);
      others       => <>);
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Monitor_Index : in Integer_4_Positive)
      is
      Message           : Record_Message := NULL_RECORD_MESSAGE;
      Device_Context    : Address        := NULL_ADDRESS;
      Rendering_Context : Address        := NULL_ADDRESS;
      begin
        Device_Context := Get_Device_Context(Window);
        if Device_Context = NULL_ADDRESS then
          raise Initialization_Error;
        end if;
        if Choose_Pixel_Format /= null and Multi_Samples > 1 then
          Choose_Pixel_Format(
            Device_Context     => Device_Context,
            Attributes_Float   => (0.0, 0.0),
            Pixel_Format       => Pixel_Format'address,
            Number_Of_Formats  => null,
            Attributes_Integer =>(
              SAMPLE_BUFFERS, 1,
              SAMPLES,        Multi_Samples,
              DOUBLE_BUFFER,  1,
              STENCIL_BITS,   8,
              DEPTH_BITS,     24,
              RED_BITS,       8,
              BLUE_BITS,      8,
              GREEN_BITS,     8,
              ALPHA_BITS,     8, 0, 0));
        elsif Choose_Pixel_Format(Device_Context, FORMAT'address) = FAILED then
          raise Call_Failure;
        end if;
        Describe_Pixel_Format(Device_Context, Pixel_Format, Pixel_Descriptor'size / Byte'size, Pixel_Descriptor'address);
        if Stencil_Bits = 0 then -- Windows XP seems to set this incorrectly
          Stencil_Bits = 8;
        end if;
        if Set_Pixel_Format(Device_Context, Pixel_Format, &win32.pfd) = 0 then
          raise Initialization_Error;
        end if;
        Rendering_Context := Create_Context(Device_Context);
        if Rendering_Context = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        if not Make_Current(Device_Context, Rendering_Context) then
          Delete_Context(Rendering_Context);
          Rendering_Context := NULL_ADDRESS;
          raise Call_Failure;
        end if;
        if
        Set_Foreground_Window(Window) = 0 or else
        Set_Focus(Window) = NULL_ADDRESS
        then
          raise Call_Failure;
        end if;
        return true;
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize(
      Monitor_Index : in Integer_4_Positive)
      is
      begin
        if Make_Current /= FAILED and then Make_Current(NULL_ADDRESS, NULL_ADDRESS) = FAILED then
          null;
        end if;
        if Rendering_Context /= null and then Delete_Rendering_Context = FAILED then
          null;
        end if;
        if Device_Context /= null and then Release_Device_Context = FAILED then
          null;
        end if;
        GLimp_RestoreGamma();
      end Finalize;
  end Import;
