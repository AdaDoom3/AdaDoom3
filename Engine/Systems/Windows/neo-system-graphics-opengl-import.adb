with Ada.Unchecked_Conversion;
with Neo.Windows; use Neo.Windows;
separate(Neo.System.Graphics.OpenGL) package body Import is
  Device_Context : Address := NULL_ADDRESS;
  function Load_Function(Name : in String_1) return Address is begin return OpenGL_Get_Procedure_Address(To_String_1_C(Name));  end Load_Function;
  function Get_Extensions return String_1                   is begin return To_String_1(OpenGL_Get_Extensions(Device_Context)); end Get_Extensions;
  procedure Swap_Buffers                                    is begin Assert(Swap_Buffers(Device_Context));                      end Swap_Buffers;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    Window               :         Address                  := Primary_Window;--HaFind_Window(To_String_2_C(To_String_2(Neo.System.SPECIFICS.Name)), NULL_STRING_2_C);
    Rendering_Context    :         Address                  := NULL_ADDRESS;
    Specifics            :         Record_Specifics         := (others => <>);
    Message              : aliased Record_Message           := (others => <>);
    Pixel_Format         : aliased Integer_4_Signed_C       := 0;
    Number_Of_Attributes : aliased Integer_4_Unsigned_C     := 0;
    Attributes_Float     : aliased Array_Float_4_Real_C     := (0.0, 0.0);
    Attributes_Integer   : aliased Array_Integer_4_Signed_C :=(
      FORMAT_SAMPLE_BUFFERS, C_TRUE,
      FORMAT_SAMPLES,        (if Antialiasing_Samples.Get > 0 then C_TRUE else C_FALSE),
      FORMAT_DOUBLE_BUFFER,  C_TRUE,
      FORMAT_BITS_STENCIL,   8,
      FORMAT_BITS_DEPTH,     24,
      FORMAT_BITS_RED,       8,
      FORMAT_BITS_BLUE,      8,
      FORMAT_BITS_GREEN,     8,
      FORMAT_BITS_ALPHA,     8,
      FORMAT_STEREO,         C_FALSE, 0, 0);
    Attributes_Context : aliased Array_Integer_4_Signed_C :=(
      CONTEXT_VERSION_MAJOR, 3,
      CONTEXT_VERSION_MINOR, 2,
      CONTEXT_FLAGS,         (if Do_Put_Debug then C_TRUE else C_FALSE),
      CONTEXT_PROFILE,       CONTEXT_CORE_PROFILE_BIT, 0);
    Descriptor : aliased Record_Pixel_Descriptor  :=(
      Version      => 1,
      Flags        => PIXEL_FORMAT_DRAW_TO_WINDOW or PIXEL_FORMAT_SUPPORT_OPENGL or PIXEL_FORMAT_DOUBLEBUFFER,
      Color_Bits   => 32,
      Depth_Bits   => 24,
      Stencil_Bits => 8,
      Alpha_Shift  => 8,
      others       => <>);
    begin
      Assert(Window);
      Device_Context := Get_Device_Context(Window);
      Assert(Device_Context);
      Assert(OpenGL_Choose_Pixel_Format(Device_Context, Attributes_Integer'unchecked_access, Attributes_Float'unchecked_access, 1, Pixel_Format'address, Number_Of_Attributes'unchecked_access));
      Assert(Describe_Pixel_Format(Device_Context, Pixel_Format, Descriptor'size / Byte'size, Descriptor'address));
      Assert(Set_Pixel_Format(Device_Context, Pixel_Format, Descriptor'address));
      Rendering_Context := OpenGL_Create_Context_Attributes(Device_Context, NULL_ADDRESS, Attributes_Context'unchecked_access);
      Assert(Rendering_Context);
      if Monitor = 1 then
        --Current_Specifics.Has_Swap_Control_Tear := Index(Get_Extension(Device_Context), "WGL_EXT_swap_control_tear") /= 0;
        Current_Specifics.Stencil_Bits          := Integer_4_Positive((if Descriptor.Stencil_Bits /= 0 then Descriptor.Stencil_Bits else Byte'size)); -- Windows XP seems to set this incorrectly
        Current_Specifics.Color_Bits            := Integer_4_Positive(Descriptor.Color_Bits);
        Current_Specifics.Depth_Bits            := Integer_4_Positive(Descriptor.Depth_Bits);
        Assert(OpenGL_Make_Current(Device_Context, Rendering_Context));
      end if;
    end Initialize;
  procedure Finalize(Monitor : in Integer_4_Positive) is
    begin
      null;
      --Assert_Dummy(Make_Current);
      --Assert_Dummy(Make_Current(NULL_ADDRESS, NULL_ADDRESS);
      --Assert_Dummy(Delete_Rendering_Context);
      --if Device_Context /= null and then Release_Device_Context = FAILED then
      --  null;
      --end if;
      --GLimp_RestoreGamma();
    end Finalize;
end Import;
