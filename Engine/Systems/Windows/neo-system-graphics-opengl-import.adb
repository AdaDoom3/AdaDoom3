with Neo.Windows; use Neo.Windows;
separate(Neo.System.Graphics.OpenGL) package body Import is
  procedure Swap_Buffers is begin null; end Swap_Buffers;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    Rendering_Context :         Address                  := NULL_ADDRESS;
    Device_Context    :         Address                  := NULL_ADDRESS;
    Window            :         Address                  := NULL_ADDRESS;
    Specifics         :         Record_Specifics         := (others => <>);
    Pixel_Descriptor  : aliased Record_Pixel_Descriptor  := (others => <>);
    Message           : aliased Record_Message           := (others => <>);
    Pixel_Format      : aliased Integer_4_Signed_C       := 0;
    Beef              : aliased Integer_4_Unsigned_C     := 16#Dead_beef#;
    Attributes_Junk   : aliased Array_Float_4_Real_C     := (0.0, 0.0);
    Attributes_Format : aliased Array_Integer_4_Signed_C :=(
      FORMAT_SAMPLE_BUFFERS, C_TRUE,
      FORMAT_SAMPLES,        Integer_4_Signed_C(Antialiasing_Samples.Get),
      FORMAT_DOUBLE_BUFFER,  C_TRUE,
      FORMAT_BITS_STENCIL,   8,
      FORMAT_BITS_DEPTH,     24,
      FORMAT_BITS_RED,       8,
      FORMAT_BITS_BLUE,      8,
      FORMAT_BITS_GREEN,     8,
      FORMAT_BITS_ALPHA,     8,
      FORMAT_STEREO,         0, 0);
    Attributes_Context : aliased Array_Integer_4_Signed_C :=(
      CONTEXT_VERSION_MAJOR, 3,
      CONTEXT_VERSION_MINOR, 2,
      CONTEXT_FLAGS,         (if Do_Put_Debug then C_TRUE else C_FALSE),
      CONTEXT_PROFILE,       CONTEXT_CORE_PROFILE_BIT, 0);
    begin
      Device_Context := Get_Device_Context(Window);
      Assert(Device_Context);
    --  Assert(Choose_Pixel_Format(Device_Context, Attributes_Format'address, Attributes_Junk'address, 1, Pixel_Format'address, Beef'unchecked_access));
    --  Assert(Describe_Pixel_Format(Device_Context, Pixel_Format, Pixel_Descriptor'size / Byte'size, Pixel_Descriptor'address));
   --   Assert(Set_Pixel_Format(Device_Context, Pixel_Format, Pixel_Descriptor'address));
      --Rendering_Context := Create_OpenGL_Context(Device_Context, NULL_ADDRESS, Attributes_Context'address);
  --    Assert(Rendering_Context);
  --    if Monitor = 1 then
   --     Current_Specifics.Has_Swap_Control_Tear := Index(Get_Extension(Device_Context), "WGL_EXT_swap_control_tear") /= 0;
  --      Current_Specifics.Stencil_Bits          := Integer_4_Positive((if Pixel_Descriptor.Stencil_Bits /= 0 then Pixel_Descriptor.Stencil_Bits else Byte'size)); -- Windows XP seems to set this incorrectly
  --      Current_Specifics.Color_Bits            := Integer_4_Positive(Pixel_Descriptor.Color_Bits);
  --      Current_Specifics.Depth_Bits            := Integer_4_Positive(Pixel_Descriptor.Depth_Bits);
  --      Assert(Make_OpenGL_Current(Device_Context, Rendering_Context));
  --    end if;
      Assert(Release_Device_Context(Window, Device_Context));
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
begin
  null;
end Import;
