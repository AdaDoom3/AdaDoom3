separate(Neo.System.Graphics) package body GCM is
  --package Import is separate;
  procedure Reset is
    begin
      null;
    end Reset;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    begin
      null;
    end Initialize;
  procedure Finalize(Monitor : in Integer_4_Positive) is
    begin
      null;
    end Finalize;
  procedure Set_Color_Mask(Do_Mask_Red, Do_Mask_Green, Do_Mask_Blue, Do_Mask_Alpha : in Boolean := True) is
    begin
      null;
    end Set_Color_Mask;
  function Get_Driver return Record_Driver is
    begin
      return(
        Reset          => Reset'access,
        Set_Color_Mask => Set_Color_Mask'access,
        Initialize     => Initialize'access,
        Finalize       => Finalize'access);
    end Get_Driver;
end GCM;
