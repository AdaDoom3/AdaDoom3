separate(Neo.System.Graphics) package body GCM is
  --package Import is separate;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    begin
      null;
    end Initialize;
  procedure Finalize(Monitor : in Integer_4_Positive) is
    begin
      null;
    end Finalize;
  function Get_Driver return Record_Driver is
    begin
      return(
        Initialize => Initialize'access,
        Finalize   => Finalize'access);
    end Get_Driver;
end GCM;
