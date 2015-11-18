separate (Neo.System.Input) package body Import is
  function Does_Support_Playstation_Devices return Boolean is begin return False; end Does_Support_Playstation_Devices;
  function Does_SUpport_Xbox_Devices        return Boolean is begin return False; end Does_Support_Xbox_Devices;
  procedure Set_Vibration(Identifier : in Integer_Address; Frequency_High, Frequency_Low : in Float_4_Percent) is
    begin
      raise Unimplemented_Feature;  
    end Set_Vibration;
  function Get_Cursor return Record_Location is
    begin
      raise Unimplemented_Feature;  
      return (others => <>);
    end Get_Cursor;
  procedure Initialize is
    begin
      raise Unimplemented_Feature;  
    end Initialize;
  function Update return Boolean is
    begin
      raise Unimplemented_Feature;  
      return False;
    end Update;
  procedure Finalize is
    begin
      raise Unimplemented_Feature;  
    end Finalize;
end Import;
