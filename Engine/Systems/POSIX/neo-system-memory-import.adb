separate(Neo.System.Memory) package body Import is
        -- fstatvfs, statvfs
  function Get_State return Record_State is
    begin
      raise Unimplemented_Feature;  
      return (others => <>); 
    end Get_State;
  procedure Set_Byte_Limits(Minimum, Maximum : in Integer_8_Unsigned) is
    begin
      raise Unimplemented_Feature; 
    end Set_Byte_Limits;
  procedure Unlock(Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned) is
    begin
      raise Unimplemented_Feature;  
    end Unlock;
  procedure Lock(Location : in Address; Number_Of_Bytes : in Integer_8_Unsigned) is
    begin
      raise Unimplemented_Feature;                                            
    end Lock;
end Import;
