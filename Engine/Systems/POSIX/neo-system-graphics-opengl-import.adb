separate(Neo.System.Graphics.OpenGL) package body Import is
  function Load_Function(Name : in String_1) return Address is
    begin 
      raise Unimplemented_Feature; 
      return NULL_ADDRESS;   
    end Load_Function;
  function Get_Extensions return String_1 is
    begin
      raise Unimplemented_Feature;  
      return NULL_STRING_1;
    end Get_Extensions;
  procedure Swap_Buffers is
    begin
      raise Unimplemented_Feature;   
    end Swap_Buffers;
  procedure Initialize(Monitor : in Integer_4_Positive) is
    begin
      raise Unimplemented_Feature;
    end Initialize;
  procedure Finalize(Monitor : in Integer_4_Positive) is
    begin
      raise Unimplemented_Feature; 
    end Finalize;
end Import;
