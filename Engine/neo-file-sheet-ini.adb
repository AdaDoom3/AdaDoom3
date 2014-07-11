separate(Neo.File.Sheet) package body INI is
	function Load(Item : in Stream_Element_Array) return Array_Type_To_Handle is
      begin
        
      end Load;
    function Convert(Item : in Array_Type_To_Handle; Format : in Discrete_Format) return Array_Stream_Element is
      begin
        for I in Item'range loop
        end loop;
      end Convert;
  end INI;
