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
--
package body Neo.Windows
  is
  ----------------------
  -- Get_Blank_Cursor --
  ----------------------
    function Get_Blank_Cursor -- To avoid resource files
      return Array_Integer_1_Unsigned
      is
      begin
        return(
          16#00#, 16#00#);
      end Get_Blank_Cursor;
  end Neo.Windows;