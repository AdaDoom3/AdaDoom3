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
  ----------------------
  -- Get_Blank_Cursor --
  ----------------------
    function Get_Blank_Cursor -- To avoid resource files
      return Array_Integer_1_Unsigned
      is
      begin
        return(
          16##, );
      end Get_Blank_Cursor;
  end Neo.Windows;