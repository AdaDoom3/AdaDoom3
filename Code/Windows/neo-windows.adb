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
with
  Neo.Foundation.Text_IO;
use
  Neo.Foundation.Text_IO;
package body Neo.Windows
  is
  --------------------
  -- Put_Last_Error --
  --------------------
    procedure Put_Last_Error(
      Prefix : in String_2 := "Last error: ")
      is
      begin
        Put_Line(Prefix & Integer_4_Unsigned_C'wide_Image(Get_Last_Error));
      end Put_Last_Error;
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
