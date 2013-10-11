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
separate(Neo.File.Image)
package body JP2
  is
  ----------
  -- Load --
  ----------
    function Load(
      Path : in String_2)
      return Array_Record_Graphic
      is
      begin
        raise Unimplemented_Feature;
      end Load;
  ----------
  -- Save --
  ----------
    procedure Save(
      Path    : in String_2;
      Graphic : in Array_Record_Graphic)
      is
      begin
        raise Unimplemented_Feature;
      end Save;
  end JP2;
