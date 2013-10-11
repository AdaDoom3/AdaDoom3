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
package body Neo.File.Image
  is
  --------------------
  -- Implementation --
  --------------------
    package body FTS
      is separate;
    package body TGA
      is separate;
    package body PCX
      is separate;
    package body TIF
      is separate;
    package body GIF
      is separate;
    package body PPM
      is separate;
    package body BMP
      is separate;
    package body PNG
      is separate;
    package body JPG
      is separate;
    package body XPM
      is separate;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title(Localize("IMAGE TEST"));
        --
      end Test;
  ----------
  -- Save --
  ----------
    procedure Save(
      Path    : in String_2;
      Graphic : in Record_Graphic)
      renames Instantiation.Save;
    procedure Save(
      Path    : in String_2;
      Graphic : in Array_Record_Graphic)
      renames Instantiation.Save;
  ----------
  -- Load --
  ----------
    function Load(
      Path  : in String_2)
      return Record_Graphic
      renames Instantiation.Load;
    function Load(
      Path  : in String_2)
      return Array_Record_Graphic
      renames Instantiation.Load;
  end Neo.File.Image;
