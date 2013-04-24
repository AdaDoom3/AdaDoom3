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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Text
  is
  ----------------
  -- Suprograms --
  ----------------
    procedure Test;
    function Get_Clipboard
      return String_2;
    procedure Set_Clipboard(
      Text : in String_2);
    function Create_Media_Name(
      Item : in String_2)
      return String_2;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    MEDIA_NAME_NON_CONFORMANT             : constant String_2    := "|:\";
    MEDIA_NAME_NON_CONFORMANT_REPLACEMENT : constant Character_1 := '/';
    MEDIA_NAME_EXTENSION_SEPORATOR        : constant Character_1 := '.';
    REPLACEMENT_FOR_EXTENDED_CHARACTER    : constant Character_1 := '~'; 
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Set_Clipboard(
          Text : in String_2);
        function Get_Clipboard
          return String_2;
      end Implementation;
  end Neo.System.Text;
