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
  Ada.Finalization,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Ada.Finalization,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Text
  is
  -----------
  -- Types --
  -----------
    type Record_String_UTF8
      is private;
  ----------------
  -- Suprograms --
  ----------------
    procedure Test;
    function Get(
      Item  : in Record_String_UTF8;
      Index : in Integer_4_Positive)
      return Character_2;
    function Get_Length(
      Item : in Record_String_UTF8)
      return Integer_4_Positive;
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
    package body Implementation 
      is separate;
  end Neo.System.Text;
