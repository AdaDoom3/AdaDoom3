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
    function Get_Language
      return Enumerated_Language;
    function Get_Clipboard
      return String_2;
    procedure Set_Clipboard(
      Text : in String_2);
-------
private
-------
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        function Get_Language
          return Enumerated_Language;
        procedure Set_Clipboard(
          Text : in String_2);
        function Get_Clipboard
          return String_2;
      end Implementation;
end Neo.System.Text;
