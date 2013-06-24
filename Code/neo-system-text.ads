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
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Output,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
package Neo.System.Text
  is
  ------------------
  -- Enumerations --
  ------------------
    type Enumerated_Language
      is(
      Arabic_Language,
      Basque_Language,
      Catalan_Language,
      Simplified_Chinese_Language,
      Traditional_Chinese_Language,
      Czech_Language,
      Danish_Language,
      Dutch_Language,
      English_Language,
      Finnish_Langauge,
      French_Langauge,
      German_Language,
      Greek_Language,
      Hebrew_Language,
      Hungarian_Language,
      Italian_Language,
      Japanese_Language,
      Korean_Language,
      Norwegian_Language,
      Polish_Language,
      Portuguese_Language,
      Brazilian_Portuguese_Language,
      Russian_Language,
      Slovakian_Language,
      Slovenian_Language,
      Spanish_Language,
      Swedish_Language,
      Turkish_Language);
  -----------------
  -- Subprograms --
  -----------------
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
