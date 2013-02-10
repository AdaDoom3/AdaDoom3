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
  Neo.Foundation.Data_Types;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types;
package Neo.Foundation.Package_Testing
  is
  ----------------
  -- Exceptions --
  ----------------
    Title_Is_Too_Large_To_Fit_On_A_Single_Line : Exception;
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    procedure Put_Title(
      Title : in String_2);
    procedure Hang_Window;
-------
private
-------
  ---------------
  -- Constants --
  ---------------
    TESTING_SEPORATOR    : constant String_2 := "_";
    TESTING_INPUT_CURSOR : constant String_2 := ">> ";
  end Neo.Foundation.Package_Testing;
