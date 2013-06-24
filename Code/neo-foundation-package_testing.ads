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
  Neo.Foundation.Data_Types;
use
  Neo.Foundation.Output,
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
    TESTING_SEPORATOR            : constant String_2 := "_";
    TESTING_INPUT_HANG_INCREMENT : constant String_2 := ">";
    NUMBER_OF_SECONDS_TO_DELAY   : constant Integer  := 3;
  end Neo.Foundation.Package_Testing;
