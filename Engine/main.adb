with Ada.Exceptions;          use Ada.Exceptions;
with Neo;                     use Neo;
with Neo.Command;             use Neo.Command;
with Neo.System;              use Neo.System;
with Neo.System.Memory;       use Neo.System.Memory;
with Neo.System.Text;         use Neo.System.Text;
with Neo.System.Text.Console; use Neo.System.Text.Console;
with Neo.System.Community;    use Neo.System.Community;
--with Neo.System.Processor;    use Neo.System.Processor;
--with Neo.System.Input;        use Neo.System.Input;
--with Neo.System.Sound;
--with Neo.System.Window;
--with Neo.System.Network;
--with Neo.File.Image;
--with Neo.File.Model;
procedure Main is
  DO_DEBUG : constant Boolean := True;
  DO_TEST  : constant Boolean := True;
  begin
    begin
      Set_Do_Put_Debug(DO_DEBUG);
      Set_Put(null);
      if DO_TEST then null;
        Neo.Command.Test;
        --Neo.System.Test;
        Neo.System.Memory.Test;
        --Neo.System.Processor.Test;
        Neo.System.Text.Test;
        Neo.System.Text.Console.Test;
        --Neo.System.Input.Test;
      end if;
      --Neo.System.Graphics.Window.Run;
    exception when Occurrence: others =>
      Put_Line("Error");
    --  Set_Errors(
    --    Get_Errors & Localize("Exception:") & To_String_2(Exception_Name(Occurrence)) & END_LINE_2 &
    --    To_String_2(Exception_Message(Occurrence)) & END_LINE_2 &(
    --    if Exception_Name(Occurrence) = "NEO.SYSTEM.CALL_FAILURE" then Get_Last_dfdsdError & END_LINE_2 else NULL_STRING_2));
    --  Is_Running.Set(False);
    end;
    --if Get_Errors /= NULL_STRING_2 then
    --  Put_Line(Get_Errors);
    --  if not Neo.System.Text.Console.Is_Running then
    --    Assert_Dummy(Is_Okay(To_String_2(Neo.System.SPECIFICS.Name), Localize(Get_Errors), Yes_No_Buttons, Error_Icon));
    --    Neo.System.Text.Console.Initialize;
    --  end if;
    --end if;
  end Main;
