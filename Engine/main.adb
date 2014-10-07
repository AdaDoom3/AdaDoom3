with Neo;                        use Neo;
with Neo.Command;                use Neo.Command;
with Neo.System;                 use Neo.System;
with Neo.System.Input;           use Neo.System.Input;
with Neo.System.Memory;          use Neo.System.Memory;
--with Neo.System.Community;       use Neo.System.Community;
with Neo.System.Processor;       use Neo.System.Processor;
with Neo.System.Text;            use Neo.System.Text;
with Neo.System.Text.Console;    use Neo.System.Text.Console;
with Neo.System.Graphics.Window; use Neo.System.Graphics.Window;
--with Neo.System.Sound;           use Neo.System.Sound;
--with Neo.System.Network;         use Neo.System.Network;
procedure Main is
  begin
    Set_Do_Put_Debug(True);
    if Do_Put_Debug then Neo.System.Text.Console.Initialize; end if;
    begin Run; exception when Occurrence: others => Handle_Exception(Occurrence); end;
    if not Neo.System.Text.Console.Is_Running and then Did_Fail and then Is_Okay(
      Icon    => Error_Icon,
      Name    => To_String_2(Neo.System.SPECIFICS.Name),
      Message => Localize("An error has occurred, would you like to view more information?"),
      Buttons => Yes_No_Buttons) then Neo.System.Text.Console.Initialize; end if;
  end Main;
