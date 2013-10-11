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
package body Neo.System.Text.Console
  is
  ------------
  -- Import --
  ------------
    package body Import
      is separate;
  -----------------------
  -- Protected_Console --
  -----------------------
    protected body Protected_Console
      is
        procedure Initialize
          is
          begin
            Console := new Task_Console;
          end Initialize;
        procedure Finalize
          is
          begin
            Finalize(Console);
            Console := null;
          end Finalize;
        function Is_Open
          return Boolean
          is
          begin
            return Console /= null;
          end Is_Open;
      end Protected_Console;
  ------------------
  -- Task_Console --
  ------------------
    task body Task_Console
      is
      begin
        ----
        Run:
        ----
          begin
            Import.Run;
          exception
            when Call_Failure =>
              Put_Debug_Line(Localize(FAILED_SPAWN_CONSOLE));
          end Run;
          Console.Finalize;
       end Task_Console;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title(Localize("EXCEPTION TEST"));
        for I in 1..1000 loop
          exit when not
            Is_Okay(
              Name    => "" & Character_2'val(16#221E#),
              Message => Localize("Continue?!"),
              Buttons => Yes_No_Buttons,
              Icon    => Enumerated_Icon'val(I mod (1 + Enumerated_Icon'pos(Enumerated_Icon'last))));
        end loop;
        if SPECIFICS.Version in Enumerated_Windows_System'range then
          Spawn;
        end if;
      end Test;
  -----------
  -- Spawn --
  -----------
    procedure Spawn(
      Name : in String_2 := Localize("Console"))
      is
      begin
        if not Console.Is_Open then
          Console.Initialize;
        else
          Put_Debug_Line(Localize(FAILED_CONSOLE_ALREADY_SPAWNED));
        end if;
      end Spawn;
  ---------------
  -- Set_Alert --
  ---------------
    procedure Set_Alert(
      Status : in Boolean)
      is
      begin
        Alert_Status.Set_Is_Doing_Something(Status);
        Import.Set_Alert(Status);
      exception
        when Call_Failure =>
          Put_Debug(Localize(FAILED_SET_ALERT));
      end Set_Alert;
  -------------
  -- Is_Open --
  -------------
    function Is_Open
      return Boolean
      is
      begin
        return Console.Is_Open;
      end Is_Open;
  -----------------
  -- Is_Alerting --
  -----------------
    function Is_Alerting
      return Boolean
      is
      begin
        return Alert_Status.Is_Doing_Something;
      end Is_Alerting;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Name    : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons := Okay_Button;
      Icon    : in Enumerated_Icon    := No_Icon)
      return Boolean
      is
      begin
        return
          Import.Is_Okay(
            Name    => Name,
            Message => Message,
            Buttons => Buttons,
            Icon    => Icon);
      exception
        when Call_Failure =>
          Put_Debug(Localize(FAILED_IS_OKAY));
          return False;
      end Is_Okay;
  --------------
  -- Send_Log --
  --------------
    procedure Send_Log
      is
      Log : String_2 := Get_Log;
      begin
        Open_Webpage(ERROR_REPORTING_URL); -- & To_String_2(To_Stream(Log(Log'last - 2048 + ERROR_REPORTING_URL'length..Log'last)));
      end Send_Log;
  --------------
  -- Save_Log --
  --------------
    procedure Save_Log
      is
      begin
        null;--Save_Log(Browse);
      end Save_Log;
  --------------
  -- Copy_Log --
  --------------
    procedure Copy_Log
      is
      begin
        Set_Clipboard(Get_Log);
      end Copy_Log;
  end Neo.System.Text.Console;
