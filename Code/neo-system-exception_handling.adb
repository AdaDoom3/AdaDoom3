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
package body Neo.System.Exception_Handling
  is
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title("EXCEPTION HANDLING TEST");
        Start_Alert;
        while Is_Okay("" & Character_2'Val(16#221E#), "Continue?!") loop
          delay 0.1;
        end loop;
        --Start_Alert; -- Should raise an exception
        Stop_Alert;
        --Stop_Alert; -- Should also raise an exception
        --Create_Error_Console();
      end Test;
  -----------------
  -- Start_Alert --
  -----------------
    procedure Start_Alert
      is
      begin
        if Is_Alerting then
          raise Alert_Started_Before_Stop;
        end if;
        Implementation.Start_Alert;
        Alert_Status.Set_Is_Doing_Something(True);
      exception
        when System_Call_Failure =>
          null;
      end Start_Alert;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      begin
        if not Is_Alerting then
          raise Alert_Stopped_Without_Start;
        end if;
        Implementation.Stop_Alert;
        Alert_Status.Set_Is_Doing_Something(False);
      exception
        when System_Call_Failure =>
          null;
      end Stop_Alert;
  --------------------------
  -- Create_Error_Console --
  --------------------------
    procedure Create_Error_Console(
      Text    : in String_2;
      Buttons : in Array_Console_Buttons)
      is
      begin
        Implementation.Create_Error_Console(Text, Buttons);
      exception
        when System_Call_Failure =>
          null;
      end Create_Error_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons := Okay_Buttons;
      Icon         : in Enumerated_Icon    := No_Icon;
      Parent_Title : in String_2           := NULL_STRING_2)
      return Boolean
      is
      begin
        if Title = NULL_STRING_2 or Message = NULL_STRING_2 then
          raise Empty_Is_Okay_Message;
        end if;
        Implementation.Is_Okay(
          Title        => Title,
          Message      => Message,
          Buttons      => Buttons,
          Icon         => Icon,
          Parent_Title => Parent_Title);
      exception
        when System_Call_Failure =>
          return False;
      end Is_Okay;
  -----------------
  -- Is_Alerting --
  -----------------
    function Is_Alerting
      return Boolean
      renames Alert_Status.Is_Doing_Something;
  end Neo.System.Exception_Handling;
