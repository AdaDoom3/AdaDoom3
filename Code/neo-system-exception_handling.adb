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
  --------------------
  -- Implementation --
  --------------------
    package body Implementation
      is separate;
  ----------
  -- Test --
  ----------
    -----------------------
    procedure Put_Something
    -----------------------
      is
      begin
        Put_Line("OMG!");
      end Put_Something;
    ----------------------------
    procedure Put_Something_Else
    ----------------------------
      is
      begin
        Put_Line("???");
      end Put_Something_Else;
    procedure Test
      is
      begin
        Put_Title("EXCEPTION HANDLING TEST");
        --Start_Alert;
        while Is_Okay("" & Character_2'Val(16#221E#), "Continue?!", Yes_No_Buttons, Warning_Icon) loop
          delay 0.1;
        end loop;
        --Start_Alert; -- Should raise an exception
        --Stop_Alert;
        --Stop_Alert; -- Should also raise an exception
        Spawn_Console(
          Title   => "Error console",
          Text    => "An error occured!",
          Buttons => (("Panic!", Put_Something'access), ("??????", Put_Something_Else'access)));
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
  -------------------
  -- Spawn_Console --
  -------------------
    procedure Spawn_Console(
      Title     : in String_2;
      Text      : in String_2;
      Buttons   : in Array_Record_Console_Button;
      Icon_Path : in String_2 := NULL_STRING_2)
      is
      begin
        if Buttons'length > MAXIMUM_NUMBER_OF_CONSOLE_BUTTONS then
          raise Too_Many_Buttons_For_Error_Console;
        end if;
        Implementation.Spawn_Console(Title, Text, Buttons, Icon_Path);
      exception
        when System_Call_Failure =>
          null;
      end Spawn_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons := Okay_Button;
      Icon         : in Enumerated_Icon    := No_Icon)
      return Boolean
      is
      begin
        if Title = NULL_STRING_2 or Message = NULL_STRING_2 then
          raise Empty_Is_Okay_Message;
        end if;
        return
          Implementation.Is_Okay(
            Title        => Title,
            Message      => Message,
            Buttons      => Buttons,
            Icon         => Icon);
      exception
        when System_Call_Failure =>
          return False;
      end Is_Okay;
  -----------------
  -- Is_Alerting --
  -----------------
    function Is_Alerting
      return Boolean
      is
      begin
        return Alert_Status.Is_Doing_Something;
      end Is_Alerting;
  end Neo.System.Exception_Handling;
