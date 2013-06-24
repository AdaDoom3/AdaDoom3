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
  ------------------
  -- Task_Console --
  ------------------
    task body Task_Console
      is
      Current_Icon_Path : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      Current_Title     : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;
      begin
        accept Initialize(
          Icon_Path : in String_2;
          Title     : in String_2)
          do
            Current_Icon_Path := To_String_2_Unbounded(Icon_Path);
            Current_Title     := To_String_2_Unbounded(Title);
          end Initialize;
        ----
        Run:
        ----
          begin
            Implementation.Run_Console(To_String_2(Current_Icon_Path), To_String_2(Current_Title));
          exception
            when System_Call_Failure =>
              Put_Debug_Line(L(FAILED_SPAWN_CONSOLE));
          end Run;
        Finalize(Console);
      end Task_Console;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title(L("EXCEPTION TEST"));
        for I in 1..1000 loop
          exit when not
            Is_Okay(
              Title   => "" & Character_2'val(16#221E#),
              Message => L("Continue?!"),
              Buttons => Yes_No_Buttons,
              Icon    => Enumerated_Icon'val(I mod (1 + Enumerated_Icon'pos(Enumerated_Icon'last))));
        end loop;
        if Get_Version in Enumerated_Windows_System'range then
          Spawn_Console(DIRECTORY_ASSETS & "ICO/icon.ico");
        end if;
      end Test;
  -------------------
  -- Spawn_Console --
  -------------------
    procedure Spawn_Console(
      Icon_Path : in String_2 := NULL_STRING_2;
      Title     : in String_2 := NAME & L(" Console"))
      is
      begin
        if Console = null then
          Console := new Task_Console;
          Console.all.Initialize(Icon_Path, Title);
        else
          Put_Debug_Line(L(FAILED_CONSOLE_ALREADY_SPAWNED));
        end if;
      end Spawn_Console;
  ---------------
  -- Set_Alert --
  ---------------
    procedure Set_Alert(
      Status : in Boolean)
      is
      begin
        Alert_Status.Set_Is_Doing_Something(Status);
        Implementation.Set_Alert(Status);
      exception
        when System_Call_Failure =>
          Put_Debug(L(FAILED_SET_ALERT));
      end Set_Alert;
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
      Title   : in String_2;
      Message : in String_2;
      Buttons : in Enumerated_Buttons := Okay_Button;
      Icon    : in Enumerated_Icon    := No_Icon)
      return Boolean
      is
      begin
        if Title = NULL_STRING_2 or Message = NULL_STRING_2 then
          raise Empty_Is_Okay_Message;
        end if;
        return
          Implementation.Is_Okay(
            Title   => Title,
            Message => Message,
            Buttons => Buttons,
            Icon    => Icon);
      exception
        when System_Call_Failure =>
          Put_Debug(L(FAILED_IS_OKAY));
          return False;
      end Is_Okay;
  ------------------
  -- Send_Catalog --
  ------------------
    procedure Send_Catalog
      is
      begin
        Open_Webpage("http://www.google.com"); -- To_UTF8
      end Send_Catalog;
  ------------------
  -- Copy_Catalog --
  ------------------
    procedure Copy_Catalog
      is
      begin
        Set_Clipboard(Get_Catalog);
      end Copy_Catalog;
  ------------------
  -- Save_Catalog --
  ------------------
    procedure Save_Catalog
      is
      begin
        null;
      end Save_Catalog;
  end Neo.System.Exception_Handling;
