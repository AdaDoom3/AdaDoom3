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
  ----------------------------
  -- Protected_Alert_Status --
  ----------------------------
    protected body Protected_Alert_Status
      is
        function Is_Alerting
          return Boolean
          is
          begin
            return Status;
          end Is_Alerting;
        procedure Set_Is_Alerting(
          New_Status : in Boolean)
          is
          begin
            Status := New_Status;
          end Is_Alerting;
      private
        Status : Boolean := False;
      end Protected_Alert_Status;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Put_Title("EXCEPTION HANDLING TEST");
      end Test;
  -----------------
  -- Start_Alert --
  -----------------
    procedure Start_Alert
      is
      begin
        if Alert_Status.Is_Alerting then
          raise Alert_Started_Before_Stop;
        end if;
        Implementation.Start_Alert;
        Alert_Status.Set_Is_Alerting(True);
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
        if not Alert_Status.Is_Alerting then
          raise Alert_Stopped_Without_Start;
        end if;
        Implementation.Stop_Alert;
        Alert_Status.Set_Is_Alerting(False);
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
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon;
      Parent_Title : in String_2 := NULL_STRING_2)
      return Boolean
      is
      begin
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
      renames Alert_Status.Is_Alerting;
  end Neo.System.Exception_Handling;
