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
      end Test;
  -----------------
  -- Start_Alert --
  -----------------
    procedure Start_Alert
      is
      begin
        if Alert_Status.Is_Alerting then
        end if;
      end Start_Alert;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      begin
      end Stop_Alert;
  --------------------------
  -- Create_Error_Console --
  --------------------------
    procedure Create_Error_Console(
      Text    : in String_2;
      Buttons : in Array_Console_Buttons)
      is
      begin
        Implementation.Create_Error_Console;
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
      end Is_Okay;
  -----------------
  -- Is_Alerting --
  -----------------
    function Is_Alerting
      return Boolean
      is
      begin
      end Is_Alerting;
  ---------------
  -- Protected --
  ---------------
    protected type Protected_Alert_Status
      is
        function Is_Alerting
          return Boolean;
        procedure Set_Is_Alerting(
          Status : in Boolean);
      private
        Status : Boolean := False;
      end Protected_Alert_Status;
  ---------------
  -- Variables --
  ---------------
    Alert_Status : Protected_Alert_Status;

  end Neo.System.Exception_Handling;