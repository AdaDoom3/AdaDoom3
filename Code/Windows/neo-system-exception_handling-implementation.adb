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
  Interfaces.C,
  Neo.Windows;
use
  Interfaces.C,
  Neo.Windows;
separate(Neo.System.Exception_Handling)
package body Implementation
  is
  ---------------
  -- Set_Alert --
  ---------------
    procedure Start_Alert
      is
      Flash_Information : aliased Record_Flash_Information := (Flags => FLASH_CONTINUOUSLY, others => <>);
      Window            :         Address                  := Find_Window(To_String_2_C(NAME), NULL_ADDRESS);
      begin
        if Window = NULL_ADDRESS then
          raise Attempted_To_Alert_Before_Window_Created;
        end if;
        if Flash_Window(Flash_Information'unchecked_access) = FAILED then
          raise System_Call_Failure;
        end if;
      end Start_Alert;
  ----------------
  -- Stop_Alert --
  ----------------
    procedure Stop_Alert
      is
      Flash_Information : aliased Record_Flash_Information := (Flags => FLASH_END, others => <>);
      Window            :         Address                  := Find_Window(To_String_2_C(NAME), NULL_ADDRESS);
      begin
        if Window = NULL_ADDRESS then
          raise Attempted_To_Alert_Before_Window_Created;
        end if;
        if Flash_Window(Flash_Information'unchecked_access) = FAILED then
          raise System_Call_Failure;
        end if;
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
        null;
      end Spawn_Console;
  -------------
  -- Is_Okay --
  -------------
    function Is_Okay(
      Title        : in String_2;
      Message      : in String_2;
      Buttons      : in Enumerated_Buttons;
      Icon         : in Enumerated_Icon)
      return Boolean
      is
      User_Interaction : Integer_4_Signed_C   := 0;
      Kind             : Integer_4_Unsigned_C := 0;
      begin
        case Buttons is
          when Okay_Button =>
            Kind := BUTTON_OKAY;
          when Yes_No_Buttons =>
            Kind := BUTTONS_YES_NO;
          when Okay_Cancel_Buttons =>
            Kind := BUTTONS_CANCEL_OKAY;
          when Retry_Cancel_Buttons =>
            Kind := BUTTONS_CANCEL_RETRY;
        end case;
        case Icon is
          when Warning_Icon =>
            Kind := Kind or ICON_WARNING;
          when Information_Icon =>
            Kind := Kind or ICON_INFORMATION;
          when Error_Icon =>
            Kind := Kind or ICON_ERROR;
          when No_Icon =>
            null;
        end case;
        User_Interaction :=
          Message_Box(
            Window  => Find_Window(To_String_2_C(NAME), NULL_ADDRESS),
            Caption => To_String_2_C(Title),
            Text    => To_String_2_C(Message),
            Kind    => Kind);
        if
        User_Interaction = PRESSED_OKAY  or
        User_Interaction = PRESSED_RETRY or
        User_Interaction = PRESSED_YES
        then
          return True;
        end if;
        return False;
      end Is_Okay;
  end Implementation;
