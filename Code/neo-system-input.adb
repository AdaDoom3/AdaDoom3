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
package body Neo.System.Input
  is
  --------------------
  -- Implementation --
  --------------------
    package body Implementation
      is separate;
    package Instantiated_Implementation
      is new Implementation(
      Add_Device     => ,
      Remove_Device  => ,
      Get_Device     => ,
      Handle_Key     => ,
      Handle_Key     => ,
      Handle_Mouse   => ,
      Handle_Stick   => ,
      Handle_Stick   => ,
      Handle_Trigger => ,
      Handle_Trigger => );
    package Implementation
      renames Instantiated_Implementation;
  ----------------
  -- Task_Input --
  ----------------
    task body Task_Input(
      Title : in String_2)
      is
      Last_Time : Time := Clock;
      begin
        accept Initialize;
        Implementation.Initialize(Title);
        Set_Priority();
        loop
          select 
            accept Finalize
              do
                Implementation.Finalize;
                exit;
              end Finalize;
          or
            accept Disable
              do
                accept Enable;
              end Disable;
          else
            if Clock >= Last_Time + DURATION_TO_WAIT_BEFORE_POLLING then
              Implementation.Poll_Devices;
              Last_Time := Clock;
            end if;
          end select;
        end loop;
      end Task_Input;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        null;
      end Test;
  ----------------
  -- Initialize --
  ----------------
    procedure Initalize
      is
      begin
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
      end Finalize;
  -------------
  -- Disable --
  -------------
    procedure Disable
      is
      begin
      end Disable;
  ------------
  -- Enable --
  ------------
    procedure Enable
      is
      begin
      end Enable;
  ----------------
  -- Add_Device --
  ----------------
    procedure Add_Device(
      Device : in Record_Device)
      is
      begin
      end Add_Device;
  -------------------
  -- Remove_Device --
  -------------------
    procedure Remove_Device(
      Identifier : in Integer_8_Unsigned)
      is
      begin
      end Remove_Device;
  ----------------
  -- Get_Device --
  ----------------
    function Get_Device(
      Identifier : in Integer_8_Unsigned)
      return Record_Device
      is
      begin
      end Get_Device;
  -----------------
  -- Get_Devices --
  -----------------
    function Get_Devices
      return Array_Record_Device
      is
      begin
        return Devices(1..Number_Of_Devices);
      end Get_Devices;
  ------------------------
  -- Is_Player_Pressing --
  ------------------------
    function Is_Player_Pressing(
      Player : in Integer_4_Positive;
      Key    : in Enumerated_Key)
      return Boolean
      is
      begin
      end Is_Player_Pressing;
    function Is_Player_Pressing(
      Player : in Record_Player;
      Key    : in Enumerated_Key)
      return Boolean;
      is
      begin
      end Is_Pressing;
  ---------------------------
  -- Get_Number_Of_Devices --
  ---------------------------
    function Get_Number_Of_Devices
      return Integer_4_Natural
      is
      begin
      end Get_Number_Of_Devices;
  -----------------
  -- Get_Devices --
  -----------------
    function Get_Devices
      return Array_Record_Device
      is
      begin
      end Get_Devices;
  ----------------
  -- Get_Device --
  ----------------
    function Get_Device(
      Identifier : in Integer_8_Unsigned)
      return Record_Device
      is
      begin
      end Get_Device;
  ----------------
  -- Get_Player --
  ----------------
    function Get_Player(
      Player : in Integer_4_Positive)
      return Record_Player
      is
      begin
      end Get_Player;
  ------------------------
  -- Get_Player_Trigger --
  ------------------------
    function Get_Player_Trigger(
      Player  : in Integer_4_Positive;
      Trigger : in Enumerated_Trigger)
      return Float_4_Percent
      is
      begin
      end Get_Player_Trigger;
    function Get_Player_Trigger(
      Player  : in Integer_4_Positive;
      Device  : in Integer_8_Unsigned;
      Trigger : in Integer_4_Positive)
      return Float_4_Percent
      is
      begin
      end Get_Player_Trigger;
  ----------------------
  -- Get_Player_Stick --
  ----------------------
    function Get_Player_Stick(
      Player : in Integer_4_Positive;
      Stick  : in Enumerated_Stick)
      return Record_Input_Coordinate
      is
      begin
      end Get_Player_Stick;
    function Get_Player_Stick(
      Player : in Integer_4_Positive;
      Device : in Integer_8_Unsigned;
      Stick  : in Integer_4_Positive)
      return Record_Input_Coordinate
      is
      begin
      end Get_Player_Stick
  ----------------------
  -- Get_Player_Mouse --
  ----------------------
    function Get_Player_Mouse(
      Player : in Integer_4_Positive)
      return Record_Input_Coordinate
      is
      begin
      end Get_Player_Mouse;
  ------------------------------------
  -- Get_Character_From_Player_Keys --
  ------------------------------------
    function Get_Character_From_Player_Keys
      return Character_2
      is
      begin
      end Get_Character_From_Player_Keys;
  -----------------------
  -- Set_Device_Player --
  -----------------------
    procedure Set_Device_Player(
      Identifier : in Integer_8_Unsigned;
      Player     : in Integer_4_Positive)
      is
      begin
      end Set_Device_Player;
  -------------------
  -- Set_Vibration --
  -------------------
    procedure Set_Vibration(
      Player                 : in Integer_4_Positive;
      Percent_Frequency_High : in Float_4_Percent;
      Percent_Frequency_Low  : in Float_4_Percent;
      Seconds                : in Duration)
      is
      begin
      end Set_Vibration;
    generic
      with
        procedure Vibration_Equation(
          Seconds_Left           : in     Float_4_Real;
          Percent_Frequency_High :    out Float_4_Percent;
          Percent_Frequency_Low  :    out Float_4_Percent);
    procedure Set_Vibration(
      Player  : in Integer_4_Positive;
      Seconds : in Duration)
      is
      begin
      end Set_Vibration;
  ----------------
  -- Handle_Key --
  ----------------
    procedure Handle_Key(
      Device : in Integer_8_Unsigned;
      Key    : in Enumerated_Key)
      is
      begin
      end Handle_Key;
    procedure Handle_Key(
      Key    : in Integer_4_Positive;
      Device : in Integer_8_Unsigned)
      is
      begin
      end Handle_Key;
  ------------------
  -- Handle_Mouse --
  ------------------
    procedure Handle_Mouse(
      Device : in Integer_8_Unsigned;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed)
      is
      begin
      end Handle_Mouse;
  ------------------
  -- Handle_Stick --
  ------------------
    procedure Handle_Stick(
      Device : in Integer_8_Unsigned;
      Stick  : in Enumerated_Stick;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed)
      is
      begin
      end Handle_Stick;
    procedure Handle_Stick(
      Device : in Integer_8_Unsigned
      Stick  : in Integer_4_Positive;
      X      : in Integer_8_Signed;
      Y      : in Integer_8_Signed)
      is
      begin
      end Handle_Stick;
  --------------------
  -- Handle_Trigger --
  --------------------
    procedure Handle_Trigger(
      Device  : in Integer_8_Unsigned;
      Trigger : in Enumerated_Trigger;
      Percent : in Float_4_Percent)
      is
      begin
      end Handle_Trigger;
    procedure Handle_Trigger(
      Device  : in Integer_8_Unsigned;
      Trigger : in Integer_4_Positive;
      Percent : in Float_4_Percent)
      is
      begin
      end Handle_Trigger;
  end Neo.System.Input;
