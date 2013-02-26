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
  System,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Interfaces,
  Interfaces.C,
  Neo.Windows,
  Neo.System,
  Neo.System.Input,
  Neo.Foundation.Package_Testing,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types;
use
  System,
  Ada.Strings,
  Ada.Strings.Wide_Fixed,
  Interfaces,
  Interfaces.C,
  Neo.Windows,
  Neo.System,
  Neo.System.Input,
  Neo.Foundation.Package_Testing,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types;
procedure Test_Input
  is
  ---------------
  -- Constants --
  ---------------
    NUMBER_OF_CONTROLLERS_ALLOWED : Integer_4_Positive := 4;
    SECONDS_TO_TEST_FOR_REMOVAL_AND_ADDITION_OF_DEVICES : Float_4_Real := 4.0;
    INPUT_WINDOW_STYLE : constant Integer_4_Unsigned_C :=(
      STYLE_NO_ACTIVATE);
  ---------------
  -- Variables --
  ---------------
    Devices              : Access_Array_Record_Device;
    Capabilities         : Record_Device_Capabilities := NULL_RECORD_DEVICE_CAPABILITIES;
    Window               : Address                    := NULL_ADDRESS;
    Number_Of_Characters : Integer_4_Unsigned_C       := 0;
    Size                 : Integer_4_Unsigned_C       := 0;
    Name_Start           : Integer_4_Signed           := 0;
    Class:
      Record_Window_Class :=(
      Size       => Record_Window_Class'Size / 8,
      Style      => 0,
      Callback   => Window_Callback'Address,
      Extra_A    => 0,
      Extra_B    => 0,
      Instance   => Get_Current_Instance,
      Icon_Small => Load_Icon(Get_Current_Instance, GENERIC_ICON),
      Icon_Large => Load_Icon(Get_Current_Instance, GENERIC_ICON),
      Cursor     => Load_Cursor(NULL_ADDRESS, GENERIC_CURSOR),
      Background => BRUSH_GRAY,
      Menu_Name  => null,
      Class_Name => To_Access_Constant_Character_2_C(Class_Name));
    Device_Setups:
      Array_Record_Device_Setup :=(
      1 =>(
        Page   => GENERIC_DESKTOP_CONTROL,
        Usage  => USE_RAW_KEYBOARD,
        Flags  => TAKE_INPUT_ON_NON_ACTIVE and IGNORE_LEGACY_MESSAGES,
        Target => Window),
      2 =>(
        Page   => GENERIC_DESKTOP_CONTROL,
        Usage  => USE_RAW_MOUSE,
        Flags  => TAKE_INPUT_ON_NON_ACTIVE and IGNORE_LEGACY_MESSAGES,
        Target => Window));
      -- 3 =>(
      --   Page   => GENERIC_DESKTOP_CONTROL,
      --   Usage  => USE_RAW_JOYSTICK,
      --   Flags  => TAKE_INPUT_ON_NON_ACTIVE,
      --   Target => Window),
      -- 4 =>(
      --   Page   => GENERIC_DESKTOP_CONTROL,
      --   Usage  => USE_RAW_GAMEPAD,
      --   Flags  => TAKE_INPUT_ON_NON_ACTIVE,
      --   Target => Window));
  --------------------
  -- Update_Devices --
  --------------------
    procedure Update_Devices
      return Access_Array_Record_Device
      is
      Number_Of_Devices : Integer_4_Unsigned_C := 0;
      begin
        if
        Get_Device_List(
          List  => NULL_ADDRESS,
          Count => Number_Of_Devices'Address,
          Size  => Record_Device_List_Element'Size / 8) = -1
        then
          raise System_Call_Failure;
        end if;
        if Number_Of_Devices = 0 then
          raise No_Input_Devices;
        end if;
        -----------
        Fetch_List:
        -----------
          declare
          List : Array_Record_Device_List_Element(1..Integer_4_Signed(Number_Of_Devices)) :=(
            others => NULL_RECORD_DEVICE_LIST_ELEMENT);
          begin
            if
            Get_Device_List(
              List  => List(1)'Address,
              Count => Number_Of_Devices'Address,
              Size  => Record_Device_List_Element'Size / 8) = -1
            then
              raise System_Call_Failure;
            end if;
            return List'Access;
          end Update_Device_List;
      end Update_Devices;
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize(
      Class_Name : in String_2)
      is
      -------------------------
      function Window_Callback(
      -------------------------
        Window        : Address;
        Message       : Integer_4_Unsigned_C;
        Data_Unsigned : Integer_4_Unsigned_C;
        Data_Signed   : Integer_4_Signed_C)
        return Integer_4_Signed_C;
        pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(
        Window        : Address;
        Message       : Integer_4_Unsigned_C;
        Data_Unsigned : Integer_4_Unsigned_C;
        Data_Signed   : Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        Size   : Integer_4_Unsigned_C       := 0;
        Device : Access_Record_Input_Device := To_Access_Record_Input_Device(Data_Signed);
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_DEVICE_INPUT =>
              if
              Get_Raw_Input_Data(
                Raw_Input   => To_Address(Data_Signed),
                Command     => GET_RAW_INPUT_DEVICE_DATA,
                Data        => NULL_ADDRESS,
                Size        => Size'Address,
                Header_Size => Record_Raw_Input_Header'Size / 8) /= 0
              then
                raise System_Call_Failure;
              end if;
              if Size < Record_RAWINPUT'Size / 8 then
                --raise System_Call_Failure;
                return; -- Unexpected packet
              end if;
              -------------
              Handle_Input:
              -------------
                declare
                Data : array (1..Size) of Integer_1_Unsigned_C := (others => 0);
                begin
                  if 
                  Get_Raw_Input_Data(
                    Raw_Input   => To_Address(Data_Signed),
                    Command     => GET_RAW_INPUT_DEVICE_DATA,
                    Data        => Data'Address,
                    Size        => Size'Address,
                    Header_Size => Record_Raw_Input_Header'Size / 8) /= Size
                  then
                    raise System_Call_Failure;
                  end if;
                  case Raw_Input.Header.Kind is
                    when KIND_IS_RAW_KEYBOARD =>
                      if Device. = EVENT_KEY_PRESS then
                        null;
                      else
                        null;
                      end if;
                    when KIND_IS_RAW_MOUSE =>
                      null;
                    when KIND_IS_RAW_HUMAN_INTERFACE_DEVICE =>
                      loop
                        // Fill up buffer,
                        int count = GetRawInputBuffer((PRAWINPUT)buffer, &bufSize, RIH_SIZE);

                        if (count <= 0)
                        {
                          break;
                        }

                        // Process all the events, 
                        const RAWINPUT* raw = (const RAWINPUT*)buffer;

                        while (count-- > 0)
                        {
                          processRawInput(*raw, background);
                          raw = NEXTRAWINPUTBLOCK(raw);
                        }
                      end loop;
                    when others =>
                      null;
                  end case;
                end Handle_Input;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      begin
        if Register_Class(Class'Address) = Integer_2_Unsigned_C(FAILED) then
          raise System_Call_Failure;
        end if;
        Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Name),
            Window_Name => To_String_2_C(Class_Name), 
            Style       => 0,
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        if Window = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        --ShowWindow(h,showCmd);
        --UpdateWindow(h);
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is 
      Empty_Device : Record_Device :=(
        Page   => 1,
        Usage  => 2,
        Flags  => DEVICE_REMOVE,
        Target => NULL_ADDRESS);
      begin
        null;
        if Register_Devices(Empty_Device'Access, 1, Record_Device'Size / 8) = FAILED then
          raise System_Call_Failure;
        end if;
        Free(Devices);
        -- cleanup_window();
      end Finalize;
  ----------
  -- Main --
  ----------
    Message   : Record_Message := (others => <>);
    Window    : Address        := Primary_Window;
    Last_Time : Float_4_Real   := 0.0;
    Devices   : Access_Array_Record_ := null;
    begin
      Put_Title("INPUT TEST");
      Initialize;
      loop
        if
        Peek_Message(
          Message        => Message'Access,
          Window         => Window,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        then
          if Message.Data = MESSAGE_QUIT then
            exit;
          elsif
          Translate_Message(Message'Access) < 2 and then
          Dispatch_Message(Message'Access) = 0
          then
            null;
          end if;
        end if;
        if Get_Time - Last_Time >= SECONDS_TO_TEST_FOR_REMOVAL_AND_ADDITION_OF_DEVICES then
          ---------------
          Update_Devices:
          ---------------
            declare
            begin
            end Update_Devices;
        end if;
      end loop;
      Finalize;
      Hang_Window;
  end Test_Input;
