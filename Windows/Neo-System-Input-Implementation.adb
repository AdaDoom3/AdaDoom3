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
  Ada.Unchecked_Deallocation,
  Ada.Unchecked_Conversion,
  Interfaces,
  Interfaces.C,
  Neo.Windows,
  Neo.System,
  Neo.System.Input,
  Neo.System.Processor,
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
  Neo.System.Processor,
  Neo.Foundation.Package_Testing,
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types;
procedure Test_Input
  is
  ---------------
  -- Constants --
  ---------------
    INPUT_WINDOW_STYLE : constant Integer_4_Unsigned_C := STYLE_NO_ACTIVATE;
    MAP_KEY:
      constant array (SUBEVENT_KEY_LEFT_MOUSE..SUBEVENT_KEY_CLEAR) of Enumerated_Key :=(
      Left_Mouse_Key,        Right_Mouse_Key,       Cancel_Key,            Middle_Mouse_Key,
      Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key, Null_Key,              Backspace_Key,
      Tab_Key,               Null_Key,              Null_Key,              Clear_Key,
      Enter_Key,             Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Pause_Break_Key,       Capital_Lock_Key, 
      Kana_Key,              Null_Key,              Junja_Key,             Final_Key,
      Hanja_Key,             Kanji_Key,             Escape_Key,            Convert_Key,
      No_Convert_Key,        Accept_Key,            Mode_Change_Key,       Space_Key, 
      Page_Up_Key,           Page_Down_Key,         End_Key,               Home_Key,
      Left_Arrow_Key,        Up_Arrow_Key,          Right_Arrow_Key,       Down_Arrow_Key,
      Select_Key,            Print_Key,             Execute_Key,           Print_Screen_Key,
      Insert_Key,            Delete_Key,            Help_Key,              Zero_Key,
      One_Key,               Two_Key,               Three_Key,             Four_Key,
      Five_Key,              Six_Key,               Seven_Key,             Eight_Key,
      Nine_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      A_Key,                 B_Key,                 C_Key,                 D_Key,
      E_Key,                 F_Key,                 G_Key,                 H_Key,
      I_Key,                 J_Key,                 K_Key,                 L_Key,
      M_Key,                 N_Key,                 O_Key,                 P_Key,
      Q_Key,                 R_Key,                 S_Key,                 T_Key,
      U_Key,                 V_Key,                 W_Key,                 X_Key,
      Y_Key,                 Z_Key,                 Left_Windows_Key,      Right_Windows_Key,
      Application_Menu_Key,  Null_Key,              System_Sleep_Key,      Pad_Zero_Key, 
      Pad_One_Key,           Pad_Two_Key,           Pad_Three_Key,         Pad_Four_Key,
      Pad_Five_Key,          Pad_Six_Key,           Pad_Seven_Key,         Pad_Eight_Key,
      Pad_Nine_Key,          Pad_Star_Key,          Pad_Plus_Key,          Separator_Key,
      Pad_Dash_Key,          Pad_Period_Key,        Pad_Slash_Key,         F1_Key,
      F2_Key,                F3_Key,                F4_Key,                F5_Key,
      F6_Key,                F7_Key,                F8_Key,                F9_Key,
      F10_Key,               F11_Key,               F12_Key,               F13_Key,
      F14_Key,               F15_Key,               F16_Key,               F17_Key,
      F18_Key,               F19_Key,               F20_Key,               F21_Key,
      F22_Key,               F23_Key,               F24_Key,               Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Number_Lock_Key,
      Scroll_Lock_Key,       OEM_Specific_1_Key,    OEM_Specific_2_Key,    OEM_Specific_3_Key,
      OEM_Specific_4_Key,    OEM_Specific_5_Key,    Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Left_Shift_Key,
      Right_Shift_Key,       Left_Control_Key,      Right_Control_Key,     Left_Alternative_Key,
      Right_Alternative_Key, Web_Backward_Key,      Web_Forward_Key,       Web_Refresh_Key,
      Web_Stop_Key,          Web_Search_Key,        Web_Favorites_Key,     Web_Home_Key,
      Volume_Mute_Key,       Volume_Down_Key,       Volume_Up_Key,         Next_Track_Key,
      Previous_Track_Key,    Stop_Track_Key,        Play_Pause_Track_Key,  Web_Mail_Key,
      Media_Select_Key,      Application_1_Key,     Application_2_Key,     Null_Key,
      Null_Key,              Semicolon_Key,         Equals_Key,            Comma_Key,
      Dash_Key,              Period_Key,            Slash_Key,             Grave_Accent_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Null_Key,              Null_Key,
      Null_Key,              Null_Key,              Left_Bracket_Key,      Backslash_Key,
      Right_Bracket_Key,     Apostrophe_Key,        OEM_Specific_7_Key,    Null_Key,
      OEM_Specific_8_Key,    OEM_102_Key,           OEM_Specific_9_Key,    OEM_Specific_10_Key,
      Process_Key,           OEM_Specific_11_Key,   Null_Key,              Null_Key,
      OEM_Specific_12_Key,   OEM_Specific_13_Key,   OEM_Specific_14_Key,   OEM_Specific_15_Key,
      OEM_Specific_16_Key,   OEM_Specific_17_Key,   OEM_Specific_18_Key,   OEM_Specific_19_Key,
      OEM_Specific_20_Key,   OEM_Specific_21_Key,   OEM_Specific_22_Key,   OEM_Specific_23_Key,
      OEM_Specific_24_Key,   Attention_Key,         Clear_Selection_Key,   Exsel_Key,
      Erase_EOF_Key,         Play_Key,              Zoom_Key,              Null_Key,
      PA1_Key,               Clear_Key);
  ---------------
  -- Variables --
  ---------------
    Devices   : Access_Array_Record_Device;
    Window    : Address                    := NULL_ADDRESS;
    Message   : Record_Message             := (others => <>);
    Last_Time : Integer_8_Unsigned         := 0;
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
  ---------------
  -- Accessors --
  ---------------
    function To_Access_Record_Device_Keyboard
      is new Ada.Unchecked_Conversion(Address, Access_Record_Device_Header);
    function To_Access_Record_Device_Header
      is new Ada.Unchecked_Conversion(Address, Access_Record_Device_Header);
  ------------------
  -- Create_Device --
  -------------------
    function Create_Device(
      Handle : in Address)
      return Record_Device
      is
      Result : Record_Device := (Identifier => To_Integer_8_Unsigned(Handle), others => <>);
      begin
        if 
        Get_Raw_Input_Device_Information(
          Device  => Device_Handle,
          Command => GET_RAW_DEVICE_NAME,
          Data    => NULL_ADDRESS,
          Size    => Number_Of_Characters'Address) /= 0
        then
          raise System_Call_Failure;
        end if;
        if Number_Of_Characters < 2 then
          raise System_Call_Failure;
        end if;
        ---------------
        Get_Identifier:
        ---------------
          declare
          Identifier : String_2(1..Integer_4_Signed(Number_Of_Characters)) := (Others => NULL_CHARACTER_2);
          begin
            if 
            Get_Raw_Input_Device_Information(
              Device  => Device_Handle,
              Command => GET_RAW_DEVICE_NAME,
              Data    => Identifier(1)'Address,
              Size    => Number_Of_Characters'Address) < 0
            then
              raise System_Call_Failure;
            end if;
            if Name_Start = 0 then
              for I in Identifier'Range loop
                if Identifier(I) /= '?' and Identifier(I) /= '\' then
                  Name_Start := I;
                  exit;
                end if;
                if I = Identifier'Last then
                  raise System_Call_Failure;
                end if;
              end loop;
            elsif Name_Start >= Identifier'Last then
              raise System_Call_Failure;
            end if;
            if
            Identifier'Last > Name_Start + 3 and then(
            Identifier(Name_Start..Name_Start + 3) = "Root" or
            Identifier(Name_Start..Name_Start + 3) = "ROOT" or
            Identifier(Name_Start..Name_Start + 3) = "root")
            then
              return (SYSTEM_DEVICE_NAME & (others => NULL_CHARACTER_2), );
            end if;
            if
            Get_Raw_Input_Device_Information(
              Device  => Device.Handle, 
              Command => GET_RAW_DEVICE_PREPARSED_DATA,
              Data    => NULL_ADDRESS,
              Size    => Size'Address) /= 0
            then 
              raise System_Call_Failure;
            end if;
            ---------------------
            Get_Device_Specifics:
            ---------------------
              declare
              ---------------------------------
              function Get_Beginning_Of_String(
              ---------------------------------
                Item : in String_2)
                return String_2
                is
                Dynamic_Item : Access_String_2 := null;
                begin
                  for I in Item'Range loop
                    if Item(I) = NULL_CHARACTER_2 then
                      if I = Item'First then
                        raise System_Call_Failure;
                      end if;
                      return Item(Item'First..I - 1);
                    end if;
                  end loop;
                  raise System_Call_Failure;
                end Get_Beginning_Of_String;
              Data   : Array_Integer_1_Unsigned_C(1..Integer_4_Signed(Size)) := (others => 0);
              File   : Address                                               := NULL_ADDRESS;
              Buffer : String_2(Result.Name'First..Result.Name'Last)         := (others => NULL_CHARACTER_2);
              -----
              begin
              -----
                File :=
                  Create_File(
                    Name                 => Identifier(1)'Address,
                    Desired_Access       => GENERIC_READ or GENERIC_WRITE,
                    Share_Mode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                    Security_Attributes  => NULL_ADDRESS,
                    Creation_Desposition => OPEN_EXISTING,
                    Flags_And_Attributes => 0,
                    Template_File        => NULL_ADDRESS);
                if File = NULL_ADDRESS or Data'Size < 8 then
                  raise System_Call_Failure;
                end if;
                if
                Get_Device_Information(
                  Device  => Device.Handle, 
                  Command => GET_RAW_DEVICE_PREPARSED_DATA,
                  Data    => Data(1)'Address,
                  Size    => Size'Address) < 0
                then 
                  raise System_Call_Failure;
                end if;
                if
                Get_Device_Product(
                  File   => File,
                  Buffer => Buffer(1)'Address,
                  Size   => Buffer'Size / 8) /= FAILED
                then
                  Device.Name := Get_Beginning_Of_String(Buffer);
                end if;
                if
                Get_Device_Manufacturer(
                  File   => File,
                  Buffer => Buffer(1)'Address,
                  Size   => Buffer'Size / 8) /= FAILED
                then
                  Device.Manufacturer := Get_Beginning_Of_String(Buffer);
                end if;
                when others =>
                  raise System_Call_Failure;
              end case;
            end if;
      end Create_Device;
  --------------------
  -- Update_Devices --
  --------------------
    procedure Update_Devices
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
          Have_Checked_Device : array (1..Integer_4_Signed(Number_Of_Devices)) of boolean := (others => False);
          Something_Has_Changed : Boolean := False;
          List : Array_Record_Device_List_Element(1..Integer_4_Signed(Number_Of_Devices)) := (others => <>);
          ------------------------
          procedure Create_Devices
          ------------------------
            is
            begin
              Devices := new Array_Record_Device(List'First..List'Last);
              for I in Devices'Range loop
                Devices(I) :=(
                  Identifier => To_Integer_8_Unsigned(List(I).Device),
                  Name       => To_String_2(List(I).Name),
                  Player     => 0);
              end loop;
            end Create_Devices;
          begin
            if
            Get_Device_List(
              List  => List(1)'Address,
              Count => Number_Of_Devices'Address,
              Size  => Record_Device_List_Element'Size / 8) = -1
            then
              raise System_Call_Failure;
            end if;
            if Devices = null then
              Create_Devices;
            else
              for I in Devices'Range loop
                for J in List'Range loop
                  if To_Integer_8_Unsigned(List(J).Device) = Devices(I).Identifier then
                    Have_Checked_Device(J) := True;
                    exit;
                  end if;
                  if J = List'Last then
                    Something_Has_Changed := True;
                    Put_Line("Identifier " & Integer_8_Unsigned'Wide_Image(Devices(I).Identifier));
                    Put_Line("Name " & Devices(I).Name);
                    Put_Line("Removed");
                    New_Line;
                  end if;
                end loop;
              end loop;
              for I in Have_Checked_Device'Range loop
                if not Have_Checked_Device(I) then
                  Something_Has_Changed := True;
                  Put_Line("Identifier " & Address'Wide_Image(List(I).Device));
                  Put_Line("Name " & To_String_2(List(I).Name));
                  Put_Line("Added");
                  New_Line;
                end if;
              end loop;
              if Something_Has_Changed then
                Free(Devices);
                Create_Devices;
              end if;
            end if;
          end Fetch_List;
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
        Size : Integer_4_Unsigned_C := 0;
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_DEVICE_INPUT =>
              if
              Get_Device_Input_Data(
                Raw_Input   => To_Address(Data_Signed),
                Command     => GET_RAW_INPUT_DEVICE_DATA,
                Data        => NULL_ADDRESS,
                Size        => Size'Address,
                Header_Size => Record_Raw_Input_Header'Size / 8) /= 0
              then
                raise System_Call_Failure;
              end if;
              if Size < Record_Device'Size / 8 then
                --raise System_Call_Failure;
                return; -- Unexpected packet
              end if;
              ----------------
              Interpret_Input:
              ----------------
                declare
                Data   : array (1..Size) of Integer_1_Unsigned_C := (others => 0);
                Header : Access_Record_Device_Header := To_Access_Record_Device_Header(Data'Address);
                begin
                  if 
                  Get_Device_Input_Data(
                    Device_Input => Data'Address,
                    Command      => GET_RAW_INPUT_DEVICE_DATA,
                    Data         => Data'Address,
                    Size         => Size'Address,
                    Header_Size  => Record_Raw_Input_Header'Size / 8) /= Size
                  then
                    raise System_Call_Failure;
                  end if;
                  for I in Devices'Range loop
                    if Devices(I).Identifier = To_Integer_8_Unsigned(Header.Device) then
                      Put_Line("Identifier " & Integer_8_Unsigned'Wide_Image(Devices(I).Identifier));
                      Put_Line("Name " & Devices(I).Name);
                      exit;
                    end if;
                    if I = Devices'Last then
                      Update_Devices;
                    end if;
                  end loop;
                  case Header.Kind is
                    when KIND_IS_RAW_KEYBOARD =>
                      ----------------
                      Handle_Keyboard:
                      ----------------
                        declare
                        Keyboard : Access_Record_Device_Keyboard := To_Access_Record_Device_Keyboard(Data'Address);
                        begin
                          if Keyboard.Message = EVENT_KEY_PRESS then
                            Put_Line("Pressed " & Enumerated_Key'Wide_Image(MAP_KEY(Keyboard.Key)));
                          else
                            Put_Line("Released " & Enumerated_Key'Wide_Image(MAP_KEY(Keyboard.Key)));
                          end if;
                          New_Line;
                        end Handle_Keyboard;
                    when KIND_IS_RAW_MOUSE =>
                      null;
                    when KIND_IS_RAW_HUMAN_INTERFACE_DEVICE =>
                      null;
                      -- loop
                      --   int count = GetRawInputBuffer((PRAWINPUT)buffer, &bufSize, RIH_SIZE);
                      --   if count <= 0 then
                      --     exit;
                      --   end if;
                      --   const RAWINPUT* raw = (const RAWINPUT*)buffer;
                      --   while count > 0 loop
                      --     processRawInput(*raw, background);
                      --     raw = NEXTRAWINPUTBLOCK(raw);
                      --     Count := Count - 1;
                      --   end loop;
                      -- end loop;
                    when others =>
                      null;
                  end case;
                end Interpret_Input;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
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
        if Window /= NULL_ADDRESS then
          if Destroy_Window(Window) = FAILED then 
            raise System_Call_Failure;
          end if;
        end if;
        Window := NULL_ADDRESS;
      end Finalize;
  ----------
  -- Main --
  ----------
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
        if
        Get_Clock_Ticks - Last_Time >= Integer_8_Unsigned(
        SECONDS_TO_TEST_FOR_REMOVAL_AND_ADDITION_OF_DEVICES * 1000) then
          Update_Devices;
          Last_Time := Get_Clock_Ticks;
        end if;
      end loop;
      Finalize;
      Hang_Window;
  end Test_Input;

