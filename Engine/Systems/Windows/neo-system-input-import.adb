with Neo.Link.Windows; use Neo.Link.Windows;
with Interfaces.C;     use Interfaces.C;
with Interfaces;       use Interfaces;
with System;           use System;
separate (Neo.System.Input)
generic
  with
  with
  with
package body Implementation
  is
  ---------------
  -- Constants --
  ---------------
    MAP_KEY : constant array(VIRTUAL_KEY_LEFT_MOUSE..VIRTUAL_KEY_CLEAR) of Enumerated_Key :=(
      Left_Mouse_Key,        Right_Mouse_Key,       Cancel_Key,            Middle_Mouse_Key,
      Auxiliary_1_Mouse_Key, Auxiliary_2_Mouse_Key, Null_Key,              Backspace_Key,
      Tab_Key,               Null_Key,              Null_Key,              Clear_Key,
      Enter_Key,             Null_Key,              Null_Key,              Shift_Key,
      Control_Key,           Alternative_Key,       Pause_Break_Key,       Capital_Lock_Key,
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
  -------------
  -- Records --
  -------------
    type Record_Lookup_Character_Internals
      is record
        Map_Key_Back : Array(Enumerated_Key'range) of Integer_2_Unsigned_C := (others => 0);
      end record;
  --------------
  -- Packages --
  --------------
    package Protected_Lookup_Character_Internals
      is new Protected_Data(Record_Lookup_Character_Internals, (others => <>));
    package Protected_Array_Map_Key_Back
      is new Protected_Data(Array_Map_Key_Back, (others => <>));
  ---------------
  -- Protected --
  ---------------
    protected type Map_Key_Back
      is
        procedure Set(
        function Get(
          Key : in Enumerated_Key
          return Integer_2_Unsigned;
      private
        Data   : Type_To_Protect := Initial_Type_To_Protect;
        Status : Boolean         := False;
      end Data;
  ---------------
  -- Variables --
  ---------------
    Lookup_Character_Data : Protected_Lookup_Character_Internals.Data;
    Window                : Address := NULL_ADDRESS;
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
  ------------------------------------
  -- Finalize_Lookup_Character_Data --
  ------------------------------------
    procedure Finalize_Lookup_Character_Data
      is
      begin
      end Finalize_Lookup_Character_Data;
  ----------------------
  -- Lookup_Character --
  ----------------------
    function Lookup_Character(
      Key                              : in Enumerated_Key;
      Is_Capital_Lock_Enabled          : in Boolean;
      Is_Number_Lock_Enabled           : in Boolean;
      Is_Left_Shift_Key_Pressed        : in Boolean;
      Is_Right_Shift_Key_Pressed       : in Boolean;
      Is_Left_Control_Key_Pressed      : in Boolean;
      Is_Right_Control_Key_Pressed     : in Boolean;
      Is_Left_Alternative_Key_Pressed  : in Boolean;
      Is_Right_Alternative_Key_Pressed : in Boolean;
      Is_Left_System_Key_Pressed       : in Boolean;
      Is_Right_System_Key_Pressed      : in Boolean;
      Is_Application_Menu_Key_Pressed  : in Boolean)
      return Character_2
      is
      -- int charCount = 0;
      -- *outputChar = 0;
      -- *deadChar = 0;
      -- short state = 0;
      -- int shift = -1;
      -- int mod = 0;
      -- WCHAR baseChar;
      -- WCHAR diacritic;
      -- KbdLayerDescriptor pKbdLayerDescriptor
      -- int capsLock = (GetKeyState(VK_CAPITAL) & 0x01);
      -- Current_Data : Record_Lookup_Character_Internals := Lookup_Character_Data.Get
      -- begin
      --   if Get_Keyboard_Layout_Name() = FAILED then
      --     raise Call_Failure;
      --   end if;
      --   if Current_Data.Layout /= Layout then
      --     if not Current_Data.Is_Initialized then
      --       -- Create Key to VK table
      --     end if;
      --     if not USE_64_BIT and then  then
      --       ptrPadding := sizeof(void *);
      --     end if;
      --     if GetSystemDirectory(systemDirectory, MAX_PATH) = FAILED then
      --       raise Call_Failure;
      --     end if;
      --     kbdLibrary          := Load_Library(layoutFile);
      --     pKbdLayerDescriptor := (KbdLayerDescriptor)GetProcAddress(kbdLibrary, "KbdLayerDescriptor");
      --     if pKbdLayerDescriptor = null then
      --       Finalize_
      --       raise Call_Failure;
      --     end if;
      --     PKBDTABLES pKbd := pKbdLayerDescriptor.All;
      --     -- Store the memory address of the following 3 structures.
      --     BYTE *base = (BYTE *) pKbd;
      --     -- First element of each structure, no offset adjustment needed.
      --     pVkToBit = pKbd->pCharModifiers->pVkToBit;
      --     -- Second element of pKbd, +4 byte offset on wow64.
      --     pVkToWcharTable = *((PVK_TO_WCHAR_TABLE *) (base + offsetof(KBDTABLES, pVkToWcharTable) + ptrPadding));
      --     -- Third element of pKbd, +8 byte offset on wow64.
      --     pDeadKey = *((PDEADKEY *) (base + offsetof(KBDTABLES, pDeadKey) + (ptrPadding * 2)));
      --     Lookup_Character_Data.Set(Current_Data);
      --   end if;
      --   -- Because this is only a structure of two bytes, we don't need to worry
      --   -- about the structure padding of __ptr64 offsets on Wow64.
      --   for I in 1.. loop
      --     exit when pVkToBit[i].Vk /= 0;
      --     state = GetAsyncKeyState(pVkToBit[i].Vk);
      --     if pVkToBit[i].Vk = VK_SHIFT then
      --       shift = i + 1; -- Get modification number for Shift key
      --     end if;
      --     if (state and not SHRT_MAX) /= 0 then
      --       if mod = 0 then
      --         mod = i + 1;
      --       else
      --         mod = 0; -- Two modifiers at the same time!
      --       end if;
      --     end if;
      --   end loop;
      --   -- Default 32 bit structure size should be 6 bytes (4 for the pointer and 2
      --   -- additional byte fields) that are padded out to 8 bytes by the compiler.
      --   unsigned short sizeVkToWcharTable = sizeof(VK_TO_WCHAR_TABLE);
      --   if not USE_64_BIT and then IsWow64 then
      --     -- If we are running under Wow64 the size of the first pointer will be
      --     -- 8 bringing the total size to 10 bytes padded out to 16.
      --     sizeVkToWcharTable = (sizeVkToWcharTable + ptrPadding + 7) & -8;
      --   end if;
      --   BYTE *ptrCurrentVkToWcharTable = (BYTE *) pVkToWcharTable;
      --   int cbSize, n;
      --   do
      --     -- cbSize is used to calculate n, and n is used for the size of pVkToWchars[j].wch[n]
      --     cbSize = *(ptrCurrentVkToWcharTable + offsetof(VK_TO_WCHAR_TABLE, cbSize) + ptrPadding);
      --     n = (cbSize - 2) / 2;
      --     -- Same as VK_TO_WCHARS pVkToWchars[] = pVkToWcharTable[i].pVkToWchars
      --     PVK_TO_WCHARS pVkToWchars = (PVK_TO_WCHARS) ((PVK_TO_WCHAR_TABLE) ptrCurrentVkToWcharTable)->pVkToWchars;
      --     if pVkToWchars /= NULL and mod < n then
      --       -- pVkToWchars[j].VirtualKey
      --       BYTE *pCurrentVkToWchars = (BYTE *) pVkToWchars;
      --       loop
      --         if ((PVK_TO_WCHARS) pCurrentVkToWchars)->VirtualKey == virtualKey {
      --           if (((PVK_TO_WCHARS) pCurrentVkToWchars)->Attributes == CAPLOK) && capsLock {
      --             if(mod == shift)
      --               mod = 0;
      --             else
      --               mod = shift;
      --             end if;
      --           end if;
      --           *outputChar = ((PVK_TO_WCHARS) pCurrentVkToWchars)->wch[mod];
      --           charCount = 1;
      --           -- Increment the pCurrentVkToWchars by the size of wch[n].
      --           pCurrentVkToWchars += sizeof(VK_TO_WCHARS) + (sizeof(WCHAR) * n);
      --           if *outputChar = WCH_NONE then
      --             charCount = 0;
      --           elsif *outputChar = WCH_DEAD then
      --             *deadChar = ((PVK_TO_WCHARS) pCurrentVkToWchars)->wch[mod];
      --             charCount = 0;
      --           end if;
      --           break;
      --         else
      --           -- Add sizeof WCHAR because we are really an array of WCHAR[n] not WCHAR[]
      --           pCurrentVkToWchars += sizeof(VK_TO_WCHARS) + (sizeof(WCHAR) * n);
      --         end if;
      --         exit when (PVK_TO_WCHARS) pCurrentVkToWchars)->VirtualKey != 0;
      --       end loop;
      --     end if;
      --     -- This is effectively the same as: ptrCurrentVkToWcharTable = pVkToWcharTable[++i];
      --     ptrCurrentVkToWcharTable += sizeVkToWcharTable;
      --     exit when cbSize /= 0;
      --   end loop;
      --   -- Code to check for dead characters...
      --   if *deadChar /= 0 then
      --     for I in 1.. loop
      --       exit when pDeadKey[i].dwBoth /= 0;
      --       baseChar = (WCHAR) pDeadKey[i].dwBoth;
      --       diacritic = (WCHAR) (pDeadKey[i].dwBoth >> 16);
      --       if baseChar = *outputChar and diacritic = *deadChar then
      --         *deadChar = 0;
      --         *outputChar = (WCHAR) pDeadKey[i].wchComposed;
      --       end if;
      --     end loop;
      --   end if;
      --   return charCount;
      end Lookup_Character;
  --------------------
  -- Update_Devices --
  --------------------
    procedure Update_Devices
      is
      Product      : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
      Manufacturer : aliased String_2(1..MAXIMUM_DESCRIPTION_CHRACTERS) := (Others => NULL_CHARACTER_2);
      Capabilities : aliased Record_Device_Capabilities                 := (others => <>);
      Number_Of_X  : aliased Integer_4_Unsigned_C                       := 0;
      Device       :         Record_Device                              := (others => <>);
      File         :         Address                                    := NULL_ADDRESS;
      begin
        if
        Get_Device_List(
          List  => NULL_ADDRESS,
          Count => Number_Of_X'address,
          Size  => Record_Device_List_Element'size / Byte'size) = -1
        then
          raise Call_Failure;
        end if;
        if Number_Of_X = 0 then
          raise No_Input_Devices_Detected;
        end if;
        -----------
        Build_List:
        -----------
          declare
          List : aliased Array_Record_Device_List_Element(1..Integer(Number_Of_X)) := (others => <>);
          begin
            if
            Get_Device_List(
              List  => List(List'first)'address,
              Count => Number_Of_X'address,
              Size  => Record_Device_List_Element'size / Byte'size) = -1
            then
              Put_Line(Integer_4_Unsigned_C'Wide_Image(Get_Last_Error));
              raise Call_Failure;
            end if;
            for I in List'range loop
              if not Is_Device_Present(Handle) then
                case List(I).Kind is
                  when KIND_IS_KEYBOARD | KIND_IS_MOUSE =>
                    Add_Device((Handle, others => <>));
                  when KIND_IS_HUMAN_INTERFACE_DEVICE =>
                    Device := (others => <>);
                    if
                    Get_Device_Information(
                      Device  => Handle,
                      Command => GET_DEVICE_NAME,
                      Data    => NULL_ADDRESS,
                      Size    => Number_Of_X'access) /= 0
                    then
                      raise Call_Failure;
                    end if;
                    if Number_Of_X < 2 then
                      raise Call_Failure;
                    end if;
                    ---------------
                    Get_Identifier:
                    ---------------
                      declare
                      Identifier : aliased String_2(1..Integer(Number_Of_X)) := (Others => NULL_CHARACTER_2);
                      begin
                        if
                        Get_Device_Information(
                          Device  => Handle,
                          Command => GET_DEVICE_NAME,
                          Data    => Identifier'access,
                          Size    => Number_Of_X'access) < 0
                        then
                          raise Call_Failure;
                        end if;
                        File :=
                          Create_File(
                            Name                 => Identifier'access,
                            Desired_Access       => GENERIC_READ or GENERIC_WRITE,
                            Share_Mode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                            Security_Attributes  => NULL_ADDRESS,
                            Creation_Desposition => OPEN_EXISTING,
                            Flags_And_Attributes => 0,
                            Template_File        => NULL_ADDRESS);
                        if File = NULL_ADDRESS then
                          raise Call_Failure;
                        end if;
                        if
                        Get_Device_Product(
                          File   => File,
                          Buffer => Product'access,
                          Size   => Product'size / Byte'size) = FAILED
                        then
                          null;
                        end if;
                        if
                        Get_Device_Manufacturer(
                          File   => File,
                          Buffer => Manufacturer'access,
                          Size   => Manufacturer'size / Byte'size) = FAILED
                        then
                          null;
                        end if;
                        if Close_Handle(File) = FAILED then
                          raise Call_Failure;
                        end if;
                        if
                        Get_Device_Information(
                          Device  => Handle,
                          Command => GET_PREPARSED_DATA,
                          Data    => NULL_ADDRESS,
                          Size    => Number_Of_X'access) /= 0
                        then
                          raise Call_Failure;
                        end if;
                        if Number_Of_X < 2 then
                          raise Call_Failure;
                        end if;
                        -------------------
                        Get_Preparsed_Data:
                        -------------------
                          declare
                          Data : aliased Array_Integer_1_Unsigned_C(1..Integer(Number_Of_X)) := (others => 0);
                          begin
                            if
                            Get_Device_Information(
                              Device  => Handle,
                              Command => GET_PREPARSED_DATA,
                              Data    => Data'access,
                              Size    => Number_Of_X'access) /= 0
                            then
                              raise Call_Failure;
                            end if;
                            if
                            Get_Device_Capabilities(
                              Data         => Data'access,
                              Capabilities => Capabilities'access) /=
                            then
                              raise Call_Failure;
                            end if;
                            if Number_Of_X > 0 then
                              ------------
                              Get_Buttons:
                              ------------
                                declare
                                Buttons : aliased Array_Record_Button_Capability(1..Integer(Number_Of_X)) := (others => <>);
                                begin
                                  if
                                  Get_Device_Buttons(
                                    Report_Kind => DEVICE_INPUT,
                                    Buttons     => Buttons'access,
                                    Length      => Number_Of_X'access,
                                    Data        => Data'access) = FAILED
                                  then
                                    raise Call_Failure;
                                  end if;
                                  Device.Number_Of_Generic_Buttons := Buttons.Bounds.Usage_Maximum - Buttons.Bounds.Usage_Minimum + 1;
                                  ---------------------
                                  Get_Button_Specifics:
                                  ---------------------
                                    declare
                                    Button_Values : aliased Array_Record_Button_Values(1..Integer(Number_Of_X)) := (others => <>);
                                    begin
                                      if
                                      Get_Device_Button_Values(
                                        Report_Kind   => DEVICE_INPUT,
                                        Button_Values => Button_Values'access,
                                        Length        => Number_Of_X'access,
                                        Data          => Data'access) = FAILED
                                      then
                                        raise Call_Failure;
                                      end if;
                                      -- ???
                                    end Get_Button_Specifics;
                                end Get_Buttons;
                            end if;
                      end Get_Identifier;
                  when others =>
                    raise Call_Failure;
                end case;
            end loop;
            for I in Devices'range loop
              for J in List'range loop
                if Devices(I).Identifier = List(J).Handle then
                  exit;
                end if;
                if J = List'last then
                  Remove_Device(Devices(I).Identifier);
                end if;
              end loop;
            end loop;
          end Build_List;
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
        Window        : in Address;
        Message       : in Integer_4_Unsigned_C;
        Data_Unsigned : in Integer_4_Unsigned_C;
        Data_Signed   : in Integer_4_Signed_C)
        return Integer_4_Signed_C;
        pragma Convention(Stdcall, Window_Callback);
      function Window_Callback(
        Window        : in Address;
        Message       : in Integer_4_Unsigned_C;
        Data_Unsigned : in Integer_4_Unsigned_C;
        Data_Signed   : in Integer_4_Signed_C)
        return Integer_4_Signed_C
        is
        Player          :         Integer_4_Positive   := 1;
        Number_Of_Bytes : aliased Integer_4_Unsigned_C := 0;
        Header          : aliased Record_Device_Header := (others => <>);
        begin
          case Message is
            when EVENT_CLOSE =>
              Post_Quit_Message(0);
              return C_FALSE;
            when EVENT_INPUT =>
              if
              Get_Device_Input_Data(
                Device      => To_Unchecked_Address(Data_Signed),
                Command     => GET_DEVICE_HEADER,
                Data        => Header'access,
                Size        => Number_Of_Bytes'access,
                Header_Size => Record_Device_Header'size / Byte'size)
                  /= Record_Device_Header'size / Byte'size
              then
                raise Call_Failure;
              end if;
              case Header.Kind is
                when KIND_IS_KEYBOARD =>
                  ----------------
                  Handle_Keyboard:
                  ----------------
                    declare
                    Keyboard    : aliased Record_Device_Keyboard := (others => <>);
                    Was_Pressed :         Boolean                := False;
                    begin
                      Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Keyboard'size / Byte'size);
                      if
                      Get_Device_Input_Data(
                        Device      => To_Unchecked_Address(Data_Signed),
                        Command     => GET_DEVICE_DATA,
                        Data        => Keyboard'access,
                        Size        => Number_Of_Bytes'access,
                        Header_Size => Record_Device_Header'size / Byte'size) /= Record_Device_Keyboard'size / Byte'size
                      then
                        raise Call_Failure;
                      end if;
                      if Keyboard.Data.Key <= MAP_KEY'last and Keyboard.Data.Key >= MAP_KEY'first then
                        if Keyboard.Data.Message = EVENT_KEY_DOWN or Keyboard.Data.Message = EVENT_SYSTEM_KEY_DOWN then
                          Was_Pressed := True;
                        end if;
                        case MAP_KEY(Keyboard.Data.Key) is
                          when Shift_Key =>
                            if Keyboard.Data.Make_Code = KEY_MAKE_CODE_FOR_LEFT then
                              Handle_Key(Left_Shift_Key, Was_Pressed, Header.Handle);
                            else
                              Handle_Key(Right_Shift_Key, Was_Pressed, Header.Handle);
                            end if;
                          when Control_Key =>
                            if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0 then
                              Handle_Key(Right_Control_Key, Was_Pressed, Header.Handle);
                            else
                              Handle_Key(Left_Control_Key, Was_Pressed, Header.Handle);
                            end if;
                          when Alternative_Key =>
                            if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_LEFT_SIDED) > 0  then
                              Handle_Key(Right_Alternative_Key, Was_Pressed, Header.Handle);
                            else
                              Handle_Key(Left_Alternative_Key, Was_Pressed, Header.Handle);
                            end if;
                          when others =>
                            Handle_Key(MAP_KEY(Keyboard.Data.Key), Was_Pressed, Header.Handle);
                        end case;
                      end if;
                    end Handle_Keyboard;
                when KIND_IS_MOUSE =>
                  -------------
                  Handle_Mouse:
                  -------------
                    declare
                    Mouse : aliased Record_Device_Mouse := (others => <>);
                    begin
                      Number_Of_Bytes := Integer_4_Unsigned_C(Record_Device_Mouse'size / Byte'size);
                      if
                      Get_Device_Input_Data(
                        Device      => To_Unchecked_Address(Data_Signed),
                        Command     => GET_DEVICE_DATA,
                        Data        => Mouse'access,
                        Size        => Number_Of_Bytes'access,
                        Header_Size => Record_Device_Header'size / Byte'size)
                          /= Record_Device_Mouse'size / Byte'size
                      then
                        raise Call_Failure;
                      end if;
                      if Mouse.Data.Last_X /= 0 or Mouse.Data.Last_Y /= 0 then
                        Handle_Mouse(Mouse.Data.Last_X, Mouse.Data.Last_Y, Header.Handle);
                      end if;
                      if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_DOWN) > 0 then
                        Handle_Key(Left_Mouse_Key, True, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_UP) > 0 then
                        Handle_Key(Left_Mouse_Key, False, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN) > 0 then
                        Handle_Key(Right_Mouse_Key, True, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_UP) > 0 then
                        Handle_Key(Right_Mouse_Key, Flase, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN) > 0 then
                        Handle_Key(Middle_Mouse_Key, True, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_UP) > 0 then
                        Handle_Key(Middle_Mouse_Key, False, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN) > 0 then
                        Handle_Key(Auxiliary_1_Mouse_Key, True, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP) > 0 then
                        Handle_Key(Auxiliary_1_Mouse_Key, False, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN) > 0 then
                        Handle_Key(Auxiliary_2_Mouse_Key, True, Header.Handle);
                      elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP) > 0 then
                        Handle_Key(Auxiliary_2_Mouse_Key, False, Header.Handle);
                      elsif
                      (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL) > 0 or
                      (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0
                      then
                        --------------------------
                        Extract_Mouse_Wheel_Delta:
                        --------------------------
                          declare
                          Δ : Integer_2_Signed :=
                            To_Unchecked_Integer_2_Signed(
                              Integer_2_Unsigned(
                                Shift_Right(
                                  Value  => Integer_4_Unsigned(Mouse.Data.Button_Flags),
                                  Amount => Integer_4_Unsigned'size / Integer_2_Unsigned)))
                                    / MOUSE_WHEEL_DELTA;
                          begin
                            if Δ < 0 then
                              for I in Δ..-1 loop
                                if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                                  Handle_Key(Horizontal_Wheel_Mouse_Left_Key, True, Header.Handle, Header.Handle);
                                  Handle_Key(Horizontal_Wheel_Mouse_Left_Key, False, Header.Handle, Header.Handle);
                                else
                                  Handle_Key(Vertical_Wheel_Mouse_Down_Key, True, Header.Handle, Header.Handle);
                                  Handle_Key(Vertical_Wheel_Mouse_Down_Key, False, Header.Handle, Header.Handle);
                                end if;
                              end loop;
                            elsif Δ > 0
                              for I in 1..Δ loop
                                if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then;
                                  Handle_Key(Horizontal_Wheel_Mouse_Right_Key, True, Header.Handle);
                                  Handle_Key(Horizontal_Wheel_Mouse_Right_Key, False, Header.Handle);
                                else
                                  Handle_Key(Vertical_Wheel_Mouse_Up_Key, True, Header.Handle);
                                  Handle_Key(Vertical_Wheel_Mouse_Up_Key, False, Header.Handle);
                                end if;
                              end loop;
                            end if;
                          end Extract_Mouse_Wheel_Delta;
                      end if;
                    end Handle_Mouse;
                when KIND_IS_GAMEPAD_OR_JOYSTICK =>
                  if
                  Get_Device_Button_Usages(
                    Report_Kind     => DEVICE_INPUT,
                    Usage_Page      => Buttons(1).Usage_Page,
                    Link_Collection => 0,
                    Usage_List      => ,
                    Usage_Length    => Number_Of_X'access,
                    Data            => Data'access,
                    Report          => Data.Hid.Raw_Data'access,
                    Report_Length   => Data.Hid.Size_Of_Hid'access) = FAILED
                  then
                    raise Call_Failure;
                  end if;
                  for I in 1..Integer(Number_Of_X) loop

                  end loop;
                when others =>
                  null;
              end case;
            when others =>
              null;
          end case;
          return Define_Window_Procedure(Window, Message, Data_Unsigned, Data_Signed);
        end Window_Callback;
      Class : aliased Record_Window_Class :=(
        Size       => Record_Window_Class'size / Byte'size,
        Style      => 0,
        Callback   => Window_Callback'address,
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
        if Register_Class(Class'unchecked_access) = Integer_2_Unsigned_C(FAILED) then
          raise Call_Failure;
        end if;
        Window :=
          Create_Window(
            Style_Extra => 0,
            Class_Name  => To_String_2_C(Class_Name),
            Window_Name => To_String_2_C(Class_Name),
            Style       => STYLE_NO_ACTIVATE,
            X           => 0,
            Y           => 0,
            Width       => 0,
            Height      => 0,
            Parent      => NULL_ADDRESS,
            Menu        => NULL_ADDRESS,
            Instance    => Get_Current_Instance,
            Parameter   => NULL_ADDRESS);
        if Window = NULL_ADDRESS then
          raise Call_Failure;
        end if;
        --------------------------------------
        Set_Message_Loop_To_Recieve_Raw_Input:
        --------------------------------------
          declare
          Device_Setups:
            Array_Record_Device_Setup :=(
            1 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_KEYBOARD,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            2 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_MOUSE,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            3 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_JOYSTICK,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window),
            4 =>(
              Page   => GENERIC_DESKTOP_CONTROL,
              Usage  => USE_RAW_GAMEPAD,
              Flags  => TAKE_INPUT_ON_NON_ACTIVE,
              Target => Window));
          begin
            if
            Register_Devices(
              Devices => Device_Setups'address,
              Number  => Device_Setups'Length,
              Size    => Record_Device_Setup'size / Byte'size) = FAILED
            then
              raise Call_Failure;
            end if;
          end Set_Message_Loop_To_Recieve_Raw_Input;
      end Initialize;
  -------------------
  -- Handle_Events --
  -------------------
    procedure Handle_Events
      is
      Message : aliased Record_Message := (others => <>);
      XINPUT_STATE  joyData[MAX_JOYSTICKS];
      begin
        if
        Peek_Message(
          Window         => Window,
          Message        => Message'address,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
        then
          if Message.Data = MESSAGE_QUIT then
            return False
          elsif
          Translate_Message(Message'address) < 2 and then
          Dispatch_Message(Message'address) = 0
          then
            null;
          end if;
        end if;
        for I in 1..NUMBER_OF_GAMEPADS loop
          Get_(I);
        end loop;
      end Poll_Devices;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      Empty_Device_Setup : aliased Record_Device_Setup :=(
        Page   => GENERIC_DESKTOP_CONTROL,
        Usage  => USE_RAW_MOUSE,
        Flags  => STOP_READING_TOP_LEVEL_DEVICES,
        Target => NULL_ADDRESS);
      begin
        Finalize_Keyboard;
        if
        Register_Devices(
          Devices => Empty_Device_Setup'address,
          Number  => Empty_Device_Setup'size / Record_Device'size,
          Size    => Record_Device'size / Byte'size) = FAILED
        then
          raise Call_Failure;
        end if;
        if Window /= NULL_ADDRESS then
          if Destroy_Window(Window) = FAILED then
            raise Call_Failure;
          end if;
        end if;
        Window := NULL_ADDRESS;
      end Finalize;
  end Neo.System.Input.Implementation;







