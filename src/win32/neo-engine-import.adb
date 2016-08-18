
--                                                                                                                    
--                                                                N E O  E N G I N E                                                    
--                                                                                                                    
--                                                         Copyright (C) 2016 Justin Squirek                                          
-- 
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later version. 
--                                                                                                                    
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.         
--                                                                                                                    
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses     
--

with Neo.Win32; use Neo.Win32;

separate (Neo.Engine) package Import is

  ---------------
  -- Variables --
  ---------------

  Primary_Window : Address;

  ---------------
  -- Clipboard --
  ---------------

  procedure Copy (Item : Str_2) is
    type Text_Array is array (Item'First..Item'Last + 1) of Char_2_C;
    type Ptr_Text_Array is access all Text_Array;
    function To_Ptr__Text_Array is new Ada.Unchecked_Conversion (Address, Access_Text_Array);
    Text : Ptr_Text_Array;
    Data : Address;
    begin
      Data := Global_Allocate (MEMORY_MOVEABLE or MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE, Text_Array'Size / Byte'Size);
      Assert (Data);
      Text := To_Ptr__Text_Array (Global_Lock (Data));
      Assert (Text /= null);
      Text (Text.All'Last) := NULL_CHAR_2_C;
      for I in Item'Range loop Text (I) := Char_2_C'Val (Char_2'Pos (Item (I))); end loop;
      Assert (Global_Unlock (Data) = 0);
      Assert (Open_Clipboard (NULL_ADDRESS) /= FAILED and then Global_Free (Data) = NULL_ADDRESS);
      Assert (Empty_Clipboard);
      Assert (Set_Clipboard_Data (CLIPBOARD_UNICODE_TEXT, Data));
      Assert (Close_Clipboard);
    end;
  function Paste return Str_2 is
    Text : Ptr_Constant_Char_2_C;
    Data : Address;
    begin
      Assert (Open_Clipboard (NULL_ADDRESS));
      Data := Get_Clipboard_Data (CLIPBOARD_UNICODE_TEXT); Assert (Data);
      Text := To_Unchecked_Access_Constant_Char_2_C (Global_Lock (Data));
      Assert (Close_Clipboard);
      return To_Str_2 (Text);
    end;

  ------------
  -- Memory --
  ------------

  function Get_Memory return Memory_State is
    State                : aliased Memory_State;
    Disk_Bytes           : aliased Int_8_Unsigned_C;
    Disk_Bytes_Available : aliased Int_8_Unsigned_C;
    begin
      Assert (Global_Memory_Status (State'Unchecked_Access));
      Assert (Get_Disk_Free_Space (null, Disk_Available'Unchecked_Access, Disk'Unchecked_Access, null));
      return ( -- Round up State.Physical to the nearest 16 megabytes due to inaccurate results on test machines
        Physical              => Int_8_Unsigned ((State.Total_Physical / 1024**2 + Byte'Size) and 16#FFFF_FFFF_FFFF_FFF0#),
        Physical_Available    => Int_8_Unsigned (State.Available_Physical),
        Disk                  => Int_8_Unsigned (Disk_Bytes),
        Disk_Available        => Int_8_Unsigned (Disk_Bytes_Available),
        Page_File             => Int_8_Unsigned (State.Total_Page_File),
        Page_File_Available   => Int_8_Unsigned (State.Available_Page_File),
        Virtual               => Int_8_Unsigned (State.Total_Virtual),
        Virtual_Available     => Int_8_Unsigned (State.Available_Virtual),
        Virtual_Available_Ext => Int_8_Unsigned (State.Available_Virtual),
        Load                  => Float_4_Percent (State.Memory_Load));
    end;

  --------------------
  -- Error_Handling --
  --------------------

  function Get_Last_Error return Int_4_Unsigned is (Int_4_Unsigned (Neo.Win32.Get_Last_Error));
  procedure Open_Text     (Path : Str_2)        is begin Execute ("explorer """ & Path & """", False); end;
  procedure Open_Webpage  (Path : Str_2)        is begin Execute ("explorer "   & Path,        True);  end;
  procedure Alert (Value : Bool) is
    Flash : aliased Flash_Information_State := (if Value then (Flags => FLASH_CONTINUOUSLY, others => <>) else (Flags => FLASH_END, others => <>));
    begin
      Assert (Window);
      Assert (Flash_Window (Flash_Information'Unchecked_Access));
    end;
  procedure Execute (Path : Str_2; Fullscreen : Bool) is
    Startup_Information : aliased Record_Startup_Information;
    Process_Information : aliased Record_Process_Information;
    begin
      Assert (Path'Length <= MAXIMUM_PATH_LENGTH);
      if Do_Fullscreen then Startup_Information.Show_Window := Int_2_Unsigned_C (MAKE_WINDOW_FULLSCREEN); end if;
      Assert (Create_Process (
        Application_Name    => null,
        Command_Line        => To_Ptr_Char_2_C (Path),
        Process_Attributes  => null,
        Thread_Attributes   => null,
        Inherit_Handles     => C_FALSE,
        Creation_Flags      => 0,
        Environment         => NULL_ADDRESS,
        Current_Directory   => null,
        Startup_Information => Startup_Information'Unchecked_Access,
        Process_Information => Process_Information'Unchecked_Access));
    end;
  function Ok (Name, Message : Str_2; Buttons : Button_Kind; Icon : Icon_Kind) return Bool is
    Force_Custom_Icon : Address; -- All of this Hook business for a damn custom icon
    function Hook (Code : Int_4_Signed_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address; pragma Convention (Stdcall, Hook);
    function Hook (Code : Int_4_Signed_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address is
      Class_Name  : aliased Str_2_C (1..1024);
      Window_Text : aliased Str_2_C (1..1024);
      Window      :         Address := To_Unchecked_Address (Int_Address (Data_Unsigned));
      Icon        :         Address;
      begin
        Assert (Get_Class_Name (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
        Assert_Dummy (Get_Window_Text (Window, Window_Text'Unrestricted_Access, Window_Text'Length) = 0);
        if Code = COMPUTER_BASED_TRAINING_ACTIVATE and To_Str_2 (Class_Name) = CLASS_NAME_DIALOG and To_Str_2 (Window_Text) = Name then
          Icon := Load_Image (
            Instance  => NULL_ADDRESS,
            Name      => To_Ptr_Constant_Char_2_C (PATH_ICON & ".ico"),
            Kind      => LOAD_ICO,
            Desired_X => 0,
            Desired_Y => 0,
            Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
          if Icon = NULL_ADDRESS then Icon := Load_Icon (Get_Current_Instance, GENERIC_ICON); end if;
          Assert_Dummy (Send_Message (
            Window        => Window,
            Message       => MESSAGE_SET_ICON,
            Data_Unsigned => 0,
            Data_Signed   => To_Unchecked_Int_Address (Icon)) = 0);
          Assert (Unhook_Windows_Hook (Force_Custom_Icon));
        else Assert_Dummy (Call_Next_Hook (Force_Custom_Icon, Code, Data_Unsigned, Data_Signed) = 0); end if;
        return 0;
      end;
    begin
      Force_Custom_Icon := Set_Windows_Hook (COMPUTER_BASED_TRAINING_HOOK, Hook'Address, NULL_ADDRESS, Get_Current_Thread_Id);
      return (case Message_Box (
        Window  => Window,
        Caption => To_Str_2_C (Name),
        Text    => To_Str_2_C (Message),
        Kind    => (
          if Window = NULL_ADDRESS then MESSAGE_BOX_SYSTEM_MODAL else 0) or (case Icon is
            when No_Icon          => 0,
            when Error_Icon       => ICON_ERROR,
            when Warning_Icon     => ICON_WARNING,
            when Information_Icon => ICON_INFORMATION)
          or (case Buttons is
            when Okay_Button          => BUTTON_OKAY,
            when Yes_No_Buttons       => BUTTONS_YES_NO,
            when Okay_Cancel_Buttons  => BUTTONS_CANCEL_OKAY,
            when Retry_Cancel_Buttons => BUTTONS_CANCEL_RETRY)) is
        when PRESSED_OKAY | PRESSED_RETRY | PRESSED_YES => True,
        when others => False);
    end;

  -----------------
  -- Information --
  -----------------

  function Get_Information return Information_State is
    Buffer           : aliased Int_4_Signed_C;
    Directory_Buffer : aliased Str_2_C (1..4096);
    Version          : aliased Record_Version_Information;
    begin
      Assert (Get_Version (Version'Unchecked_Access));
      Assert (Get_Username (null, Buffer'Unchecked_Access) = 0 and then Get_Last_Error = ERROR_INSUFFICIENT_BUFFER);
      Assert (Get_Module_File_Name (NULL_ADDRESS, Directory_Buffer'unrestricted_access, Directory_Buffer'Length) /= Int_4_Unsigned_C (FAILED));
      Directory_Buffer (Int_Size_C (Index (To_Str_2 (Directory_Buffer), "\", Backward))) := NULL_Char_2_C;
      declare
      Username  : aliased Access_Str_2_C := new Str_2_C (1..Int_Size_C (Buffer));
      Directory :         Str_2          := To_Str_2 (Directory_Buffer);
      begin
        Assert (Get_Username (Username, Buffer'Unchecked_Access));
        Assert (Is_Running_In_Emulated_32_Bit (Get_Current_Process, Buffer'Unchecked_Access));
        return (
          Name      => Delete (To_Str_2_Unbounded (Directory), 1, Index (Directory, "\", Backward)),
          Path      => To_Str_2_Unbounded (Directory),
          Username  => To_Str_2_Unbounded (To_Str_2 (Username.all)),
          Bit_Size  => (if Buffer = C_TRUE then 64 else 32),
          Version   => (case Version.Platform_Identifier is
            when 1 => (case Version.Major is
              when 4 => (case Version.Minor is
                when 0 => (case Version.Service_Pack (2) is
                  when 'B' | 'C' => "Windows_1_4_B_System",
                  when others    => "Windows_1_4_A_System"),
                when 10 => (case Version.Service_Pack (2) is
                  when 'A'       => "Windows_1_4_10_B_System",
                  when others    => "Windows_1_4_10_A_System"),
                when 90          => "Windows_1_4_90_System",
                when others      => "Windows_1_4_System"),
              when others        => "Windows_1_System"),
            when 2 => (case Version.Major is
              when 5 => (case Version.Minor is
                when 1           => "Windows_2_5_1_System",
                when others      => "Windows_2_5_System"),
              when 6 => (case Version.Minor is
                when 1           => "Windows_2_6_1_System",
                when 2           => "Windows_2_6_2_System",
                when others      => "Windows_2_6_System"),
              when others        => "Windows_2_System"),
            when others          => "Windows_System"));
        end;
    end;

  ---------------
  -- Windowing --
  ---------------

  package Vector_Address is new Vectors (Address);
  STYLE_FULLSCREEN : constant Int_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBARLESS_AND_BORDERLESS;
  STYLE_WINDOWED   : constant Int_4_Unsigned_C := STYLE_VISIBLE_INITIALLY or STYLE_TITLEBAR_MENU or STYLE_TITLEBAR or STYLE_BORDER_THIN_LINE or STYLE_BORDER_SIZABLE or STYLE_BOX_ICONIZE;
  Original_Clip    : aliased Record_Rectangle;
  Icon             :         Address;
  Cursor_Inactive  :         Address;
  Cursor_Active    :         Address;
  Cursor_Current   :         Enumerated_Cursor := Inactive_Cursor;
  Monitor_Windows  :         Vector_Address.Unsafe.Vector;
  procedure Minimize                      is begin Assert_Dummy (Show_Window (Primary_Window, MAKE_WINDOW_GO_TO_ICONIC) = 0); end;
  function Fullscreen_Only return Boolean is (False);
  function Get_Borders return Vector_Border.Unsafe.Vector is
    Borders   : Vector_Record_Border.Unsafe.Vector;
    Rectangle : Record_Rectangle;
    function Callback_Monitor (Monitor, Device_Context : Address; Screen : Access_Record_Rectangle; Data : Ptr_Int_Address) return Int_4_Signed_C; pragma Convention (Stdcall, Callback_Monitor);
    function Callback_Monitor (Monitor, Device_Context : Address; Screen : Access_Record_Rectangle; Data : Ptr_Int_Address) return Int_4_Signed_C is
      Monitor_Information : aliased Record_Monitor_Information := (others => <>);
      begin
        Assert (Get_Monitor_Information (Monitor, Monitor_Information'Address));
        Borders.Append ( (
          Left   => Int_8_Signed (Monitor_Information.Monitor.Left),
          Right  => Int_8_Signed (Monitor_Information.Monitor.Right),
          Top    => Int_8_Signed (Monitor_Information.Monitor.Top),
          Bottom => Int_8_Signed (Monitor_Information.Monitor.Bottom)));
        return C_TRUE;
      end;
    begin
      Assert (Enumerate_Display_Monitor (
        Device_Context => NULL_ADDRESS,
        Clip           => NULL_ADDRESS,
        Callback       => Callback_Monitor'Address,
        Data           => 0));
      Assert (Borders.Length > 0);
      Assert (Get_Window_Rectangle (Primary_Window, Rectangle'Address));
      Borders.Replace_Element (1, (
        Bottom => Int_8_Signed (Rectangle.Bottom),
        Top    => Int_8_Signed (Rectangle.Top),
        Left   => Int_8_Signed (Rectangle.Left),
        Right  => Int_8_Signed (Rectangle.Right)));
      return Borders;
    end;
  function Get_Decoration return Border_State is
    begin
      return (
        Top    => Int_8_Signed (Get_System_Metrics (DATA_TITLE_BAR_HEIGHT) + Get_System_Metrics (DATA_BORDER_HEIGHT)),
        Right  => Int_8_Signed (Get_System_Metrics (DATA_BORDER_WIDTH)),
        Left   => Int_8_Signed (Get_System_Metrics (DATA_BORDER_WIDTH)),
        Bottom => Int_8_Signed (Get_System_Metrics (DATA_BORDER_HEIGHT)));
    end;
  procedure Set_Cursor (X, Y : Int_8_Signed) is
    begin
      Cursor_Current := Cursor;
      if WORD_SIZE = 32 then case Cursor is
        when System_Cursor   => Assert_Dummy (Change_Class_Setting_32 (Primary_Window, SET_CLASS_CURSOR, Int_4_Unsigned_C (To_Unchecked_Int_Address (Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR)))));
        when Inactive_Cursor => Assert_Dummy (Change_Class_Setting_32 (Primary_Window, SET_CLASS_CURSOR, Int_4_Unsigned_C (To_Unchecked_Int_Address (Cursor_Inactive))));
        when Active_Cursor   => Assert_Dummy (Change_Class_Setting_32 (Primary_Window, SET_CLASS_CURSOR, Int_4_Unsigned_C (To_Unchecked_Int_Address (Cursor_Active))));
      end case; else case Cursor is
        when System_Cursor   => Assert_Dummy (Change_Class_Setting_64 (Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Int_Address (Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR))));
        when Inactive_Cursor => Assert_Dummy (Change_Class_Setting_64 (Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Int_Address (Cursor_Inactive)));
        when Active_Cursor   => Assert_Dummy (Change_Class_Setting_64 (Primary_Window, SET_CLASS_CURSOR, To_Unchecked_Int_Address (Cursor_Active)));
      end case; end if;
    end;
  procedure Assert_Only_Instance is
    Handle : Address := Create_Mutex (NULL_ADDRESS, C_TRUE, To_Str_2_C (Get_Information.Name));
    Window : Address;
    begin
      if Get_Last_Error /= NO_ERROR then
        Assert (Release_Mutex (Handle));
        Window := Find_Window (To_Str_2_C (Get_Information.Name), NULL_STRING_2_C);
        if Window /= NULL_ADDRESS then
          Assert_Dummy (Show_Window (Window, MAKE_WINDOW_NORMALIZE) = 0);
          Handle := Set_Focus (Window);
          Assert (Set_Foreground_Window (Window));
          Handle := Set_Active_Window (Window);
        end if;
        raise Call_Failure;
      end if;
    end;
  procedure Clip_Mouse (Undo : Boolean := False; Hide : Boolean := False) is
    Rectangle : aliased Record_Rectangle;
    begin
      Assert (Get_Window_Rectangle (Primary_Window, Rectangle'Address));
      while Show_Cursor (Boolean'Pos (not Hide)) > (if Hide -1 else 0) loop null; end loop;
      if Undo and Original_Clip /= (others => <>) then
        Assert (Clip_Cursor (Original_Clip'Address));
        Original_Clip := (others => <>);
      else
        if Original_Clip = (others => <>) then
          Assert (Get_Clip_Cursor_Area (Original_Clip'Address));
          Assert (Original_Clip /= (others => <>));
        end if;
        Assert (Clip_Cursor (Rectangle'Address));
      end if;
    end;
  procedure Minimize is
    begin
      raise Unimplemented;
    end;
  procedure Fullscreen is
    begin
      raise Unimplemented;
    end;
  procedure Assert_Only_Instance is
    begin
      raise Unimplemented;
    end;
  procedure Initialize_Multi_Monitor is
    begin
      raise Unimplemented;
    end;
  procedure Finalize_Multi_Monitor is
    begin
      raise Unimplemented;
    end;
  procedure Initialize_Windowing is
    begin
      raise Unimplemented;
    end;
  procedure Finalize_Windowing is
    begin
      raise Unimplemented;
    end;
    begin
      raise Unimplemented;
    end;
    begin
      raise Unimplemented;
    end;
  procedure Adjust (X, Y : Int_4_Signed; Width, Height : Int_4_Positive; Fullscreen : Bool) is
    begin
      if Primary_Window = NULL_ADDRESS then
        Primary_Window := Create_Window (
          Style_Extra => 0,
          Class_Name  => To_Str_2_C (Neo.System.SPECIFICS.Name),
          Window_Name => To_Str_2_C (Neo.System.SPECIFICS.Name),
          Style       => (if Do_Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED) or STYLE_ICONIC_INITIALLY,
          X           => Int_4_Signed_C (X),
          Y           => Int_4_Signed_C (Y),
          Width       => Int_4_Signed_C (Width),
          Height      => Int_4_Signed_C (Height),
          Parent      => NULL_ADDRESS,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS);
        Assert (Primary_Window);
        Assert_Dummy (Show_Window (Primary_Window, MAKE_WINDOW_GO_TO_ICONIC));
        Assert_Dummy (Show_Window (Primary_Window, MAKE_WINDOW_RESTORE));
      else
        Assert (Change_Window_Setting (Primary_Window, SET_WINDOW_STYLE, (if Do_Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED)) /= 0);
        Assert_Dummy (Set_Window_Position (
          Window       => Primary_Window,
          Insert_After => (if Do_Fullscreen then INSERT_ON_TOP_OF_EVERYTHING else REMOVE_ON_TOP_OF_EVERYTHING),
          X            => Int_4_Signed_C (X),
          Y            => Int_4_Signed_C (Y),
          Width        => Int_4_Signed_C (Width),
          Height       => Int_4_Signed_C (Height),
          Flags        => 0));
      end if;
    end;
  procedure Adjust_Windowed (Width, Height : Int_4_Positive) is
    begin
      raise Unimplemented;
    end;
  function Update_Windowing return Bool is
    begin
      raise Unimplemented;
      return False;
    end;
  function Get_Borders return Vector_Border.Unsafe.Vector is
    Junk : Vector_Border.Unsafe.Vector;
    begin
      raise Unimplemented;
      return Junk;
    end;
    begin
      raise Unimplemented;
      return (others => <>);
    end;

  -----------
  -- Input --
  -----------

  procedure Initialize_Input is
    begin
      raise Unimplemented;
    end;
  procedure Finalize_Input is
    begin
      raise Unimplemented;
    end;
  procedure Set_Vibration (Id : Int_Address; High, Low : Float_4_Percent) is
    begin
      raise Unimplemented;
    end;
  function Get_Cursor return Cursor_state is
    begin
      raise Unimplemented;
      return (others => <>);
    end;
  function Update_Input return Bool is
    begin
      raise Unimplemented;
      return False;
    end;

  -------------
  -- Console --
  -------------

  procedure Run is
    begin
      raise Unimplemented;
    end;
end;































  --------------
  -- Graphics --
  --------------

  function Callback_Window (Window : Address; Message : Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Window);
  function Callback_Window (Window : Address; Message : Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address is
    Render_Context :         Address := NULL_ADDRESS;
    Device_Context :         Address := NULL_ADDRESS;
    Descriptor     : aliased Record_Pixel_Descriptor := (
      Version      => 1,
      Flags        => PIXEL_FORMAT_DRAW_TO_WINDOW or PIXEL_FORMAT_SUPPORT_OPENGL or PIXEL_FORMAT_DOUBLEBUFFER,
      Color_Bits   => 24,
      Depth_Bits   => 24,
      Stencil_Bits => 8,
      Alpha_Shift  => 8,
      others       => <>);
    begin
      if Message = EVENT_DESTROY then Post_Quit_Message (0);
      elsif Message = EVENT_CREATE then
        Device_Context := Get_Device_Context (Window);
        Assert (Device_Context);
        Assert (Set_Pixel_Format (Device_Context, Choose_Pixel_Format (Device_Context, Descriptor'Unchecked_Access), Descriptor'Address));
        Render_Context := OpenGL_Create_Context (Device_Context);
        Assert (Render_Context);
        Assert (OpenGL_Make_Current (Device_Context, Render_Context));
        OpenGL_Get_Extensions            := To_Unchecked_Access_OpenGL_Get_Extensions            (OpenGL_Get_Procedure_Address (To_String_1_C ("wglGetExtensionsStringARB")));
        OpenGL_Swap_Interval             := To_Unchecked_Access_OpenGL_Swap_Interval             (OpenGL_Get_Procedure_Address (To_String_1_C ("wglSwapIntervalEXT")));
        OpenGL_Get_Pixel_Format_Integer  := To_Unchecked_Access_OpenGL_Get_Pixel_Format_Integer  (OpenGL_Get_Procedure_Address (To_String_1_C ("wglGetPixelFormatAttribivARB")));
        OpenGL_Get_Pixel_Format_Float    := To_Unchecked_Access_OpenGL_Get_Pixel_Format_Float    (OpenGL_Get_Procedure_Address (To_String_1_C ("wglGetPixelFormatAttribfvARB")));
        OpenGL_Choose_Pixel_Format       := To_Unchecked_Access_OpenGL_Choose_Pixel_Format       (OpenGL_Get_Procedure_Address (To_String_1_C ("wglChoosePixelFormatARB")));
        OpenGL_Create_Context_Attributes := To_Unchecked_Access_OpenGL_Create_Context_Attributes (OpenGL_Get_Procedure_Address (To_String_1_C ("wglCreateContextAttribsARB")));
        Assert (OpenGL_Get_Extensions            /= null);
        Assert (OpenGL_Swap_Interval             /= null);
        Assert (OpenGL_Get_Pixel_Format_Integer  /= null);
        Assert (OpenGL_Get_Pixel_Format_Float    /= null);
        Assert (OpenGL_Choose_Pixel_Format       /= null);
        Assert (OpenGL_Create_Context_Attributes /= null);
        Assert (OpenGL_Delete_Context (Render_Context));
        Assert (Release_Device_Context (Window, Device_Context));
        return 0;
      end if;
      return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed);
    end Callback_Window;
  Window  :         Address             := NULL_ADDRESS;
  Message : aliased Record_Message      := (others => <>);
  Class   : aliased Record_Window_Class := (
    Callback   => Callback_Window'Address,
    Instance   => Get_Current_Instance,
    Class_Name => To_Ptr_Constant_Char_2_C ("Dummy"),
    others     => <>);
  begin
    Assert (Register_Class (Class'Unchecked_Access) /= Int_2_Unsigned_C (FAILED));
    Window := Create_Window (
      Style_Extra => 0,
      Class_Name  => To_Str_2_C ("Dummy"),
      Window_Name => To_Str_2_C ("Dummy"),
      Style       => 0,
      X           => 40,
      Y           => 40,
      Width       => 640,
      Height      => 480,
      Parent      => NULL_ADDRESS,
      Menu        => 0,
      Instance    => Get_Current_Instance,
      Parameter   => NULL_ADDRESS);
    Assert (Window);
    while Peek_Message (
      Message        => Message'Unchecked_Access,
      Window         => Window,
      Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
      Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
      Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
    loop
      Assert_Dummy (Translate_Message (Message'Unchecked_Access));
      Assert_Dummy (Dispatch_Message (Message'Unchecked_Access));
    end loop;

  ---------------
  -- Windowing --
  ---------------

  procedure Set_Cursor (X, Y : Ptr_Int_8_Signed) is
    begin null;
    end Set_Cursor;




    end;
  procedure Adjust_Fullscreen is
    Context : Address := Get_Device_Context (Get_Desktop_Window);
    begin
      Assert (Context);
      Adjust (
        X             => 0,
        Y             => 0,
        Width         => Int_4_Positive (Get_Device_Capabilities (Context, DATA_HORIZONTAL_RESOLUTION)),
        Height        => Int_4_Positive (Get_Device_Capabilities (Context, DATA_VERTICAL_RESOLUTION)),
        Do_Fullscreen => True);
      Assert (Release_Device_Context (Get_Desktop_Window, Context));
    end;
  procedure Adjust_Windowed (Width, Height : Ptr_Int_4_Positive) is
    Context : Address := Get_Device_Context (Get_Desktop_Window);
    begin
      Assert (Context);
      Adjust (
        X             => Int_4_Positive (Get_Device_Capabilities (Context, DATA_HORIZONTAL_RESOLUTION)) / 2 - Width  / 2,
        Y             => Int_4_Positive (Get_Device_Capabilities (Context, DATA_VERTICAL_RESOLUTION))   / 2 - Height / 2,
        Width         => Width,
        Height        => Height,
        Do_Fullscreen => False);
      Assert (Release_Device_Context (Get_Desktop_Window, Context));
    end Adjust_Windowed;
  procedure Initialize_Multi_Monitor is
    function Callback_Window (Window : Address; Message : Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Window);
    function Callback_Window (Window : Address; Message : Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Int_Address) return Int_Address is
      begin
        case Message is when EVENT_CLOSE => Post_Quit_Message (0); return Int_Address (C_FALSE);
        when others => null; end case;
        return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    Borders : Vector_Record_Border.Unsafe.Vector;
    Class   : aliased Record_Window_Class := (
      Size       => Record_Window_Class'Size / Byte'Size,
      Style      => 0,
      Callback   => Callback_Window'Address,
      Extra_A    => 0,
      Extra_B    => 0,
      Instance   => Get_Current_Instance,
      Icon_Small => Icon,
      Icon_Large => Icon,
      Background => BRUSH_GRAY,
      Menu_Name  => null,
      Class_Name => To_Ptr_Constant_Char_2_C (To_Str_2 (Neo.System.SPECIFICS.Name) & Localize (MULTI_MONITOR_NAME)),
      Cursor     => (case Cursor_Current is
        when Inactive_Cursor => Cursor_Inactive,
        when Active_Cursor   => Cursor_Active,
        when System_Cursor   => Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR)));
    begin
      Assert (Register_Class (Class'unrestricted_access) /= Int_2_Unsigned_C (FAILED));
      for Border of Borders loop
        if Border.Left /= 0 and Border.Top /= 0 then
          Multi_Monitor_Windows.Append (
            Create_Window (
              Style_Extra => 0,
              Style       => STYLE_FULLSCREEN or STYLE_NO_ACTIVATE,
              X           => Int_4_Signed_C (Border.Left),
              Y           => Int_4_Signed_C (Border.Top),
              Width       => Int_4_Signed_C (Border.Right  - Border.Left),
              Height      => Int_4_Signed_C (Border.Bottom - Border.Top),
              Parent      => NULL_ADDRESS,
              Menu        => 0,
              Instance    => Get_Current_Instance,
              Parameter   => NULL_ADDRESS,
              Class_Name  => To_Str_2_C (To_Str_2 (Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME),
              Window_Name => To_Str_2_C (To_Str_2 (Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME)));
            Assert (Multi_Monitor_Windows.Last_Element);
            Assert_Dummy (Show_Window (Multi_Monitor_Windows.Last_Element, MAKE_WINDOW_NORMALIZE));
            Assert (Update_Window (Multi_Monitor_Windows.Last_Element));
          end if;
        end loop;
      Assert_Dummy (Show_Window (Primary_Window, MAKE_WINDOW_NORMALIZE));
      Assert (Update_Window (Primary_Window));
    end Initialize_Multi_Monitor;
  procedure Finalize_Multi_Monitor is
    begin
      for Window of Multi_Monitor_Windows loop
        Assert_Dummy (Show_Window (Window, MAKE_WINDOW_HIDE));
        Assert (Destroy_Window (Window));
      end loop;
      Assert (Unregister_Class (To_Str_2_C (To_Str_2 (Neo.System.SPECIFICS.Name) & MULTI_MONITOR_NAME), NULL_ADDRESS));
    end Finalize_Multi_Monitor;
  procedure Initialize is
    function Callback_Window (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Window);
    function Callback_Window (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address is
      Point : Record_Point;
      begin
        case Message is
          when EVENT_CLOSE     => Post_Quit_Message (0); return Int_Address (C_FALSE);
          when EVENT_COMMAND   => if Data_Unsigned = SUBEVENT_MENU_POPOUT or Data_Unsigned = SUBEVENT_SCREEN_SAVER_START then return 0; end if;
          when EVENT_CHARACTER =>
            if (Get_Key_State (Int_4_Signed_C (VIRTUAL_KEY_CONTROL)) and 16#8000#) = 0 then
              Inject ( (Text_Kind, Text => To_Str_2_Unbounded (Char_2'val (Int_4_Signed (Data_Unsigned))), others => <>));
            end if;
          when EVENT_ACTIVATION_CHANGE =>
            if    (Data_Unsigned and 16#0000_FFFF#) = 0 or (Data_Unsigned and 16#FFFF_0000#) /= 0 then Activate (False, False, Int_8_Signed (Point.X), Int_8_Signed (Point.Y));
            elsif (Data_Unsigned and 16#0000_FFFF#) = SUBEVENT_CLICK_ACTIVATION then                   Activate (True,  True,  Int_8_Signed (Point.X), Int_8_Signed (Point.Y));
            else                                                                                       Activate (True,  False, Int_8_Signed (Point.X), Int_8_Signed (Point.Y));
            end if;
          when EVENT_SIZE_CHANGED =>
            case Data_Unsigned is
              when SUBEVENT_ICONIZED     => Change_State (Iconic_Change);
              when SUBEVENT_FULLSCREENED => Change_State (Fullscreen_Change);
            when others => null; end case;
            return 0;
          when EVENT_SIZING =>
            declare
            Screen : Access_Record_Rectangle := To_Unchecked_Access_Record_Rectangle (Data_Signed);
            Result : Record_Border           :=
              Resize (
                Border => (
                  Left   => Int_8_Signed (Screen.Left),
                  Right  => Int_8_Signed (Screen.Right),
                  Top    => Int_8_Signed (Screen.Top),
                  Bottom => Int_8_Signed (Screen.Bottom)),
                Kind => (case Data_Unsigned is
                  when SUBEVENT_RESIZE_LEFT         => Left_Resize,
                  when SUBEVENT_RESIZE_RIGHT        => Right_Resize,
                  when SUBEVENT_RESIZE_TOP          => Top_Resize,
                  when SUBEVENT_RESIZE_TOP_LEFT     => Top_Left_Resize,
                  when SUBEVENT_RESIZE_TOP_RIGHT    => Top_Right_Resize,
                  when SUBEVENT_RESIZE_BOTTOM       => Bottom_Resize,
                  when SUBEVENT_RESIZE_BOTTOM_LEFT  => Bottom_Left_Resize,
                  when SUBEVENT_RESIZE_BOTTOM_RIGHT => Bottom_Right_Resize,
                  when others                       => Other_Resize));
            begin
              Screen.all := (
                Left   => Int_4_Signed_C (Result.Left),
                Right  => Int_4_Signed_C (Result.Right),
                Top    => Int_4_Signed_C (Result.Top),
                Bottom => Int_4_Signed_C (Result.Bottom));
            end;
        when others => null; end case;
        return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    Class : aliased Record_Window_Class := (others => <>);
    begin
      Icon := Load_Image (
        Instance  => Get_Current_Instance,
        Name      => To_Ptr_Constant_Char_2_C (PATH_ASSETS & "\ico\" & NAME_ICON & ".ico"),
        Kind      => LOAD_ICO,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_ADDRESS then Icon := Load_Icon (Get_Current_Instance, GENERIC_ICON); end if;
      Cursor_Inactive := Load_Image (
        Instance  => Get_Current_Instance,
        Name      => To_Ptr_Constant_Char_2_C (PATH_ASSETS & "\cur\" & NAME_CURSOR_INACTIVE & ".cur"),
        Kind      => LOAD_CUR,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Cursor_Inactive = NULL_ADDRESS then Cursor_Inactive := Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR); end if;
      Cursor_Active := Load_Image (
        Instance  => Get_Current_Instance,
        Name      => To_Ptr_Constant_Char_2_C (PATH_ASSETS & "\cur\" & NAME_CURSOR_ACTIVE & ".cur"),
        Kind      => LOAD_CUR,
        Desired_X => 0,
        Desired_Y => 0,
        Load      => LOAD_FROM_FILE);
      if Cursor_Active = NULL_ADDRESS then Cursor_Active := Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR); end if;
      Class := (
        Callback   => Callback_Window'Address,
        Instance   => Get_Current_Instance,
        Icon_Small => Icon,
        Icon_Large => Icon,
        Cursor     => Cursor_Inactive,
        Background => BRUSH_GRAY,
        Class_Name => To_Ptr_Constant_Char_2_C (Neo.System.SPECIFICS.Name),
        others     => <>);
      Assert (Register_Class (Class'Unchecked_Access) /= Int_2_Unsigned_C (FAILED));
    end Initialize;
  function Update return Boolean is
    Message : aliased Record_Message := (others => <>);
    begin
      while Peek_Message (
        Message        => Message'Unchecked_Access,
        Window         => NULL_ADDRESS,
        Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
        Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
        Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
      loop
        if Message.Data = MESSAGE_QUIT then return False; end if;
        Assert_Dummy (Translate_Message (Message'Unchecked_Access));
        Assert_Dummy (Dispatch_Message (Message'Unchecked_Access));
      end loop;
      return True;
    end Update;
  procedure Finalize is
    begin
      if Primary_Window /= NULL_ADDRESS then
        Assert_Dummy (Show_Window (Primary_Window, MAKE_WINDOW_HIDE) = 0);
        Assert_Dummy (Destroy_Window (Primary_Window));
      end if;
      Assert_Dummy (Unregister_Class (To_Str_2_C (Neo.System.SPECIFICS.Name), NULL_ADDRESS));
      Primary_Window := NULL_ADDRESS;
    end Finalize;

  -----------
  -- Input --
  -----------

  MAP_KEY : constant array (VIRTUAL_KEY_LEFT_MOUSE..VIRTUAL_KEY_CLEAR) of Enumerated_Key := (
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
  Gamepad_States : aliased array (0..3) of Record_Gamepad := (others => <>);
  Name_Class     : aliased Str_2_C                    := To_Str_2_C (SPECIFICS.Name & " " & Localize (NAME_POSTFIX));
  Window         :         Address                       := NULL_ADDRESS;
  procedure Set_Vibration (Identifier : Ptr_Int_Address; Frequency_High, Frequency_Low : Float_4_Percent) is
    Vibration : aliased Record_Vibration := (
      Left_Motor_Speed  => Int_2_Unsigned_C (Frequency_Low  / 100.0 * Float_4_Real (Int_2_Unsigned_C'last)),
      Right_Motor_Speed => Int_2_Unsigned_C (Frequency_High / 100.0 * Float_4_Real (Int_2_Unsigned_C'last)));
    begin
      if Identifier in 0..3 then Assert (Set_XInput_State (Int_4_Unsigned_C (Identifier), Vibration'Unchecked_Access) = 0); end if;
    end Set_Vibration;
  function Get_Cursor return Record_Location is
    Point : aliased Record_Point := (others => <>);
    begin
      Assert (Get_Cursor_Position (Point'Address));
      return (Int_8_Signed (Point.X), Int_8_Signed (Point.Y));
    end Get_Cursor;
  procedure Initialize is
    function Window_Callback (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned : Ptr_Int_Address; Data_Signed : Ptr_Int_Address) return Int_Address; pragma Convention (Stdcall, Window_Callback);
    function Window_Callback (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned : Ptr_Int_Address; Data_Signed : Ptr_Int_Address) return Int_Address is
      Identifier      :         Int_Address      := 0;
      Player          :         Int_4_Positive   := 1;
      Number_Of_Bytes : aliased Int_4_Unsigned_C := 0;
      Header          : aliased Record_Device_Header := (others => <>);
      begin
        case Message is
          when EVENT_CLOSE => Post_Quit_Message (0); return Int_Address (C_FALSE);
          when EVENT_INPUT =>
            Number_Of_Bytes := Record_Device_Header'object_size / Byte'object_size;
            Assert (Get_Device_Input_Data (
              Device      => To_Unchecked_Address (Data_Signed),
              Command     => GET_DEVICE_HEADER,
              Data        => Header'Address,
              Size        => Number_Of_Bytes'Address,
              Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Header'object_size / Byte'object_size);
            Identifier := To_Unchecked_Int_Address (Header.Device);
            if not Devices.Has_Element (Identifier) then return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed); end if;
            case Header.Kind is
              when KIND_IS_KEYBOARD =>
                declare
                Keyboard   : aliased Record_Device_Keyboard := (others => <>);
                Is_Pressed :         Boolean                := False;
                begin
                  Number_Of_Bytes := Int_4_Unsigned_C (Record_Device_Keyboard'object_size / Byte'object_size);
                  Assert (Get_Device_Input_Data (
                    Device      => To_Unchecked_Address (Data_Signed),
                    Command     => GET_DEVICE_DATA,
                    Data        => Keyboard'Address,
                    Size        => Number_Of_Bytes'Address,
                    Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Keyboard'object_size / Byte'object_size);
                  if Keyboard.Data.Key <= MAP_KEY'last and Keyboard.Data.Key >= MAP_KEY'first then
                    if Keyboard.Data.Message = EVENT_KEY_DOWN or Keyboard.Data.Message = EVENT_SYSTEM_KEY_DOWN then Is_Pressed := True; end if;
                    Inject_Key (Identifier, Is_Pressed => Is_Pressed, Key => (case MAP_KEY (Keyboard.Data.Key) is
                      when Shift_Key       => (if Keyboard.Data.Make_Code = KEY_MAKE_CODE_FOR_LEFT          then Left_Shift_Key       else Right_Shift_Key),
                      when Control_Key     => (if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Control_Key     else Right_Control_Key),
                      when Alternative_Key => (if (Keyboard.Data.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Alternative_Key else Right_Alternative_Key),
                      when others          => MAP_KEY (Keyboard.Data.Key)));
                  end if;
                end;
              when KIND_IS_MOUSE =>
                declare
                Mouse : aliased Record_Device_Mouse := (others => <>);
                begin
                  Number_Of_Bytes := Int_4_Unsigned_C (Record_Device_Mouse'object_size / Byte'object_size);
                  Assert (Get_Device_Input_Data (
                    Device      => To_Unchecked_Address (Data_Signed),
                    Command     => GET_DEVICE_DATA,
                    Data        => Mouse'Address,
                    Size        => Number_Of_Bytes'Address,
                    Header_Size => Record_Device_Header'object_size / Byte'object_size) = Record_Device_Mouse'object_size / Byte'object_size);
                  if Mouse.Data.Last_X /= 0 or Mouse.Data.Last_Y /= 0 then Inject_Mouse (Identifier, (Int_8_Signed (Mouse.Data.Last_X), Int_8_Signed (Mouse.Data.Last_Y))); end if;
                  if    (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_DOWN)       > 0 then Inject_Key (Identifier, Left_Mouse_Key,        True);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_LEFT_UP)         > 0 then Inject_Key (Identifier, Left_Mouse_Key,        False);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN)      > 0 then Inject_Key (Identifier, Right_Mouse_Key,       True);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_RIGHT_UP)        > 0 then Inject_Key (Identifier, Right_Mouse_Key,       False);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN)     > 0 then Inject_Key (Identifier, Middle_Mouse_Key,      True);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_UP)       > 0 then Inject_Key (Identifier, Middle_Mouse_Key,      False);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN)    > 0 then Inject_Key (Identifier, Auxiliary_1_Mouse_Key, True);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP)      > 0 then Inject_Key (Identifier, Auxiliary_1_Mouse_Key, False);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN)    > 0 then Inject_Key (Identifier, Auxiliary_2_Mouse_Key, True);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP)      > 0 then Inject_Key (Identifier, Auxiliary_2_Mouse_Key, False);
                  elsif (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL) > 0 or (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then
                    Inject_Key (Identifier, Is_Pressed => True, Key => (
                      if To_Unchecked_Int_2_Signed (Int_2_Unsigned (Shift_Right (Amount => 16, Value => Int_8_Unsigned (Mouse.Data.Button_Flags) and 16#0000_0000_FFFF_0000#))) / MOUSE_WHEEL_DELTA < 0
                      then (if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then Horizontal_Wheel_Left_Key else Vertical_Wheel_Down_Key)
                      else (if (Mouse.Data.Button_Flags and SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL) > 0 then Horizontal_Wheel_Right_Key else Vertical_Wheel_Up_Key)));
                  end if;
                end;
            when others => null; end case;
        when others => null; end case;
        return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed);
      end Window_Callback;
    Current_Virtual_Key : Int_2_Unsigned_C := 0;
    Class : aliased Record_Window_Class := (
      Size       => Record_Window_Class'object_size / Byte'object_size,
      Style      => 0,
      Callback   => Window_Callback'Address,
      Extra_A    => 0,
      Extra_B    => 0,
      Instance   => Get_Current_Instance,
      Icon_Small => Load_Icon (Get_Current_Instance, GENERIC_ICON),
      Icon_Large => Load_Icon (Get_Current_Instance, GENERIC_ICON),
      Cursor     => Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR),
      Background => BRUSH_GRAY,
      Menu_Name  => null,
      Class_Name => To_Ptr_Constant_Char_2_C (To_Str_2 (Name_Class)));
    begin
      Assert (Register_Class (Class'Unchecked_Access) /= Int_2_Unsigned_C (FAILED));
      Window := Create_Window (
        Style_Extra => 0,
        Class_Name  => Name_Class,
        Window_Name => Name_Class,
        Style       => STYLE_NO_ACTIVATE,
        X           => 0,
        Y           => 0,
        Width       => 0,
        Height      => 0,
        Parent      => NULL_ADDRESS,
        Menu        => 0,
        Instance    => Get_Current_Instance,
        Parameter   => NULL_ADDRESS);
      Assert (Window);
      declare
      Device_Setups : aliased Array_Record_Device_Setup := (
        (GENERIC_DESKTOP_CONTROL, USE_RAW_KEYBOARD, TAKE_INPUT_ON_NON_ACTIVE, Window),
        (GENERIC_DESKTOP_CONTROL, USE_RAW_MOUSE,    TAKE_INPUT_ON_NON_ACTIVE, Window),
        (GENERIC_DESKTOP_CONTROL, USE_RAW_JOYSTICK, TAKE_INPUT_ON_NON_ACTIVE, Window));
      begin
        Assert (Register_Devices (
          Devices => Device_Setups'Address,
          Number  => Device_Setups'Length,
          Size    => Record_Device_Setup'object_size / Byte'object_size));
      end;
    end Initialize;
  function Update return Boolean is
    Number_Of_Devices : aliased Int_4_Unsigned_C := 0;
    State             : aliased Record_Gamepad_State := (others => <>);
    Message           : aliased Record_Message       := (others => <>);
    Has_Gamepad       :         Array_Boolean (1..4)  := (others => False);
    File              :         Address              := NULL_ADDRESS;
    Current_Device    :         Ordered_Map_Record_Device.Cursor;
    procedure Unpack_Button (Player : Ptr_Int_4_Signed; Raw : Ptr_Int_2_Unsigned_C; Key :'AddressEnumerated_Xbox_Key) is
      begin
        if (State.Gamepad.Buttons and Raw) /= (Gamepad_States (Player).Buttons and Raw) then
          if (State.Gamepad.Buttons and Raw) > 0 then Inject_Key (Int_Address (Player), Key, True);
          else Inject_Key (Int_Address (Player), Key, False); end if;
        end if;
      end Unpack_Button;
    procedure Unpack_Stick (Player : Ptr_Int_4_Signed; Stick :'AddressEnumerated_Stick; X, Y : Ptr_Int_2_Signed_C) is
      begin
        Inject_Stick (Int_Address (Player), Stick, ( (
          if X > 0 then Float_4_Range (Float_4_Real (X) / Float_4_Real (Int_2_Signed_C'last))
          else          Float_4_Range (Float_4_Real (X) / Float_4_Real (Int_2_Signed_C'first))), (
          if Y > 0 then Float_4_Range (Float_4_Real (Y) / Float_4_Real (Int_2_Signed_C'last))
          else          Float_4_Range (Float_4_Real (Y) / Float_4_Real (Int_2_Signed_C'first)))));
      end Unpack_Stick;
    begin
      Assert (Get_Device_List (NULL_ADDRESS, Number_Of_Devices'Address, Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
      Assert (Number_Of_Devices /= 0);
      declare
      List : aliased Array_Record_Device_List_Element (1..Integer (Number_Of_Devices)) := (others => <>);
      begin
        Assert (Get_Device_List (List (List'first)'Address, Number_Of_Devices'Address, Record_Device_List_Element'Object_Size / Byte'object_size) /= -1);
        for I in List'range loop
          if not Has_Device (To_Unchecked_Int_Address (List (I).Handle)) then
            case List (I).Kind is
              when KIND_IS_KEYBOARD => Add_Device (To_Unchecked_Int_Address (List (I).Handle), (Keyboard_Device, others => <>));
              when KIND_IS_MOUSE    => Add_Device (To_Unchecked_Int_Address (List (I).Handle), (Mouse_Device,    others => <>));
            when others => null; end case;
          end if;
        end loop;
        Current_Device := Devices.First;
        while Devices.Has_Element (Current_Device) loop
          if Devices.Key (Current_Device) in 0..3 then
            Has_Gamepad (Int_4_Signed (Devices.Key (Current_Device)) + 1) := True;
            if Get_XInput_State (Int_4_Unsigned_C (Devices.Key (Current_Device)), State'Unchecked_Access) /= 0 then Devices.Delete (Current_Device); end if;
          else
            for J in List'range loop
              if Devices.Key (Current_Device) = To_Unchecked_Int_Address (List (J).Handle) then exit; end if;
              if J = List'last then Devices.Delete (Current_Device); end if;
            end loop;
          end if;
          Devices.Next (Current_Device);
        end loop;
        for I in Gamepad_States'range loop
          if not Has_Gamepad (I + 1) then
            if Get_XInput_State (Int_4_Unsigned_C (I), State'Unchecked_Access) = 0 then Add_Device (Int_Address (I), (XBox_Device, others => <>)); end if;
          end if;
        end loop;
      end;
      while Peek_Message (
        Window         => Window,
        Message        => Message'Unchecked_Access,
        Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
        Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
        Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED
      loop
        if Message.Data = MESSAGE_QUIT then return False; end if;
        Assert_Dummy (Translate_Message (Message'Unchecked_Access));
        Assert_Dummy (Dispatch_Message (Message'Unchecked_Access));
      end loop;
      begin for I in Gamepad_States'range loop
        if Get_XInput_State (Int_4_Unsigned_C (I), State'Unchecked_Access) = 0 and then Gamepad_States (I) /= State.Gamepad then
          Unpack_Button (I, GAMEPAD_A,                     Green_A_Key);
          Unpack_Button (I, GAMEPAD_B,                     Red_B_Key);
          Unpack_Button (I, GAMEPAD_X,                     Blue_X_Key);
          Unpack_Button (I, GAMEPAD_Y,                     Yellow_Y_Key);
          Unpack_Button (I, GAMEPAD_BACK,                  Back_Key);
          Unpack_Button (I, GAMEPAD_START,                 Start_Key);
          Unpack_Button (I, GAMEPAD_THUMB_LEFT,            Left_Stick_Key);
          Unpack_Button (I, GAMEPAD_THUMB_RIGHT,           Right_Stick_Key);
          Unpack_Button (I, GAMEPAD_BUMPER_LEFT,           Left_Bumper_Key);
          Unpack_Button (I, GAMEPAD_BUMPER_RIGHT,          Right_Bumper_Key);
          Unpack_Button (I, GAMEPAD_DIRECTIONAL_PAD_UP,    Directional_Pad_Up_Key);
          Unpack_Button (I, GAMEPAD_DIRECTIONAL_PAD_DOWN,  Directional_Pad_Down_Key);
          Unpack_Button (I, GAMEPAD_DIRECTIONAL_PAD_LEFT,  Directional_Pad_Left_Key);
          Unpack_Button (I, GAMEPAD_DIRECTIONAL_PAD_RIGHT, Directional_Pad_Right_Key);
          if State.Gamepad.Thumb_Left_X  /= Gamepad_States (I).Thumb_Left_X  or State.Gamepad.Thumb_Left_Y  /= Gamepad_States (I).Thumb_Left_Y  then Unpack_Stick (I, Left_Stick,  State.Gamepad.Thumb_Left_X,  State.Gamepad.Thumb_Left_Y);  end if;
          if State.Gamepad.Thumb_Right_X /= Gamepad_States (I).Thumb_Right_X or State.Gamepad.Thumb_Right_Y /= Gamepad_States (I).Thumb_Right_Y then Unpack_Stick (I, Right_Stick, State.Gamepad.Thumb_Right_X, State.Gamepad.Thumb_Right_Y); end if;
          if State.Gamepad.Left_Trigger  /= Gamepad_States (I).Left_Trigger  then Inject_Trigger (Int_Address (I), Left_Trigger,  Float_4_Real (State.Gamepad.Left_Trigger)  / Float_4_Real (Int_1_Unsigned_C'last) * 100.0); end if;
          if State.Gamepad.Right_Trigger /= Gamepad_States (I).Right_Trigger then Inject_Trigger (Int_Address (I), Right_Trigger, Float_4_Real (State.Gamepad.Right_Trigger) / Float_4_Real (Int_1_Unsigned_C'last) * 100.0); end if;
          Gamepad_States (I) := State.Gamepad;
        end if;
      end loop; exception when others => null; end; -- "TERRIBLE HORRIBLE NO GOOD VERY BAD HACK"
      return True;
    end Update;
  procedure Finalize is
    Empty_Device_Setup : aliased Record_Device_Setup := (GENERIC_DESKTOP_CONTROL, USE_RAW_MOUSE, STOP_READING_TOP_LEVEL_DEVICES, NULL_ADDRESS);
    begin
      Assert_Dummy (Register_Devices (Empty_Device_Setup'Address, Empty_Device_Setup'Size / Record_Device'object_size, Record_Device'object_size / Byte'object_size));
      if Window /= NULL_ADDRESS then Assert_Dummy (Destroy_Window (Window)); end if;
      Window := NULL_ADDRESS;
    end Finalize;


  -------------
  -- Console --
  -------------

  procedure Run is
    DO_DISABLE_RESIZE       : constant Boolean                  := False;
    FONT_GROUP_BOX_SIZE     : constant Float                    := 1.2;
    GROUP_BOX_SIDE_MARGIN   : constant Float                    := 1.2;
    FONT_CONSOLE            : constant Str_2                 := "Courier New";
    FONT_DIALOG             : constant Str_2                 := "Tahoma";
    NAME_BUTTON             : constant Str_2_C               := To_Str_2_C ("Button");
    NAME_GROUP              : constant Str_2_C               := To_Str_2_C ("Group");
    NAME_EDIT               : constant Str_2_C               := To_Str_2_C ("Edit");
    IDENTIFIER_START        : constant Int_4_Signed         := 16#0000_0666#;
    PIXELS_PER_INCH         : constant Int_4_Signed_C       := 72;
    BUTTON_WIDTH_DLU        : constant Int_4_Signed_C       := 50;
    BUTTON_HEIGHT_DLU       : constant Int_4_Signed_C       := 14;
    FONT_CONSOLE_SIZE       : constant Int_4_Signed_C       := -11;
    SCROLL_FACTOR           : constant Int_8_Natural        := 500;
    MARGIN_BUTTON           : constant Int_4_Signed_C       := 4;
    MARGIN                  : constant Int_4_Signed_C       := 7;
    Name_Class              : aliased Str_2_C                := To_Str_2_C (SPECIFICS.Name & " " & Localize (NAME_POSTFIX));
    Hack                    : aliased Str_2_C                := To_Str_2_C ("SINISTER HACK TO GET DIALOG BASE UNITS");
    Buffer                  : aliased Str_2_C (1..10000)      := (others => NULL_Char_2_C);
    Non_Client_Metrics      : aliased Record_Non_Client_Metrics := (others => <>);
    Text_Metric             : aliased Record_Text_Metric        := (others => <>);
    Rectangle               : aliased Record_Rectangle          := (others => <>);
    Message                 : aliased Record_Message            := (others => <>);
    Class                   : aliased Record_Window_Class       := (others => <>);
    Size                    : aliased Record_Size               := (others => <>);
    Y                       :         Int_4_Signed_C        := 0;
    Message_Box_Font_Height :         Int_4_Signed_C        := 0;
    Dialog_Base_Unit_Height :         Int_4_Signed_C        := 0;
    Dialog_Base_Unit_Width  :         Int_4_Signed_C        := 0;
    Text_Box_Font_Height    :         Int_4_Signed_C        := 1;
    Text_Box_Font_Width     :         Int_4_Signed_C        := 1;
    Dialog_Font_Height      :         Int_4_Signed_C        := 1;
    Dialog_Font_Width       :         Int_4_Signed_C        := 1;
    Output_Box_Height       :         Int_4_Signed_C        := 0;
    Output_Box_Width        :         Int_4_Signed_C        := 0;
    Input_Box_Height        :         Int_4_Signed_C        := 0;
    Start_Selection         :         Int_4_Signed_C        := 0;
    End_Selection           :         Int_4_Signed_C        := 0;
    Margin_Group_Top        :         Int_4_Signed_C        := 0;
    Margin_Group            :         Int_4_Signed_C        := 0;
    Current_Height          :         Int_4_Signed_C        := 0;
    Current_Width           :         Int_4_Signed_C        := 0;
    Console_Height          :         Int_4_Signed_C        := 0;
    Console_Width           :         Int_4_Signed_C        := 0;
    Button_Height           :         Int_4_Signed_C        := 0;
    Button_Width            :         Int_4_Signed_C        := 0;
    Right_Count             :         Int_4_Signed_C        := 0;
    Left_Count              :         Int_4_Signed_C        := 0;
    Number_Of_Lines         :         Int_4_Signed_C        := 0;
    Current_Line            :         Int_4_Signed_C        := 0;
    Current_Lines           :         Int_8_Natural         := 0;
    Is_At_Bottom            :         Boolean                   := True;
    Is_First_Time           :         Boolean                   := True;
    Was_At_Bottom           :         Boolean                   := False;
    Was_At_Minimum_Width    :         Boolean                   := False;
    Was_At_Minimum_Height   :         Boolean                   := False;
    Failed_Button_Font_Set  :         Boolean                   := False;
    Do_Process_Character    :         Boolean                   := False;
    Do_Skip_Message         :         Boolean                   := False;
    Current_Log             :         Str_2_Unbounded        := NULL_STRING_2_UNBOUNDED;
    Current_Input           :         Char_2               := NULL_Char_2;
    Message_Box_Window      :         Address                   := NULL_ADDRESS;
    Output_Group_Box        :         Address                   := NULL_ADDRESS;
    Input_Group_Box         :         Address                   := NULL_ADDRESS;
    Edit_Background         :         Address                   := NULL_ADDRESS;
    Font_Text_Box           :         Address                   := NULL_ADDRESS;
    Font_Buttons            :         Address                   := NULL_ADDRESS;
    Output_Box              :         Address                   := NULL_ADDRESS;
    Input_Box               :         Address                   := NULL_ADDRESS;
    Context                 :         Address                   := NULL_ADDRESS;
    Console                 :         Address                   := NULL_ADDRESS;
    Icon                    :         Address                   := NULL_ADDRESS;
    Buttons : array (CONSOLE_BUTTONS'range) of Address := (others => NULL_ADDRESS);
    function Is_Scrollbar_At_Bottom return Boolean is
      Scroll_Information : aliased Record_Scroll_Information := (others => <>);
      begin
        Assert (Get_Scroll_Information (Output_Box, 1, Scroll_Information'Unchecked_Access));
        return Scroll_Information.Position + Number_of_Lines >= Scroll_Information.Maximum;
      end Is_Scrollbar_At_Bottom;
    procedure Set_Text (Handle : Address; Text : Str_2) is
      begin
        Assert (Send_Message (
          Window        => Handle,
          Message       => EVENT_SET_TEXT,
          Data_Unsigned => 0,
          Data_Signed   => To_Unchecked_Int_Address (To_Ptr_Constant_Char_2_C (Localize (Text)))));  
      end Set_Text;
    procedure Set_Font (Handle : Address; Font : Address) is
      begin
        Assert_Dummy (Send_Message (
          Window        => Handle,
          Message       => EVENT_SET_FONT,
          Data_Signed   => 0,
          Data_Unsigned => To_Unchecked_Int_Address (Font)));
      end Set_Font;
    procedure Set_Sizes is
      Minimum_Width : Int_4_Signed_C := 0;
      Box_Padding   : Int_4_Signed_C := 0;
      Border_Height : Int_4_Signed_C := Get_System_Metrics (DATA_BORDER_HEIGHT);
      Border_Width  : Int_4_Signed_C := Get_System_Metrics (DATA_BORDER_WIDTH);
      begin
        Margin_Group      := Int_4_Signed_C (Float (Text_Metric.Height) / GROUP_BOX_SIDE_MARGIN);
        Margin_Group_Top  := Int_4_Signed_C (Float (Text_Metric.Height) * FONT_GROUP_BOX_SIZE);
        Box_Padding       := Int_4_Signed_C (Float (Text_Box_Font_Width) / 1.5);
        Output_Box_Width  := 2 * Box_Padding + (Text_Box_Font_Width  * Int_4_Signed_C (Get_Line_Size)) + Get_System_Metrics (DATA_SCROLL_BAR_WIDTH);
        Output_Box_Height := 2 * Box_Padding + (Text_Box_Font_Height * Int_4_Signed_C (NUMBER_OF_OUTPUT_ROWS));
        Input_Box_Height  := 2 * Box_Padding + Text_Box_Font_Height;
        Console_Width     := (MARGIN * Dialog_Base_Unit_Width + Margin_Group + Border_Width) * 2 + Output_Box_Width;
        Console_Height    := (MARGIN * Dialog_Base_Unit_Height) * 4 + (Border_Height + Margin_Group + Margin_Group_Top) * 2 + Output_Box_Height + Input_Box_Height + BUTTON_HEIGHT + Get_System_Metrics (DATA_TITLE_BAR_HEIGHT);
        if Buttons'Length > 0 then
          Minimum_Width := (Buttons'Length - 1) * MARGIN_BUTTON * Dialog_Base_Unit_Width + MARGIN * 2 * Dialog_Base_Unit_Width + BUTTON_WIDTH * Buttons'Length + Border_Width * 2;
          if Console_Width < Minimum_Width then
            Output_Box_Width := Output_Box_Width + Minimum_Width - Console_Width;
            Console_Width := Minimum_Width;
          end if;
        end if;
        if Current_Height < Console_Height or (Was_At_Minimum_Height and Current_Height > Console_Height) then Current_Height := Console_Height; end if;
        if Current_Width  < Console_Width  or (Was_At_Minimum_Width  and Current_Width  > Console_Width)  then Current_Width  := Console_Width; end if;
        Output_Box_Width      := Current_Width  - (Console_Width  - Output_Box_Width);
        Output_Box_Height     := Current_Height - (Console_Height - Output_Box_Height);
        Was_At_Minimum_Height := Current_Height < Console_Height + Border_Height;
        Was_At_Minimum_Width  := Current_Width  < Console_Width  + Border_Width;
        Number_Of_Lines       := (Output_Box_Height - 2 * Box_Padding) / Text_Box_Font_Height;
      end Set_Sizes;
    procedure Create_Fonts is 
      begin
        if Failed_Button_Font_Set then
          Font_Buttons :=
            Create_Font (
              Height           => Message_Box_Font_Height,
              Width            => 0,
              Escapement       => 0,
              Orientation      => 0,
              Weight           => FONT_WEIGHT_LIGHT,
              Italic           => 0,
              Underline        => 0,
              Strike_Out       => 0,
              Char_Set    => DEFAULT_Char_SET,
              Output_Precision => FONT_OUT_DEFAULT_PRECISION,
              Clip_Precision   => FONT_CLIP_DEFAULT_PRECISION,
              Quality          => FONT_DEFAULT_QUALITY,
              Pitch_And_Family => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
              Face             => To_Ptr_Constant_Char_2_C (FONT_DIALOG));
          Assert (Font_Buttons);
        end if;
        Font_Text_Box :=
          Create_Font (
            Height           => FONT_CONSOLE_SIZE,
            Width            => 0,
            Escapement       => 0,
            Orientation      => 0,
            Weight           => FONT_WEIGHT_LIGHT,
            Italic           => 0,
            Underline        => 0,
            Strike_Out       => 0,
            Char_Set    => DEFAULT_Char_SET,
            Output_Precision => FONT_OUT_DEFAULT_PRECISION,
            Clip_Precision   => FONT_CLIP_DEFAULT_PRECISION,
            Quality          => FONT_DEFAULT_QUALITY,
            Pitch_And_Family => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
            Face             => To_Ptr_Constant_Char_2_C (FONT_CONSOLE));
        Assert (Font_Text_Box);
        Assert_Dummy (Select_Object (Context, Font_Text_Box));
        Assert (Get_Text_Metrics (Context, Text_Metric'Address));
        Text_Box_Font_Height := Text_Metric.Height;
        Assert (Get_Text_Extent_Point (
          Device_Context => Context,
          Text           => To_Ptr_Char_2_C ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"),
          Count          => 52,
          Size           => Size'Address));
        Text_Box_Font_Width := Int_4_Signed_C ( (Size.X / 26 + 1) / 2);
        Assert_Dummy (Select_Object (Context, Font_Buttons));
        Assert (Get_Text_Metrics (Context, Text_Metric'Address));
      end Create_Fonts;
    procedure Get_Sizes_From_Message_Box is
      Hook : Address := NULL_ADDRESS;
      procedure Wait_For_Set_With_Inifinite_Loop is
        begin
          for I Ptr_Int_4_Signed'range loop -- The Windows API made me do it. I swear
            exit when Dialog_Base_Unit_Width /= 0;
            delay 0.0001;
          end loop;
          if Dialog_Base_Unit_Width = 0 then raise Call_Failure; end if;
        end Wait_For_Set_With_Inifinite_Loop;
      function Callback_Message_Box (Code : Ptr_Int_4_Signed_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Message_Box);
      function Callback_Message_Box (Code : Ptr_Int_4_Signed_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address is
        Class_Name  : aliased Str_2_C (1..256) := (others => NULL_Char_2_C);
        Window_Text : aliased Str_2_C (1..256) := (others => NULL_Char_2_C);
        Window      :         Address            := To_Unchecked_Address (Int_Address (Data_Unsigned));
        function Callback_Message_Box_Children (Window : Address; Data_Signed : Ptr_Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Message_Box_Children);
        function Callback_Message_Box_Children (Window : Address; Data_Signed : Ptr_Int_Address) return Int_Address is
          Rectangle : aliased Record_Rectangle := (others => <>);
          begin
            Assert (Get_Class_Name (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
            Assert_Dummy (Get_Window_Text (Window, Window_Text'Unrestricted_Access, Window_Text'Length));
            if Dialog_Base_Unit_Width = 0 and "Button" = To_Str_2 (Class_Name) then
              Assert (Get_Window_Rectangle (Window, Rectangle'Address));
              Button_Width            := (Rectangle.Right - Rectangle.Left);
              Button_Height           := (Rectangle.Bottom - Rectangle.Top);
              Assert_Dummy (Select_Object (Context, To_Unchecked_Address (Int_Address (To_Unchecked_Int_4_Unsigned (Send_Message (Window, MESSAGE_GET_FONT, 0, 0))))));
              Assert (Get_Text_Metrics (Context, Text_Metric'Address));
              Message_Box_Font_Height := Text_Metric.Height;
              Dialog_Base_Unit_Width  := Button_Width / BUTTON_WIDTH_DLU;
              Dialog_Base_Unit_Height := Button_Height / BUTTON_HEIGHT_DLU;
            end if;
            return Int_Address (C_TRUE);
          end Callback_Message_Box_Children;
        begin
          Assert (Get_Class_Name (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
          Assert_Dummy (Get_Window_Text (Window, Window_Text'Unrestricted_Access, Window_Text'Length));
          if Code = COMPUTER_BASED_TRAINING_ACTIVATE and To_Str_2 (Class_Name) = CLASS_NAME_DIALOG and To_Str_2 (Window_Text) = To_Str_2 (Hack) then
            Assert_Dummy (Enumerate_Child_Windows (Window, Callback_Message_Box_Children'Address, 0)); -- I heard you like callbacks
            Wait_For_Set_With_Inifinite_Loop;
            Assert (Destroy_Window (Window));
            Assert (Unhook_Windows_Hook (Hook));
          else Assert_Dummy (Call_Next_Hook (Hook, Code, Data_Unsigned, Data_Signed)); end if;
          return Int_Address (C_FALSE);
        end Callback_Message_Box;
      begin
        Hook := Set_Windows_Hook (COMPUTER_BASED_TRAINING_HOOK, Callback_Message_Box'Address, NULL_ADDRESS, Get_Current_Thread_Id);
        Assert_Dummy (Message_Box (NULL_ADDRESS, To_Str_2_C (SPECIFICS.Name), Hack, 0));
        Wait_For_Set_With_Inifinite_Loop;
      end Get_Sizes_From_Message_Box;
    function Callback_Window (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address; pragma Convention (Stdcall, Callback_Window);
    function Callback_Window (Window : Address; Message : Ptr_Int_4_Unsigned_C; Data_Unsigned, Data_Signed : Ptr_Int_Address) return Int_Address is
      Point : Record_Point;
      function To_Windows_Color (Color : Record_Color) return Int_4_Unsigned_C is
        begin
          return Int_4_Unsigned_C (Int_4_Unsigned (Color.Red) or Shift_Left (Int_4_Unsigned (Color.Green), 8) or Shift_Left (Int_4_Unsigned (Color.Blue), 16));
        end To_Windows_Color;
      begin
        case Message is
          when EVENT_CLOSE => Post_Quit_Message (0); return Int_Address (C_TRUE);
          when EVENT_COMMAND =>
            case Data_Unsigned is
              when Int_Address (SUBEVENT_MENU_POPOUT) | Int_Address (SUBEVENT_SCREEN_SAVER_START) => return Int_Address (C_TRUE);
            when others => null; end case;
          when EVENT_CREATE =>
            Edit_Background := Create_Solid_Brush (To_Windows_Color (COLOR_BACKGROUND));
            Assert (Edit_Background);
          when EVENT_CONTROL_STATIC_COLOR | EVENT_CONTROL_DYNAMIC_COLOR =>
            if To_Unchecked_Address (Data_Signed) = Output_Box or To_Unchecked_Address (Data_Signed) = Input_Box then
              Assert (Set_Background_Color (To_Unchecked_Address (Int_Address (Data_Unsigned)), To_Windows_Color (COLOR_BACKGROUND)) /= INVALID_COLOR);
              Assert (Set_Text_Color       (To_Unchecked_Address (Int_Address (Data_Unsigned)), To_Windows_Color (COLOR_TEXT))       /= INVALID_COLOR);
              return To_Unchecked_Int_Address (Edit_Background);
            end if;
          when EVENT_BUTTON_COMMAND =>
            for I in CONSOLE_BUTTONS'range loop
              if Data_Unsigned = Int_Address (I + IDENTIFIER_START) then
                if CONSOLE_BUTTONS (I).Action = null then Post_Quit_Message (0);
                else CONSOLE_BUTTONS (I).Action.all; end if;
                return Int_Address (C_TRUE);
              end if;
            end loop;
          when EVENT_GET_MINIMUM_MAXIMUM_SIZE_INFORMATION =>
            declare
            Minimum_Maximum_Information : Access_Record_Minimum_Maximum_Information := To_Ptr_Record_Minimum_Maximum_Information (To_Unchecked_Address (Data_Signed));
            begin
              Minimum_Maximum_Information.Minimum_Track_Size.X := Console_Width;
              Minimum_Maximum_Information.Minimum_Track_Size.Y := Console_Height;
            end;
            return Int_Address (C_TRUE);
          when EVENT_SIZE_CHANGED =>
            if Data_Unsigned /= Int_Address (SUBEVENT_ICONIZED) then
              Was_At_Minimum_Height := False;
              Was_At_Minimum_Width  := False;
              Assert_Dummy (Change_Window_Setting (
                Window  => Console,
                Command => SET_WINDOW_STYLE_EXTRA,
                Setting => STYLE_EXTRA_COMPOSITED));
              Is_At_Bottom := Is_Scrollbar_At_Bottom;
              Assert (Get_Window_Rectangle (Console, Rectangle'Address));
              Current_Width := Rectangle.Right - Rectangle.Left;
              Current_Height := Rectangle.Bottom - Rectangle.Top;
              Set_Sizes;
              Assert (Set_Window_Position (
                Window       => Console,
                Insert_After => NULL_ADDRESS,
                X            => Rectangle.Left,
                Y            => Rectangle.Top,
                Width        => Current_Width,
                Height       => Current_Height,
                Flags        => 0));
              Y := Dialog_Base_Unit_Width * MARGIN;
              Assert (Set_Window_Position (
                Window       => Output_Group_Box,
                Insert_After => NULL_ADDRESS,
                X            => Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width + Margin_Group * 2,
                Height       => Output_Box_Height + Margin_Group_Top + Margin_Group,
                Flags        => 0));
              Y := Y + Margin_Group_Top;
              Assert (Set_Window_Position (
                Window       => Output_Box,
                Insert_After => NULL_ADDRESS,
                X            => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width,
                Height       => Output_Box_Height,
                Flags        => 0));
              Y := Y + Output_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;
              Assert (Set_Window_Position (
                Window       => Input_Group_Box,
                Insert_After => NULL_ADDRESS,
                X            => Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Height       => Input_Box_Height + Margin_Group_Top + Margin_Group,
                Width        => Output_Box_Width + Margin_Group * 2,
                Flags        => 0));
              Y := Y + Margin_Group_Top;
              Assert (Set_Window_Position (
                Window       => Input_Box,
                Insert_After => NULL_ADDRESS,
                X            => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                Y            => Y,
                Width        => Output_Box_Width,
                Height       => Input_Box_Height,
                Flags        => 0));
              Y := Y + Input_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;
              Right_Count := 0;
              Left_Count  := 0;
              for I in Buttons'range loop
                Assert (Set_Window_Position (
                  Window       => Buttons (I),
                  Insert_After => NULL_ADDRESS,
                  Width        => Button_Width,
                  Height       => Button_Height,
                  Flags        => 0,
                  Y            => Y,
                  X            => (
                    if CONSOLE_BUTTONS (I).Action /= null then -- Left justify
                      Dialog_Base_Unit_Width * (MARGIN + Left_Count * MARGIN_BUTTON) + Left_Count * BUTTON_WIDTH
                    else -- Right justify
                      Output_Box_Width + Margin_Group * 2 + Dialog_Base_Unit_Width * MARGIN - (Right_Count + 1) * BUTTON_WIDTH - Dialog_Base_Unit_Width * Right_Count * MARGIN_BUTTON)));
                if CONSOLE_BUTTONS (I).Action = null then Right_Count := Right_Count + 1;
                else Left_Count := Left_Count + 1; end if;
              end loop;
              if Is_At_Bottom then
                Assert_Dummy (Send_Message (
                  Window        => Output_Box,
                  Message       => EVENT_SCROLL_VERTICALLY,
                  Data_Unsigned => SUBEVENT_SCROLL_BOTTOM,
                  Data_Signed   => 0));
              end if;
              Assert (Redraw_Window (Window, NULL_ADDRESS, NULL_ADDRESS, 1));
              Assert_Dummy (Change_Window_Setting (
                Window  => Console,
                Command => SET_WINDOW_STYLE_EXTRA,
                Setting => 0));
              return Int_Address (C_TRUE);
            end if;
        when others => null; end case;
        return Define_Window_Procedure (Window, Message, Data_Unsigned, Data_Signed);
      end Callback_Window;
    begin        
      Context := Get_Device_Context (Get_Desktop_Window);
      Assert (Context);
      Icon :=
        Load_Image ( -- Loads the icon nicely for the Aero theme, but on the "classic" theme it looks pixelated on the title bar
          Instance  => Get_Current_Instance,
          Name      => To_Ptr_Constant_Char_2_C (PATH_ASSETS & "\ico\" & NAME_ICON & ".ico"),
          Kind      => LOAD_ICO,
          Desired_X => 0,
          Desired_Y => 0,
          Load      => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_ADDRESS then
        Icon := Load_Icon (Get_Current_Instance, GENERIC_ICON);
      end if;
      if System_Parameter_Information ( -- Fails under XP....
        Action       => GET_NON_CLIENT_METRICS,
        User_Profile => 0,
        Parameter_B  => Non_Client_Metrics'Address,
        Parameter_A  => Non_Client_Metrics.Size) = FAILED then
        Failed_Button_Font_Set := True;
      else
        Font_Buttons := Create_Font_Indirect (Non_Client_Metrics.Message_Font'Unchecked_Access);
        Assert (Font_Buttons);
      end if;
      Get_Sizes_From_Message_Box;
      Create_Fonts;
      Set_Sizes;
      Class := (
        Callback   => Callback_Window'Address,
        Instance   => Get_Current_Instance,
        Icon_Small => Icon,
        Icon_Large => Icon,
        Cursor     => Load_Cursor (NULL_ADDRESS, GENERIC_CURSOR),
        Background => BRUSH_WINDOW,
        Class_Name => To_Ptr_Constant_Char_2_C (To_Str_2 (Name_Class)),
        others     => <>);
      Assert (Register_Class (Class'Unchecked_Access) /= Int_2_Unsigned_C (FAILED));
      Console :=
        Create_Window (
          Class_Name  => Name_Class,
          Window_Name => Name_Class,
          X           => Get_Device_Capabilities (Context, DATA_HORIZONTAL_RESOLUTION) / 2 - Console_Width  / 2,
          Y           => Get_Device_Capabilities (Context, DATA_VERTICAL_RESOLUTION)   / 2 - Console_Height / 2,
          Width       => Console_Width,
          Height      => Console_Height,
          Parent      => NULL_ADDRESS,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       =>
            STYLE_ICONIC_INITIALLY or
            STYLE_BORDER_SIZABLE   or
            STYLE_TITLEBAR_MENU    or
            STYLE_BORDER_THIN_LINE or
            STYLE_BOX_ICONIZE      or
            STYLE_BOX_FULLSCREEN);
      Assert (Console);
      Output_Box :=
        Create_Window (
          Class_Name  => NAME_EDIT,
          Window_Name => NULL_STRING_2_C,
          X           => 0,
          Y           => 0,
          Width       => 0,
          Height      => 0,
          Parent      => Console,
          Menu        => 0,
          Instance    => Get_Current_Instance,
          Parameter   => NULL_ADDRESS,
          Style_Extra => 0,
          Style       =>
            STYLE_HAS_VERTICAL_SCROLL_BAR   or
            STYLE_VISIBLE_INITIALLY         or
            STYLE_BORDER_THIN_LINE          or
            STYLE_ALIGN_TEXT_TO_LEFT        or
            STYLE_MULTI_LINE                or
            STYLE_NO_USER_EDITING           or
            STYLE_CHILD);
      Assert (Output_Box);
      Set_Font (Output_Box, Font_Text_Box);
      Assert_Dummy (Show_Window (Console, MAKE_WINDOW_GO_TO_ICONIC)); -- "The fact that this works is a major windows bug, good find!"
      Assert_Dummy (Show_Window (Console, MAKE_WINDOW_RESTORE));
      Assert (Set_Focus (Output_Box));
      while Message.Data /= MESSAGE_QUIT loop
        if Length (Current_Log) /= Get_Log'Length then
          Current_Log := To_Str_2_Unbounded (Get_Log);
          Is_At_Bottom := Is_Scrollbar_At_Bottom;
          Assert_Dummy (Send_Message (
            Window        => Output_Box,
            Message       => EVENT_TEXT_GET_SELECTION,
            Data_Unsigned => To_Unchecked_Int_Address (Start_Selection'Address),
            Data_Signed   => To_Unchecked_Int_Address (End_Selection'Address)));
          Current_Line := Send_Message (Output_Box, EVENT_TEXT_GET_LINE, 0, 0);
          Assert_Dummy (Send_Message (
            Window        => Output_Box,
            Message       => EVENT_SET_REDRAW,
            Data_Unsigned => Int_Address (C_FALSE),
            Data_Signed   => 0));
          Set_Text (Output_Box, Get_Log);
          Assert_Dummy (Send_Message (
            Window        => Output_Box,
            Message       => EVENT_TEXT_SET_SELECTION,
            Data_Unsigned => Int_Address (Start_Selection),
            Data_Signed   => Int_Address (End_Selection)));            
          if Is_At_Bottom or Is_First_Time then
            Is_First_Time := False;
            Assert_Dummy (Send_Message (
              Window        => Output_Box,
              Message       => EVENT_SCROLL_VERTICALLY,
              Data_Unsigned => SUBEVENT_SCROLL_BOTTOM,
              Data_Signed   => 0));
            Current_Line := Send_Message (Output_Box, EVENT_TEXT_GET_LINE, 0, 0);
          else
            for I in 1..Current_Line loop
              Assert_Dummy (Send_Message (
                Window        => Output_Box,
                Message       => EVENT_SCROLL_VERTICALLY,
                Data_Unsigned => SUBEVENT_SCROLL_DOWN_LINE,
                Data_Signed   => 0));
            end loop;
          end if;
          Current_Lines := Get_Number_Of_Lines;
          Assert (Send_Message (
            Window        => Output_Box,
            Message       => EVENT_SET_REDRAW,
            Data_Unsigned => Int_Address (C_TRUE),
            Data_Signed   => 0));
        end if;
        if Peek_Message (
          Message        => Message'Unchecked_Access,
          Window         => NULL_ADDRESS,
          Filter_Minimum => IGNORE_MESSAGE_FILTER_MINIMUM,
          Filter_Maximum => IGNORE_MESSAGE_FILTER_MAXIMUM,
          Command        => REMOVE_MESSAGES_AFTER_PROCESSING) /= FAILED then
          case Message.Data is
            when EVENT_MOUSE_WHEEL_VERTICAL =>
              if Get_Focus /= Output_Box then
                if To_Unchecked_Int_2_Signed (Int_2_Unsigned (Shift_Right (Int_8_Unsigned (Message.Data_Unsigned) and 16#0000_0000_FFFF_0000#, 16))) / MOUSE_WHEEL_DELTA < 0
                then for I in 1..Current_Lines / SCROLL_FACTOR loop Assert_Dummy (Send_Message (Output_Box, EVENT_SCROLL_VERTICALLY, 1, 0)); end loop;
                else for I in 1..Current_Lines / SCROLL_FACTOR loop Assert_Dummy (Send_Message (Output_Box, EVENT_SCROLL_VERTICALLY, 0, 0)); end loop; end if;
              end if;
            when EVENT_KEY_DOWN =>
              Do_Process_Character := True;
              case Int_2_Unsigned_C (Message.Data_Unsigned) is
                when VIRTUAL_KEY_V =>
                  if (Get_Key_State (Int_4_Signed_C (VIRTUAL_KEY_CONTROL)) and 16#8000#) > 0 and Get_Focus /= Input_Box then
                    Set_Input_Entry (Get_Input_Entry & Get_Clipboard);
                    Set_Text (Input_Box, Get_Input_Entry);
                  end if;
              when others => null; end case;
            when EVENT_CHARACTER =>
              if Do_Process_Character then
                Do_Process_Character := False;
                Current_Input := Char_2'val (Int_4_Signed (Message.Data_Unsigned));
                if not Is_Control (Current_Input) then
                  Assert_Dummy (Send_Message (
                    Window        => Input_Box,
                    Message       => EVENT_TEXT_GET_BUFFER,
                    Data_Unsigned => Int_Address (Buffer'Length),
                    Data_Signed   => To_Unchecked_Int_Address (Buffer'Address)));
                  Set_Input_Entry (To_Str_2 (Buffer) & Current_Input);
                  if Get_Focus /= Input_Box then Set_Text (Input_Box, Get_Input_Entry); end if;
                elsif Current_Input = Char_2'val (Char_1'Pos (ASCII.CR)) then
                  Assert_Dummy (Send_Message (
                    Window        => Input_Box,
                    Message       => EVENT_TEXT_GET_BUFFER,
                    Data_Unsigned => Int_Address (Buffer'Length),
                    Data_Signed   => To_Unchecked_Int_Address (Buffer'Address)));
                  Set_Input_Entry (To_Str_2 (Buffer));
                  if Get_Input_Entry /= NULL_STRING_2 then
                    Put_Line (Get_Input_Entry);
                    if Get_Input_Entry /= NULL_STRING_2 then
                      Handle (Get_Input_Entry);
                      Set_Input_Entry (NULL_STRING_2);
                      Set_Text (Input_Box, NULL_STRING_2);
                    end if;
                  end if;
                  Do_Skip_Message := True;
                elsif Current_Input = Char_2'val (Char_1'Pos (ASCII.BS)) and Get_Input_Entry /= NULL_STRING_2 then
                  Set_Input_Entry (Get_Input_Entry (1..Get_Input_Entry'last - 1));
                  if Get_Focus /= Input_Box then Set_Text (Input_Box, Get_Input_Entry); end if;
                elsif Current_Input = Char_2'val (Char_1'Pos (ASCII.HT)) then Do_Skip_Message := True; end if;
              end if;
          when others => null; end case;
          if not Do_Skip_Message then
            Assert_Dummy (Translate_Message (Message'Unchecked_Access));
            Assert_Dummy (Dispatch_Message (Message'Unchecked_Access));
          end if;
          Do_Skip_Message := False;
        end if;
      end loop;
      Assert_Dummy (Show_Window (Console, MAKE_WINDOW_HIDE));
      Assert (Destroy_Window (Console));
      Assert (Unregister_Class (Name_Class, NULL_ADDRESS));
      Console := NULL_ADDRESS;
      Assert (Release_Device_Context (Get_Desktop_Window, Context));
    end Run;
