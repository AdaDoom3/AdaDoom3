
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

with Neo.Win32; use Neo.Win32;

-- System-dependant subprograms for Microsoft Windows®
separate (Neo.Engine) package body Import is

  -----------------
  -- Information --
  -----------------

  -- This section is at the top of the package so that the exe location path can be used to title the game windows - see globals below

  -- Get OS and executable information
  function Get_Information_Internal return Information_State is
    Dir     : aliased Str_C (1..4096);
    Buffer  : aliased Int_C;
    Version : aliased OSVERSIONINFOEX;
    begin

      -- Fetch strings
      Assert (GetVersionExW (Version'Unchecked_Access));
      Assert (GetModuleFileNameW (NULL_PTR, Dir'Unrestricted_Access, Dir'Length));

      -- Snip off the exe name
      Dir (Int_Size_C (Index (To_Str (Dir), "\", Backward))) := NULL_Char_16_C;          
      
      -- Get buffer size
      Assert (GetUserNameW (null, Buffer'Unchecked_Access) = 0 and then GetLastError = ERROR_INSUFFICIENT_BUFFER); 
      declare
      Username : aliased Ptr_Str_16_C := new Str_C (1..Int_Size_C (Buffer));
      begin
        Assert (GetUserNameW (Username, Buffer'Unchecked_Access));
        Assert (IsWow64Process (GetCurrentProcess, Buffer'Unchecked_Access));

        -- Return the junk
        return (Name      => Delete (To_Str_16_Unbound (To_Str (Dir)), 1, Index (To_Str (Dir), "\", Backward)), -- Get first-level directory
                Path      => To_Str_16_Unbound (To_Str (Dir)),
                Username  => To_Str_16_Unbound (To_Str (Username.All)),
                Bit_Size  => (if Buffer = 1 then 64 else 32),
                OS        => To_Str_16_Unbound (NULL_STR & -- ???

                             -- Identify the version of Windows if it older than 7 - this is officially deprecated...
                             (case Version.dwPlatformId is
                                when 1 =>
                                  (case Version.dwMajorVersion is
                                     when 4 =>
                                       (case Version.dwMinorVersion is
                                          when 0 =>
                                            (case Version.szCSDVersion (2) is
                                               when 'B' | 'C' => "Windows_1_32_B_System",
                                               when others    => "Windows_1_32_A_System"),
                                          when 10 =>
                                             (case Version.szCSDVersion (2) is
                                                when 'A'    => "Windows_1_32_10_B_System",
                                                when others => "Windows_1_32_10_A_System"),
                                          when 90     => "Windows_1_32_90_System",
                                          when others => "Windows_1_32_System"),
                                    when others => "Windows_1_System"),
                                when 2 =>
                                  (case Version.dwMajorVersion is
                                     when 5 =>
                                       (case Version.dwMinorVersion is
                                          when 1      => "Windows_2_5_1_System",
                                          when others => "Windows_2_5_System"),
                                     when 6 =>
                                       (case Version.dwMinorVersion is
                                          when 1      => "Windows_2_6_1_System",
                                          when 2      => "Windows_2_6_2_System",
                                          when others => "Windows_2_6_System"),
                                     when others => "Windows_2_System"),
                                when others => "Windows_System")));
        end;
    end;

  -- Constant for removal of overhead since OS data is not expected to change
  INFORMATION_INTERNAL : constant Information_State := Get_Information_Internal;
  function Get_Information return Information_State is (INFORMATION_INTERNAL);

  -------------
  -- Globals --
  -------------

  -- Window titles
  APP_NAME           : constant Str  := To_Str (Get_Information.Name);
  GAME_NAME          : aliased Str_C := To_Str_C (APP_NAME);
  INPUT_NAME         : aliased Str_C := To_Str_C (APP_NAME & " Input");
  CONSOLE_NAME       : aliased Str_C := To_Str_C (APP_NAME & " Console");
  MULTI_MONITOR_NAME : aliased Str_C := To_Str_C (APP_NAME & " Multi-monitor");

  -- Asset paths
  WIN32_PATH_ICON            : aliased Str_C := To_Str_C (PATH_ICON             & ".ico");
  WIN32_PATH_CURSOR_ACTIVE   : aliased Str_C := To_Str_C (PATH_CURSOR_ACTIVE    & ".cur");
  WIN32_PATH_CURSOR_INACTIVE : aliased Str_C := To_Str_C (PATH_CURSOR_INACTIVE  & ".cur");

  -- Main "HWND"s for the invisible input window and actual game window
  Game, Input : aliased Ptr;

  -- Window handles for multi-monitor mode
  package Vector_Ptr is new Vectors (Ptr);
  Multi_Monitor_Windows : Vector_Ptr.Unsafe.Vector;

  ------------
  -- Vulkan --
  ------------

  -- Pointer to dll
  Vulkan_DLL : Ptr := LoadLibraryW (To_Str_C ("vulkan.dll"));

  -- Load a pointer to a procedure based on a name
  function Get_Vulkan_Procedure (Name : Str) return Ptr is (GetProcAddressW (Vulkan_DLL, To_Str_C (Name)));

  -- Fetch extension strings
  function Get_Extensions return Array_Str_8_Unbounded is ((VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_WIN32_SURFACE_EXTENSION_NAME));

  -- Finalization and initialization (mostly revolve around loading the dll)
  procedure Finalize_Vulkan is begin Assert (FreeLibraryW (Vulkan_DLL)); end;
  procedure Initialize_Vulkan is
    begin
      Assert (Vulkan_DLL); -- Should already be initialized, so check the result
      vkCreateWin32SurfaceKHR := To_vkCreateWin32SurfaceKHR_Ptr (Get_Vulkan_Procedure ("vkCreateWin32SurfaceKHR"));
    end;

  -- Create a new surface
  procedure Create_Surface (Instance : VkInstance; Surface : in out VkSurfaceKHR) is
    Surface_Create_Info : aliased VkWin32SurfaceCreateInfoKHR := (hWnd => Game, hInstance => GetModuleHandleNULL, others => <>);
    begin -- TODO Multi_Monitor_Mode
      Vulkan.Assert (vkCreateWin32SurfaceKHR (Instance, Surface_Create_Info'Access, NULL_PTR, Surface'Access));
    end;

  ---------------
  -- Clipboard --
  ---------------

  -- Set the clipboard with custom text
  procedure Copy (Item : Str) is

    -- Declare a new type to handle an unchecked conversion
    type Text_Array is array (Item'First..Item'Last + 1) of Char_16_C;
    type Ptr_Text_Array is access all Text_Array;
    function To_Ptr_Text_Array is new Ada.Unchecked_Conversion (Ptr, Ptr_Text_Array);
    Text : Ptr_Text_Array;

    -- Prepare global data
    Data : Ptr;
    begin
      Data := GlobalAlloc (MEMORY_MOVEABLE or MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE, Text_Array'Object_Size / Byte'Object_Size);
      Assert (Data);
      Text := To_Ptr_Text_Array (GlobalLock (Data));
      Assert (Text /= null);
      Text (Text.All'Last) := NULL_Char_16_C; -- Null terminate

      -- Write the text to data and send it to the clipboard
      for I in Item'Range loop Text (I) := To_Char_16_C (Item (I)); end loop;
      Assert (GlobalUnlock (Data) = 0);
      Assert (OpenClipboard (NULL_PTR) /= 0 and then GlobalFree (Data) = NULL_PTR);
      Assert (EmptyClipboard);
      Assert (SetClipboardData (CLIPBOARD_UNICODE_TEXT, Data));
      Assert (CloseClipboard);
    end;

  -- Grab the current clipboard text
  function Paste return Str is
    Text : Ptr_Const_Char_16_C;
    Data : Ptr;
    begin
      Assert (OpenClipboard (NULL_PTR));
      Data := GetClipboardData (CLIPBOARD_UNICODE_TEXT); Assert (Data);
      Text := To_Ptr_Const_Char_16_C (GlobalLock (Data));
      Assert (CloseClipboard);
      return To_Str (Text);
    end;

  --------------------
  -- Error_Handling --
  --------------------

  -- Get the system error code. Return values are not descriptive in the Win32 API
  function Last_Error return Int_32_Unsigned is (Int_32_Unsigned (GetLastError));

  -- Calls used for sending log or trace information
  procedure Open_Text    (Path : Str) is begin Execute ("explorer """ & Path & """"); end;
  procedure Open_Webpage (Path : Str) is begin Execute ("explorer "   & Path);        end;

  -- Make the main game window flash (or stop flashing)
  procedure Alert (Val : Bool) is
    Flash_Info : aliased FLASHWINFO := (dwFlags => (if Val then FLASH_CONTINUOUSLY else FLASH_END), others => <>);
    begin
      Assert (Game);
      Assert (FlashWindowEx (Flash_Info'Unchecked_Access));
    end;

  -- Execute a commandline statement
  procedure Execute (Path : Str) is
    Startup_Info : aliased STARTUPINFO;
    Process_Info : aliased PROCESS_INFORMATION;
    begin
      Assert (Path'Length <= MAXIMUM_PATH_LENGTH);
      Assert (CreateProcessW (lpApplicationName    => null,
                              lpCommandLine        => To_Ptr_Char_16_C (Path),
                              lpProcessAttributes  => NULL_PTR,
                              lpThreadAttributes   => NULL_PTR,
                              bInheritHandles      => 0,
                              dwCreationFlags      => 0,
                              lpEnvironment        => NULL_PTR,
                              lpCurrentDirectory   => null,
                              lpStartupInfo        => Startup_Info'Unchecked_Access,
                              lpProcessInformation => Process_Info'Unchecked_Access));
    end;

  -- Create a message box 
  function Ok (Name, Message : Str; Buttons : Buttons_Kind; Icon : Icon_Kind) return Bool is

    -- Temporary variables for pointer passing
    C_Name    : aliased Str_C := To_Str_C (Name);
    C_Message : aliased Str_C := To_Str_C (Message);

    -- Strap in for a custom icon
    Force_Custom_Icon : Ptr; 
    function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      Class_Name  : aliased Str_C (1..1024);
      Window_Text : aliased Str_C (1..1024);
      Window      :         Ptr := To_Ptr (Int_Ptr (wParam));
      Icon        :         Ptr;
      begin

        -- Identify the message box by class and window text
        Assert (GetClassNameW (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
        Ignore (GetWIndowTextW (Window, Window_Text'Unrestricted_Access, Window_Text'Length) = 0);
        if nCode = HCBT_ACTIVATE and To_Str (Class_Name) = CLASS_NAME_DIALOG and To_Str (Window_Text) = Name then

          -- Load that icon!
          Icon := LoadImageW (hinst     => NULL_PTR,
                              lpszName  => WIN32_PATH_ICON'Access,
                              uType     => LOAD_ICO,
                              cxDesired => 0,
                              cyDesired => 0,
                              fuLoad    => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
          if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, GENERIC_ICON); end if;

          -- Well that was fun...
          Ignore (SendMessageW (Window, MESSAGE_SET_ICON, 0, To_Int_Ptr (Icon)) = 0);
          Assert (UnhookWindowsHookEx (Force_Custom_Icon));
        else Ignore (CallNextHookEx (Force_Custom_Icon, nCode, wParam, lParam) = 0); end if;
        return 0;
      end;
    begin

      -- Create a hook to find the message box window which will then be able to set the icon
      Force_Custom_Icon := SetWindowsHookExW (WH_CBT, CBTProc'Address, NULL_PTR, GetCurrentThreadId);

      -- Do the call
      return (case MessageBoxW (hWnd      => Game,
                                lpCaption => C_Name'Unchecked_Access,
                                lpText    => C_Message'Unchecked_Access,
                                uType     => (if Game = NULL_PTR then MB_SYSTEMMODAL else 0)
                                             or (case Icon is
                                                   when No_Icon          => 0,
                                                   when Error_Icon       => MB_ICONERROR,
                                                   when Warning_Icon     => MB_ICONWARNING,
                                                   when Information_Icon => MB_ICONINFORMATION)
                                             or (case Buttons is
                                                   when Okay_Button          => MB_OK,
                                                   when Yes_No_Buttons       => MB_YESNO,
                                                   when Okay_Cancel_Buttons  => MB_OKCANCEL,
                                                   when Retry_Cancel_Buttons => MB_RETRYCANCEL)) is
                when IDOK | IDRETRY | IDYES => True, -- It's OK!
                when others => False);               -- Boo
    end;

  ---------------
  -- Windowing --
  ---------------

  -- Styles for game windowing modes
  STYLE_FULLSCREEN : constant Int_32_Unsigned_C := STYLE_VISIBLE_INITIALLY or WS_SYSMENU or WS_POPUP   or WS_TOPMOST;
  STYLE_WINDOWED   : constant Int_32_Unsigned_C := STYLE_VISIBLE_INITIALLY or WS_SYSMENU or WS_CAPTION or WS_BORDER or STYLE_BORDER_SIZABLE or WS_MAXIMIZEBOX;

  -- Icons and cursors
  Icon, Cursor_Inactive, Cursor_Active : Ptr;

  Original_Clip : aliased RECT := (others => <>);

  -- Conversion functions for rectangles and borders
  function To_Border (Rectangle : RECT)    return Border_State is ((Int_64 (Rectangle.top), Int_64 (Rectangle.bottom), Int_64 (Rectangle.left), Int_64 (Rectangle.right)));
  function To_RECT (Border : Border_State) return RECT         is ((Int_C (Border.Left), Int_C (Border.Top), Int_C (Border.Right), Int_C (Border.Bottom)));

  -- Find out if the API supports windowed mode (e.g. phone)
  function Fullscreen_Only return Bool is (False);

  -- Force the game into fullscreen mode
  procedure Minimize is begin Ignore (ShowWindow (Game, SW_SHOWMINIMIZED) = 0); end; 

  -- Change the cursor position
  procedure Set_Cursor (Pos : Cursor_State) is begin Assert (SetCursorPos (Int_C (Pos.X), Int_C (Pos.Y))); end;

  -- Get the size of windowed mode "decorations"
  function Get_Decoration return Border_State is ((Top    => Int_64 (GetSystemMetrics (DATA_TITLE_BAR_HEIGHT) + GetSystemMetrics (DATA_BORDER_HEIGHT)),
                                                   Right  => Int_64 (GetSystemMetrics (DATA_BORDER_WIDTH)),
                                                   Left   => Int_64 (GetSystemMetrics (DATA_BORDER_WIDTH)),
                                                   Bottom => Int_64 (GetSystemMetrics (DATA_BORDER_HEIGHT))));

  -- Internal procedure for initializing or adjusting the game window
  procedure Adjust_Windowing (X, Y : Int_64; Width, Height : Int_64_Positive; Fullscreen : Bool) is
    begin
      if Game = NULL_PTR then
        Game := CreateWindowExW (dwExStyle    => 0,
                                 lpClassName  => GAME_NAME'Access,
                                 lpWindowName => GAME_NAME'Access,
                                 dwStyle      => (if Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED) or STYLE_ICONIC_INITIALLY,
                                 x            => Int_C (X),
                                 y            => Int_C (Y),
                                 nWidth       => Int_C (Width),
                                 nHeight      => Int_C (Height),
                                 hWndParent   => NULL_PTR,
                                 hMenu        => 0,
                                 hInstance    => GetModuleHandleNULL,
                                 lpParam      => NULL_PTR);
        Assert (Game);
        Ignore (ShowWindow (Game, SW_SHOWMINIMIZED));
        Ignore (ShowWindow (Game, SW_RESTORE));
      else
        Assert (SetWindowLongW (Game, SET_WINDOW_STYLE, (if Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED)) /= 0);
        Assert (SetWindowPos (hWnd            => Game,
                              hWndInsertAfter => NULL_PTR,
                              X               => Int_C (X),
                              Y               => Int_C (Y),
                              cx              => Int_C (Width),
                              cy              => Int_C (Height),
                              uFlags          => 0));
      end if;
    end;

  -- Comment here !!!
  procedure Maximize is
    Context : Ptr := GetDC (GetDesktopWindow);
    begin
      Assert (Context);
      Adjust_Windowing (X          => 0,
                        Y          => 0,
                        Width      => Int_64_Positive (GetDeviceCaps (Context, DATA_HORIZONTAL_RESOLUTION)),
                        Height     => Int_64_Positive (GetDeviceCaps (Context, DATA_VERTICAL_RESOLUTION)),
                        Fullscreen => True);
      Assert (ReleaseDC (GetDesktopWindow, Context));
    end;
  procedure Make_Windowed is
    Context : Ptr := GetDC (GetDesktopWindow);
    begin
      Assert (Context);
      Adjust_Windowing (X          => Int_64 (GetDeviceCaps (Context, DATA_HORIZONTAL_RESOLUTION)) / 2 - Windowed_Width.Get  / 2,
                        Y          => Int_64 (GetDeviceCaps (Context, DATA_VERTICAL_RESOLUTION))   / 2 - Windowed_Height.Get / 2,
                        Width      => Windowed_Width.Get,
                        Height     => Windowed_Height.Get,
                        Fullscreen => False);
      Assert (ReleaseDC (GetDesktopWindow, Context));
    end;

  -- Cursor styling
  procedure Set_Cursor_Style_Internal (Kind : Ptr) is
    begin
      if WORD_SIZE = 32 then Ignore (SetClassLongW (Game, GCLP_HCURSOR, Int_32_Unsigned_C (To_Int_Ptr (Kind))));
      else Ignore (SetClassLongPtrW (Game, GCLP_HCURSOR, To_Int_Ptr (Kind))); end if;
    end;
  procedure Set_Cursor_Style (Kind : Cursor_Kind) is
    begin
      Set_Cursor_Style_Internal ((case Kind is
                                    when System_Cursor   => LoadCursorW (NULL_PTR, GENERIC_CURSOR),
                                    when Inactive_Cursor => Cursor_Inactive,
                                    when Active_Cursor   => Cursor_Active));
    end;

  -- Check if another instance of the game is running
  function Only_Instance return Bool is
    Handle : Ptr;
    Window : Ptr := FindWindowW (GAME_NAME'Access, null);
    begin
      if Window /= NULL_PTR then
        Ignore (ShowWindow (Window, SW_SHOWNORMAL) = 0);
        Handle := SetFocus (Window);
        Assert (SetForegroundWindow (Window));
        Handle := SetActiveWindow (Window);
        return False;
      end if;
      return True;
    end;

  -- Hide or show the cursor
  procedure Hide_Cursor (Do_Hide : Bool := True) is
    begin

      -- Modify the "internal display counter"...
      if Do_Hide then while ShowCursor (0) > -1 loop null; end loop;
      else            while ShowCursor (1) <  0 loop null; end loop; end if;
    end;

  -- Restrict or unrestrict cursor movement (also hide or show cursor)
  procedure Clip_Cursor (Do_Clip : Boolean := True) is
    Rectangle : aliased RECT;
    begin
      Assert (GetWindowRect (Game, Rectangle'Unchecked_Access));

      -- Unclip cursor
      if not Do_Clip and Original_Clip /= (others => <>) then
        Assert (ClipCursor (Original_Clip'Unchecked_Access));
        Original_Clip := (others => <>);

      -- Clip it
      elsif Do_Clip then
        if Original_Clip = (others => <>) then
          Assert (GetClipCursor (Original_Clip'Unchecked_Access));
          Assert (Original_Clip /= (others => <>));
        end if;
        Assert (ClipCursor (Rectangle'Unchecked_Access));
      end if;
    end;

  -- Register the application class and make the callback for the game window
  procedure Initialize_Windowing is

    -- Game window callback
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      begin        
        case uMsg is
          when WM_CLOSE => PostQuitMessage (0); return 0;

          -- Stop the OS from hiding or stopping the game
          when WM_COMMAND => if wParam = SC_KEYMENU or wParam = SC_SCREENSAVE then return 0; end if;
          when WM_SYSKEYDOWN => return 0;

          -- Inject characters into player one (this unfortunatly could not be separated from the windowing thread easily)
          when WM_CHAR =>
            if (GetKeyState (Int_C (VK_CONTROL)) and 16#8000#) = 0 then -- Do not inject text into the input system if ctrl is held
              Inject_Into_Player_1 ((Text_Impulse, (Text_Impulse, 1, NO_COMBO), To_Str_16_Unbound (Char_16'Val (Int (wParam)))));
            end if;

          -- Pass window action information to the engine
          when WM_ACTIVATE => Activated.Set (if (wParam and 16#0000_FFFF#) = 0 or (wParam and 16#FFFF_0000#) /= 0 then Other_Deactivated
                                             elsif (wParam and 16#0000_FFFF#) = WA_CLICKACTIVE then Click_Activated else Other_Activated);
          when WM_SIZE =>
            case wParam is
              when SIZE_MINIMIZED => Activated.Set (Minimize_Deactivated);
              when SIZE_MAXIMIZED => Mode.Set (Fullscreen_Mode); 
            when others => null; end case;
            return 0;

          -- Extract sizing event information then let the engine modify the final result 
          when WM_SIZING =>
            declare
            Result : Ptr_RECT := To_Ptr_RECT (lParam);
            begin
              Result.All := To_RECT (Resize (Border => To_Border (Result.All),
                                             Kind   => (case wParam is
                                                          when WMSZ_LEFT        => Left_Resize,
                                                          when WMSZ_RIGHT       => Right_Resize,
                                                          when WMSZ_TOP         => Top_Resize,
                                                          when WMSZ_TOPLEFT     => Top_Left_Resize,
                                                          when WMSZ_TOPRIGHT    => Top_Right_Resize,
                                                          when WMSZ_BOTTOM      => Bottom_Resize,
                                                          when WMSZ_BOTTOMLEFT  => Bottom_Left_Resize,
                                                          when WMSZ_BOTTOMRIGHT => Bottom_Right_Resize,
                                                          when others           => Other_Resize)));
            end;
        when others => null; end case;
        return DefWindowProcW (hwnd, uMsg, wParam, lParam);
      end;

    -- Register the application class
    Class : aliased WNDCLASSEX;
    begin

      -- Load the game window icon
      Icon := LoadImageW (hinst     => GetModuleHandleNULL,
                          lpszName  => WIN32_PATH_ICON'Access,
                          uType     => LOAD_ICO,
                          cxDesired => 0,
                          cyDesired => 0,
                          fuLoad    => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, GENERIC_ICON); end if;

      -- Load cursors
      Cursor_Inactive := LoadImageW (hinst     => GetModuleHandleNULL,
                                     lpszName  => WIN32_PATH_CURSOR_INACTIVE'Access,
                                     uType     => LOAD_CUR,
                                     cxDesired => 0,
                                     cyDesired => 0,
                                     fuLoad    => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Cursor_Inactive = NULL_PTR then Cursor_Inactive := LoadCursorW (NULL_PTR, GENERIC_CURSOR); end if;
      Cursor_Active := LoadImageW (hinst     => GetModuleHandleNULL,
                                   lpszName  => WIN32_PATH_CURSOR_ACTIVE'Access,
                                   uType     => LOAD_CUR,
                                   cxDesired => 0,
                                   cyDesired => 0,
                                   fuLoad    => LOAD_FROM_FILE);
      if Cursor_Active = NULL_PTR then Cursor_Active := LoadCursorW (NULL_PTR, GENERIC_CURSOR); end if;

      -- Register the main window class (the actual window creation is done in Adjust_Windowing)
      Class := (lpfnWndProc   => WindowProc'Address,
                hInstance     => GetModuleHandleNULL,
                hIconSm       => Icon,
                hIcon         => Icon,
                hCursor       => Cursor_Inactive,
                hbrBackground => BRUSH_GRAY,
                lpszClassName => To_Ptr_Const_Char_16_C (GAME_NAME),
                others        => <>);
      Assert (RegisterClassExW (Class'Access));
    end;

  -- Pump the OS message loop
  function Update_Windowing return Bool is
    Message : aliased MSG;
    begin
      while PeekMessageW (Message'Access, Game, 0, 0, PM_REMOVE) /= 0 loop
        if Message.message = WM_QUIT then return False; end if;
        Ignore (TranslateMessage (Message'Access));
        Ignore (DispatchMessageW (Message'Access));
      end loop;
      return True;
    end;

  -- Kill the game window
  procedure Finalize_Windowing is
    begin
      if Game /= NULL_PTR then
        Ignore (ShowWindow (Game, SW_HIDE) = 0);
        Ignore (DestroyWindow (Game));
      end if;
      Ignore (UnregisterClassW (Game_Name'Access, NULL_PTR));
      Game := NULL_PTR;
    end;

  -- Fetch all monitor borders (Internal)
  function Get_Monitors return Border_Array is
    Monitors : Vector_Border.Unsafe.Vector;

    -- Callback for hook
    function MonitorEnumProc (hMonitor, hdcMonitor : Ptr; lprcMonitor : Ptr_RECT; dwData : Ptr_Int_Ptr) return Int_C with Convention => Stdcall;
    function MonitorEnumProc (hMonitor, hdcMonitor : Ptr; lprcMonitor : Ptr_RECT; dwData : Ptr_Int_Ptr) return Int_C is
      Info : aliased MONITORINFO;
      begin
        Assert (GetMonitorInfoW (hMonitor, Info'Unchecked_Access));
        Monitors.Append (To_Border (Info.rcMonitor));
        return 1;
      end;
    begin

      -- Call hook
      Assert (EnumDisplayMonitors (NULL_PTR, null, MonitorEnumProc'Address, 0));
      Assert (Monitors.Length > 0);
      return Vector_Borders.To_Unsafe_Array (Monitors);
    end;

  -- Look at all the monitors on a system (excluding the main one) and return their borders
  function Get_Windows return Border_Array is
    begin

      -- Enumerate all monitors if we are in multi-monitor mode
      if Mode.Get = Multi_Monitor_Mode then return Get_Monitors; end if;
    
      -- Return only the main window
      declare
      Result : aliased RECT;
      begin
        Assert (GetWindowRect (Game, Result'Unchecked_Access));
        return (1 => To_Border (Result));
      end;
    end;

  -- Spawn auxiliary windows for other monitors
  procedure Initialize_Multi_Monitor is

    -- Monitor message loop (it acts as a slave to the main window)
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      begin
        if uMsg = WM_CLOSE then PostQuitMessage (0); return 0; end if;
        return DefWindowProcW (hwnd, uMsg, wParam, lParam);
      end;

    -- Register a new class specifically for multi-monitor windows
    Class : aliased WNDCLASSEX := (cbSize        => WNDCLASSEX'Size / Byte'Size,
                                   style         => 0,
                                   lpfnWndProc   => WindowProc'Address,
                                   cbClsExtra    => 0,
                                   cbWndExtra    => 0,
                                   hInstance     => GetModuleHandleNULL,
                                   hIconSm       => Icon,
                                   hIcon         => Icon,
                                   hbrBackground => BRUSH_GRAY,
                                   lpszMenuName  => null,
                                   lpszClassName => To_Ptr_Const_Char_16_C (MULTI_MONITOR_NAME),
                                   hCursor       => (case Cursor.Get is
                                                       when Inactive_Cursor => Cursor_Inactive,
                                                       when Active_Cursor   => Cursor_Active,
                                                       when System_Cursor   => LoadCursorW (NULL_PTR, GENERIC_CURSOR)));
    begin
      Assert (RegisterClassExW (Class'Unchecked_Access));

      -- For every monitor border create a window that fits it
      for Monitor of Get_Monitors loop
        if Monitor.Left /= 0 and Monitor.Top /= 0 then
          Multi_Monitor_Windows.Append (CreateWindowExW (dwExStyle => 0,
                                                         dwStyle      => STYLE_FULLSCREEN or STYLE_NO_ACTIVATE,
                                                         x            => Int_C (Monitor.Left),
                                                         y            => Int_C (Monitor.Top),
                                                         nWidth       => Int_C (Monitor.Right  - Monitor.Left),
                                                         nHeight      => Int_C (Monitor.Bottom - Monitor.Top),
                                                         hWndParent   => NULL_PTR,
                                                         hMenu        => 0,
                                                         hInstance    => GetModuleHandleNULL,
                                                         lpParam      => NULL_PTR,
                                                         lpClassName  => MULTI_MONITOR_NAME'Access,
                                                         lpWindowName => MULTI_MONITOR_NAME'Access));
          Assert (Multi_Monitor_Windows.Last_Element);
          Ignore (ShowWindow (Multi_Monitor_Windows.Last_Element, SW_SHOWNORMAL));
          Assert (UpdateWindow (Multi_Monitor_Windows.Last_Element));
        end if;
      end loop;

      -- Put the game into the foreground (the extra windows are now its slaves)
      Ignore (ShowWindow (Game, SW_SHOWNORMAL));
      Assert (UpdateWindow (Game));
    end;

  -- Destroy auxiliary windows for other monitors 
  procedure Finalize_Multi_Monitor is
    begin
      for Window of Multi_Monitor_Windows loop
        Ignore (ShowWindow (Window, SW_HIDE));
        Assert (DestroyWindow (Window));
      end loop;
      Assert (UnregisterClassW (MULTI_MONITOR_NAME'Access, NULL_PTR));
    end;

  -----------
  -- Input --
  -----------

  -- Map of "VKeys" to our key type: https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  VK_MAP : constant array (VK_LBUTTON..VK_OEM_CLEAR) of Key_Kind :=(
    Null_Key,           Null_Key,         Cancel_Key,           Null_Key, -- There are mouse "keys" here, but we don't care
    Null_Key,           Null_Key,         Null_Key,             Backspace_Key,
    Tab_Key,            Null_Key,         Null_Key,             Clear_Key,
    Enter_Key,          Null_Key,         Null_Key,             Shift_Key,
    Ctrl_Key,           Alt_Key,          Pause_Break_Key,      Capital_Lock_Key,
    Kana_Key,           Null_Key,         Junja_Key,            Final_Key,
    Hanja_Key,          Kanji_Key,        Escape_Key,           Convert_Key,
    No_Convert_Key,     Accept_Key,       Mode_Change_Key,      Space_Key,
    Page_Up_Key,        Page_Down_Key,    End_Key,              Home_Key,
    Left_Arrow_Key,     Up_Arrow_Key,     Right_Arrow_Key,      Down_Arrow_Key,
    Select_Key,         Print_Key,        Execute_Key,          Print_Screen_Key,
    Insert_Key,         Delete_Key,       Help_Key,             Zero_Key,
    One_Key,            Two_Key,          Three_Key,            Four_Key,
    Five_Key,           Six_Key,          Seven_Key,            Eight_Key,
    Nine_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    A_Key,              B_Key,            C_Key,                D_Key,
    E_Key,              F_Key,            G_Key,                H_Key,
    I_Key,              J_Key,            K_Key,                L_Key,
    M_Key,              N_Key,            O_Key,                P_Key,
    Q_Key,              R_Key,            S_Key,                T_Key,
    U_Key,              V_Key,            W_Key,                X_Key,
    Y_Key,              Z_Key,            Left_Windows_Key,     Right_Windows_Key,
    App_Menu_Key,       Null_Key,         System_Sleep_Key,     Pad_Zero_Key,
    Pad_One_Key,        Pad_Two_Key,      Pad_Three_Key,        Pad_Four_Key,
    Pad_Five_Key,       Pad_Six_Key,      Pad_Seven_Key,        Pad_Eight_Key,
    Pad_Nine_Key,       Pad_Star_Key,     Pad_Plus_Key,         Separator_Key,
    Pad_Dash_Key,       Pad_Period_Key,   Pad_Slash_Key,        F1_Key,
    F2_Key,             F3_Key,           F4_Key,               F5_Key,
    F6_Key,             F7_Key,           F8_Key,               F9_Key,
    F10_Key,            F11_Key,          F12_Key,              F13_Key,
    F14_Key,            F15_Key,          F16_Key,              F17_Key,
    F18_Key,            F19_Key,          F20_Key,              F21_Key,
    F22_Key,            F23_Key,          F24_Key,              Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Number_Lock_Key,
    Scroll_Lock_Key,    OEM_1_Key,        OEM_2_Key,            OEM_3_Key,
    OEM_4_Key,          OEM_5_Key,        Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Left_Shift_Key,
    Right_Shift_Key,    Left_Ctrl_Key,    Right_Ctrl_Key,       Left_Alt_Key,
    Right_Alt_Key,      Web_Backward_Key, Web_Forward_Key,      Web_Refresh_Key,
    Web_Stop_Key,       Web_Search_Key,   Web_Favorites_Key,    Web_Home_Key,
    Volume_Mute_Key,    Volume_Down_Key,  Volume_Up_Key,        Next_Track_Key,
    Previous_Track_Key, Stop_Track_Key,   Play_Pause_Track_Key, Web_Mail_Key,
    Media_Select_Key,   App_1_Key,        App_2_Key,            Null_Key,
    Null_Key,           Semicolon_Key,    Equals_Key,           Comma_Key,
    Dash_Key,           Period_Key,       Slash_Key,            Grave_Accent_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Null_Key,             Null_Key,
    Null_Key,           Null_Key,         Left_Bracket_Key,     Backslash_Key,
    Right_Bracket_Key,  Apostrophe_Key,   OEM_7_Key,            Null_Key,
    OEM_8_Key,          OEM_102_Key,      OEM_9_Key,            OEM_10_Key,
    Process_Key,        OEM_11_Key,       Null_Key,             Null_Key,
    OEM_12_Key,         OEM_13_Key,       OEM_14_Key,           OEM_15_Key,
    OEM_16_Key,         OEM_17_Key,       OEM_18_Key,           OEM_19_Key,
    OEM_20_Key,         OEM_21_Key,       OEM_22_Key,           OEM_23_Key,
    OEM_24_Key,         Attention_Key,    Clear_Selection_Key,  Exsel_Key,
    Erase_EOF_Key,      Play_Key,         Zoom_Key,             Null_Key,
    PA1_Key,            Clear_Key);

  -- Internal representation of Xvox controllers
  Gamepads : aliased array (0..3) of XINPUT_GAMEPAD;

  -- Vibrate an Xbox controller
  procedure Vibrate (Id : Int_Ptr; Hz_High, Hz_Low : Real_32_Percent) is
    Vibration : aliased XINPUT_VIBRATION := (wLeftMotorSpeed  => Int_16_Unsigned_C (Hz_Low  / 100.0 * Real (Int_16_Unsigned_C'Last)),
                                             wRightMotorSpeed => Int_16_Unsigned_C (Hz_High / 100.0 * Real (Int_16_Unsigned_C'Last)));
    begin if Id in 0..3 then Assert (XInputSetState (Int_32_Unsigned_C (Id), Vibration'Unchecked_Access) = 0); end if; end;

  -- Fetch raw cursor coordinates from the system cursor
  function Get_Cursor return Cursor_state is
    Pt : aliased POINT;
    begin
      Assert (GetCursorPos (Pt'Unchecked_Access));
      return (Int_64 (Pt.X), Int_64 (Pt.Y));
    end;

  -- Handle messages for keyboards and mice
  procedure Initialize_Input is
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      Id     :         Int_Ptr;
      Bytes  : aliased Int_32_Unsigned_C;
      Header : aliased RAWINPUTHEADER;
      begin
        case uMsg is
          when WM_CLOSE => PostQuitMessage (0); return 0;
          when WM_INPUT =>

            -- Find if the input message belongs to a mouse or keyboard
            Bytes := RAWINPUTHEADER'Object_Size / Byte'Object_Size;
            Assert (GetRawInputData (hRawInput    => To_Ptr (lParam),
                                     uiCommand    => GET_DEVICE_HEADER,
                                     pData        => Header'Address,
                                     pcbSize      => Bytes'Address,
                                     cbSizeHeader => RAWINPUTHEADER'Object_Size / Byte'Object_Size) = RAWINPUTHEADER'Object_Size / Byte'Object_Size);
            Id := To_Int_Ptr (Header.hDevice);
            if not Devices.Has (Id) then return DefWindowProcW (hwnd, uMsg, wParam, lParam); end if;
            case Header.dwType is

              -- Its a keyboard...
              when RIM_TYPEKEYBOARD =>
                Bytes := Int_32_Unsigned_C (RAWKEYBOARD'Object_Size / Byte'Object_Size);
                declare Keyboard : aliased RAWKEYBOARD; begin
                  Assert (GetRawInputData (hRawInput    => To_Ptr (lParam),
                                           uiCommand    => GET_DEVICE_DATA,
                                           pData        => Keyboard'Address,
                                           pcbSize      => Bytes'Address,
                                           cbSizeHeader => RAWINPUTHEADER'Object_Size / Byte'Object_Size) = RAWKEYBOARD'Object_Size / Byte'Object_Size);
                  if Keyboard.VKey <= VK_MAP'Last and Keyboard.VKey >= VK_MAP'First then
                    Inject_Key (Id   => Id,
                                Down => Keyboard.Message = WM_KEYDOWN or Keyboard.Message = WM_SYSKEYDOWN,
                                Key  => (case VK_MAP (Keyboard.VKey) is
                                           when Shift_Key => (if Keyboard.MakeCode = KEY_MAKE_CODE_FOR_LEFT           then Left_Shift_Key else Right_Shift_Key),
                                           when Ctrl_Key  => (if (Keyboard.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Ctrl_Key  else Right_Ctrl_Key),
                                           when Alt_Key   => (if (Keyboard.Flags and SUBEVENT_KEY_IS_RIGHT_SIDED) = 0 then Left_Alt_Key   else Right_Alt_Key),
                                           when others    => VK_MAP (Keyboard.VKey)));
                  end if;
                end;

              -- Its a mouse...
              when RIM_TYPEMOUSE =>
                Bytes := Int_32_Unsigned_C (RAWMOUSE'Object_Size / Byte'Object_Size);
                declare Mouse : aliased RAWMOUSE; begin
                  Assert (GetRawInputData (hRawInput    => To_Ptr (lParam),
                                           uiCommand    => GET_DEVICE_DATA,
                                           pData        => Mouse'Address,
                                           pcbSize      => Bytes'Address,
                                           cbSizeHeader => RAWINPUTHEADER'Object_Size / Byte'Object_Size) = RAWMOUSE'Object_Size / Byte'Object_Size);
                  if Mouse.lLastX /= 0 or Mouse.lLastY /= 0 then Inject_Cursor (Id, (Int_64 (Mouse.lLastX), Int_64 (Mouse.lLastY))); end if;
                  if    (Mouse.usButtons and RI_MOUSE_LEFT_BUTTON_DOWN)   > 0 then Inject_Button (Id, Left_Button,   True);
                  elsif (Mouse.usButtons and RI_MOUSE_LEFT_BUTTON_UP)     > 0 then Inject_Button (Id, Left_Button,   False);
                  elsif (Mouse.usButtons and RI_MOUSE_RIGHT_BUTTON_DOWN)  > 0 then Inject_Button (Id, Right_Button,  True);
                  elsif (Mouse.usButtons and RI_MOUSE_RIGHT_BUTTON_UP)    > 0 then Inject_Button (Id, Right_Button,  False);
                  elsif (Mouse.usButtons and RI_MOUSE_MIDDLE_BUTTON_DOWN) > 0 then Inject_Button (Id, Middle_Button, True);
                  elsif (Mouse.usButtons and RI_MOUSE_MIDDLE_BUTTON_UP)   > 0 then Inject_Button (Id, Middle_Button, False);
                  elsif (Mouse.usButtons and RI_MOUSE_BUTTON_4_DOWN)      > 0 then Inject_Button (Id, Aux_1_Button,  True);
                  elsif (Mouse.usButtons and RI_MOUSE_BUTTON_4_UP)        > 0 then Inject_Button (Id, Aux_1_Button,  False);
                  elsif (Mouse.usButtons and RI_MOUSE_BUTTON_5_DOWN)      > 0 then Inject_Button (Id, Aux_2_Button,  True);
                  elsif (Mouse.usButtons and RI_MOUSE_BUTTON_5_UP)        > 0 then Inject_Button (Id, Aux_2_Button,  False);
                  elsif (Mouse.usButtons and RI_MOUSE_VERTICAL_WHEEL) > 0 or (Mouse.usButtons and RI_MOUSE_HORIZONTAL_WHEEL) > 0 then
                    Inject_Button (Id     => Id,
                                   Down   => True,
                                   Button => (if To_Int_16_Signed (Int_16_Unsigned (Shift_Right (Int_64_Unsigned (Mouse.usButtons) and 16#0000_0000_FFFF_0000#, 16))) / MOUSE_WHEEL_DELTA < 0
                                              then (if (Mouse.usButtons and RI_MOUSE_HORIZONTAL_WHEEL) > 0 then Wheel_Left_Button  else Wheel_Down_Button)
                                              else (if (Mouse.usButtons and RI_MOUSE_HORIZONTAL_WHEEL) > 0 then Wheel_Right_Button else Wheel_Up_Button)));
                  end if;
                end;
            when others => null; end case;
        when others => null; end case;
        return DefWindowProcW (hwnd, uMsg, wParam, lParam);
      end;

    -- Register the input class and create the hidden input window 
    Class : aliased WNDCLASSEX := (cbSize        => WNDCLASSEX'Size / Byte'Size,
                                   style         => 0,
                                   lpfnWndProc   => WindowProc'Address,
                                   cbClsExtra    => 0,
                                   cbWndExtra    => 0,
                                   hInstance     => GetModuleHandleNULL,
                                   hIconSm       => LoadIconW (GetModuleHandleNULL, GENERIC_ICON),
                                   hIcon         => LoadIconW (GetModuleHandleNULL, GENERIC_ICON),
                                   hCursor       => LoadCursorW (NULL_PTR, GENERIC_CURSOR),
                                   hbrBackground => BRUSH_GRAY,
                                   lpszMenuName  => null,
                                   lpszClassName => To_Ptr_Const_Char_16_C (INPUT_NAME));
    begin
      Assert (RegisterClassExW (Class'Unchecked_Access));
      Input := CreateWindowExW (dwExStyle    => 0,
                                lpClassName  => INPUT_NAME'Access,
                                lpWindowName => INPUT_NAME'Access,
                                dwStyle      => STYLE_NO_ACTIVATE,
                                x            => 0,
                                y            => 0,
                                nWidth       => 0,
                                nHeight      => 0,
                                hWndParent   => NULL_PTR,
                                hMenu        => 0,
                                hInstance    => GetModuleHandleNULL,
                                lpParam      => NULL_PTR);
      Assert (Input);
      declare
      type Array_RAWINPUTDEVICE is array (Positive range <>) of RAWINPUTDEVICE;
      Setups : aliased Array_RAWINPUTDEVICE := ((GENERIC_DESKTOP, USE_RAW_KEYBOARD, RIDEV_INPUTSINK, Input),
                                                (GENERIC_DESKTOP, USE_RAW_MOUSE,    RIDEV_INPUTSINK, Input));
      begin
        Assert (RegisterRawInputDevices (Setups'Address, Setups'Length, RAWINPUTDEVICE'Object_Size / Byte'Object_Size));
      end;
    end;

  -- Unregister devices by overwriting and kill the input window
  procedure Finalize_Input is
    Null_Setup : aliased RAWINPUTDEVICE := (GENERIC_DESKTOP, USE_RAW_MOUSE, STOP_READING_TOP_LEVEL_DEVICES, NULL_PTR);
    begin
      Ignore (RegisterRawInputDevices (Null_Setup'Address, Null_Setup'Size / RAWINPUTDEVICE'Size, RAWINPUTDEVICE'Size / Byte'Size));
      if Input /= NULL_PTR then Ignore (DestroyWindow (Input)); end if;
      Input := NULL_PTR;
    end;

  -- Pump messages, check device connectivity, and poll devices that don't send messages
  function Update_Input return Bool is
    Device_Count   : aliased Int_32_Unsigned_C;
    State          : aliased XINPUT_STATE;
    Message        : aliased MSG;
    Has_Gamepad    : Array_Bool (1..4);
    Current_Device : Ordered_Device.Cursor;

    -- Perform range or boolean checks for devices then inject results
    procedure Unpack_Button (Player : Int; Raw : Int_16_Unsigned_C; Button : Gamepad_Kind) is
      begin
        if (State.Gamepad.wButtons and Raw) /= (Gamepads (Player).wButtons and Raw) then
          if (State.Gamepad.wButtons and Raw) > 0 then Inject_Button (Int_Ptr (Player), Button, True);
          else Inject_Button (Int_Ptr (Player), Button, False); end if;
        end if;
      end;
    procedure Unpack_Stick (Player : Int; Stick : Stick_Kind; X, Y : Int_16_Signed_C) is
      begin
        Inject_Stick (Int_Ptr (Player), Stick, ((if X > 0 then Real_32_Range (Real (X) / Real (Int_16_Signed_C'Last))
                                                 else          Real_32_Range (Real (X) / Real (Int_16_Signed_C'First))),
                                                (if Y > 0 then Real_32_Range (Real (Y) / Real (Int_16_Signed_C'Last))
                                                 else          Real_32_Range (Real (Y) / Real (Int_16_Signed_C'First)))));
      end;
    begin

      -- Check if an input device has been added or removed... This needs more comments !!!
      Assert (GetRawInputDeviceList (NULL_PTR, Device_Count'Unchecked_Access, RAWINPUTDEVICELIST'Object_Size / Byte'Object_Size) /= -1);
      Assert (Device_Count);
      declare
      List : aliased array (1..Int (Device_Count)) of RAWINPUTDEVICELIST;
      begin
        Assert (GetRawInputDeviceList (List'Address, Device_Count'Unchecked_Access, RAWINPUTDEVICELIST'Object_Size / Byte'Object_Size) /= -1);
        for I in List'Range loop
          if not Has_Device (To_Int_Ptr (List (I).hDevice)) then
            case List (I).dwType is
              when RIM_TYPEKEYBOARD => Add_Device (To_Int_Ptr (List (I).hDevice), (Keyboard_Device, others => <>));
              when RIM_TYPEMOUSE    => Add_Device (To_Int_Ptr (List (I).hDevice), (Mouse_Device,    others => <>));
            when others => null; end case;
          end if;
        end loop;
        Current_Device := Devices.First;
        while Devices.Has (Current_Device) loop
          if Devices.Key (Current_Device) in 0..3 then
            Has_Gamepad (Int (Devices.Key (Current_Device)) + 1) := True;
            if XInputGetState (Int_32_Unsigned_C (Devices.Key (Current_Device)), State'Unchecked_Access) /= 0 then Devices.Delete (Current_Device); end if;
          else
            for J in List'Range loop
              exit when Devices.Key (Current_Device) = To_Int_Ptr (List (J).hDevice);
              if J = List'Last then Devices.Delete (Current_Device); end if;
            end loop;
          end if;
          Devices.Next (Current_Device);
        end loop;
        for I in Gamepads'Range loop
          if not Has_Gamepad (I + 1) and then XInputGetState (Int_32_Unsigned_C (I), State'Unchecked_Access) = 0 then Add_Device (Int_Ptr (I), (Gamepad_Device, others => <>)); end if;
        end loop;
      end;

      -- Pump the message loop
      while PeekMessageW (Message'Unchecked_Access, Input, 0, 0, PM_REMOVE) /= 0 loop
        if Message.message = WM_QUIT then return False; end if;
        Ignore (TranslateMessage (Message'Unchecked_Access));
        Ignore (DispatchMessageW (Message'Unchecked_Access));
      end loop;

      -- Inject Xbox controller input
      begin for I in Gamepads'Range loop
        if XInputGetState (Int_32_Unsigned_C (I), State'Unchecked_Access) = 0 and then Gamepads (I) /= State.Gamepad then
          Unpack_Button (I, XINPUT_GAMEPAD_A,              A_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_B,              B_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_X,              X_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_Y,              Y_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_BACK,           Back_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_START,          Start_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_LEFT_THUMB,     Left_Stick_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_RIGHT_THUMB,    Right_Stick_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_LEFT_SHOULDER,  Left_Bumper_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_RIGHT_SHOULDER, Right_Bumper_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_DPAD_UP,        DPad_Up_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_DPAD_DOWN,      DPad_Down_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_DPAD_LEFT,      DPad_Left_Button);
          Unpack_Button (I, XINPUT_GAMEPAD_DPAD_RIGHT,     DPad_Right_Button);

          -- Convert ranges
          if State.Gamepad.sThumbLX /= Gamepads (I).sThumbLX or State.Gamepad.sThumbLY /= Gamepads (I).sThumbLY then Unpack_Stick (I, Left_Stick,  State.Gamepad.sThumbLX, State.Gamepad.sThumbLY); end if;
          if State.Gamepad.sThumbRX /= Gamepads (I).sThumbRX or State.Gamepad.sThumbRY /= Gamepads (I).sThumbRY then Unpack_Stick (I, Right_Stick, State.Gamepad.sThumbRX, State.Gamepad.sThumbRY); end if;
          if State.Gamepad.bLeftTrigger  /= Gamepads (I).bLeftTrigger  then Inject_Trigger (Int_Ptr (I), Left_Trigger,  Real (State.Gamepad.bLeftTrigger)  / Real (Int_8_Unsigned_C'Last) * 100.0); end if;
          if State.Gamepad.bRightTrigger /= Gamepads (I).bRightTrigger then Inject_Trigger (Int_Ptr (I), Right_Trigger, Real (State.Gamepad.bRightTrigger) / Real (Int_8_Unsigned_C'Last) * 100.0); end if;
          Gamepads (I) := State.Gamepad;
        end if;
      end loop; exception when others => null; end; -- Random crashes ???
      return True;
    end;

  -------------
  -- Console --
  -------------

  -- Why doesn't SelectObject work ???
  procedure Run_Console is 

    -- Console stuff
    type Button_State is record
        Message : Str (1..4);
        Action  : access procedure;
      end record;
    CONSOLE_BUTTONS : constant array (1..3) of Button_State := (("Save", Save_Log'Access), ("Send", Send_Log'Access), ("Quit", null));
    Current_Input : Char_16        := NULL_CHAR_16;
    Current_Log   : Str_16_Unbound := NULL_STR_16_UNBOUND;
    Current_Lines : Int_64_Natural := 0;
    Buttons       : array (CONSOLE_BUTTONS'range) of Ptr;

    -- Constants
    NUMBER_OF_OUTPUT_ROWS : constant Int_32_Unsigned_C := 25;
    SCROLL_FACTOR         : constant Int_64_Natural    := 500;
    DO_DISABLE_RESIZE     : constant Bool  := False;
    GROUP_BOX_SIDE_MARGIN : constant Real  := 1.2;
    FONT_GROUP_BOX_SIZE   : constant Real  := 1.2;
    FONT_CONSOLE_SIZE     : constant Int_C := -11;
    PIXELS_PER_INCH       : constant Int_C := 72;
    BUTTON_WIDTH_DLU      : constant Int_C := 50;
    BUTTON_HEIGHT_DLU     : constant Int_C := 14;
    MARGIN_BUTTON         : constant Int_C := 4;
    MARGIN                : constant Int_C := 7;
    IDENTIFIER_START      : constant Int   := 666;

    -- Text
    ERROR_REPORTING_URL : constant Str  := "http://www.google.com";
    LABEL_INPUT_ENTRY   : constant Str  := "Input";
    LABEL_OUTPUT        : constant Str  := "Output";
    FONT_CONSOLE        : aliased Str_C := To_Str_C ("Courier New");
    FONT_DIALOG         : aliased Str_C := To_Str_C ("Tahoma");
    NAME_BUTTON         : aliased Str_C := To_Str_C ("Button");
    NAME_GROUP          : aliased Str_C := To_Str_C ("Group");
    NAME_EDIT           : aliased Str_C := To_Str_C ("Edit");

    -- Variables
    Message     : aliased MSG := (others => <>);
    Buffer      : aliased Str_C (1..99999);
    Metrics     : aliased NONCLIENTMETRICS;
    Class       : aliased WNDCLASSEX;
    Text_Metric : aliased TEXTMETRIC;
    Rectangle   : aliased RECT;

    -- Window, font, and icon pointers
    Output_Group_Box, Input_Group_Box,
    Font_Text_Box, Font_Buttons,
    Output_Box, Input_Box,
    Edit_Background,
    Context,
    Console,
    Icon : Ptr;

    -- Sizing variables
    Y,
    Message_Box_Font_Height,
    Dialog_Base_Unit_Height, Dialog_Base_Unit_Width,
    Output_Box_Height, Output_Box_Width, Input_Box_Height,
    Start_Selection, End_Selection,
    Margin_Group_Top, Margin_Group,
    Current_Height, Current_Width,
    Console_Height, Console_Width,
    Button_Height, Button_Width,
    Right_Count, Left_Count,
    Number_Of_Lines,
    Text_Box_Font_Height, Text_Box_Font_Width,
    Current_Line : Int_C;

    -- Flags
    Is_At_Bottom,
    Is_First_Time : Bool := True;
    Was_At_Bottom,
    Was_At_Minimum_Width,
    Was_At_Minimum_Height,
    Button_Font_Set,
    Do_Process_Character,
    Do_Skip_Message : Bool := False;

    -- Message procedures for convience
    procedure Set_Text (Handle : Ptr; Text : Str) is begin Assert (SendMessageW (Handle, WM_SETTEXT, 0, To_Int_Ptr (To_Str_C (Text)'Address))); end;
    procedure Set_Font (Handle, Font : Ptr)       is begin Ignore (SendMessageW (Handle, WM_SETFONT, 0, To_Int_Ptr (Font))); end;

    -- Return true if the output text window's scrollbar is at the bottom
    function Scrollbar_At_Bottom return Bool is
      Scroll_Info : aliased SCROLLINFO;
      begin
        Assert (GetScrollInfo (Output_Box, 1, Scroll_Info'Unchecked_Access));
        return Scroll_Info.nPos + Number_of_Lines >= Scroll_Info.nMax;
      end;

    -- A single function for setting all GUI sizing globals
    procedure Set_Sizes is
      Minimum_Width : Int_C;
      Box_Padding   : Int_C;
      Border_Height : Int_C := GetSystemMetrics (DATA_BORDER_HEIGHT);
      Border_Width  : Int_C := GetSystemMetrics (DATA_BORDER_WIDTH);
      begin
        Margin_Group      := Int_C (Float (Text_Metric.tmHeight) / GROUP_BOX_SIDE_MARGIN);
        Margin_Group_Top  := Int_C (Float (Text_Metric.tmHeight) * FONT_GROUP_BOX_SIZE);
        Box_Padding       := Int_C (Float (Text_Box_Font_Width) / 1.5);
        Output_Box_Width  := 2 * Box_Padding + (Text_Box_Font_Width  * Int_C (Line_Size)) + GetSystemMetrics (SM_CYVTHUMB);
        Output_Box_Height := 2 * Box_Padding + (Text_Box_Font_Height * Int_C (NUMBER_OF_OUTPUT_ROWS));
        Input_Box_Height  := 2 * Box_Padding + Text_Box_Font_Height;
        Console_Width     := (MARGIN * Dialog_Base_Unit_Width + Margin_Group + Border_Width) * 2 + Output_Box_Width;
        Console_Height    := (MARGIN * Dialog_Base_Unit_Height) * 4 + (Border_Height + Margin_Group + Margin_Group_Top) * 2
                             + Output_Box_Height + Input_Box_Height + BUTTON_HEIGHT + GetSystemMetrics (DATA_TITLE_BAR_HEIGHT);
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
      end;

    -- Create and set fonts for the output box
    procedure Create_Fonts is 
      Font_Size : aliased SIZE;
      Some_BS   : aliased Str_C := To_Str_C ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
      begin
        if not Button_Font_Set then
          Font_Buttons := CreateFontW (nHeight            => Message_Box_Font_Height,
                                       nWidth             => 0,
                                       nEscapement        => 0,
                                       nOrientation       => 0,
                                       fnWeight           => FONT_WEIGHT_LIGHT,
                                       fdwItalic          => 0,
                                       fdwUnderline       => 0,
                                       fdwStrikeOut       => 0,
                                       fdwCharSet         => DEFAULT_CHARACTER_SET,
                                       fdwOutputPrecision => FONT_OUT_DEFAULT_PRECISION,
                                       fdwClipPrecision   => FONT_CLIP_DEFAULT_PRECISION,
                                       fdwQuality         => FONT_DEFAULT_QUALITY,
                                       fdwPitchAndFamily  => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
                                       lpszFace           => FONT_DIALOG'Unchecked_Access);
          Assert (Font_Buttons);
        end if;
        Font_Text_Box := CreateFontW (nHeight            => FONT_CONSOLE_SIZE,
                                      nWidth             => 0,
                                      nEscapement        => 0,
                                      nOrientation       => 0,
                                      fnWeight           => FONT_WEIGHT_LIGHT,
                                      fdwItalic          => 0,
                                      fdwUnderline       => 0,
                                      fdwStrikeOut       => 0,
                                      fdwCharSet         => DEFAULT_CHARACTER_SET,
                                      fdwOutputPrecision => FONT_OUT_DEFAULT_PRECISION,
                                      fdwClipPrecision   => FONT_CLIP_DEFAULT_PRECISION,
                                      fdwQuality         => FONT_DEFAULT_QUALITY,
                                      fdwPitchAndFamily  => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
                                      lpszFace           => FONT_CONSOLE'Unchecked_Access);
        Assert (Font_Text_Box);
        --Ignore (SelectObject (Context, Font_Text_Box)); -- ???
        Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
        Text_Box_Font_Height := Text_Metric.tmHeight;
        Assert (GetTextExtentPoint32W (hdc      => Context,
                                       lpString => Some_BS'Unchecked_Access,
                                       c        => 52,
                                       lpSize   => Font_Size'Unchecked_Access));
        Text_Box_Font_Width := Int_C ((Font_Size.cx / 26 + 1) / 2);
        --Ignore (SelectObject (Context, Font_Buttons)); -- ???
        Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
      end;

    -- Set the global variable Dialog_Base_Unit_Width used in calculating all UI proportions by spawning a message box and measuring it
    procedure Get_Sizes_From_Message_Box is
      Hook : Ptr;

      -- Procedure to wait for the callback-callback (below) to set Dialog_Base_Unit_Width
      procedure Wait_For_Set_With_Infinite_Loop is
        begin
          for I in Int'range loop -- The Windows API made me do it
            exit when Dialog_Base_Unit_Width /= 0;
            delay 0.0001;
          end loop;
          if Dialog_Base_Unit_Width = 0 then raise Program_Error; end if;
        end;

      -- Callback to acquire message box
      function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
      function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr is
        Class_Name  : aliased Str_C (1..256);
        Window_Text : aliased Str_C (1..256);
        Window      :         Ptr := To_Ptr (Int_Ptr (wParam));

        -- Once we find the message box we have to look through its children to find a button
        function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C with Convention => Stdcall;
        function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C is
          Rectangle : aliased RECT;
          begin

            -- Find a message box button to calculate dialog base units
            Assert (GetClassNameW (hwnd, Class_Name'Unrestricted_Access, Class_Name'Length));
            Ignore (GetWIndowTextW (hwnd, Window_Text'Unrestricted_Access, Window_Text'Length));
            if Dialog_Base_Unit_Width = 0 and "Button" = To_Str (Class_Name) then

              -- Do the Microsoft recommended math and measurements: https://support.microsoft.com/en-us/kb/125681
              Assert (GetWindowRect (hwnd, Rectangle'Unchecked_Access));
              Button_Width            := (Rectangle.right  - Rectangle.left);
              Button_Height           := (Rectangle.bottom - Rectangle.top);
              --Ignore (SelectObject (Context, To_Ptr (Int_Ptr (To_Int_32_Unsigned (SendMessageW (hwnd, MESSAGE_GET_FONT, 0, 0)))))); -- ???
              Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
              Message_Box_Font_Height := Text_Metric.tmHeight;
              Dialog_Base_Unit_Width  := Button_Width  / BUTTON_WIDTH_DLU;
              Dialog_Base_Unit_Height := Button_Height / BUTTON_HEIGHT_DLU;
            end if;
            return 1;
          end;
        begin

          -- Find the newly created message-box window 
          Assert (GetClassNameW (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
          Ignore (GetWIndowTextW (Window, Window_Text'Unrestricted_Access, Window_Text'Length));
          if nCode = HCBT_ACTIVATE and To_Str (Class_Name) = CLASS_NAME_DIALOG and To_Str (Window_Text) = To_Str (CONSOLE_NAME) then

            -- Get the dialog base units then close the message box
            Ignore (EnumChildWindows (Window, EnumWindowsProc'Address, 0));
            Wait_For_Set_With_Infinite_Loop;
            Assert (DestroyWindow (Window));
            Assert (UnhookWindowsHookEx (Hook));

          -- Not the message box, continue the hook
          else Ignore (CallNextHookEx (Hook, nCode, wParam, lParam)); end if;
          return 0;
        end;
      begin

        -- Create a message box and a callback that checks newly created windows, then wait for Dialog_Base_Unit_Width to be set
        Hook := SetWindowsHookExW (WH_CBT, CBTProc'Address, NULL_PTR, GetCurrentThreadId);
        Ignore (MessageBoxW (NULL_PTR, GAME_NAME'Access, CONSOLE_NAME'Unchecked_Access, 0));
        Wait_For_Set_With_Infinite_Loop;
      end;

    -- Console window callback
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is

      -- Conversion function
      function To_Windows_Color (Color : Color_State) return Int_32_Unsigned_C is (Int_32_Unsigned_C (Shift_Left (Int_32_Unsigned (Color.Green), 8) or
                                                                                                      Shift_Left (Int_32_Unsigned (Color.Blue), 16) or
                                                                                                                  Int_32_Unsigned (Color.Red)));
      begin
        case uMsg is
          when WM_CLOSE => PostQuitMessage (0); return 1;
          when WM_SYSCOMMAND => if wParam = Int_Ptr (SC_KEYMENU) or wParam = Int_Ptr (SC_SCREENSAVE) then return 1; end if;
          when WM_CREATE => Edit_Background := CreateSolidBrush (To_Windows_Color (COLOR_BACKGROUND)); Assert (Edit_Background);

          -- Set colors
          when WM_CTLCOLORSTATIC | WM_CTLCOLOREDIT =>
            if To_Ptr (lParam) = Output_Box or To_Ptr (lParam) = Input_Box then
              Assert (SetBkColor   (To_Ptr (Int_Ptr (wParam)), To_Windows_Color (COLOR_BACKGROUND)) /= CLR_INVALID);
              Assert (SetTextColor (To_Ptr (Int_Ptr (wParam)), To_Windows_Color (COLOR_FOREGROUND)) /= CLR_INVALID);
              return To_Int_Ptr (Edit_Background);
            end if;

          -- Check for button presses
          when WM_COMMAND =>
            for I in CONSOLE_BUTTONS'range loop
              if wParam = Int_Ptr (I + IDENTIFIER_START) then
                if CONSOLE_BUTTONS (I).Action = null then PostQuitMessage (0);
                else CONSOLE_BUTTONS (I).Action.All; end if;
                return 1;
              end if;
            end loop;

          -- Restrict resizing
          when WM_GETMINMAXINFO =>
            declare
            Min_Max : Ptr_MINMAXINFO := To_Ptr_MINMAXINFO (lParam);
            begin
              Min_Max.ptMinTrackSize.x := Console_Width;
              Min_Max.ptMinTrackSize.y := Console_Height;
              return 1;
            end;

          -- Resize main and child windows
          when WM_SIZE =>
            if wParam /= Int_Ptr (SIZE_MINIMIZED) then

              -- Comment here !!!
              Ignore (SetWindowLongW (Console, SET_WINDOW_STYLE_EXTRA, WS_EX_COMPOSITED));
              Assert (GetWindowRect (Console, Rectangle'Unchecked_Access));
              Was_At_Minimum_Height := False;
              Was_At_Minimum_Width  := False;
              Is_At_Bottom          := Scrollbar_At_Bottom;
              Current_Width         := Rectangle.Right  - Rectangle.Left;
              Current_Height        := Rectangle.Bottom - Rectangle.Top;
              Set_Sizes;
              Assert (SetWindowPos (hWnd            => Console,
                                    hWndInsertAfter => NULL_PTR,
                                    X               => Rectangle.Left,
                                    Y               => Rectangle.Top,
                                    cx              => Current_Width,
                                    cy              => Current_Height,
                                    uFlags          => 0));
              Y := Dialog_Base_Unit_Width * MARGIN;
              Assert (SetWindowPos (hWnd            => Output_Group_Box,
                                    hWndInsertAfter => NULL_PTR,
                                    X               => Dialog_Base_Unit_Width * MARGIN,
                                    Y               => Y,
                                    cx              => Output_Box_Width + Margin_Group * 2,
                                    cy              => Output_Box_Height + Margin_Group_Top + Margin_Group,
                                    uFlags          => 0));
              Y := Y + Margin_Group_Top;
              Assert (SetWindowPos (hWnd            => Output_Box,
                                    hWndInsertAfter => NULL_PTR,
                                    X               => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                                    Y               => Y,
                                    cx              => Output_Box_Width,
                                    cy              => Output_Box_Height,
                                    uFlags          => 0));
              Y := Y + Output_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;
              Assert (SetWindowPos (hWnd            => Input_Group_Box,
                                    hWndInsertAfter => NULL_PTR,
                                    X               => Dialog_Base_Unit_Width * MARGIN,
                                    Y               => Y,
                                    cy              => Input_Box_Height + Margin_Group_Top + Margin_Group,
                                    cx              => Output_Box_Width + Margin_Group * 2,
                                    uFlags          => 0));
              Y := Y + Margin_Group_Top;
              Assert (SetWindowPos (hWnd            => Input_Box,
                                    hWndInsertAfter => NULL_PTR,
                                    X               => Margin_Group + Dialog_Base_Unit_Width * MARGIN,
                                    Y               => Y,
                                    cx              => Output_Box_Width,
                                    cy              => Input_Box_Height,
                                    uFlags          => 0));
              Y := Y + Input_Box_Height + Dialog_Base_Unit_Width * MARGIN + Margin_Group;

              -- Button repositioning
              Right_Count := 0;
              Left_Count  := 0;
              for I in Buttons'range loop
                Assert (SetWindowPos (hWnd            => Buttons (I),
                                      hWndInsertAfter => NULL_PTR,
                                      cx              => Button_Width,
                                      cy              => Button_Height,
                                      uFlags          => 0,
                                      Y               => Y,
                                      X               => (if CONSOLE_BUTTONS (I).Action /= null then -- Left justify
                                                            Dialog_Base_Unit_Width * (MARGIN + Left_Count * MARGIN_BUTTON) + Left_Count * BUTTON_WIDTH
                                                          else -- Right justify
                                                            Output_Box_Width + Margin_Group * 2 + Dialog_Base_Unit_Width * MARGIN - (Right_Count + 1) * BUTTON_WIDTH
                                                            - Dialog_Base_Unit_Width * Right_Count * MARGIN_BUTTON)));
                if CONSOLE_BUTTONS (I).Action = null then Right_Count := Right_Count + 1;
                else Left_Count := Left_Count + 1; end if;
              end loop;

              -- Keep the scrollbar at the bottom if it was before resizing
              if Is_At_Bottom then Ignore (SendMessageW (Output_Box, WM_VSCROLL, SUBEVENT_SCROLL_BOTTOM, 0)); end if;
              Assert (RedrawWindow (hwnd, null, NULL_PTR, 1));
              Ignore (SetWindowLongW (Console, SET_WINDOW_STYLE_EXTRA, 0));
              return 1;
            end if;
        when others => null; end case;
        return DefWindowProcW (hwnd, uMsg, wParam, lParam);
      end;
    begin 

      -- Load the context which is used globally
      Context := GetDC (GetDesktopWindow);
      Assert (Context);

      -- Load the icon
      Icon := LoadImageW (hinst     => GetModuleHandleNULL, -- Loads the icon nicely for the Aero theme, but on "classic" theme it looks pixelated on the title bar
                          lpszName  => WIN32_PATH_ICON'Access,
                          uType     => LOAD_ICO,
                          cxDesired => 0,
                          cyDesired => 0,
                          fuLoad    => LOAD_FROM_FILE or DEFAULT_ICON_SIZE);
      if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, GENERIC_ICON); end if;

      -- Set global font and size variables
      if SystemParametersInfoW (SPI_GETNONCLIENTMETRICS, 0, Metrics'Address, Metrics.cbSize) = 0 then Button_Font_Set := False; -- Fails under XP....
      else Font_Buttons := CreateFontIndirectW (Metrics.lfMessageFont'Unchecked_Access); Assert (Font_Buttons); end if;
      Get_Sizes_From_Message_Box;
      Create_Fonts;      
      Set_Sizes;

      -- Create the main console window
      Class := (lpfnWndProc   => WindowProc'Address,
                hInstance     => GetModuleHandleNULL,
                hIconSm       => Icon,
                hIcon         => Icon,
                hCursor       => LoadCursorW (NULL_PTR, GENERIC_CURSOR),
                hbrBackground => BRUSH_WINDOW,
                lpszClassName => To_Ptr_Const_Char_16_C (CONSOLE_NAME),
                others        => <>);
      Assert (RegisterClassExW (Class'Unchecked_Access));
      Console := CreateWindowExW (lpClassName  => CONSOLE_NAME'Unchecked_Access,
                                  lpWindowName => CONSOLE_NAME'Unchecked_Access,
                                  x            => GetDeviceCaps (Context, DATA_HORIZONTAL_RESOLUTION) / 2 - Console_Width  / 2,
                                  y            => GetDeviceCaps (Context, DATA_VERTICAL_RESOLUTION)   / 2 - Console_Height / 2,
                                  nWidth       => Console_Width,
                                  nHeight      => Console_Height,
                                  hWndParent   => NULL_PTR,
                                  hMenu        => 0,
                                  hInstance    => GetModuleHandleNULL,
                                  lpParam      => NULL_PTR,
                                  dwExStyle    => 0,
                                  dwStyle      =>
                                    STYLE_ICONIC_INITIALLY or
                                    STYLE_BORDER_SIZABLE   or
                                    WS_SYSMENU    or
                                    WS_BORDER or
                                    WS_MAXIMIZEBOX      or
                                    STYLE_BOX_FULLSCREEN);
      Assert (Console);

      -- Create the output box border
      Output_Group_Box := CreateWindowExW (lpClassName  => NAME_BUTTON'Unchecked_Access,
                                           lpWindowName => null,
                                           x            => 0,
                                           y            => 0,
                                           nWidth       => 0,
                                           nHeight      => 0,
                                           hWndParent   => Console,
                                           hMenu        => 0,
                                           hInstance    => GetModuleHandleNULL,
                                           lpParam      => NULL_PTR,
                                           dwExStyle    => 0,
                                           dwStyle      => STYLE_VISIBLE_INITIALLY or BS_GROUPBOX or STYLE_CHILD);
      Assert (Output_Group_Box);
      Set_Font (Output_Group_Box, Font_Buttons);
      Set_Text (Output_Group_Box, Localize (LABEL_OUTPUT));

      -- Create the output box
      Output_Box := CreateWindowExW (lpClassName  => NAME_EDIT'Unchecked_Access,
                                     lpWindowName => null,
                                     x            => 0,
                                     y            => 0,
                                     nWidth       => 0,
                                     nHeight      => 0,
                                     hWndParent   => Console,
                                     hMenu        => 0,
                                     hInstance    => GetModuleHandleNULL,
                                     lpParam      => NULL_PTR,
                                     dwExStyle    => 0,
                                     dwStyle      =>
                                       WS_VSCROLL   or
                                       STYLE_VISIBLE_INITIALLY         or
                                       WS_BORDER          or
                                       STYLE_ALIGN_TEXT_TO_LEFT        or
                                       STYLE_MULTI_LINE                or
                                       STYLE_NO_USER_EDITING           or
                                       STYLE_CHILD);
      Assert (Output_Box);
      Set_Font (Output_Box, Font_Text_Box);

      -- Create the input box border
      Input_Group_Box := CreateWindowExW (lpClassName  => NAME_BUTTON'Unchecked_Access,
                                          lpWindowName => null,
                                          x            => 0,
                                          y            => 0,
                                          nHeight      => 0,
                                          nWidth       => 0,
                                          hWndParent   => Console,
                                          hMenu        => 0,
                                          hInstance    => GetModuleHandleNULL,
                                          lpParam      => NULL_PTR,
                                          dwExStyle    => 0,
                                          dwStyle      => STYLE_VISIBLE_INITIALLY or BS_GROUPBOX or STYLE_CHILD);
      Assert (Input_Group_Box);
      Set_Font (Input_Group_Box, Font_Buttons);
      Set_Text (Input_Group_Box, LABEL_INPUT_ENTRY);

      -- Create the input box
      Input_Box := CreateWindowExW (lpClassName  => NAME_EDIT'Unchecked_Access,
                                    lpWindowName => null,
                                    x            => 0,
                                    y            => 0,
                                    nWidth       => 0,
                                    nHeight      => 0,
                                    hWndParent   => Console,
                                    hMenu        => 0,
                                    hInstance    => GetModuleHandleNULL,
                                    lpParam      => NULL_PTR,
                                    dwExStyle    => 0,
                                    dwStyle      =>
                                      STYLE_VISIBLE_INITIALLY  or
                                      WS_BORDER   or
                                      STYLE_ALIGN_TEXT_TO_LEFT or
                                      STYLE_MULTI_LINE         or
                                      STYLE_CHILD);
      Assert (Input_Box);
      Set_Font (Input_Box, Font_Text_Box);

      -- Create the buttons
      for I in Buttons'range loop
        Buttons (I) := CreateWindowExW (lpClassName  => NAME_BUTTON'Unchecked_Access,
                                        lpWindowName => null,
                                        nWidth       => 0,
                                        nHeight      => 0,
                                        hWndParent   => Console,
                                        hMenu        => Int_Ptr (I + IDENTIFIER_START),
                                        hInstance    => GetModuleHandleNULL,
                                        lpParam      => NULL_PTR,
                                        dwExStyle    => 0,
                                        dwStyle      => STYLE_PUSH_BUTTON or STYLE_VISIBLE_INITIALLY or STYLE_CHILD,
                                        y            => 0,
                                        x            => 0);
        Assert (Buttons (I));
        Set_Font (Buttons (I), Font_Buttons);
        Set_Text (Buttons (I), Localize (CONSOLE_BUTTONS (I).Message));
      end loop;

      -- Force the console window into the foreground
      Ignore (ShowWindow (Console, SW_SHOWMINIMIZED)); -- "The fact that this works is a major windows bug, good find!"
      Ignore (ShowWindow (Console, SW_RESTORE));
      Assert (SetFocus (Output_Box));

      -- Main loop
      while Message.message /= WM_QUIT loop

        -- Handle new output
        if Length (Current_Log) /= Log'Length then

          -- Set the text in the output box
          Is_At_Bottom := Scrollbar_At_Bottom;
          Current_Log  := To_Str_16_Unbound (Log);
          Current_Line := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
          Ignore (SendMessageW (Output_Box, EM_GETSEL, To_Int_Ptr (Start_Selection'Address), To_Int_Ptr (End_Selection'Address)));
          Ignore (SendMessageW (Output_Box, WM_SETREDRAW, 0, 0));
          Set_Text (Output_Box, Log);
          Ignore (SendMessageW (Output_Box, EM_SETSEL, Int_Ptr (Start_Selection), Int_Ptr (End_Selection)));

          -- If the scroll bar was at the bottom, make sure it stays there            
          if Is_At_Bottom or Is_First_Time then
            Is_First_Time := False;
            Ignore (SendMessageW (Output_Box, WM_VSCROLL, SUBEVENT_SCROLL_BOTTOM, 0));
            Current_Line := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
          else
            for I in 1..Current_Line loop Ignore (SendMessageW (Output_Box, WM_VSCROLL, SUBEVENT_SCROLL_DOWN_LINE, 0)); end loop;
          end if;
          Current_Lines := Lines;
          Assert (SendMessageW (Output_Box, WM_SETREDRAW, 1, 0));
        end if;

        -- Pump the message loop
        if PeekMessageW (Message'Unchecked_Access, Console, 0, 0, PM_REMOVE) /= 0 then
          case Message.message is

            -- Scale mouse scrolling
            when WM_MOUSEWHEEL =>
              if GetFocus /= Output_Box then
                for I in 1..Current_Lines / SCROLL_FACTOR loop 
                   Ignore (SendMessageW (Output_Box,
                                         WM_VSCROLL, 

                                         -- Figure out the direction of the scroll
                                         (if To_Int_16_Signed (Int_16_Unsigned (Shift_Right (Int_64_Unsigned (Message.wParam)
                                          and 16#0000_0000_FFFF_0000#, 16))) / MOUSE_WHEEL_DELTA < 0 then 1 else 0), 0));
                end loop;
              end if;

            -- Capture paste events (Ctrl + V) into the input box and change focus
            when WM_KEYDOWN =>
              Do_Process_Character := True;
              if Int_16_Unsigned_C (Message.wParam) = VK_V_KEY
                and then (GetKeyState (Int_C (VK_CONTROL)) and 16#8000#) > 0 -- Is Ctrl pressed?
                and then GetFocus /= Input_Box
              then
                Input_Entry (Input_Entry & Paste);
                Set_Text (Input_Box, Input_Entry);
              end if;

            -- Put the character in the right spot
            when WM_CHAR =>
              if Do_Process_Character then
                Do_Process_Character := False;
                Current_Input := Char_16'Val (Int (Message.wParam));
                if not Is_Control (Current_Input) then
                  Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Buffer'Length), To_Int_Ptr (Buffer'Address)));
                  Input_Entry (To_Str (Buffer) & Current_Input);
                  if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;

                -- Comment here !!!
                elsif Current_Input = To_Char_16 (ASCII.CR) then
                  Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Buffer'Length), To_Int_Ptr (Buffer'Address)));
                  Input_Entry (To_Str (Buffer));
                  if Input_Entry /= NULL_STR then
                    Line (Input_Entry);
                    if Input_Entry /= NULL_STR then
                      Submit (Input_Entry);
                      Input_Entry (NULL_STR);
                      Set_Text (Input_Box, NULL_STR);
                    end if;
                  end if;
                  Do_Skip_Message := True;

                -- Comment here !!!
                elsif Current_Input = To_Char_16 (ASCII.BS) and Input_Entry /= NULL_STR then
                  -- Input_Entry (Input_Entry (1..Input_Entry'Last - 1));
                  if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;
                elsif Current_Input = To_Char_16 (ASCII.HT) then Do_Skip_Message := True; end if;
              end if;
          when others => null; end case;

          -- Pass on the message if we didn't handle it already
          if not Do_Skip_Message then
            Ignore (TranslateMessage (Message'unchecked_access));
            Ignore (DispatchMessageW (Message'unchecked_access));
          end if;
          Do_Skip_Message := False;
        end if;
      end loop;

      -- Hide the main window then kill it
      Ignore (ShowWindow (Console, SW_HIDE));
      Assert (DestroyWindow (Console));
      Console := NULL_PTR;
      Assert (UnregisterClassW (CONSOLE_NAME'Access, NULL_PTR));
      Assert (ReleaseDC (GetDesktopWindow, Context));
    end;
  end;