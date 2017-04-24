
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

with Neo.API.Win32; use Neo.API.Win32;

-- System-dependant subprograms for Microsoft Windows®
separate (Neo.Engine) package body System is

  -----------------
  -- Information --
  -----------------

  -- Get OS and executable information
  function Get_Information_Internal return Information_State is
    Version : aliased OSVERSIONINFOEX     := (others => <>);
    Folder  : aliased Str_C (1..MAX_PATH) := (others => NULL_CHAR_C);
    Buffer  : aliased Int_C               := 0;
    begin

      -- Fetch strings
      Ignore (GetVersionExW (Version'Unchecked_Access));
      Line (GetLastError'Wide_Image);
      Assert (GetModuleFileNameW (NULL_PTR, Folder'Unrestricted_Access, Folder'Length));

      -- Remove the name
      Folder (Int_Size_C (Index (To_Str (Folder), "\", Backward))) := NULL_CHAR_C;          
      
      -- Get buffer size
      Assert (GetUserNameW (null, Buffer'Unchecked_Access) = 0 and then GetLastError = ERROR_INSUFFICIENT_BUFFER); 
      declare
      function Get_Version return Str is (Version.wki100_ver_major'Img & Version.wki100_ver_minor'Img);
      Username : aliased Ptr_Str_16_C := new Str_C (1..Int_Size_C (Buffer) + 5);
      begin
        Assert (GetUserNameW (Username, Buffer'Unchecked_Access));
        Assert (IsWow64Process (GetCurrentProcess, Buffer'Unchecked_Access));
        return (Name      => Delete (To_Str_Unbound (To_Str (Folder)), 1, Index (To_Str (Folder), "\", Backward)), -- First-level
                Path      => To_Str_Unbound (To_Str (Folder)),
                Username  => To_Str_Unbound (To_Str (Username.all)),
                Bit_Size  => (if Buffer = 1 then 64 else 32),
                OS        => To_Str_Unbound (NULL_STR & "Windows " &
                               (case Version.wki100_ver_major is
                                  when 5 =>
                                    (case Version.wki100_ver_minor is
                                       when 0 => "2000"
                                       when 1 => "XP"
                                       when 2 => "XP x64"
                                       when others => Get_Version)
                                  when 6 =>
                                    (case Version.wki100_ver_minor is
                                       when 0 => "Vista"
                                       when 1 => "7"
                                       when 2 => "8"
                                       when 3 => "8.1"
                                       when others => Get_Version)
                                  when 10 => 
                                    (case Version.wki100_ver_minor is
                                       when 0 => "10"
                                       when others => "10." & Get_Version) -- "The Last version of Windows..."
                                  when others => Get_Version)));
        end;
    end;

  -- Constant info to remove overhead
  INFORMATION_INTERNAL : constant Information_State := Get_Information_Internal;
  function Get_Information return Information_State is (INFORMATION_INTERNAL);

  -------------
  -- Globals --
  -------------

  -- Window titles
  APP_NAME           : constant Str   := To_Str (Get_Information.Name);
  GAME_NAME          : aliased  Str_C := To_Str_C (APP_NAME);
  INPUT_NAME         : aliased  Str_C := To_Str_C (APP_NAME & " Input");
  CONSOLE_NAME       : aliased  Str_C := To_Str_C (APP_NAME & " Console");
  MULTI_MONITOR_NAME : aliased  Str_C := To_Str_C (APP_NAME & " Multi-monitor");

  -- Asset paths
  WIN32_PATH_ICON            : aliased Str_C := To_Str_C (Get_Information.Path & PATH_ICON             & ".ico");
  WIN32_PATH_CURSOR_ACTIVE   : aliased Str_C := To_Str_C (Get_Information.Path & PATH_CURSOR_ACTIVE    & ".cur");
  WIN32_PATH_CURSOR_INACTIVE : aliased Str_C := To_Str_C (Get_Information.Path & PATH_CURSOR_INACTIVE  & ".cur");

  -- Main "HWND"s for the invisible input window and game window
  Game, Input : aliased Ptr := NULL_PTR;

  -- Window handles for multi-monitor mode
  package Vector_Ptr is new Vectors (Ptr);
  Multi_Monitor_Windows : Vector_Ptr.Unsafe.Vector;

  -------------
  -- Console --
  -------------

  -- A task-isolated GUI console application for debugging
  procedure Run_Console is separate;

  ------------
  -- Vulkan --
  ------------

  -- Pointer to the driver dll
  Vulkan_DLL : Ptr := LoadLibraryW (To_Str_C ("vulkan-1.dll"));

  -- Load a pointer to a procedure based on a name
  function Get_Vulkan_Subprogram (Name : Str) return Ptr is (GetProcAddress (Vulkan_DLL, To_Str_8_C (Name)));

  -- Fetch extension strings
  function Get_Vulkan_Extensions return Str_8_C is (VK_KHR_SURFACE_EXTENSION_NAME & VK_KHR_WIN32_SURFACE_EXTENSION_NAME);

  -- Finalization and initialization (mostly revolve around loading the dll)
  procedure Finalize_Vulkan_Library is begin Assert (FreeLibrary (Vulkan_DLL)); end;
  procedure Initialize_Vulkan_Library is begin vkCreateWin32SurfaceKHR := To_Ptr_vkCreateWin32SurfaceKHR (Get_Vulkan_Subprogram ("vkCreateWin32SurfaceKHR")); end;

  -- Create a new surface
  function Create_Vulkan_Surface (Instance : Ptr) return Ptr is
    Result       : aliased Ptr                         := NULL_PTR;
    Surface_Info : aliased VkWin32SurfaceCreateInfoKHR := (hWnd => Game, hInstance => GetModuleHandleNULL, others => <>);
    begin
      Assert (vkCreateWin32SurfaceKHR (Instance, Surface_Info'Access, NULL_PTR, Result'Access));
      Assert (Result);
      return Result;
    end;

  ---------------
  -- Clipboard --
  ---------------

  -- Set the clipboard with custom text
  procedure Copy (Item : Str) is

    -- Declare a new type to handle an unchecked conversion
    type Text_Array is array (Item'First..Item'Last + 1) of Char_16_C;
    type Ptr_Text_Array is Access all Text_Array;
    function To_Ptr_Text_Array is new Unchecked_Conversion (Ptr, Ptr_Text_Array);
    Text : Ptr_Text_Array := null;

    -- Prepare global data
    Data : Ptr := NULL_PTR;
    begin
      Data := GlobalAlloc (GMEM_MOVEABLE, Text_Array'Object_Size / Byte'Object_Size);
      Assert (Data);
      Text := To_Ptr_Text_Array (GlobalLock (Data));
      Assert (Text /= null);
      Text (Text.all'Last) := NULL_CHAR_C; 

      -- Write the text to data and send it to the clipboard
      for I in Item'Range loop Text (I) := To_Char_16_C (Item (I)); end loop;
      Assert (GlobalUnlock (Data) = 0);
      Assert (OpenClipboard (NULL_PTR) /= 0 and then GlobalFree (Data) = NULL_PTR);
      Assert (EmptyClipboard);
      Assert (SetClipboardData (CF_UNICODETEXT, Data));
      Assert (CloseClipboard);
    end;

  -- Grab the current clipboard text
  function Paste return Str is
    Text : Ptr_Const_Char_16_C := null;
    Data : Ptr                 := NULL_PTR;
    begin
      Assert (OpenClipboard (NULL_PTR));
      Data := GetClipboardData (CF_UNICODETEXT); Assert (Data);
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
    Flash_Info : aliased FLASHWINFO := (dwFlags => (if Val then FLASH_CONTINUOUSLY else FLASHW_STOP), others => <>);
    begin
      Assert (Game);
      Assert (FlashWindowEx (Flash_Info'Unchecked_Access));
    end;

  -- Execute a commandline statement
  procedure Execute (Path : Str) is
    Startup_Info : aliased STARTUPINFO         := (others => <>);
    Process_Info : aliased PROCESS_INFORMATION := (others => <>);
    begin
      Assert (Path'Length <= MAX_PATH - 1); -- Subtract one to account for null terminator
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
      Class_Name  : aliased Str_C (1..1024) := (others => NULL_CHAR_C);
      Window_Text : aliased Str_C (1..1024) := (others => NULL_CHAR_C);
      Window      :         Ptr             := To_Ptr (Int_Ptr (wParam));
      Icon        :         Ptr             := NULL_PTR;
      begin

        -- Identify the message box by class and window text
        Assert (GetClassNameW (Window, Class_Name'Unrestricted_Access, Class_Name'Length));
        Ignore (GetWIndowTextW (Window, Window_Text'Unrestricted_Access, Window_Text'Length) = 0);
        if nCode = HCBT_ACTIVATE and To_Str (Class_Name) = DIALOG_CLASS and To_Str (Window_Text) = Name then

          -- Load that icon!
          Icon := LoadImageW (hinst     => NULL_PTR,
                              lpszName  => WIN32_PATH_ICON'Access,
                              uType     => IMAGE_ICON,
                              cxDesired => 0,
                              cyDesired => 0,
                              fuLoad    => LR_LOADFROMFILE or LR_DEFAULTSIZE);
          if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, IDI_APPLICATION); end if;

          -- Well that was fun...
          Ignore (SendMessageW (Window, WM_SETICON, 0, To_Int_Ptr (Icon)) = 0);
          Assert (UnhookWindowsHookEx (Force_Custom_Icon));
        else Ignore (CallNextHookEx (Force_Custom_Icon, nCode, wParam, lParam) = 0); end if;
        return 0;
      end;
    begin

      -- Create a hook to find the message box window which will then be able to set the icon
      Force_Custom_Icon := SetWindowsHookExW (WH_CBT, CBTProc'Unchecked_Access, NULL_PTR, GetCurrentThreadId);

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
                when IDOK | IDRETRY | IDYES => True,
                when others => False);
    end;

  ---------------
  -- Windowing --
  ---------------

  -- Styles for game windowing modes
  STYLE_FULLSCREEN : constant Int_32_Unsigned_C := WS_VISIBLE or WS_SYSMENU or WS_POPUP   or WS_TOPMOST;
  STYLE_WINDOWED   : constant Int_32_Unsigned_C := WS_VISIBLE or WS_SYSMENU or WS_CAPTION or WS_BORDER or STYLE_BORDER_SIZABLE or WS_MAXIMIZEBOX;

  -- Icons and cursors
  Icon, Cursor_Inactive, Cursor_Active : Ptr := NULL_PTR;
  Original_Clip : aliased RECT := (others => <>);

  -- Conversion functions for rectangles and borders
  function To_Border (Rectangle : RECT)      return Border_State is ((Int_64 (Rectangle.top), Int_64 (Rectangle.bottom), Int_64 (Rectangle.left), Int_64 (Rectangle.right)));
  function To_RECT   (Border : Border_State) return RECT         is ((Int_C  (Border.Left),   Int_C  (Border.Top),       Int_C  (Border.Right),   Int_C  (Border.Bottom)));

  -- Find out if the API supports windowed mode (e.g. phone)
  function Fullscreen_Only return Bool is (False);

  -- Force the game into fullscreen mode
  procedure Minimize is begin Ignore (ShowWindow (Game, SW_SHOWMINIMIZED) = 0); end; 

  -- Change the cursor position
  procedure Set_Cursor (Pos : Cursor_State) is begin Assert (SetCursorPos (Int_C (Pos.X), Int_C (Pos.Y))); end;

  -- Get the size of windowed mode "decorations"
  function Get_Decoration return Border_State is ((Top    => Int_64 (GetSystemMetrics (SM_CYSIZE) + GetSystemMetrics (SM_CYFRAME)),
                                                   Right  => Int_64 (GetSystemMetrics (SM_CXFRAME)),
                                                   Left   => Int_64 (GetSystemMetrics (SM_CXFRAME)),
                                                   Bottom => Int_64 (GetSystemMetrics (SM_CYFRAME))));

  -- Internal procedure for initializing or adjusting the game window
  procedure Adjust_Windowing (X, Y : Int_64; Width, Height : Int_64_Positive; Fullscreen : Bool) is
    begin
      if Game = NULL_PTR then
        Game := CreateWindowExW (dwExStyle    => 0,
                                 lpClassName  => GAME_NAME'Access,
                                 lpWindowName => GAME_NAME'Access,
                                 dwStyle      => (if Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED) or WS_MINIMIZE,
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
        Assert (SetWindowLongW (Game, GWL_STYLE, (if Fullscreen then STYLE_FULLSCREEN else STYLE_WINDOWED)) /= 0);
        Assert (SetWindowPos (hWnd            => Game,
                              hWndInsertAfter => NULL_PTR,
                              X               => Int_C (X),
                              Y               => Int_C (Y),
                              cx              => Int_C (Width),
                              cy              => Int_C (Height),
                              uFlags          => 0));
      end if;
    end;

  -- Style changes to switch window modes
  procedure Maximize is
    Context : Ptr := GetDC (GetDesktopWindow);
    begin
      Assert (Context);
      Adjust_Windowing (X          => 0,
                        Y          => 0,
                        Width      => Int_64_Positive (GetDeviceCaps (Context, SM_CYDLGFRAME)),
                        Height     => Int_64_Positive (GetDeviceCaps (Context, SM_CXHTHUMB)),
                        Fullscreen => True);
      Assert (ReleaseDC (GetDesktopWindow, Context));
    end;
  procedure Make_Windowed is
    Context : Ptr := GetDC (GetDesktopWindow);
    begin
      Assert (Context);
      Adjust_Windowing (X          => Int_64 (GetDeviceCaps (Context, SM_CYDLGFRAME)) / 2 - Windowed_Width.Get  / 2,
                        Y          => Int_64 (GetDeviceCaps (Context, SM_CXHTHUMB))   / 2 - Windowed_Height.Get / 2,
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
    Handle : Ptr := NULL_PTR;
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

      -- Modify the "internal display counter"
      if Do_Hide then while ShowCursor (0) > -1 loop null; end loop;
      else            while ShowCursor (1) <  0 loop null; end loop; end if;
    end;

  -- Restrict or unrestrict cursor movement (also hide or show cursor)
  procedure Clip_Cursor (Do_Clip : Boolean := True) is
    Rectangle : aliased RECT := (others => <>);
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
              Inject_Into_Player_1 ((Text_Impulse, (Text_Impulse, 1, NO_COMBO), To_Str_Unbound (Char_16'Val (Int (wParam)))));
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
              Result.all := To_RECT (Resize (Border => To_Border (Result.all),
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

    -- Main window class
    Class : aliased WNDCLASSEX := (others => <>);
    begin

      -- Load the game window icon
      Icon := LoadImageW (hinst     => GetModuleHandleNULL,
                          lpszName  => WIN32_PATH_ICON'Access,
                          uType     => IMAGE_ICON,
                          cxDesired => 0,
                          cyDesired => 0,
                          fuLoad    => LR_LOADFROMFILE or LR_DEFAULTSIZE);
      if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, IDI_APPLICATION); end if;

      -- Load cursors
      Cursor_Inactive := LoadImageW (hinst     => GetModuleHandleNULL,
                                     lpszName  => WIN32_PATH_CURSOR_INACTIVE'Access,
                                     uType     => IMAGE_CURSOR,
                                     cxDesired => 0,
                                     cyDesired => 0,
                                     fuLoad    => LR_LOADFROMFILE or LR_DEFAULTSIZE);
      if Cursor_Inactive = NULL_PTR then Cursor_Inactive := LoadCursorW (NULL_PTR, GENERIC_CURSOR); end if;
      Cursor_Active := LoadImageW (hinst     => GetModuleHandleNULL,
                                   lpszName  => WIN32_PATH_CURSOR_ACTIVE'Access,
                                   uType     => IMAGE_CURSOR,
                                   cxDesired => 0,
                                   cyDesired => 0,
                                   fuLoad    => LR_LOADFROMFILE);
      if Cursor_Active = NULL_PTR then Cursor_Active := LoadCursorW (NULL_PTR, GENERIC_CURSOR); end if;

      -- Register the main window class (the actual window creation is done in Adjust_Windowing)
      Class := (lpfnWndProc   => WindowProc'Unchecked_Access,
                hInstance     => GetModuleHandleNULL,
                hIconSm       => Icon,
                hIcon         => Icon,
                hCursor       => Cursor_Inactive,
                hbrBackground => COLOR_GRAYTEXT,
                lpszClassName => To_Ptr_Const_Char_16_C (GAME_NAME),
                others        => <>);
      Assert (RegisterClassExW (Class'Access));
    end;

  -- Pump the OS message loop
  function Update_Windowing return Bool is
    Message : aliased MSG := (others => <>);
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
  function Get_Monitors return Vector_Border.Unsafe_Array is
    Monitors : Vector_Border.Unsafe.Vector;

    -- Callback for hook
    function MonitorEnumProc (hMonitor, hdcMonitor : Ptr; lprcMonitor : Ptr_RECT; dwData : Ptr_Int_Ptr) return Int_C with Convention => Stdcall;
    function MonitorEnumProc (hMonitor, hdcMonitor : Ptr; lprcMonitor : Ptr_RECT; dwData : Ptr_Int_Ptr) return Int_C is
      Info : aliased MONITORINFO := (others => <>);
      begin
        Assert (GetMonitorInfoW (hMonitor, Info'Unchecked_Access));
        Monitors.Append (To_Border (Info.rcMonitor));
        return 1;
      end;
    begin

      -- Call hook
      Assert (EnumDisplayMonitors (NULL_PTR, null, MonitorEnumProc'Unchecked_Access, 0));
      Assert (Monitors.Length > 0);
      return Vector_Border.To_Unsafe_Array (Monitors);
    end;

  -- Look at all the monitors on a system (excluding the main one) and return their borders
  function Get_Windows return Vector_Border.Unsafe_Array is
    begin

      -- Enumerate all monitors if we are in multi-monitor mode
      if Mode.Get = Multi_Monitor_Mode then return Get_Monitors; end if;
    
      -- Return only the main window
      declare
      Result : aliased RECT := (others => <>);
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
    Class : aliased WNDCLASSEX := (cbSize        => WNDCLASSEX'Object_Size / Byte'Object_Size,
                                   lpfnWndProc   => WindowProc'Unchecked_Access,
                                   hInstance     => GetModuleHandleNULL,
                                   hIconSm       => Icon,
                                   hIcon         => Icon,
                                   hbrBackground => COLOR_GRAYTEXT,
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
                                                         dwStyle      => STYLE_FULLSCREEN or WS_DISABLED,
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
  Gamepads : aliased array (0..3) of XINPUT_GAMEPAD := (others => (others => <>));

  -- Vibrate an Xbox controller
  procedure Vibrate (Id : Int_Ptr; Hz_High, Hz_Low : Real_32_Percent) is
    Vibration : aliased XINPUT_VIBRATION := (wLeftMotorSpeed  => Int_16_Unsigned_C (Hz_Low  / 100.0 * Real (Int_16_Unsigned_C'Last)),
                                             wRightMotorSpeed => Int_16_Unsigned_C (Hz_High / 100.0 * Real (Int_16_Unsigned_C'Last)));
    begin if Id in 0..3 then Assert (XInputSetState (Int_32_Unsigned_C (Id), Vibration'Unchecked_Access) = 0); end if; end;

  -- Fetch raw cursor coordinates from the system cursor
  function Get_Cursor return Cursor_state is
    Pt : aliased POINT := (others => <>);
    begin
      Assert (GetCursorPos (Pt'Unchecked_Access));
      return (Int_64 (Pt.X), Int_64 (Pt.Y));
    end;

  -- Handle messages for keyboards and mice
  procedure Initialize_Input is
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function WindowProc (hwnd : Ptr; uMsg : Int_32_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      Id     :         Int_Ptr           := 0;
      Bytes  : aliased Int_32_Unsigned_C := 0;
      Header : aliased RAWINPUTHEADER    := (others => <>);
      begin
        case uMsg is
          when WM_CLOSE => PostQuitMessage (0); return 0;
          when WM_INPUT =>

            -- Find if the input message belongs to a mouse or keyboard
            Bytes := RAWINPUTHEADER'Object_Size / Byte'Object_Size;
            Assert (GetRawInputData (hRawInput    => To_Ptr (lParam),
                                     uiCommand    => GET_DEVICE_HEADER,
                                     pData        => Header'Unchecked_Access,
                                     pcbSize      => Bytes'Unchecked_Access,
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
                                           pData        => Keyboard'Unchecked_Access,
                                           pcbSize      => Bytes'Unchecked_Access,
                                           cbSizeHeader => RAWINPUTHEADER'Object_Size / Byte'Object_Size) = RAWKEYBOARD'Object_Size / Byte'Object_Size);

                  -- Inject the mapped VK code
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
                                           pData        => Mouse'Unchecked_Access,
                                           pcbSize      => Bytes'Unchecked_Access,
                                           cbSizeHeader => RAWINPUTHEADER'Object_Size / Byte'Object_Size) = RAWMOUSE'Object_Size / Byte'Object_Size);

                  -- Handle various buttons
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

                  -- Deal with scrolling
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
    Class : aliased WNDCLASSEX := (lpfnWndProc   => WindowProc'Unchecked_Access,
                                   hInstance     => GetModuleHandleNULL,
                                   hIconSm       => LoadIconW (GetModuleHandleNULL, IDI_APPLICATION),
                                   hIcon         => LoadIconW (GetModuleHandleNULL, IDI_APPLICATION),
                                   hCursor       => LoadCursorW (NULL_PTR, GENERIC_CURSOR),
                                   hbrBackground => COLOR_GRAYTEXT,
                                   lpszClassName => To_Ptr_Const_Char_16_C (INPUT_NAME),
                                   others        => <>);
    begin
      Assert (RegisterClassExW (Class'Unchecked_Access));
      Input := CreateWindowExW (dwExStyle    => 0,
                                lpClassName  => INPUT_NAME'Access,
                                lpWindowName => INPUT_NAME'Access,
                                dwStyle      => WS_DISABLED,
                                x            => 0,
                                y            => 0,
                                nWidth       => 0,
                                nHeight      => 0,
                                hWndParent   => NULL_PTR,
                                hMenu        => 0,
                                hInstance    => GetModuleHandleNULL,
                                lpParam      => NULL_PTR);
      Assert (Input);

      -- Register classes of input devices with Raw Input so that the event loop will recieve thier messages
      declare
      type Array_RAWINPUTDEVICE is array (Positive range <>) of RAWINPUTDEVICE;
      Setups : aliased Array_RAWINPUTDEVICE := ((GENERIC_DESKTOP, USE_RAW_KEYBOARD, RIDEV_INPUTSINK, Input),
                                                (GENERIC_DESKTOP, USE_RAW_MOUSE,    RIDEV_INPUTSINK, Input));
      begin
        Assert (RegisterRawInputDevices (Setups'Unchecked_Access, Setups'Length, RAWINPUTDEVICE'Object_Size / Byte'Object_Size));
      end;
    end;

  -- Unregister input devices by overwriting the setup values then kill the input window
  procedure Finalize_Input is
    Null_Setup : aliased RAWINPUTDEVICE := (GENERIC_DESKTOP, USE_RAW_MOUSE, STOP_READING_TOP_LEVEL_DEVICES, NULL_PTR);
    begin
      Ignore (RegisterRawInputDevices (Null_Setup'Unchecked_Access, Null_Setup'Object_Size / RAWINPUTDEVICE'Object_Size, RAWINPUTDEVICE'Object_Size / Byte'Object_Size));
      if Input /= NULL_PTR then Ignore (DestroyWindow (Input)); end if;
      Input := NULL_PTR;
    end;

  -- Pump messages, check device connectivity, and poll devices that don't send messages
  function Update_Input return Bool is
    Device_Count   : aliased Int_32_Unsigned_C := 0;
    State          : aliased XINPUT_STATE      := (others => <>);
    Message        : aliased MSG               := (others => <>);
    Has_Gamepad    : Array_Bool (1..4)         := (others => False);
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
        Inject_Stick (Int_Ptr (Player), Stick, ((if X > 0 then Real_Range (Real (X) / Real (Int_16_Signed_C'Last))
                                                 else          Real_Range (Real (X) / Real (Int_16_Signed_C'First))),
                                                (if Y > 0 then Real_Range (Real (Y) / Real (Int_16_Signed_C'Last))
                                                 else          Real_Range (Real (Y) / Real (Int_16_Signed_C'First)))));
      end;

    -- Perform black magic
    function Get_Device_Name (Device : Ptr) return Str is
      Buff : Str_16_c (1..500);
      Buff_S : aliased Int_32_Unsigned_C := 500;
      Buff2 : Str_16_c (1..500);
      Buff_S2 : aliased Int_32_Unsigned_C := Buff2'Length * Char_16_C'Object_Size;
      File : Ptr;
      Result : Str_Unbound;
      begin
        Assert (GetRawInputDeviceInfoW (Device, RIDI_DEVICENAME, Buff (1)'Unchecked_Access, Buff_S'Unchecked_Access));
        File := CreateFileW (lpFileName            => Buff'Unchecked_Access,
                             dwDesiredAccess       => GENERIC_READ or GENERIC_WRITE,
                             dwShareMode           => FILE_SHARE_READ or FILE_SHARE_WRITE,
                             lpSecurityAttributes  => NULL_PTR,
                             dwCreationDisposition => OPEN_EXISTING,
                             dwFlagsAndAttributes  => 0,
                             hTemplateFile         => NULL_PTR);
        Assert (File);
        Assert (HidD_GetProductString (Buff (1)'Unchecked_Access, Buff2 (1)'Unchecked_Access, Buff_S2));
        CloseHandle (File);
        return To_Str (Buff);
      end;
    begin

      -- Fetch a complete list of RawInput devices for querying purposes
      Assert (GetRawInputDeviceList (NULL_PTR, Device_Count'Unchecked_Access, RAWINPUTDEVICELIST'Object_Size / Byte'Object_Size) /= -1);
      Assert (Device_Count);
      declare
      List : aliased array (1..Int (Device_Count)) of RAWINPUTDEVICELIST := (others => (others => <>));
      begin
        Assert (GetRawInputDeviceList (List'Unchecked_Access, Device_Count'Unchecked_Access, RAWINPUTDEVICELIST'Object_Size / Byte'Object_Size) /= -1);

        -- Look for keyboards and mice then add them to the internal list of devices if they are new
        for I in List'Range loop
          if not Has_Device (To_Int_Ptr (List (I).hDevice)) then
            case List (I).dwType is
              when RIM_TYPEKEYBOARD => Add_Device (To_Int_Ptr (List (I).hDevice), (Keyboard_Device, others => <>));
              when RIM_TYPEMOUSE    => Add_Device (To_Int_Ptr (List (I).hDevice), (Mouse_Device,    others => <>));
            when others => null; end case;
          end if;
        end loop;

        -- Remove unplugged or unused devices
        Current_Device := Devices.First;
        while Devices.Has (Current_Device) loop

          -- XInput devices
          if Devices.Key (Current_Device) in 0..3 then
            Has_Gamepad (Int (Devices.Key (Current_Device)) + 1) := True;
            if XInputGetState (Int_32_Unsigned_C (Devices.Key (Current_Device)), State'Unchecked_Access) /= 0 then Devices.Delete (Current_Device); end if;

          -- RawInput devices
          else
            for J in List'Range loop
              exit when Devices.Key (Current_Device) = To_Int_Ptr (List (J).hDevice);
              if J = List'Last then Devices.Delete (Current_Device); end if;
            end loop;
          end if;
          Devices.Next (Current_Device);
        end loop;

        -- Query the XInput API for Xbox 360 controllers and add them to our device list
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

      -- Inject Xbox 360 controller input
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
end;