
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

-- A task-isolated GUI console application for debugging
separate (Neo.Engine.System) package body Console is

  -- Button settings
  type Button_State is record
      Message : Str (1..4);
      Action  : access procedure;
    end record;
  CONSOLE_BUTTONS : constant array (1..2) of Button_State := (("Save", Save_Log'Access), ("Send", Send_Log'Access));
  Buttons         : array (CONSOLE_BUTTONS'Range) of Ptr  := (others => NULL_PTR);

  -- Timing
  POLLING_DURATION : constant Duration := 0.02; 
  Last_Time        :          Time     := Clock;
  
  -- Text
  ERROR_REPORTING_URL : constant Str   := "http://www.google.com";
  LABEL_INPUT_ENTRY   : constant Str   := "Input";
  LABEL_OUTPUT        : constant Str   := "Output";
  FONT_CONSOLE        : aliased  Str_C := To_Str_C ("Consolas");
  NAME_BUTTON         : aliased  Str_C := To_Str_C ("Button");
  NAME_GROUP          : aliased  Str_C := To_Str_C ("Group");
  NAME_EDIT           : aliased  Str_C := To_Str_C ("Edit");

  -- Constants
  DO_DISABLE_RESIZE     : constant Bool  := False;
  GROUP_BOX_SIDE_MARGIN : constant Real  := 1.2;
  FONT_GROUP_BOX_SIZE   : constant Real  := 1.2;
  IDENTIFIER_START      : constant Int_C := 666;
  SCROLL_FACTOR         : constant Int_C := 500;
  FONT_CONSOLE_SIZE     : constant Int_C := -11;
  NUMBER_OF_OUTPUT_ROWS : constant Int_C := 25;
  BUTTON_HEIGHT_DLU     : constant Int_C := 14;
  BUTTON_WIDTH_DLU      : constant Int_C := 50;
  PIXELS_PER_INCH       : constant Int_C := 72;
  MARGIN_BUTTON         : constant Int_C := 4;
  MARGIN                : constant Int_C := 7;

  -- System call parameters
  Message       : aliased MSG               := (others => <>);
  Text_Metric   : aliased TEXTMETRIC        := (others => <>);
  Class         : aliased WNDCLASSEX        := (others => <>);
  Metrics       : aliased NONCLIENTMETRICS  := (others => <>);
  Rectangle     : aliased RECT              := (others => <>);
  Input_Buffer  : aliased Str_C (1..500000) := (others => NULL_CHAR_C); -- Buffer size is arbitrary !!!
  
  -- Console output tracking
  Cutoff, Cutoff_Lines,
  Current_Lines : Int_64_Unsigned := 0;
  Current_Input : Char            := NULL_CHAR;
  Current_Log   : Str_Unbound     := NULL_STR_UNBOUND;
  
  -- Window, font, and icon pointers
  Output_Group_Box, Input_Group_Box,
  Font_Text_Box, Font_Buttons,
  Output_Box, Input_Box,
  Edit_Background,
  Context,
  Console,
  Icon : aliased Ptr := NULL_PTR;

  -- Sizing variables
  Output_Box_Height, Output_Box_Width, Input_Box_Height,
  Text_Box_Font_Height, Text_Box_Font_Width,
  Start_Selection, End_Selection,
  Margin_Group_Top, Margin_Group,
  Number_Of_Lines, Current_Line,
  Current_Height, Current_Width,
  Console_Height, Console_Width,
  Button_Height, Button_Width,
  Message_Box_Font_Height,
  DBU_Height, DBU_Width,
  Y : aliased Int_C := 0;

  -- Flags
  Close_Console,
  Do_Process_Character,
  Was_At_Minimum_Width,
  Was_At_Minimum_Height,
  Was_At_Bottom, Is_At_Bottom,
  Do_Skip_Message : Bool := False;

  -- Message procedures for convience
  procedure Set_Text (Handle : Ptr; Text : Str) is begin Assert (SendMessageW (Handle, WM_SETTEXT, 0, To_Int_Ptr (To_Str_C (Text)'Address))); end;
  procedure Set_Font (Handle : Ptr; Font : Ptr) is begin Ignore (SendMessageW (Handle, WM_SETFONT, To_Int_Ptr (Font), 1)); end;

  -- Return true if the output text window's scrollbar is at the bottom
  function Scrollbar_At_Bottom return Bool is
    Scroll_Info : aliased SCROLLINFO := (fMask => SIF_ALL, others => <>);
    begin
      Assert (GetScrollInfo (Output_Box, SB_VERT, Scroll_Info'Unchecked_Access));
      return Scroll_Info.nPos + Number_of_Lines >= Scroll_Info.nMax;
    end;

  -- Create and set fonts for the output box
  procedure Create_Fonts is 
    Font_Size : aliased Neo.API.Windows.SIZE := (others => <>);
    Some_BS   : aliased Str_C := To_Str_C ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
    begin
      Assert (SystemParametersInfoW (SPI_GETNONCLIENTMETRICS, Metrics.cbSize, Metrics'Address, 0));
      Font_Buttons := CreateFontIndirectW (Metrics.lfMessageFont'Unchecked_Access); Assert (Font_Buttons);
      Font_Text_Box := CreateFontW (nHeight            => FONT_CONSOLE_SIZE,
                                    nWidth             => 0,
                                    nEscapement        => 0,
                                    nOrientation       => 0,
                                    fnWeight           => 0,
                                    fdwItalic          => 0,
                                    fdwUnderline       => 0,
                                    fdwStrikeOut       => 0,
                                    fdwCharSet         => DEFAULT_CHARSET,
                                    fdwOutputPrecision => OUT_DEFAULT_PRECIS,
                                    fdwClipPrecision   => CLIP_DEFAULT_PRECIS,
                                    fdwQuality         => DEFAULT_QUALITY,
                                    fdwPitchAndFamily  => FF_MODERN,
                                    lpszFace           => FONT_CONSOLE'Unchecked_Access);
      Assert (Font_Text_Box);
      Ignore (SelectObject (Context, Font_Text_Box));
      Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
      Text_Box_Font_Height := Text_Metric.tmHeight;
      Assert (GetTextExtentPoint32W (hdc      => Context,
                                     lpString => C (Some_BS),
                                     c        => 52,
                                     lpSize   => Font_Size'Unchecked_Access));
      Text_Box_Font_Width := Int_C ((Font_Size.cx / 26 + 1) / 2);
      Ignore (SelectObject (Context, Font_Buttons));
      Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
    end;
    
  -- A single function for setting all GUI sizing globals
  procedure Set_Sizes is
    Minimum_Width,
    Box_Padding   : Int_C := 0;
    Border_Height : Int_C := GetSystemMetrics (SM_CYFRAME);
    Border_Width  : Int_C := GetSystemMetrics (SM_CXFRAME);
    begin
      Margin_Group      := Int_C (Float (Text_Metric.tmHeight) / GROUP_BOX_SIDE_MARGIN);
      Margin_Group_Top  := Int_C (Float (Text_Metric.tmHeight) * FONT_GROUP_BOX_SIZE);
      Box_Padding       := Int_C (Float (Text_Box_Font_Width) / 1.5);
      Output_Box_Width  := 2 * Box_Padding + (Text_Box_Font_Width  * Int_C (Line_Size)) + GetSystemMetrics (SM_CYVTHUMB);
      Output_Box_Height := 2 * Box_Padding + (Text_Box_Font_Height * Int_C (NUMBER_OF_OUTPUT_ROWS));
      Input_Box_Height  := 2 * Box_Padding + Text_Box_Font_Height;
      Console_Width     := (MARGIN * DBU_Width + Margin_Group + Border_Width) * 2 + Output_Box_Width;
      Console_Height    := (MARGIN * DBU_Height) * 4 + (Border_Height + Margin_Group + Margin_Group_Top) * 2
                           + Output_Box_Height + Input_Box_Height + BUTTON_HEIGHT + GetSystemMetrics (SM_CYSIZE);
      if Buttons'Length > 0 then
        Minimum_Width := (Buttons'Length - 1) * MARGIN_BUTTON * DBU_Width + MARGIN * 2 * DBU_Width + BUTTON_WIDTH * Buttons'Length + Border_Width * 2;
        if Console_Width < Minimum_Width then
          Output_Box_Width := Output_Box_Width + Minimum_Width - Console_Width;
          Console_Width := Minimum_Width;
        end if;
      end if;
      if Current_Height < Console_Height or (Was_At_Minimum_Height and Current_Height > Console_Height) then Current_Height := Console_Height; end if;
      if Current_Width  < Console_Width  or (Was_At_Minimum_Width  and Current_Width  > Console_Width)  then Current_Width  := Console_Width;  end if;
      Output_Box_Width      := Current_Width  - (Console_Width  - Output_Box_Width);
      Output_Box_Height     := Current_Height - (Console_Height - Output_Box_Height);
      Was_At_Minimum_Height := Current_Height < Console_Height + Border_Height;
      Was_At_Minimum_Width  := Current_Width  < Console_Width  + Border_Width;
      Number_Of_Lines       := (Output_Box_Height - 2 * Box_Padding) / Text_Box_Font_Height;
    end;

  -- Set the global variable DBU_Width used in calculating all UI proportions by spawning a message box and measuring it
  procedure Get_Sizes_From_Message_Box is
    Hook : Ptr := NULL_PTR;

    -- Procedure to wait for the callback-callback (below) to set DBU_Width
    procedure Wait_For_Set_With_Infinite_Loop is
      begin
        for I in Int'Range loop -- The Windows API made me do it
          exit when DBU_Width /= 0;
          delay 0.0001;
        end loop;
        if DBU_Width = 0 then raise Program_Error; end if;
      end;

    -- Callback to acquire message box
    function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
    function CBTProc (nCode : Int_C; wParam, lParam : Int_Ptr) return Int_Ptr is
      Class_Name, Window_Text : aliased Str_C (1..256) := (others => NULL_CHAR_C);
      Window                  :         Ptr            := To_Ptr (Int_Ptr (wParam));

      -- Once we find the message box we have to look through its children to find a button
      function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C with Convention => Stdcall;
      function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C is
        Rectangle : aliased RECT := (others => <>);
        begin

          -- Find a message box button to calculate dialog base units
          Assert (GetClassNameW (hwnd, C (Class_Name), Class_Name'Length));
          Ignore (GetWindowTextW (hwnd, C (Window_Text), Window_Text'Length));
          if DBU_Width = 0 and "Button" = S (Class_Name) then

            -- Do the Microsoft recommended math and measurements: https://support.microsoft.com/en-us/kb/125681
            Assert (GetWindowRect (hwnd, Rectangle'Unchecked_Access));
            Button_Width            := (Rectangle.right  - Rectangle.left);
            Button_Height           := (Rectangle.bottom - Rectangle.top);
            Ignore (SelectObject (Context, To_Ptr (Int_Ptr (To_Int_32_Unsigned (SendMessageW (hwnd, WM_GETFONT, 0, 0))))));
            Assert (GetTextMetricsW (Context, Text_Metric'Unchecked_Access));
            Message_Box_Font_Height := Text_Metric.tmHeight;
            DBU_Width  := Button_Width  / BUTTON_WIDTH_DLU;
            DBU_Height := Button_Height / BUTTON_HEIGHT_DLU;
          end if;
          return 1;
        end;
      begin

        -- Find the newly created message-box window 
        Assert (GetClassNameW  (Window, C (Class_Name),  Class_Name'Length));
        Ignore (GetWindowTextW (Window, C (Window_Text), Window_Text'Length));
        if nCode = HCBT_ACTIVATE and S (Class_Name) = S (DIALOG_CLASS) and S (Window_Text) = S (CONSOLE_NAME) then

          -- Get the dialog base units then close the message box
          Ignore (EnumChildWindows (Window, EnumWindowsProc'address, 0));
          Wait_For_Set_With_Infinite_Loop;
          Assert (DestroyWindow (Window));
          Assert (UnhookWindowsHookEx (Hook));

        -- Not the message box, continue the hook
        else Ignore (CallNextHookEx (Hook, nCode, wParam, lParam)); end if;
        return 0;
      end;
    begin

      -- Create a message box and a callback that checks newly created windows, then wait for DBU_Width to be set
      Hook := SetWindowsHookExW (WH_CBT, CBTProc'Address, NULL_PTR, GetCurrentThreadId);
      Ignore (MessageBoxW (NULL_PTR, C (GAME_NAME), C (CONSOLE_NAME), 0));
      Wait_For_Set_With_Infinite_Loop;
    end;

  -- Console window callback
  function Console_Window_Proc (hwnd : Ptr; uMsg : Int_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr with Convention => Stdcall;
  function Console_Window_Proc (hwnd : Ptr; uMsg : Int_Unsigned_C; wParam, lParam : Int_Ptr) return Int_Ptr is

    -- Conversion function
    function To_Windows_Color (Color : Color_State) return Int_Unsigned_C is (Int_Unsigned_C (Shift_Left (Int_Unsigned (Color.Green), 8) or
                                                                                              Shift_Left (Int_Unsigned (Color.Blue), 16) or
                                                                                                          Int_Unsigned (Color.Red)));
    begin
      case uMsg is
        when WM_CLOSE      => PostQuitMessage (0); Close_Console := True; return 1;
        when WM_CREATE     => Edit_Background := CreateSolidBrush (To_Windows_Color (COLOR_BACKGROUND)); Assert (Edit_Background);
        when WM_SYSCOMMAND => if wParam = Int_Ptr (SC_KEYMENU) or wParam = Int_Ptr (SC_SCREENSAVE) then return 1; end if;

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
            if wParam = Int_Ptr (I + Int (IDENTIFIER_START)) then
              CONSOLE_BUTTONS (I).Action.all;
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

            -- Query user inputed sizes then calculate the new sizes with Set_Sizes
            Ignore (SetWindowLongW (Console, GWL_EXSTYLE, WS_EX_COMPOSITED));
            Assert (GetWindowRect  (Console, Rectangle'Unchecked_Access));
            Was_At_Minimum_Height := False;
            Was_At_Minimum_Width  := False;
            Is_At_Bottom          := Scrollbar_At_Bottom;
            Current_Width         := Rectangle.Right  - Rectangle.Left;
            Current_Height        := Rectangle.Bottom - Rectangle.Top;
            Set_Sizes;

            -- Apply new sizes
            Assert (SetWindowPos (hWnd            => Console,
                                  hWndInsertAfter => NULL_PTR,
                                  X               => Rectangle.Left,
                                  Y               => Rectangle.Top,
                                  cx              => Current_Width,
                                  cy              => Current_Height,
                                  uFlags          => 0));
            Y := DBU_Width * MARGIN;
            Assert (SetWindowPos (hWnd            => Output_Group_Box,
                                  hWndInsertAfter => NULL_PTR,
                                  X               => DBU_Width * MARGIN,
                                  Y               => Y,
                                  cx              => Output_Box_Width + Margin_Group * 2,
                                  cy              => Output_Box_Height + Margin_Group_Top + Margin_Group,
                                  uFlags          => 0));
            Y := Y + Margin_Group_Top;
            Assert (SetWindowPos (hWnd            => Output_Box,
                                  hWndInsertAfter => NULL_PTR,
                                  X               => Margin_Group + DBU_Width * MARGIN,
                                  Y               => Y,
                                  cx              => Output_Box_Width,
                                  cy              => Output_Box_Height,
                                  uFlags          => 0));
            Y := Y + Output_Box_Height + DBU_Width * MARGIN + Margin_Group;
            Assert (SetWindowPos (hWnd            => Input_Group_Box,
                                  hWndInsertAfter => NULL_PTR,
                                  X               => DBU_Width * MARGIN,
                                  Y               => Y,
                                  cy              => Input_Box_Height + Margin_Group_Top + Margin_Group,
                                  cx              => Output_Box_Width + Margin_Group * 2,
                                  uFlags          => 0));
            Y := Y + Margin_Group_Top;
            Assert (SetWindowPos (hWnd            => Input_Box,
                                  hWndInsertAfter => NULL_PTR,
                                  X               => Margin_Group + DBU_Width * MARGIN,
                                  Y               => Y,
                                  cx              => Output_Box_Width,
                                  cy              => Input_Box_Height,
                                  uFlags          => 0));
            Y := Y + Input_Box_Height + DBU_Width * MARGIN + Margin_Group;

            -- Button repositioning
            for I in Buttons'Range loop
              Assert (SetWindowPos (hWnd            => Buttons (I),
                                    hWndInsertAfter => NULL_PTR,
                                    cx              => Button_Width,
                                    cy              => Button_Height,
                                    uFlags          => 0,
                                    Y               => Y,
                                    X               => Output_Box_Width + Margin_Group * 2 + DBU_Width * MARGIN -
                                                       Int_C (I) * BUTTON_WIDTH  - DBU_Width * Int_C (I - 1) * MARGIN_BUTTON));
            end loop;

            -- Keep the scrollbar at the bottom if it was there before resizing
            if Is_At_Bottom then Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_ENDSCROLL, 0)); end if;
            Assert (RedrawWindow (hwnd, null, NULL_PTR, 1));
            Ignore (SetWindowLongW (Console, GWL_EXSTYLE, 0));
            return 1;
          end if;
      when others => null; end case;
      return DefWindowProcW (hwnd, uMsg, wParam, lParam);
    end;
  
  -- Execute the main loop
  procedure Run is 
    begin 
    
      -- Load the context which is used globally
      Context := GetDC (GetDesktopWindow);
      Assert (Context);
      
      -- Load the icon
      Icon := LoadImageW (hinst     => GetModuleHandleW (null), -- Loads the icon nicely for the Aero theme, but not on the "classic" theme
                          lpszName  => C (WIN32_PATH_ICON),
                          uType     => IMAGE_ICON,
                          cxDesired => 0,
                          cyDesired => 0,
                          fuLoad    => LR_LOADFROMFILE or LR_DEFAULTSIZE);
      if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleW (null), IDI_APPLICATION); end if;
    
      -- Set global font and size variables
      Get_Sizes_From_Message_Box;
      Create_Fonts;      
      Set_Sizes;
      
      -- Create the main console window
      Class := (lpfnWndProc   => Console_Window_Proc'Address,
                hInstance     => GetModuleHandleW (null),
                hIconSm       => Icon,
                hIcon         => Icon,
                hCursor       => LoadCursorW (NULL_PTR, IDI_APPLICATION),
                hbrBackground => COLOR_WINDOW,
                lpszClassName => C (CONSOLE_NAME),
                others        => <>);
      Assert (RegisterClassExW (Class'Unchecked_Access));
      Console := CreateWindowExW (lpClassName  => C (CONSOLE_NAME),
                                  lpWindowName => C (CONSOLE_NAME),
                                  x            => GetDeviceCaps (Context, SM_CYDLGFRAME) / 2 - Console_Width  / 2,
                                  y            => GetDeviceCaps (Context, SM_CXHTHUMB)   / 2 - Console_Height / 2,
                                  nWidth       => Console_Width,
                                  nHeight      => Console_Height,
                                  hWndParent   => NULL_PTR,
                                  hMenu        => 0,
                                  hInstance    => GetModuleHandleW (null),
                                  lpParam      => NULL_PTR,
                                  dwExStyle    => 0,
                                  dwStyle      => WS_MINIMIZE or WS_SIZEBOX or WS_SYSMENU or WS_BORDER or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);
      Assert (Console);
    
      -- Create the output box border
      Output_Group_Box := CreateWindowExW (lpClassName  => C (NAME_BUTTON),
                                           lpWindowName => null,
                                           x            => 0,
                                           y            => 0,
                                           nWidth       => 0,
                                           nHeight      => 0,
                                           hWndParent   => Console,
                                           hMenu        => 0,
                                           hInstance    => GetModuleHandleW (null),
                                           lpParam      => NULL_PTR,
                                           dwExStyle    => 0,
                                           dwStyle      => WS_VISIBLE or BS_GROUPBOX or WS_CHILD);
      Assert (Output_Group_Box);
      Set_Font (Output_Group_Box, Font_Buttons);
      Set_Text (Output_Group_Box, Localize (LABEL_OUTPUT));
    
      -- Create the output box
      Output_Box := CreateWindowExW (lpClassName  => C (NAME_EDIT),
                                     lpWindowName => null,
                                     x            => 0,
                                     y            => 0,
                                     nWidth       => 0,
                                     nHeight      => 0,
                                     hWndParent   => Console,
                                     hMenu        => 0,
                                     hInstance    => GetModuleHandleW (null),
                                     lpParam      => NULL_PTR,
                                     dwExStyle    => 0,
                                     dwStyle      => WS_VSCROLL or WS_VISIBLE or WS_BORDER or ES_MULTILINE or ES_READONLY or WS_CHILD);
      Assert (Output_Box);
      Set_Font (Output_Box, Font_Text_Box);
    
      -- Create the input box border
      Input_Group_Box := CreateWindowExW (lpClassName  => C (NAME_BUTTON),
                                          lpWindowName => null,
                                          x            => 0,
                                          y            => 0,
                                          nHeight      => 0,
                                          nWidth       => 0,
                                          hWndParent   => Console,
                                          hMenu        => 0,
                                          hInstance    => GetModuleHandleW (null),
                                          lpParam      => NULL_PTR,
                                          dwExStyle    => 0,
                                          dwStyle      => WS_VISIBLE or BS_GROUPBOX or WS_CHILD);
      Assert (Input_Group_Box);
      Set_Font (Input_Group_Box, Font_Buttons);
      Set_Text (Input_Group_Box, LABEL_INPUT_ENTRY);
    
      -- Create the input box
      Input_Box := CreateWindowExW (lpClassName  => C (NAME_EDIT),
                                    lpWindowName => null,
                                    x            => 0,
                                    y            => 0,
                                    nWidth       => 0,
                                    nHeight      => 0,
                                    hWndParent   => Console,
                                    hMenu        => 0,
                                    hInstance    => GetModuleHandleW (null),
                                    lpParam      => NULL_PTR,
                                    dwExStyle    => 0,
                                    dwStyle      => WS_VISIBLE or WS_BORDER or ES_MULTILINE or WS_CHILD);
      Assert (Input_Box);
      Set_Font (Input_Box, Font_Text_Box);
    
      -- Create the buttons
      for I in Buttons'Range loop
        Buttons (I) := CreateWindowExW (lpClassName  => C (NAME_BUTTON),
                                        lpWindowName => null,
                                        nWidth       => 0,
                                        nHeight      => 0,
                                        hWndParent   => Console,
                                        hMenu        => Int_Ptr (Int_C (I) + IDENTIFIER_START),
                                        hInstance    => GetModuleHandleW (null),
                                        lpParam      => NULL_PTR,
                                        dwExStyle    => 0,
                                        dwStyle      => WS_VISIBLE or WS_CHILD,
                                        y            => 0,
                                        x            => 0);
        Assert (Buttons (I));
        Set_Font (Buttons (I), Font_Buttons);
        Set_Text (Buttons (I), Localize (CONSOLE_BUTTONS (I).Message));
      end loop;
    
      -- Force the console window into the foreground
      Ignore (ShowWindow (Console, SW_SHOWMINIMIZED)); -- "The fact that this works is a major windows bug, good find!"
      Ignore (ShowWindow (Console, SW_RESTORE));
      Assert (SetFocus (Input_Box));
    
      -- Message and output update loop
      Outter: while not Close_Console loop
    
        -- Pump the message loop
        while PeekMessageW (Message'Unchecked_Access, Console, 0, 0, PM_REMOVE) /= 0 loop
        
          -- Preprocess certain messages
          Do_Skip_Message := False;
          case Message.message is
          
            -- End the console
            when WM_QUIT => exit Outter;
    
            -- Scale mouse scrolling
            when WM_MOUSEWHEEL =>
              if GetFocus /= Output_Box then
                for I in 1..Current_Lines / Int_64_Natural (SCROLL_FACTOR) loop 
                   Ignore (SendMessageW (hWnd   => Output_Box,
                                         Msg    => WM_VSCROLL, 
                                         lParam => 0,
    
                                         -- Figure out the direction of the scroll
                                         wParam => (if To_Int_16_Signed (Int_16_Unsigned (Shift_Right (Int_64_Unsigned (Message.wParam)
                                                    and 16#0000_0000_FFFF_0000#, 16))) / MOUSE_WHEEL_DELTA < 0 then 1 else 0)));
                end loop;
              end if;
    
            -- Capture paste events (Ctrl + V) into the input box and change focus
            when WM_KEYDOWN =>
              if GetFocus = Output_Box then Assert (HideCaret (Output_Box)); end if;
              Do_Process_Character := True;
              if Int_16_Unsigned_C (Message.wParam) = VK_V_KEY
                and then (GetKeyState (Int_C (VK_CONTROL)) and 16#8000#) > 0 -- Is Ctrl pressed?
                and then GetFocus /= Input_Box
              then
                Input_Entry (Input_Entry & Paste);
                Set_Text (Input_Box, Input_Entry);
              end if;
    
            -- Deal with text input
            when WM_CHAR =>
              if Do_Process_Character then
                Current_Input := Char'Val (Int (Message.wParam));
                Do_Process_Character := False;
    
                -- Capture text input and put it in the entry box
                if not Is_Control (Current_Input) then
                  Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Input_Buffer'Length), To_Int_Ptr (Input_Buffer'Address)));
                  Input_Entry (S (Input_Buffer) & Current_Input);
                  if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;
    
                -- Handle a command submission
                elsif Current_Input = To_Char_16 (ASCII.CR) then
                  Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Input_Buffer'Length), To_Int_Ptr (Input_Buffer'Address)));
                  Input_Entry (S (Input_Buffer));
                  if Input_Entry /= NULL_STR then
                    Line (Input_Entry);
                    if Input_Entry /= NULL_STR then
                      Submit (Input_Entry);
                      Input_Entry (NULL_STR);
                      Set_Text (Input_Box, NULL_STR);
                    end if;
                  end if;
                  Do_Skip_Message := True;
                  
                -- Handle backspace by removing the last character from the entry box
                elsif Current_Input = To_Char_16 (ASCII.BS) and Input_Entry /= NULL_STR then
                  Input_Entry (Head (Input_Entry, Str (Input_Entry)'Length - 1));
                  if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;
    
                -- Ignore tabs
                elsif Current_Input = To_Char_16 (ASCII.HT) then Do_Skip_Message := True; end if;
              end if;
          when others => null; end case;
          
          -- Update input entry from other tasks ???
    
          -- Pass on the message if we need to
          if not Do_Skip_Message then
            Ignore (TranslateMessage (Message'Unchecked_Access));
            Ignore (DispatchMessageW (Message'Unchecked_Access));
          end if;
        end loop;
        
        -- Hide the cursor in the output box
        if GetFocus = Output_Box then Ignore (HideCaret (Output_Box)); end if;
    
        -- Handle new output
        if Lines /= Current_Lines then
    
          -- Record the current selection, scrollbar position, and log snapshot
          Current_Log   := Log;
          Current_Lines := Lines;
          Current_Line  := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
          Is_At_Bottom  := Scrollbar_At_Bottom;
          
          -- Prepare for repaint
          Ignore (SendMessageW (Output_Box, WM_SETREDRAW, 0, 0));
          
          -- Save user selection
          Ignore (SendMessageW (Output_Box, EM_GETSEL, To_Int_Ptr (Start_Selection'Address), To_Int_Ptr (End_Selection'Address)));
          Start_Selection := Start_Selection - Int_C (Cutoff);
          End_Selection   := End_Selection   - Int_C (Cutoff);
          
          -- Trim the log output if our buffer overflows
          if Length (Current_Log) > Input_Buffer'Length then -- This needs cleanup !!!
            Cutoff       := Int_64_Natural (Index (Current_Log, To_Str (ASCII.CR), Input_Buffer'Length) + 1);
            Cutoff_Lines := Int_64_Natural (Count (Head (Current_Log, Natural (Cutoff)), To_Str (ASCII.CR)));
            Current_Log  := Tail (Current_Log, Length (Current_Log) - Natural (Cutoff));
          end if;
          
          -- Set output text and user selection
          Set_Text (Output_Box, S (Current_Log));
          Ignore (SendMessageW (Output_Box, EM_SETSEL, Int_Ptr (Start_Selection) + Int_Ptr (Cutoff), Int_Ptr (End_Selection) + Int_Ptr (Cutoff)));
    
          -- If the scroll bar was at the bottom, make sure it stays there          
          if Is_At_Bottom then
            Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_ENDSCROLL, 0));
            Current_Line := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
          else
            for I in 1..Current_Line - Int_C (Cutoff_Lines) loop Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_LINEDOWN, 0)); end loop;
          end if;
     
          -- Repaint
          Assert (SendMessageW (Output_Box, WM_SETREDRAW, 1, 0));
        end if;
        
        -- Give up some cycles
        delay POLLING_DURATION - (Clock - Last_Time); Last_Time := Clock;
      end loop Outter;
      
      -- Hide the console window then kill it
      Ignore (ShowWindow (Console, SW_HIDE));
      Assert (DestroyWindow (Console));
      Assert (UnregisterClassW (C (CONSOLE_NAME), NULL_PTR));
      Assert (ReleaseDC (GetDesktopWindow, Context));
    end;
end;
