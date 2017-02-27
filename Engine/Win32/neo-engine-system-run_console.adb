
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
separate (Neo.Engine.System) procedure Run_Console is

  -- Button settings
  type Button_State is record
      Message : Str (1..4);
      Action  : access procedure;
    end record;
  CONSOLE_BUTTONS : constant array (1..3) of Button_State := (("Save", Save_Log'access), ("Send", Send_Log'access), ("Quit", null));
  Buttons         : array (CONSOLE_BUTTONS'range) of Ptr;

  -- Constants
  DO_DISABLE_RESIZE     : constant Bool  := False;
  GROUP_BOX_SIDE_MARGIN : constant Real  := 1.2;
  FONT_GROUP_BOX_SIZE   : constant Real  := 1.2;
  IDENTIFIER_START      : constant Int   := 666;
  SCROLL_FACTOR         : constant Int_C := 500;
  FONT_CONSOLE_SIZE     : constant Int_C := -11;
  NUMBER_OF_OUTPUT_ROWS : constant Int_C := 25;
  BUTTON_HEIGHT_DLU     : constant Int_C := 14;
  BUTTON_WIDTH_DLU      : constant Int_C := 50;
  PIXELS_PER_INCH       : constant Int_C := 72;
  MARGIN_BUTTON         : constant Int_C := 4;
  MARGIN                : constant Int_C := 7;

  -- Text
  ERROR_REPORTING_URL : constant Str   := "http://www.google.com";
  LABEL_INPUT_ENTRY   : constant Str   := "Input";
  LABEL_OUTPUT        : constant Str   := "Output";
  FONT_CONSOLE        : aliased  Str_C := To_Str_C ("Courier New");
  FONT_DIALOG         : aliased  Str_C := To_Str_C ("Tahoma");
  NAME_BUTTON         : aliased  Str_C := To_Str_C ("Button");
  NAME_GROUP          : aliased  Str_C := To_Str_C ("Group");
  NAME_EDIT           : aliased  Str_C := To_Str_C ("Edit");

  -- Variables
  Current_Input :         Char_16          := NULL_CHAR_16;
  Current_Log   :         Str_Unbound      := NULL_STR_UNBOUND;
  Current_Lines :         Int_64_Natural   := 0;
  Message       : aliased MSG              := (others => <>);
  Buffer        : aliased Str_C (1..2**9)  := (others => NULL_CHAR_16_C); -- Buffer size is arbitrary !!!
  Metrics       : aliased NONCLIENTMETRICS := (others => <>);
  Class         : aliased WNDCLASSEX       := (others => <>);
  Text_Metric   : aliased TEXTMETRIC       := (others => <>);
  Rectangle     : aliased RECT             := (others => <>);

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
  Dialog_Base_Unit_Height, Dialog_Base_Unit_Width,
  Text_Box_Font_Height, Text_Box_Font_Width,
  Start_Selection, End_Selection,
  Margin_Group_Top, Margin_Group,
  Current_Height, Current_Width,
  Console_Height, Console_Width,
  Button_Height, Button_Width,
  Right_Count, Left_Count,
  Message_Box_Font_Height,
  Number_Of_Lines,
  Y, Current_Line : aliased Int_C := 0;

  -- Flags
  Was_At_Bottom,
  Button_Font_Set,
  Do_Process_Character,
  Was_At_Minimum_Width,
  Was_At_Minimum_Height,
  Do_Skip_Message : Bool := False;
  Is_At_Bottom, Is_First_Time : Bool := True;

  -- Message procedures for convience
  procedure Set_Text (Handle : Ptr; Text : Str) is begin Assert (SendMessageW (Handle, WM_SETTEXT, 0, To_Int_Ptr (To_Str_C (Text)'address))); end;
  procedure Set_Font (Handle, Font : Ptr)       is begin Ignore (SendMessageW (Handle, WM_SETFONT, 0, To_Int_Ptr (Font))); end;

  -- Return true if the output text window's scrollbar is at the bottom
  function Scrollbar_At_Bottom return Bool is
    Scroll_Info : aliased SCROLLINFO := (others => <>);
    begin
      Ignore (GetScrollInfo (Output_Box, 1, Scroll_Info'unchecked_access));
      return Scroll_Info.nPos + Number_of_Lines >= Scroll_Info.nMax;
    end;

  -- A single function for setting all GUI sizing globals
  procedure Set_Sizes is
    Minimum_Width : Int_C := 0;
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
      Console_Width     := (MARGIN * Dialog_Base_Unit_Width + Margin_Group + Border_Width) * 2 + Output_Box_Width;
      Console_Height    := (MARGIN * Dialog_Base_Unit_Height) * 4 + (Border_Height + Margin_Group + Margin_Group_Top) * 2
                           + Output_Box_Height + Input_Box_Height + BUTTON_HEIGHT + GetSystemMetrics (SM_CYSIZE);
      if Buttons'length > 0 then
        Minimum_Width := (Buttons'length - 1) * MARGIN_BUTTON * Dialog_Base_Unit_Width + MARGIN * 2 * Dialog_Base_Unit_Width + BUTTON_WIDTH * Buttons'length + Border_Width * 2;
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

  -- Create and set fonts for the output box
  procedure Create_Fonts is 
    Font_Size : aliased SIZE  := (others => <>);
    Some_BS   : aliased Str_C := To_Str_C ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
    begin
      if not Button_Font_Set then
        Font_Buttons := CreateFontW (nHeight            => Message_Box_Font_Height,
                                     nWidth             => 0,
                                     nEscapement        => 0,
                                     nOrientation       => 0,
                                     fnWeight           => FW_LIGHT,
                                     fdwItalic          => 0,
                                     fdwUnderline       => 0,
                                     fdwStrikeOut       => 0,
                                     fdwCharSet         => DEFAULT_CHARSET,
                                     fdwOutputPrecision => OUT_DEFAULT_PRECIS,
                                     fdwClipPrecision   => CLIP_DEFAULT_PRECIS,
                                     fdwQuality         => DEFAULT_QUALITY,
                                     fdwPitchAndFamily  => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
                                     lpszFace           => FONT_DIALOG'unchecked_access);
        Assert (Font_Buttons);
      end if;
      Font_Text_Box := CreateFontW (nHeight            => FONT_CONSOLE_SIZE,
                                    nWidth             => 0,
                                    nEscapement        => 0,
                                    nOrientation       => 0,
                                    fnWeight           => FW_LIGHT,
                                    fdwItalic          => 0,
                                    fdwUnderline       => 0,
                                    fdwStrikeOut       => 0,
                                    fdwCharSet         => DEFAULT_CHARSET,
                                    fdwOutputPrecision => OUT_DEFAULT_PRECIS,
                                    fdwClipPrecision   => CLIP_DEFAULT_PRECIS,
                                    fdwQuality         => DEFAULT_QUALITY,
                                    fdwPitchAndFamily  => FONT_FAMILY_MODERN or FONT_FIXED_PITCH,
                                    lpszFace           => FONT_CONSOLE'unchecked_access);
      Assert (Font_Text_Box);
      Ignore (SelectObject (Context, Font_Text_Box)); -- ???
      Assert (GetTextMetricsW (Context, Text_Metric'unchecked_access));
      Text_Box_Font_Height := Text_Metric.tmHeight;
      Assert (GetTextExtentPoint32W (hdc      => Context,
                                     lpString => Some_BS'unchecked_access,
                                     c        => 52,
                                     lpSize   => Font_Size'unchecked_access));
      Text_Box_Font_Width := Int_C ((Font_Size.cx / 26 + 1) / 2);
      Ignore (SelectObject (Context, Font_Buttons)); -- ???
      Assert (GetTextMetricsW (Context, Text_Metric'unchecked_access));
    end;

  -- Set the global variable Dialog_Base_Unit_Width used in calculating all UI proportions by spawning a message box and measuring it
  procedure Get_Sizes_From_Message_Box is
    Hook : Ptr := NULL_PTR;

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
      Class_Name  : aliased Str_C (1..256) := (others => NULL_CHAR_C);
      Window_Text : aliased Str_C (1..256) := (others => NULL_CHAR_C);
      Window      :         Ptr            := To_Ptr (Int_Ptr (wParam));

      -- Once we find the message box we have to look through its children to find a button
      function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C with Convention => Stdcall;
      function EnumWindowsProc (hwnd : Ptr; lParam : Int_Ptr) return Int_C is
        Rectangle : aliased RECT := (others => <>);
        begin

          -- Find a message box button to calculate dialog base units
          Assert (GetClassNameW (hwnd, Class_Name'Unrestricted_Access, Class_Name'length));
          Ignore (GetWIndowTextW (hwnd, Window_Text'Unrestricted_Access, Window_Text'length));
          if Dialog_Base_Unit_Width = 0 and "Button" = To_Str (Class_Name) then

            -- Do the Microsoft recommended math and measurements: https://support.microsoft.com/en-us/kb/125681
            Assert (GetWindowRect (hwnd, Rectangle'unchecked_access));
            Button_Width            := (Rectangle.right  - Rectangle.left);
            Button_Height           := (Rectangle.bottom - Rectangle.top);
            Ignore (SelectObject (Context, To_Ptr (Int_Ptr (To_Int_32_Unsigned (SendMessageW (hwnd, WM_GETFONT, 0, 0)))))); -- ???
            Assert (GetTextMetricsW (Context, Text_Metric'unchecked_access));
            Message_Box_Font_Height := Text_Metric.tmHeight;
            Dialog_Base_Unit_Width  := Button_Width  / BUTTON_WIDTH_DLU;
            Dialog_Base_Unit_Height := Button_Height / BUTTON_HEIGHT_DLU;
          end if;
          return 1;
        end;
      begin

        -- Find the newly created message-box window 
        Assert (GetClassNameW  (Window, Class_Name'Unrestricted_Access,  Class_Name'length));
        Ignore (GetWIndowTextW (Window, Window_Text'Unrestricted_Access, Window_Text'length));
        if nCode = HCBT_ACTIVATE and To_Str (Class_Name) = CLASS_NAME_DIALOG and To_Str (Window_Text) = To_Str (CONSOLE_NAME) then

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

      -- Create a message box and a callback that checks newly created windows, then wait for Dialog_Base_Unit_Width to be set
      Hook := SetWindowsHookExW (WH_CBT, CBTProc'address, NULL_PTR, GetCurrentThreadId);
      Ignore (MessageBoxW (NULL_PTR, GAME_NAME'access, CONSOLE_NAME'unchecked_access, 0));
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
        when WM_CLOSE      => PostQuitMessage (0); return 1;
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

            -- Query user inputed sizes then calculate the new sizes with Set_Sizes
            Ignore (SetWindowLongW (Console, GWL_EXSTYLE, WS_EX_COMPOSITED));
            Assert (GetWindowRect  (Console, Rectangle'unchecked_access));
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

            -- Keep the scrollbar at the bottom if it was there before resizing
            if Is_At_Bottom then Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_ENDSCROLL, 0)); end if;
            Assert (RedrawWindow (hwnd, null, NULL_PTR, 1));
            Ignore (SetWindowLongW (Console, GWL_EXSTYLE, 0));
            return 1;
          end if;
      when others => null; end case;
      return DefWindowProcW (hwnd, uMsg, wParam, lParam);
    end;

-- Start of Run_Console
begin 

  -- Load the context which is used globally
  Context := GetDC (GetDesktopWindow);
  Assert (Context);

  -- Load the icon
  Icon := LoadImageW (hinst     => GetModuleHandleNULL, -- Loads the icon nicely for the Aero theme, but on "classic" theme it looks pixelated on the title bar
                      lpszName  => WIN32_PATH_ICON'access,
                      uType     => IMAGE_ICON,
                      cxDesired => 0,
                      cyDesired => 0,
                      fuLoad    => LR_LOADFROMFILE or LR_DEFAULTSIZE);
  if Icon = NULL_PTR then Icon := LoadIconW (GetModuleHandleNULL, IDI_APPLICATION); end if;

  -- Set global font and size variables
  if SystemParametersInfoW (SPI_GETNONCLIENTMETRICS, 0, Metrics'address, Metrics.cbSize) = 0 then Button_Font_Set := False; -- Fails under XP....
  else Font_Buttons := CreateFontIndirectW (Metrics.lfMessageFont'unchecked_access); Assert (Font_Buttons); end if;
  Get_Sizes_From_Message_Box;
  Create_Fonts;      
  Set_Sizes;

  -- Create the main console window
  Class := (lpfnWndProc   => WindowProc'address,
            hInstance     => GetModuleHandleNULL,
            hIconSm       => Icon,
            hIcon         => Icon,
            hCursor       => LoadCursorW (NULL_PTR, GENERIC_CURSOR),
            hbrBackground => COLOR_WINDOW,
            lpszClassName => To_Ptr_Const_Char_16_C (CONSOLE_NAME),
            others        => <>);
  Assert (RegisterClassExW (Class'unchecked_access));
  Console := CreateWindowExW (lpClassName  => CONSOLE_NAME'unchecked_access,
                              lpWindowName => CONSOLE_NAME'unchecked_access,
                              x            => GetDeviceCaps (Context, SM_CYDLGFRAME) / 2 - Console_Width  / 2,
                              y            => GetDeviceCaps (Context, SM_CXHTHUMB)   / 2 - Console_Height / 2,
                              nWidth       => Console_Width,
                              nHeight      => Console_Height,
                              hWndParent   => NULL_PTR,
                              hMenu        => 0,
                              hInstance    => GetModuleHandleNULL,
                              lpParam      => NULL_PTR,
                              dwExStyle    => 0,
                              dwStyle      => WS_MINIMIZE or WS_SIZEBOX or WS_SYSMENU or WS_BORDER or WS_MAXIMIZEBOX or WS_MINIMIZEBOX);
  Assert (Console);

  -- Create the output box border
  Output_Group_Box := CreateWindowExW (lpClassName  => NAME_BUTTON'unchecked_access,
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
                                       dwStyle      => WS_VISIBLE or BS_GROUPBOX or WS_CHILD);
  Assert (Output_Group_Box);
  Set_Font (Output_Group_Box, Font_Buttons);
  Set_Text (Output_Group_Box, Localize (LABEL_OUTPUT));

  -- Create the output box
  Output_Box := CreateWindowExW (lpClassName  => NAME_EDIT'unchecked_access,
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
                                 dwStyle      => WS_VSCROLL or WS_VISIBLE or WS_BORDER or ES_MULTILINE or ES_READONLY or WS_CHILD);
  Assert (Output_Box);
  Set_Font (Output_Box, Font_Text_Box);

  -- Create the input box border
  Input_Group_Box := CreateWindowExW (lpClassName  => NAME_BUTTON'unchecked_access,
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
                                      dwStyle      => WS_VISIBLE or BS_GROUPBOX or WS_CHILD);
  Assert (Input_Group_Box);
  Set_Font (Input_Group_Box, Font_Buttons);
  Set_Text (Input_Group_Box, LABEL_INPUT_ENTRY);

  -- Create the input box
  Input_Box := CreateWindowExW (lpClassName  => NAME_EDIT'unchecked_access,
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
                                dwStyle      => WS_VISIBLE or WS_BORDER or ES_MULTILINE or WS_CHILD);
  Assert (Input_Box);
  Set_Font (Input_Box, Font_Text_Box);

  -- Create the buttons
  for I in Buttons'range loop
    Buttons (I) := CreateWindowExW (lpClassName  => NAME_BUTTON'unchecked_access,
                                    lpWindowName => null,
                                    nWidth       => 0,
                                    nHeight      => 0,
                                    hWndParent   => Console,
                                    hMenu        => Int_Ptr (I + IDENTIFIER_START),
                                    hInstance    => GetModuleHandleNULL,
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
  Assert (SetFocus (Output_Box));

  -- Main loop
  while Message.message /= WM_QUIT loop

    -- Handle new output
    if Length (Current_Log) /= Log'length then

      -- Set the text in the output box
      Is_At_Bottom := Scrollbar_At_Bottom;
      Current_Log  := To_Str_Unbound (Log);
      Current_Line := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
      Ignore (SendMessageW (Output_Box, EM_GETSEL, To_Int_Ptr (Start_Selection'address), To_Int_Ptr (End_Selection'address)));
      Ignore (SendMessageW (Output_Box, WM_SETREDRAW, 0, 0));
      Set_Text (Output_Box, Log);
      Ignore (SendMessageW (Output_Box, EM_SETSEL, Int_Ptr (Start_Selection), Int_Ptr (End_Selection)));

      -- If the scroll bar was at the bottom, make sure it stays there            
      if Is_At_Bottom or Is_First_Time then
        Is_First_Time := False;
        Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_ENDSCROLL, 0));
        Current_Line := SendMessageW (Output_Box, EM_GETFIRSTVISIBLELINE, 0, 0);
      else
        for I in 1..Current_Line loop Ignore (SendMessageW (Output_Box, WM_VSCROLL, SB_LINEDOWN, 0)); end loop;
      end if;
      Current_Lines := Lines;
      Assert (SendMessageW (Output_Box, WM_SETREDRAW, 1, 0));
    end if;

    -- Pump the message loop
    if PeekMessageW (Message'unchecked_access, Console, 0, 0, PM_REMOVE) /= 0 then
      case Message.message is

        -- Scale mouse scrolling
        when WM_MOUSEWHEEL =>
          if GetFocus /= Output_Box then
            for I in 1..Current_Lines / SCROLL_FACTOR loop 
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
            Current_Input := Char_16'val (Int (Message.wParam));
            Do_Process_Character := False;

            -- Capture text input and put it in the entry box
            if not Is_Control (Current_Input) then
              Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Buffer'length), To_Int_Ptr (Buffer'address)));
              Input_Entry (To_Str (Buffer) & Current_Input);
              if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;

            -- Handle a command submission
            elsif Current_Input = To_Char_16 (ASCII.CR) then
              Ignore (SendMessageW (Input_Box, WM_GETTEXT, Int_Ptr (Buffer'length), To_Int_Ptr (Buffer'address)));
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

            -- Handle backspace by removing the last character from the entry box
            --elsif Current_Input = To_Char_16 (ASCII.BS) and Input_Entry /= NULL_STR then
            --  Input_Entry (Input_Entry (1..Input_Entry'Last - 1));
            --  if GetFocus /= Input_Box then Set_Text (Input_Box, Input_Entry); end if;

            -- Ignore tabs
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
  Assert (UnregisterClassW (CONSOLE_NAME'access, NULL_PTR));
  Assert (ReleaseDC (GetDesktopWindow, Context));
end;