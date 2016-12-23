
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

with Ada.Unchecked_Conversion;

-- Custom binding to the infamous Win32 API: http://web.archive.org/web/20150115092801/http://msdn.microsoft.com/en-us/library/windows/apps/dn424765.aspx
package Neo.API.Win32 is

  -----------
  -- Types --
  -----------

  -- UINT
  -- BYTE
  -- SIZE_T
  -- HGLOBAL
  -- LPVOID
  -- BOOL
  -- HWND
  -- HANDLE
  -- LPMEMORYSTATUSEX
  -- LPCTSTR
  -- PULARGE_INTEGER
  -- DWORD                        Int_32_Unsigned_C
  -- PFLASHWINFO
  -- LPTSTR
  -- LPSECURITY_ATTRIBUTES
  -- LPSTARTUPINFO
  -- LPPROCESS_INFORMATION
  -- HICON
  -- HINSTANCE
  -- HHOOK
  -- HOOKPROC
  -- LPOSVERSIONINFO
  -- HMODULE
  -- LONG 
  -- SHORT
  -- WPARAM
  -- LPARAM

  -----------
  -- Flags --
  -----------

  -- https://blogs.msdn.microsoft.com/openspecification/2010/04/01/about-the-access_mask-structure/
  GENERIC_READ  : constant Int_32_Unsigned_C := 16#8000_0000#; -- 0x00000000
  GENERIC_WRITE : constant Int_32_Unsigned_C := 16#4000_0000#; -- 0x00000000

  -- https://web.archive.org/web/20161024170359/https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
  FILE_SHARE_READ  : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0x00000000
  FILE_SHARE_WRITE : constant Int_32_Unsigned_C := 16#0000_0002#; -- 0x00000000
  OPEN_EXISTING    : constant Int_32_Unsigned_C := 3;             -- 0

  -- https://web.archive.org/web/20160525074306/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx
  PM_REMOVE : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0x0000

  -- https://web.archive.org/web/20141228152842/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644977(v=vs.85).aspx
  WH_CBT        : constant Int_C := 5; -- 0
  HCBT_ACTIVATE : constant Int_C := 5; -- 0
 
  -- https://web.archive.org/web/20160605071718/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505(v=vs.85).aspx
  MB_SYSTEMMODAL     : constant Int_32_Unsigned_C := 16#0000_1000#; -- 0x00000000L
  MB_ICONINFORMATION : constant Int_32_Unsigned_C := 16#0000_0040#; -- 0x00000000L
  MB_ICONWARNING     : constant Int_32_Unsigned_C := 16#0000_0030#; -- 0x00000000L
  MB_ICONERROR       : constant Int_32_Unsigned_C := 16#0000_0010#; -- 0x00000000L
  MB_OK              : constant Int_32_Unsigned_C := 16#0000_0000#; -- 0x00000000L
  MB_YESNO           : constant Int_32_Unsigned_C := 16#0000_0004#; -- 0x00000000L
  MB_OKCANCEL        : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0x00000000L
  MB_RETRYCANCEL     : constant Int_32_Unsigned_C := 16#0000_0005#; -- 0x00000000L
  IDOK               : constant Int_C             := 1;             -- 0
  IDRETRY            : constant Int_C             := 4;             -- 0
  IDYES              : constant Int_C             := 6;             -- 0

  -- https://web.archive.org/web/20140209214910/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632647(v=vs.85).aspx
  WMSZ_BOTTOMRIGHT : constant Int_Ptr := 16#0000_0008#; -- 0
  WMSZ_BOTTOMLEFT  : constant Int_Ptr := 16#0000_0007#; -- 0
  WMSZ_TOPRIGHT    : constant Int_Ptr := 16#0000_0005#; -- 0
  WMSZ_TOPLEFT     : constant Int_Ptr := 16#0000_0004#; -- 0
  WMSZ_BOTTOM      : constant Int_Ptr := 16#0000_0006#; -- 0
  WMSZ_RIGHT       : constant Int_Ptr := 16#0000_0002#; -- 0
  WMSZ_LEFT        : constant Int_Ptr := 16#0000_0001#; -- 0
  WMSZ_TOP         : constant Int_Ptr := 16#0000_0003#; -- 0

  -- http://web.archive.org/web/20160808214158/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724385(v=vs.85).aspx
  SM_CYVTHUMB : constant Int_C := 9;  -- 0 
  SM_CYSIZE   : constant Int_C := 31; -- 0
  SM_CXFRAME  : constant Int_C := 32; -- 0
  SM_CYFRAME  : constant Int_C := 33; -- 0
 
  -- https://web.archive.org/web/20140714222448/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx
  RI_MOUSE_LEFT_BUTTON_DOWN   : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0x0000
  RI_MOUSE_LEFT_BUTTON_UP     : constant Int_32_Unsigned_C := 16#0000_0002#; -- 0x0000
  RI_MOUSE_RIGHT_BUTTON_DOWN  : constant Int_32_Unsigned_C := 16#0000_0004#; -- 0x0000
  RI_MOUSE_RIGHT_BUTTON_UP    : constant Int_32_Unsigned_C := 16#0000_0008#; -- 0x0000
  RI_MOUSE_MIDDLE_BUTTON_UP   : constant Int_32_Unsigned_C := 16#0000_0020#; -- 0x0000
  RI_MOUSE_MIDDLE_BUTTON_DOWN : constant Int_32_Unsigned_C := 16#0000_0010#; -- 0x0000
  RI_MOUSE_HORIZONTAL_WHEEL   : constant Int_32_Unsigned_C := 16#0000_0800#; -- 0x0000
  RI_MOUSE_VERTICAL_WHEEL     : constant Int_32_Unsigned_C := 16#0000_0400#; -- 0x0000
  RI_MOUSE_BUTTON_4_DOWN      : constant Int_32_Unsigned_C := 16#0000_0040#; -- 0x0000
  RI_MOUSE_BUTTON_4_UP        : constant Int_32_Unsigned_C := 16#0000_0080#; -- 0x0000
  RI_MOUSE_BUTTON_5_DOWN      : constant Int_32_Unsigned_C := 16#0000_0100#; -- 0x0000
  RI_MOUSE_BUTTON_5_UP        : constant Int_32_Unsigned_C := 16#0000_0200#; -- 0x0000   
 
  -- https://web.archive.org/web/20140321085736/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645571(v=vs.85).aspx
  RIM_TYPEKEYBOARD : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0
  RIM_TYPEMOUSE    : constant Int_32_Unsigned_C := 16#0000_0000#; -- 0
    
  -- https://web.archive.org/web/20160821131741/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645597%28v=vs.85%29.aspx
  RIDI_DEVICENAME    : constant Int_32_Unsigned_C := 16#2000_0007#; -- 0x00000000
  RIDI_DEVICEINFO    : constant Int_32_Unsigned_C := 16#2000_000b#; -- 0x00000000
  RIDI_PREPARSEDDATA : constant Int_32_Unsigned_C := 16#2000_0005#; -- 0x00000000

  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms633548(v=vs.85).aspx
  SW_SHOWNORMAL    : constant Int_C := 1; -- 0
  SW_HIDE          : constant Int_C := 0; -- 0
  SW_SHOWMINIMIZED : constant Int_C := 2; -- 0
  SW_RESTORE       : constant Int_C := 9; -- 0

  -- https://web.archive.org/web/20160404075807/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_gamepad(v=vs.85).aspx
  XINPUT_GAMEPAD_DPAD_UP        : constant Int_16_Unsigned_C := 16#0001#; -- 0x0000
  XINPUT_GAMEPAD_DPAD_DOWN      : constant Int_16_Unsigned_C := 16#0002#; -- 0x0000
  XINPUT_GAMEPAD_DPAD_LEFT      : constant Int_16_Unsigned_C := 16#0004#; -- 0x0000
  XINPUT_GAMEPAD_DPAD_RIGHT     : constant Int_16_Unsigned_C := 16#0008#; -- 0x0000
  XINPUT_GAMEPAD_START          : constant Int_16_Unsigned_C := 16#0010#; -- 0x0000
  XINPUT_GAMEPAD_BACK           : constant Int_16_Unsigned_C := 16#0020#; -- 0x0000
  XINPUT_GAMEPAD_LEFT_THUMB     : constant Int_16_Unsigned_C := 16#0040#; -- 0x0000
  XINPUT_GAMEPAD_RIGHT_THUMB    : constant Int_16_Unsigned_C := 16#0080#; -- 0x0000
  XINPUT_GAMEPAD_LEFT_SHOULDER  : constant Int_16_Unsigned_C := 16#0100#; -- 0x0000
  XINPUT_GAMEPAD_RIGHT_SHOULDER : constant Int_16_Unsigned_C := 16#0200#; -- 0x0000
  XINPUT_GAMEPAD_A              : constant Int_16_Unsigned_C := 16#1000#; -- 0x0000
  XINPUT_GAMEPAD_B              : constant Int_16_Unsigned_C := 16#2000#; -- 0x0000
  XINPUT_GAMEPAD_X              : constant Int_16_Unsigned_C := 16#4000#; -- 0x0000
  XINPUT_GAMEPAD_Y              : constant Int_16_Unsigned_C := 16#8000#; -- 0x0000

  -- https://web.archive.org/web/20150109064332/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645565(v=vs.85).aspx
  RIDEV_INPUTSINK  : constant Int_32_Unsigned_C := 16#0000_0100#; -- 0x00000000
  GENERIC_DESKTOP  : constant Int_16_Unsigned_C := 16#0001#;      -- 0x00 ???
  USE_RAW_KEYBOARD : constant Int_16_Unsigned_C := 16#0006#;      -- 0x00 ???
  USE_RAW_MOUSE    : constant Int_16_Unsigned_C := 16#0002#;      -- 0x00 ???

  -- 
  GET_DEVICE_HEADER              : constant Int_32_Unsigned_C := 16#1000_0005#;
  GET_DEVICE_DATA                : constant Int_32_Unsigned_C := 16#1000_0003#;
  STOP_READING_TOP_LEVEL_DEVICES : constant Int_32_Unsigned_C := 16#0000_0001#;
  KEY_MAKE_CODE_FOR_LEFT         : constant Int_16_Unsigned_C := 16#002A#;
  MOUSE_WHEEL_DELTA              : constant Int_16_Signed     := 120;

  -- http://web.archive.org/web/20160713003824/https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  VK_LBUTTON   : constant Int_16_Unsigned_C := 16#0001#; -- 0x00
  VK_OEM_CLEAR : constant Int_16_Unsigned_C := 16#00FE#; -- 0x00
  VK_V_KEY     : constant Int_16_Unsigned_C := 16#0056#; -- 0x00
  VK_CONTROL   : constant Int_16_Unsigned_C := 16#0011#; -- 0x00

  -- https://web.archive.org/web/20150109034335/http://msdn.microsoft.com/en-us/library/windows/desktop/ff468925(v=vs.85).aspx
  WM_SIZING              : constant Int_32_Unsigned_C := 16#0000_0214#; -- 0x0000
  WM_CLOSE               : constant Int_32_Unsigned_C := 16#0000_0010#; -- 0x0000
  WM_CHAR                : constant Int_32_Unsigned_C := 16#0000_0102#; -- 0x0000
  WM_CREATE              : constant Int_32_Unsigned_C := 16#0000_0001#; -- 0x0000
  WM_CTLCOLORSTATIC      : constant Int_32_Unsigned_C := 16#0000_0138#; -- 0x0000
  WM_GETMINMAXINFO       : constant Int_32_Unsigned_C := 16#0000_0024#; -- 0x0000
  WM_INPUT               : constant Int_32_Unsigned_C := 16#0000_00FF#; -- 0x0000
  WM_QUIT                : constant Int_32_Unsigned_C := 16#0000_0012#; -- 0x0000
  WM_COMMAND             : constant Int_32_Unsigned_C := 16#0000_0111#; -- 0x0000
  WM_VSCROLL             : constant Int_32_Unsigned_C := 16#0000_0115#; -- 0x0000
  WM_SETREDRAW           : constant Int_32_Unsigned_C := 16#0000_000B#; -- 0x0000 
  WM_MOUSEWHEEL          : constant Int_32_Unsigned_C := 16#0000_020A#; -- 0x0000
  WM_GETTEXT             : constant Int_32_Unsigned_C := 16#0000_000D#; -- 0x0000
  WM_KEYDOWN             : constant Int_32_Unsigned_C := 16#0000_0100#; -- 0x0000 
  WM_SYSKEYDOWN          : constant Int_32_Unsigned_C := 16#0000_0104#; -- 0x0000 
  WM_SETTEXT             : constant Int_32_Unsigned_C := 16#0000_000C#; -- 0x0000
  WM_SETFONT             : constant Int_32_Unsigned_C := 16#0000_0030#; -- 0x0000 
  WM_SIZE                : constant Int_32_Unsigned_C := 16#0000_0005#; -- 0x0000
  WM_ACTIVATE            : constant Int_32_Unsigned_C := 16#0000_0006#; -- 0x0000
  WM_SYSCOMMAND          : constant Int_32_Unsigned_C := 16#0000_0112#; -- 0x0000
  WM_CTLCOLOREDIT        : constant Int_32_Unsigned_C := 16#0000_0133#; -- 0x0000
  EM_GETSEL              : constant Int_32_Unsigned_C := 16#0000_00B0#; -- 0x0000
  EM_SETSEL              : constant Int_32_Unsigned_C := 16#0000_00B1#; -- 0x0000
  EM_GETFIRSTVISIBLELINE : constant Int_32_Unsigned_C := 16#0000_00CE#; -- 0x0000

  -- 
  CLR_INVALID : constant Int_32_Unsigned_C := 16#FFFF_FFFF#; --

  -- 
  SPI_GETNONCLIENTMETRICS : constant Int_32_Unsigned_C := 16#0000_0029#; -- 

  -- http://web.archive.org/web/20160623235135/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633589(v=vs.85).aspx
  GCLP_HCURSOR : constant Int_C := -12; -- 00

  -- http://web.archive.org/web/20160722055429/https://msdn.microsoft.com/en-us/library/windows/desktop/ms646360(v=vs.85).aspx
  SC_SCREENSAVE : constant Int_Ptr := 16#0000_F140#; -- 0x0000
  SC_KEYMENU    : constant Int_Ptr := 16#0000_F100#; -- 0x0000

  -- http://web.archive.org/web/20141224060057/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632646(v=vs.85).aspx
  SIZE_MAXIMIZED : constant Int_Ptr := 16#0000_0002#; -- 0
  SIZE_MINIMIZED : constant Int_Ptr := 16#0000_0001#; -- 0

  -- TODO Finish the renaming !!!
  WA_CLICKACTIVE : constant Int_Ptr := 16#0000_0002#; -- 0

  -- 
  WS_TOPMOST : constant Int_32_Unsigned_C := 16#0000_0008#; -- 0x00000000L
  WS_POPUP          : constant Int_32_Unsigned_C := 16#8000_0000#; -- WS_POPUP
  BS_GROUPBOX                            : constant Int_32_Unsigned_C := 16#0000_0007#; -- 
  WS_CAPTION                             : constant Int_32_Unsigned_C := 16#00C0_0000#; -- 
  WS_VSCROLL              : constant Int_32_Unsigned_C := 16#0020_0000#; -- 
  WS_EX_COMPOSITED                     : constant Int_32_Unsigned_C := 16#0200_0000#; -- 
  WS_MAXIMIZEBOX                          : constant Int_32_Unsigned_C := 16#0002_0000#; -- 
  WS_SYSMENU                        : constant Int_32_Unsigned_C := 16#0008_0000#; -- WS_SYSMENU
  WS_BORDER                     : constant Int_32_Unsigned_C := 16#0080_0000#; -- WS_BORDER

  SUBEVENT_SCROLL_BOTTOM                    : constant Int_Ptr := 16#0000_0007#;
  SUBEVENT_SCROLL_DOWN_LINE              : constant Int_Ptr := 16#0000_0001#;
  SUBEVENT_KEY_IS_RIGHT_SIDED                 : constant Int_16_Unsigned_C := 16#0002#;

  STYLE_BOX_FULLSCREEN                       : constant Int_32_Unsigned_C := 16#0001_0000#; -- WS_
  STYLE_CHILD                                : constant Int_32_Unsigned_C := 16#4000_0000#; -- WS_
  STYLE_ALIGN_TEXT_TO_LEFT                   : constant Int_32_Unsigned_C := 16#0000_0000#; -- WS_
  STYLE_MULTI_LINE                           : constant Int_32_Unsigned_C := 16#0000_0004#; -- WS_
  STYLE_NO_USER_EDITING                      : constant Int_32_Unsigned_C := 16#0000_0800#; -- WS_
  STYLE_PUSH_BUTTON                          : constant Int_32_Unsigned_C := 16#0000_0000#; -- WS_
  STYLE_ICONIC_INITIALLY                     : constant Int_32_Unsigned_C := 16#2000_0000#; -- WS_ICONIC??
  STYLE_NO_ACTIVATE                          : constant Int_32_Unsigned_C := 16#0800_0000#; -- WS_
  STYLE_VISIBLE_INITIALLY                    : constant Int_32_Unsigned_C := 16#1000_0000#; -- WS_
  STYLE_BORDER_SIZABLE                       : constant Int_32_Unsigned_C := 16#0004_0000#; -- WS_

  DATA_VERTICAL_RESOLUTION                   : constant Int_C   := 10; -- SM_CXHTHUMB
  DATA_HORIZONTAL_RESOLUTION                 : constant Int_C   := 8; -- SM_CYDLGFRAME

  MEMORY_MOVEABLE                            : constant Int_32_Unsigned_C := 16#0000_0002#;
  MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE         : constant Int_32_Unsigned_C := 16#0000_16000#;
  CLIPBOARD_UNICODE_TEXT                     : constant Int_32_Unsigned_C := 16#0000_000D#;
  MAXIMUM_PATH_LENGTH                        : constant Int_Size_C       := 32_768 - 1; -- Minus one to account for null terminator
  CLASS_NAME_DIALOG                          : constant Str_16           := "#32770";
  GENERIC_ICON                               : constant Int_Ptr      := 16#7F00#;
  ERROR_INSUFFICIENT_BUFFER                  : constant Int_32_Unsigned_C := 16#0000_007A#;
  GENERIC_CURSOR                             : constant Int_Ptr      := 16#7F00#;
  NO_ERROR                                   : constant Int_32_Unsigned_C := 16#0000_0000#;
  SET_WINDOW_STYLE                           : constant Int_C   := -16;
  FLASH_CONTINUOUSLY                         : constant Int_32_Unsigned_C := 16#0000_0004#;
  FLASH_END                                  : constant Int_32_Unsigned_C := 16#0000_0000#;
  LOAD_FROM_FILE                             : constant Int_32_Unsigned_C := 16#0000_0010#;
  LOAD_ICO                                   : constant Int_32_Unsigned_C := 16#0000_0001#;
  LOAD_CUR                                   : constant Int_32_Unsigned_C := 16#0000_0002#;
  DEFAULT_ICON_SIZE                          : constant Int_32_Unsigned_C := 16#0000_0040#;
  MESSAGE_SET_ICON                           : constant Int_32_Unsigned_C := 16#0000_0080#;
  WTF                         : constant Int_C := -1;
  WTF2                       : constant Int_C := -2;
  INSERT_ON_TOP_OF_EVERYTHING : constant Ptr := To_Ptr(Int_Ptr(To_Int_32_Unsigned_C(WTF)));
  REMOVE_ON_TOP_OF_EVERYTHING : constant Ptr := To_Ptr(Int_Ptr(To_Int_32_Unsigned_C(WTF2)));
  BRUSH_GRAY                                 : constant Int_Ptr      := 16#0011#;
  FONT_WEIGHT_LIGHT                          : constant Int_C   := 300;
  DEFAULT_CHARACTER_SET                      : constant Int_32_Unsigned_C := 16#0000_0001#;
  FONT_OUT_DEFAULT_PRECISION                 : constant Int_32_Unsigned_C := 16#0000_0000#;
  FONT_CLIP_DEFAULT_PRECISION                : constant Int_32_Unsigned_C := 16#0000_0000#;
  FONT_DEFAULT_QUALITY                       : constant Int_32_Unsigned_C := 16#0000_0000#;
  FONT_FAMILY_MODERN                         : constant Int_32_Unsigned_C := 16#0000_0030#;
  FONT_FIXED_PITCH                           : constant Int_32_Unsigned_C := 16#0000_0001#;
  MESSAGE_GET_FONT                           : constant Int_32_Unsigned_C := 16#0000_0031#;
  SET_WINDOW_STYLE_EXTRA                     : constant Int_C   := -20;
  BRUSH_WINDOW                               : constant Int_Ptr      := 16#0005#; -- COLOR_WINDOW

  ---------------
  -- Callbacks --
  ---------------

  -- https://web.archive.org/web/20141228152842/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644977(v=vs.85).aspx
  type HOOKPROC is access function (nCode  : Int_C;   -- int    
                                    wParam : Int_Ptr; -- WPARAM 
                                    lParam : Int_Ptr) -- LPARAM 
                                    return Int_Ptr    -- LRESULT 
                                    with Convention => Stdcall;

  -- https://web.archive.org/web/20160516212433/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633573(v=vs.85).aspx
  type WNDPROC is access function (hwnd   : Ptr;               -- HWND   
                                   uMsg   : Int_32_Unsigned_C; -- UINT   
                                   wParam : Int_Ptr;           -- WPARAM 
                                   lParam : Int_Ptr)           -- LPARAM 
                                   return Int_Ptr              -- LRESULT 
                                   with Convention => Stdcall;

  -- https://web.archive.org/web/20140625144949/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633498(v=vs.85).aspx
  type WNDENUMPROC is access function (hwnd   : Ptr;     -- HWND   
                                       lParam : Int_Ptr) -- LPARAM 
                                       return Int_C      -- BOOL 
                                       with Convention => Stdcall;

  -- https://web.archive.org/web/20150109225815/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145061(v=vs.85).aspx
  type Ptr_RECT;
  type MONITORENUMPROC is access function (hMonitor    : Ptr;         -- HMONITOR 
                                           hdcMonitor  : Ptr;         -- HDC      
                                           lprcMonitor : Ptr_RECT;    -- LPRECT   
                                           dwData      : Ptr_Int_Ptr) -- LPARAM   
                                           return Int_C               -- BOOL 
                                           with Convention => Stdcall;

  ----------------
  -- Structures --
  ----------------
    
  -- https://web.archive.org/web/20120511143301/http://msdn.microsoft.com/en-us/library/windows/desktop/ms679348(v=vs.85).aspx
  type FLASHWINFO is record
      cbSize    : Int_32_Unsigned_C := FLASHWINFO'Size / Byte'Size; -- UINT  
      hwnd      : Ptr;               -- HWND  
      dwFlags   : Int_32_Unsigned_C; -- DWORD 
      uCount    : Int_32_Unsigned_C; -- UINT  
      dwTimeout : Int_32_Unsigned_C; -- DWORD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20150114021053/http://msdn.microsoft.com/en-us/library/windows/desktop/ms686331(v=vs.85).aspx
  type STARTUPINFO is record
      cb              : Int_32_Unsigned_C;    -- DWORD  
      lpReserved      : Ptr_Str_16_C;         -- LPTSTR 
      lpDesktop       : Ptr_Str_16_C;         -- LPTSTR 
      lpTitle         : Ptr_Str_16_C;         -- LPTSTR 
      dwX             : Int_32_Unsigned_C;    -- DWORD
      dwY             : Int_32_Unsigned_C;    -- DWORD
      dwXSize         : Int_32_Unsigned_C;    -- DWORD
      dwYSize         : Int_32_Unsigned_C;    -- DWORD
      dwXCountChars   : Int_32_Unsigned_C;    -- DWORD
      dwYCountChars   : Int_32_Unsigned_C;    -- DWORD
      dwFillAttribute : Int_32_Unsigned_C;    -- DWORD
      dwFlags         : Int_32_Unsigned_C;    -- DWORD
      wShowWindow     : Int_16_Unsigned_C;    -- WORD   
      cbReserved2     : Int_16_Unsigned_C;    -- WORD   
      lpReserved2     : Ptr_Int_8_Unsigned_C; -- LPBYTE 
      hStdInput       : Ptr;                  -- HANDLE
      hStdOutput      : Ptr;                  -- HANDLE
      hStdError       : Ptr;                  -- HANDLE
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160302060800/https://msdn.microsoft.com/en-us/library/windows/desktop/ms684873(v=vs.85).aspx
  type PROCESS_INFORMATION is record
      hProcess    : Ptr;               -- HANDLE 
      hThread     : Ptr;               -- HANDLE 
      dwProcessId : Int_32_Unsigned_C; -- DWORD  
      dwThreadId  : Int_32_Unsigned_C; -- DWORD  
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160611121457/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724833(v=vs.85).aspx
  type OSVERSIONINFOEX is record
      dwOSVersionInfoSize : Int_32_Unsigned_C; -- DWORD 
      dwMajorVersion      : Int_32_Unsigned_C; -- DWORD 
      dwMinorVersion      : Int_32_Unsigned_C; -- DWORD 
      dwBuildNumber       : Int_32_Unsigned_C; -- DWORD 
      dwPlatformId        : Int_32_Unsigned_C; -- DWORD 
      szCSDVersion        : Str_16 (1..128);   -- TCHAR[128]
      wServicePackMajor   : Int_16_Unsigned_C; -- WORD
      wServicePackMinor   : Int_16_Unsigned_C; -- WORD
      wSuiteMask          : Int_16_Unsigned_C; -- WORD
      wProductType        : Int_8_Unsigned_C;  -- BYTE
      wReserved           : Int_8_Unsigned_C;  -- BYTE
    end record with Convention => C;
    
  -- https://web.archive.org/web/20141225053346/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162897(v=vs.85).aspx
  type RECT is record
      left   : Int_C; -- LONG
      top    : Int_C; -- LONG
      right  : Int_C; -- LONG
      bottom : Int_C; -- LONG
    end record with Convention => C;
    
  -- https://web.archive.org/web/20150114082405/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145065(v=vs.85).aspx
  type MONITORINFO is record
      cbSize    : Int_32_Unsigned_C := MONITORINFO'Size / Byte'Size; -- DWORD 
      rcMonitor : RECT;              -- RECT  
      rcWork    : RECT;              -- RECT  
      dwFlags   : Int_32_Unsigned_C; -- DWORD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160129165906/https://msdn.microsoft.com/en-us/library/windows/desktop/dd162805(v=vs.85).aspx
  type POINT is record
      x : Int_C; -- LONG 
      y : Int_C; -- LONG 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160131052119/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644958(v=vs.85).aspx
  type MSG is record
      hwnd    : Ptr;               -- HWND   
      message : Int_32_Unsigned_C; -- UINT   
      wParam  : Int_Ptr;           -- WPARAM 
      lParam  : Int_Ptr;           -- LPARAM 
      time    : Int_32_Unsigned_C; -- DWORD  
      pt      : POINT;             -- POINT  
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160527143421/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633577(v=vs.85).aspx
  type WNDCLASSEX is record
      cbSize        : Int_32_Unsigned_C := WNDCLASSEX'Size / Byte'Size;   -- UINT      
      style         : Int_32_Unsigned_C;   -- UINT      
      lpfnWndProc   : Ptr;                 -- WNDPROC   
      cbClsExtra    : Int_C;               -- int       
      cbWndExtra    : Int_C;               -- int       
      hInstance     : Ptr;                 -- HINSTANCE 
      hIcon         : Ptr;                 -- HICON     
      hCursor       : Ptr;                 -- HCURSOR   
      hbrBackground : Int_Ptr;             -- HBRUSH    
      lpszMenuName  : Ptr;--Ptr_Const_Char_16_C; -- LPCTSTR   
      lpszClassName : Ptr;--Ptr_Const_Char_16_C; -- LPCTSTR   
      hIconSm       : Ptr;                 -- HICON     
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160404075807/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_gamepad(v=vs.85).aspx
  type XINPUT_GAMEPAD is record
      wButtons      : Int_16_Unsigned_C; -- WORD  
      bLeftTrigger  : Int_8_Unsigned_C;  -- BYTE  
      bRightTrigger : Int_8_Unsigned_C;  -- BYTE  
      sThumbLX      : Int_16_Signed_C;   -- SHORT 
      sThumbLY      : Int_16_Signed_C;   -- SHORT 
      sThumbRX      : Int_16_Signed_C;   -- SHORT 
      sThumbRY      : Int_16_Signed_C;   -- SHORT 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20150101223349/http://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_state(v=vs.85).aspx
  type XINPUT_STATE is record
      dwPacketNumber : Int_32_Unsigned_C; -- DWORD          
      Gamepad        : XINPUT_GAMEPAD;    -- XINPUT_GAMEPAD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160509011549/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_vibration(v=vs.85).aspx
  type XINPUT_VIBRATION is record
      wLeftMotorSpeed  : Int_16_Unsigned_C; -- WORD 
      wRightMotorSpeed : Int_16_Unsigned_C; -- WORD 
    end record with Convention => C;
    
 -- https://web.archive.org/web/20150109064332/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645565(v=vs.85).aspx
 type RAWINPUTDEVICE is record
      usUsagePage : Int_16_Unsigned_C; -- USHORT 
      usUsage     : Int_16_Unsigned_C; -- USHORT 
      dwFlags     : Int_32_Unsigned_C; -- DWORD  
      hwndTarget  : Ptr;               -- HWND
    end record with Convention => C;
    
  -- https://web.archive.org/web/20140321090313/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645568(v=vs.85).aspx
  type RAWINPUTDEVICELIST is record
      hDevice : Ptr;               -- HANDLE 
      dwType  : Int_32_Unsigned_C; -- DWORD  
    end record with Convention => C;
 
  -- https://web.archive.org/web/20140321085736/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645571(v=vs.85).aspx
  type RAWINPUTHEADER is record
      dwType  : Int_32_Unsigned_C; -- DWORD  
      dwSize  : Int_32_Unsigned_C; -- DWORD  
      hDevice : Ptr;               -- HANDLE 
      wParam  : Int_C;             -- WPARAM 
    end record with Convention => C;
  
  -- https://web.archive.org/web/20150107130951/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645575(v=vs.85).aspx
  type RAWKEYBOARD is record
      Header           : RAWINPUTHEADER;    -- RAWINPUTHEADER
      MakeCode         : Int_16_Unsigned_C; -- USHORT
      Flags            : Int_16_Unsigned_C; -- USHORT
      Reserved         : Int_16_Unsigned_C; -- USHORT
      VKey             : Int_16_Unsigned_C; -- USHORT
      Message          : Int_32_Unsigned_C; -- UINT
      ExtraInformation : Int_32_Unsigned_C; -- ULONG
    end record with Convention => C;
  
  -- https://web.archive.org/web/20140714222448/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx
  type RAWMOUSE is record
      Header             : RAWINPUTHEADER;    -- RAWINPUTHEADER
      usFlags            : Int_16_Unsigned_C; -- USHORT 
      usButtons          : Int_32_Unsigned_C; -- ULONG 
      ulRawButtons       : Int_32_Unsigned_C; -- ULONG  
      lLastX             : Int_C;             -- LONG   
      lLastY             : Int_C;             -- LONG   
      ulExtraInformation : Int_32_Unsigned_C; -- ULONG  
    end record with Convention => C;
  
  -- https://web.archive.org/web/20141109015511/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145106(v=vs.85).aspx
  type SIZE is record
      cx : Int_32_Unsigned_C; -- LONG 
      cy : Int_32_Unsigned_C; -- LONG 
    end record with Convention => C;
  
  -- https://web.archive.org/web/20121031105705/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787537(v=vs.85).aspx
  type SCROLLINFO is record
      cbSize    : Int_32_Unsigned_C := SCROLLINFO'Size / Byte'Size; -- UINT 
      fMask     : Int_32_Unsigned_C; -- UINT ??? := 16#001F#
      nMin      : Int_C;             -- int  
      nMax      : Int_C;             -- int  
      nPage     : Int_32_Unsigned_C; -- UINT 
      nPos      : Int_C;             -- int  
      nTrackPos : Int_C;             -- int  
    end record with Convention => C;
  
  -- https://web.archive.org/web/20140902090739/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632605(v=vs.85).aspx
  type MINMAXINFO is record
      ptReserved     : POINT; -- POINT 
      ptMaxSize      : POINT; -- POINT 
      ptMaxPosition  : POINT; -- POINT 
      ptMinTrackSize : POINT; -- POINT 
      ptMaxTrackSize : POINT; -- POINT 
    end record with Convention => C;
  
  -- https://web.archive.org/web/20160901051039/https://msdn.microsoft.com/en-us/library/windows/desktop/dd145037(v=vs.85).aspx
  type LOGFONT is record  
      lfHeight         : Int_C;            -- LONG  
      lfWidth          : Int_C;            -- LONG  
      lfEscapement     : Int_C;            -- LONG  
      lfOrientation    : Int_C;            -- LONG  
      lfWeight         : Int_C;            -- LONG  
      lfItalic         : Int_8_Unsigned_C; -- BYTE  
      lfUnderline      : Int_8_Unsigned_C; -- BYTE  
      lfStrikeOut      : Int_8_Unsigned_C; -- BYTE  
      lfCharSet        : Int_8_Unsigned_C; -- BYTE  
      lfOutPrecision   : Int_8_Unsigned_C; -- BYTE  
      lfClipPrecision  : Int_8_Unsigned_C; -- BYTE  
      lfQuality        : Int_8_Unsigned_C; -- BYTE  
      lfPitchAndFamily : Int_8_Unsigned_C; -- BYTE  
      lfFaceName       : Str_16_C (1..32); -- TCHAR[LF_FACESIZE]
    end record with Convention => C;
  
  -- https://web.archive.org/web/20160808212528/https://msdn.microsoft.com/en-us/library/windows/desktop/ff729175(v=vs.85).aspx
  type NONCLIENTMETRICS is record
      cbSize             :         Int_32_Unsigned_C := NONCLIENTMETRICS'Size / Byte'Size; -- UINT
      iBorderWidth       :         Int_C;             -- int     
      iScrollWidth       :         Int_C;             -- int     
      iScrollHeight      :         Int_C;             -- int     
      iCaptionWidth      :         Int_C;             -- int     
      iCaptionHeight     :         Int_C;             -- int     
      lfCaptionFont      : aliased LOGFONT;           -- LOGFONT 
      iSmCaptionWidth    :         Int_C;             -- int     
      iSmCaptionHeight   :         Int_C;             -- int     
      lfSmCaptionFont    : aliased LOGFONT;           -- LOGFONT 
      iMenuWidth         :         Int_C;             -- int     
      iMenuHeight        :         Int_C;             -- int     
      lfMenuFont         : aliased LOGFONT;           -- LOGFONT 
      lfStatusFont       : aliased LOGFONT;           -- LOGFONT 
      lfMessageFont      : aliased LOGFONT;           -- LOGFONT 
      iPaddedBorderWidth :         Int_C;             -- int     
    end record with Convention => C;
  
  -- https://web.archive.org/web/20150113013334/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145132(v=vs.85).aspx
  type TEXTMETRIC is record
      tmHeight           : Int_C;          -- LONG  
      tmAscent           : Int_C;          -- LONG  
      tmDescent          : Int_C;          -- LONG  
      tmInternalLeading  : Int_C;          -- LONG  
      tmExternalLeading  : Int_C;          -- LONG  
      tmAveCharWidth     : Int_C;          -- LONG  
      tmMaxCharWidth     : Int_C;          -- LONG  
      tmWeight           : Int_C;          -- LONG  
      tmOverhang         : Int_C;          -- LONG  
      tmDigitizedAspectX : Int_C;          -- LONG  
      tmDigitizedAspectY : Int_C;          -- LONG  
      tmFirstChar        : Char_16_C;      -- TCHAR 
      tmLastChar         : Char_16_C;      -- TCHAR 
      tmDefaultChar      : Char_16_C;      -- TCHAR 
      tmBreakChar        : Char_16_C;      -- TCHAR 
      tmItalic           : Int_8_Unsigned; -- BYTE  
      tmUnderlined       : Int_8_Unsigned; -- BYTE  
      tmStruckOut        : Int_8_Unsigned; -- BYTE  
      tmPitchAndFamily   : Int_8_Unsigned; -- BYTE  
      tmCharSet          : Int_8_Unsigned; -- BYTE  
    end record with Convention => C;
  
  -----------------
  -- Conversions --
  -----------------
  
  -- https://web.archive.org/web/20141225053346/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162897(v=vs.85).aspx
  type Ptr_RECT        is access all RECT;
  function To_Ptr_RECT is new Ada.Unchecked_Conversion (Int_Ptr, Ptr_RECT);

  -- https://web.archive.org/web/20140902090739/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632605(v=vs.85).aspx
  type Ptr_MINMAXINFO        is access all MINMAXINFO;
  function To_Ptr_MINMAXINFO is new Ada.Unchecked_Conversion (Int_Ptr, Ptr_MINMAXINFO);

  -----------------
  -- Subprograms --
  -----------------

  -- https://web.archive.org/web/20161024170359/https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
  function CreateFileW (lpFileName            : Ptr;               -- LPCTSTR
                        dwDesiredAccess       : Int_32_Unsigned_C; -- DWORD
                        dwShareMode           : Int_32_Unsigned_C; -- DWORD
                        lpSecurityAttributes  : Ptr;               -- LPSECURITY_ATTRIBUTES
                        dwCreationDisposition : Int_32_Unsigned_C; -- DWORD
                        dwFlagsAndAttributes  : Int_32_Unsigned_C; -- DWORD
                        hTemplateFile         : Ptr)               -- HANDLE
                        return Ptr                                 -- HANDLE
                        with Import => True, Convention => StdCall, External_Name => "CreateFileW";

  -- https://web.archive.org/web/20140310215229/http://msdn.microsoft.com/en-us/library/windows/hardware/ff539681(v=vs.85).aspx
  function HidD_GetProductString (HidDeviceObject : Ptr;               -- HANDLE 
                                  Buffer          : Ptr;               -- PVOID  
                                  BufferLength    : Int_32_Unsigned_C) -- ULONG  
                                  return Int_C                         -- BOOLEAN
                                  with Import => True, Convention => StdCall, External_Name => "HidD_GetProductString";

  -- https://web.archive.org/web/20140529093020/http://msdn.microsoft.com/en-us/library/windows/hardware/ff538959(v=vs.85).aspx
  function HidD_GetManufacturerString (HidDeviceObject : Ptr;               -- HANDLE 
                                       Buffer          : Ptr;               -- PVOID  
                                       BufferLength    : Int_32_Unsigned_C) -- ULONG  
                                       return Int_C                         -- BOOLEAN
                                       with Import => True, Convention => StdCall, External_Name => "HidD_GetManufacturerString";

  -- https://web.archive.org/web/20160821131741/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645597%28v=vs.85%29.aspx
  function GetRawInputDeviceInfoW (hDevice   : Ptr;               -- HANDLE
                                   uiCommand : Int_32_Unsigned_C; -- UINT
                                   pData     : Ptr;               -- LPVOID
                                   pcbSize   : Ptr)               -- PUINT 
                                   return Int_32_Unsigned_C       -- UINT 
                                   with Import => True, Convention => StdCall, External_Name => "GetRawInputDeviceInfoW";

  -- https://web.archive.org/web/20161114174855/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683212(v=vs.85).aspx
  function GetProcAddress (hModule    : Ptr;     -- HMODULE
                           lpProcName : Str_8_C) -- LPCSTR
                           return Ptr            -- FARPROC 
                           with Import => True, Convention => StdCall, External_Name => "GetProcAddress";

  -- https://web.archive.org/web/20160216085152/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683152(v=vs.85).aspx
  function FreeLibrary (hModule : Ptr) -- HMODULE
                        return Int_C   -- BOOL
                        with Import => True, Convention => StdCall, External_Name => "FreeLibrary";

  -- https://web.archive.org/web/20160608204937/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644945(v=vs.85).aspx
  procedure PostQuitMessage (nExitCode : Int_C) -- int 
                             with Import => True, Convention => StdCall, External_Name => "PostQuitMessage";

  -- https://web.archive.org/web/20160413212433/https://msdn.microsoft.com/en-us/library/windows/desktop/ms679360(v=vs.85).aspx
  function GetLastError return Int_32_Unsigned_C -- DWORD 
                        with Import => True, Convention => StdCall, External_Name => "GetLastError";
             
  -- https://web.archive.org/web/20160801144224/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724451(v=vs.85).aspx                       
  function GetVersionExW (lpVersionInfo : access OSVERSIONINFOEX) -- LPOSVERSIONINFOEX
                          return Int_C                            -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetVersionExW";
       
  -- https://web.archive.org/web/20141228222857/http://msdn.microsoft.com/en-us/library/windows/desktop/ms724432(v=vs.85).aspx                             
  function GetUserNameW (lpBuffer :        Ptr_Str_16_C; -- LPTSTR  
                         lpnSize  : access Int_C)        -- LPDWORD 
                         return Int_C                    -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "GetUserNameW";

  -- https://web.archive.org/web/20160722062551/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683197(v=vs.85).aspx    
  function GetModuleFileNameW (hModule    : Ptr;               -- HMODULE 
                               lpFilename : Ptr_Str_16_C;      -- LPTSTR  
                               nSize      : Int_32_Unsigned_C) -- DWORD   
                               return Int_32_Unsigned_C        -- DWORD 
                               with Import => True, Convention => StdCall, External_Name => "GetModuleFileNameW";
                                    
  -- https://web.archive.org/web/20150325193818/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683179(v=vs.85).aspx                       
  function GetCurrentProcess return Ptr -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "GetCurrentProcess";

  -- https://web.archive.org/web/20160830030005/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724947(v=vs.85).aspx         
  function SystemParametersInfoW (uiAction : Int_32_Unsigned_C; -- UINT  
                                  uiParam  : Int_32_Unsigned_C; -- UINT  
                                  pvParam  : Ptr;               -- PVOID 
                                  fWinIni  : Int_32_Unsigned_C) -- UINT  
                                  return Int_C                  -- BOOL 
                                  with Import => True, Convention => StdCall, External_Name => "SystemParametersInfoW";
 
  -- https://web.archive.org/web/20160526201612/https://msdn.microsoft.com/en-us/library/windows/desktop/aa366574(v=vs.85).aspx                  
  function GlobalAlloc (uFlags  : Int_32_Unsigned_C; -- UINT   
                        dwBytes : Int_Size_C)        -- SIZE_T 
                        return Ptr                   -- HGLOBAL 
                        with Import => True, Convention => StdCall, External_Name => "GlobalAlloc";
  
  -- https://web.archive.org/web/20150109225811/http://msdn.microsoft.com/en-us/library/windows/desktop/aa366584(v=vs.85).aspx                             
  function GlobalLock (hMem : Ptr) -- HGLOBAL 
                       return Ptr  -- LPVOID 
                       with Import => True, Convention => StdCall, External_Name => "GlobalLock";
                 
  -- https://web.archive.org/web/20130719230854/http://msdn.microsoft.com/en-us/library/windows/desktop/aa366595(v=vs.85).aspx                   
  function GlobalUnlock (hMem : Ptr)  -- HGLOBAL 
                         return Int_C -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "GlobalUnlock";
  
  -- https://web.archive.org/web/20160128221557/https://msdn.microsoft.com/en-us/library/windows/desktop/ms649048(v=vs.85).aspx
  function OpenClipboard (hWndNewOwner : Ptr) -- HWND 
                          return Int_C        -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "OpenClipboard";

  -- https://web.archive.org/web/20150109082331/http://msdn.microsoft.com/en-us/library/windows/desktop/aa366579(v=vs.85).aspx
  function GlobalFree (hMem : Ptr) -- HGLOBAL 
                       return Ptr  -- HGLOBAL 
                       with Import => True, Convention => StdCall, External_Name => "GlobalFree";

  -- https://web.archive.org/web/20140605000630/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649037(v=vs.85).aspx
  function EmptyClipboard return Int_C -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "EmptyClipboard";

  -- https://web.archive.org/web/20141230011623/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649051(v=vs.85).aspx
  function SetClipboardData (uFormat : Int_32_Unsigned_C; -- UINT   
                             hMem    : Ptr)               -- HANDLE 
                             return Ptr                   -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "SetClipboardData";

  -- https://web.archive.org/web/20150109231948/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649035(v=vs.85).aspx
  function CloseClipboard return Int_C -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "CloseClipboard";
                      
  -- https://web.archive.org/web/20150118081856/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649039(v=vs.85).aspx              
  function GetClipboardData (uFormat : Int_32_Unsigned_C) -- UINT 
                             return Ptr                   -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "GetClipboardData";
                                    
  -- https://web.archive.org/web/20150325220213/https://msdn.microsoft.com/en-us/library/windows/desktop/ms679347(v=vs.85).aspx
  function FlashWindowEx (pfwi : access FLASHWINFO) -- PFLASHWINFO 
                          return Int_C              -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "FlashWindowEx";
                     
  -- https://web.archive.org/web/20160607101505/https://msdn.microsoft.com/en-us/library/windows/desktop/ms682425%28v=vs.85%29.aspx               
  function CreateProcessW (lpApplicationName    :        Ptr_Str_16_C;        -- LPCTSTR               
                           lpCommandLine        :        Ptr_Char_16_C;       -- LPTSTR                
                           lpProcessAttributes  :        Ptr;                 -- LPSECURITY_ATTRIBUTES 
                           lpThreadAttributes   :        Ptr;                 -- LPSECURITY_ATTRIBUTES 
                           bInheritHandles      :        Int_C;               -- BOOL                  
                           dwCreationFlags      :        Int_32_Unsigned_C;   -- DWORD                 
                           lpEnvironment        :        Ptr;                 -- LPVOID                
                           lpCurrentDirectory   :        Ptr_Str_16_C;        -- LPCTSTR               
                           lpStartupInfo        : access STARTUPINFO;         -- LPSTARTUPINFO         
                           lpProcessInformation : access PROCESS_INFORMATION) -- LPPROCESS_INFORMATION 
                           return Int_C                                       -- BOOL 
                           with Import => True, Convention => StdCall, External_Name => "CreateProcessW";
                     
  -- https://web.archive.org/web/20160607232247/https://msdn.microsoft.com/en-us/library/windows/desktop/ms684139(v=vs.85).aspx    
  function IsWow64Process (hProcess     :        Ptr;   -- HANDLE 
                           Wow64Process : access Int_C) -- PBOOL  
                           return Int_C                 -- BOOL 
                           with Import => True, Convention => StdCall, External_Name => "IsWow64Process";
                  
  -- https://web.archive.org/web/20150325211309/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683183(v=vs.85).aspx                  
  function GetCurrentThreadId return Int_32_Unsigned_C -- DWORD 
                              with Import => True, Convention => StdCall, External_Name => "GetCurrentThreadId";
                                    
  -- https://web.archive.org/web/20160603220225/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633582(v=vs.85).aspx
  function GetClassNameW (hWnd        : Ptr;          -- HWND
                          lpClassName : Ptr_Str_16_C; -- LPTSTR 
                          nMaxCount   : Int_C)        -- int    
                          return Int_C                -- int
                          with Import => True, Convention => StdCall, External_Name => "GetClassNameW";
                           
  -- https://web.archive.org/web/20150816061650/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648045(v=vs.85).aspx         
  function LoadImageW (hinst     : Ptr;                 -- HINSTANCE 
                       lpszName  : Ptr_Str_16_C;        -- LPCTSTR   
                       uType     : Int_32_Unsigned_C;   -- UINT      
                       cxDesired : Int_C;               -- int       
                       cyDesired : Int_C;               -- int       
                       fuLoad    : Int_32_Unsigned_C)   -- UINT      
                       return Ptr                       -- HANDLE 
                       with Import => True, Convention => StdCall, External_Name => "LoadImageW";

  -- https://web.archive.org/web/20140321051739/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648072(v=vs.85).aspx                    
  function LoadIconW (hInstance  : Ptr;     -- HINSTANCE 
                      lpIconName : Int_Ptr) -- LPCTSTR   
                      return Ptr            -- HICON 
                      with Import => True, Convention => StdCall, External_Name => "LoadIconW";
                      
  -- https://web.archive.org/web/20150101200053/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648391(v=vs.85).aspx                           
  function LoadCursorW (hInstance    : Ptr;     -- HINSTANCE 
                        lpCursorName : Int_Ptr) -- LPCTSTR   
                        return Ptr              -- HCURSOR 
                        with Import => True, Convention => StdCall, External_Name => "LoadCursorW";
  
  -- https://web.archive.org/web/20150619032402/https://msdn.microsoft.com/en-us/library/windows/desktop/ms646312%28v=vs.85%29.aspx       
  function SetFocus (hWnd : Ptr) -- HWND 
                     return Ptr  -- HWND 
                     with Import => True, Convention => StdCall, External_Name => "SetFocus";
                
  -- https://web.archive.org/web/20150118051947/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648396(v=vs.85).aspx                    
  function ShowCursor (bShow : Int_C) -- BOOL 
                       return Int_C   -- int 
                       with Import => True, Convention => StdCall, External_Name => "ShowCursor";
                                    
  -- https://web.archive.org/web/20120814174530/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648387(v=vs.85).aspx                   
  function GetClipCursor (lpRect : access RECT) -- LPRECT 
                          return Int_C          -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetClipCursor";
                     
  -- https://web.archive.org/web/20160624094706/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648390(v=vs.85).aspx               
  function GetCursorPos (lpPoint : access POINT) -- LPPOINT
                         return Int_C            -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "GetCursorPos";
                                    
  -- http://web.archive.org/web/20160213181912/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683199(v=vs.85).aspx                 
  function GetModuleHandleNULL return Ptr -- HINSTANCE 
                               with Import => True, Convention => C, External_Name => "rts_get_hInstance";
                                    
  -- https://web.archive.org/web/20140402070151/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644993(v=vs.85).aspx
  function UnhookWindowsHookEx (hhk : Ptr)   -- HHOOK 
                                return Int_C -- BOOL 
                                with Import => True, Convention => StdCall, External_Name => "UnhookWindowsHookEx";
                              
  -- https://web.archive.org/web/20160804094136/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644990(v=vs.85).aspx          
  function SetWindowsHookExW (idHook     : Int_C;             -- int       
                              lpfn       : Ptr;               -- HOOKPROC  
                              hMod       : Ptr;               -- HINSTANCE 
                              dwThreadId : Int_32_Unsigned_C) -- DWORD     
                              return Ptr                      -- HHOOK 
                              with Import => True, Convention => StdCall, External_Name => "SetWindowsHookExW";
                                    
  -- https://web.archive.org/web/20160605071718/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505(v=vs.85).aspx
  function MessageBoxW (hWnd      : Ptr;               -- HWND    
                        lpText    : Ptr_Str_16_C;      -- LPCTSTR 
                        lpCaption : Ptr_Str_16_C;      -- LPCTSTR 
                        uType     : Int_32_Unsigned_C) -- UINT    
                        return Int_C                   -- int 
                        with Import => True, Convention => StdCall, External_Name => "MessageBoxW";
                             
  -- https://web.archive.org/web/20150815182728/https://msdn.microsoft.com/en-us/library/windows/desktop/ms632680(v=vs.85).aspx       
  function CreateWindowExW (dwExStyle    : Int_32_Unsigned_C; -- DWORD     
                            lpClassName  : Ptr_Str_16_C;      -- LPCTSTR 
                            lpWindowName : Ptr_Str_16_C;      -- LPCTSTR   
                            dwStyle      : Int_32_Unsigned_C; -- DWORD     
                            x            : Int_C;             -- int       
                            y            : Int_C;             -- int       
                            nWidth       : Int_C;             -- int       
                            nHeight      : Int_C;             -- int       
                            hWndParent   : Ptr;               -- HWND      
                            hMenu        : Int_Ptr;           -- HMENU     
                            hInstance    : Ptr;               -- HINSTANCE 
                            lpParam      : Ptr)               -- LPVOID    
                            return Ptr                        -- HWND 
                            with Import => True, Convention => StdCall, External_Name => "CreateWindowExW";
                                  
  -- https://web.archive.org/web/20160808214158/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724385(v=vs.85).aspx  
  function GetSystemMetrics (nIndex : Int_C) -- int 
                             return Int_C    -- int 
                             with Import => True, Convention => StdCall, External_Name => "GetSystemMetrics";
                                 
  -- https://web.archive.org/web/20160223131327/https://msdn.microsoft.com/en-us/library/windows/desktop/ms682411(v=vs.85).aspx   
  function CreateMutexW (lpMutexAttributes : Ptr;          -- LPSECURITY_ATTRIBUTES 
                         bInitialOwner     : Int_C;        -- BOOL                  
                         lpName            : Ptr_Str_16_C) -- LPCTSTR               
                         return Ptr                        -- HANDLE 
                         with Import => True, Convention => StdCall, External_Name => "CreateMutexW";
                                    
  -- https://web.archive.org/web/20160505131151/https://msdn.microsoft.com/en-us/library/windows/desktop/ms685066(v=vs.85).aspx             
  function ReleaseMutex (hMutex : Ptr) -- HANDLE 
                         return Int_C  -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "ReleaseMutex";
                                    
  -- https://web.archive.org/web/20160605125803/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633499(v=vs.85).aspx
  function FindWindowW (lpClassName  : Ptr_Str_16_C; -- LPCTSTR 
                        lpWindowName : Ptr_Str_16_C) -- LPCTSTR 
                        return Ptr                   -- HWND 
                        with Import => True, Convention => StdCall, External_Name => "FindWindowW";
                                  
  -- https://web.archive.org/web/20160131040502/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633545(v=vs.85).aspx
  function SetWindowPos (hWnd            : Ptr;               -- HWND 
                         hWndInsertAfter : Ptr;               -- HWND 
                         X               : Int_C;             -- int  
                         Y               : Int_C;             -- int  
                         cx              : Int_C;             -- int  
                         cy              : Int_C;             -- int  
                         uFlags          : Int_32_Unsigned_C) -- UINT 
                         return Int_C                         -- BOOL
                         with Import => True, Convention => StdCall, External_Name => "SetWindowPos";
                                    
  -- https://web.archive.org/web/20150118082712/http://msdn.microsoft.com/en-us/library/windows/desktop/dd144901(v=vs.85).aspx
  function GetMonitorInfoW (hMonitor :        Ptr;         -- HMONITOR      
                            lpmi     : access MONITORINFO) -- LPMONITORINFO 
                            return Int_C                   -- BOOL 
                            with Import => True, Convention => StdCall, External_Name => "GetMonitorInfoW";
                       
  -- https://web.archive.org/web/20150107150037/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162610(v=vs.85).aspx             
  function EnumDisplayMonitors (hdc      :        Ptr;   -- HDC             
                                lprcClip : access RECT;  -- LPCRECT         
                                lpfnEnum :        Ptr;   -- MONITORENUMPROC 
                                dwData   :        Int_C) -- LPARAM          
                                return Int_C             -- BOOL 
                                with Import => True, Convention => StdCall, External_Name => "EnumDisplayMonitors";
                                 
  -- https://web.archive.org/web/20150110222635/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633519(v=vs.85).aspx  
  function GetWindowRect (hWnd   :        Ptr;  -- HWND   
                          lpRect : access RECT) -- LPRECT 
                          return Int_C          -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetWindowRect";
                                
  -- https://web.archive.org/web/20160519224341/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633591(v=vs.85).aspx
  function SetWindowLongW (hWnd      : Ptr;               -- HWND 
                           nIndex    : Int_C;             -- int  
                           dwNewLong : Int_32_Unsigned_C) -- LONG 
                           return Int_32_Unsigned_C       -- LONG 
                           with Import => True, Convention => StdCall, External_Name => "SetWindowLongW";
                                  
  -- https://web.archive.org/web/20150311211447/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633504%28v=vs.85%29.aspx
  function GetDesktopWindow return Ptr -- HWND 
                            with Import => True, Convention => StdCall, External_Name => "GetDesktopWindow";
                                    
  -- https://web.archive.org/web/20151125225543/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633572(v=vs.85).aspx    
  function DefWindowProcW (hWnd   : Ptr;               -- HWND   
                           Msg    : Int_32_Unsigned_C; -- UINT   
                           wParam : Int_Ptr;           -- WPARAM 
                           lParam : Int_Ptr)           -- LPARAM 
                           return Int_Ptr              -- LRESULT 
                           with Import => True, Convention => StdCall, External_Name => "DefWindowProcW";
                                 
  -- https://web.archive.org/web/20150605201750/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633539(v=vs.85).aspx
  function SetForegroundWindow (hWnd : Ptr)  -- HWND 
                                return Int_C -- BOOL 
                                with Import => True, Convention => StdCall, External_Name => "SetForegroundWindow";
                                    
  -- https://web.archive.org/web/20150109231947/http://msdn.microsoft.com/en-us/library/windows/desktop/ms646311(v=vs.85).aspx
  function SetActiveWindow (hWnd : Ptr) -- HWND 
                            return Ptr  -- HWND 
                            with Import => True, Convention => StdCall, External_Name => "SetActiveWindow";
                                    
  -- https://web.archive.org/web/20150110222629/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162911(v=vs.85).aspx 
  function RedrawWindow (hWnd       :        Ptr;               -- HWND 
                         lprcUpdate : access RECT;              -- const RECT*
                         hrgnUpdate :        Ptr;               -- HRGN 
                         flags      :        Int_32_Unsigned_C) -- UINT 
                         return Int_C                           -- BOOL
                         with Import => True, Convention => StdCall, External_Name => "RedrawWindow";
                                 
  -- https://web.archive.org/web/20150118074638/http://msdn.microsoft.com/en-us/library/windows/desktop/dd144871(v=vs.85).aspx   
  function GetDC (hWnd : Ptr) -- HWND 
                  return Ptr  -- HDC 
                  with Import => True, Convention => StdCall, External_Name => "GetDC";
                                    
  -- https://web.archive.org/web/20160716193153/https://msdn.microsoft.com/en-us/library/windows/desktop/dd162920(v=vs.85).aspx 
  function ReleaseDC (hWnd : Ptr;  -- HWND 
                      hDC  : Ptr)  -- HDC  
                      return Int_C -- int 
                      with Import => True, Convention => StdCall, External_Name => "ReleaseDC";
                                    
  -- https://web.archive.org/web/20150505170208/https://msdn.microsoft.com/en-us/library/windows/desktop/dd144877(v=vs.85).aspx
  function GetDeviceCaps (hdc    : Ptr;   -- HDC 
                          nIndex : Int_C) -- int 
                          return Int_C    -- int 
                          with Import => True, Convention => StdCall, External_Name => "GetDeviceCaps";
                                    
  -- https://web.archive.org/web/20151126051230/https://msdn.microsoft.com/en-us/library/windows/desktop/ms646301(v=vs.85).aspx              
  function GetKeyState (nVirtKey : Int_C)        -- int 
                        return Int_16_Unsigned_C -- SHORT 
                        with Import => True, Convention => StdCall, External_Name => "GetKeyState";
                                    
  -- https://web.archive.org/web/20160609192805/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633587(v=vs.85).aspx
  function RegisterClassExW (lpwcx : access WNDCLASSEX) -- const WNDCLASSEX*
                             return Int_16_Unsigned_C   -- ATOM 
                             with Import => True, Convention => StdCall, External_Name => "RegisterClassExW";
                                    
  -- https://web.archive.org/web/20160408124849/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinputsetstate(v=vs.85).aspx
  function XInputSetState (dwUserIndex :        Int_32_Unsigned_C; -- DWORD            
                           pVibration  : access XINPUT_VIBRATION)  -- XINPUT_VIBRATION*
                           return Int_32_Unsigned_C                -- DWORD
                           with Import => True, Convention => StdCall, External_Name => "XInputSetState";
                                
  -- https://web.archive.org/web/20160404075716/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinputgetstate(v=vs.85).aspx    
  function XInputGetState (dwUserIndex :        Int_32_Unsigned_C; -- DWORD        
                           pState      : access XINPUT_STATE)      -- XINPUT_STATE*
                           return Int_32_Unsigned_C                -- DWORD
                           with Import => True, Convention => StdCall, External_Name => "XInputGetState";
                                   
  -- https://web.archive.org/web/20140321145747/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645596(v=vs.85).aspx
  function GetRawInputData (hRawInput    : Ptr;               -- HRAWINPUT 
                            uiCommand    : Int_32_Unsigned_C; -- UINT      
                            pData        : Ptr;               -- LPVOID    
                            pcbSize      : Ptr;               -- PUINT     
                            cbSizeHeader : Int_32_Unsigned_C) -- UINT      
                            return Int_32_Unsigned_C          -- UINT 
                            with Import => True, Convention => StdCall, External_Name => "GetRawInputData";
                                    
  -- https://web.archive.org/web/20150114022224/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645600(v=vs.85).aspx
  function RegisterRawInputDevices (pRawInputDevices : Ptr;               -- PCRAWINPUTDEVICE 
                                    uiNumDevices     : Int_32_Unsigned_C; -- UINT             
                                    cbSize           : Int_32_Unsigned_C) -- UINT             
                                    return Int_C                          -- BOOL 
                                    with Import => True, Convention => StdCall, External_Name => "RegisterRawInputDevices";
                           
  -- https://web.archive.org/web/20140608172723/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645598(v=vs.85).aspx         
  function GetRawInputDeviceList (pRawInputDeviceList :        Ptr;                -- PRAWINPUTDEVICELIST 
                                  puiNumDevices       : access Int_32_Unsigned_C;  -- PUINT               
                                  cbSize              :        Int_32_Unsigned_C)  -- UINT                
                                  return Int_C                                     -- UINT 
                                  with Import => True, Convention => StdCall, External_Name => "GetRawInputDeviceList";
         
  -- https://web.archive.org/web/20140320170251/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787583(v=vs.85).aspx                           
  function GetScrollInfo (hwnd  :        Ptr;        -- HWND         
                          fnBar :        Int_C;      -- int          
                          lpsi  : access SCROLLINFO) -- LPSCROLLINFO 
                          return Int_C               -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetScrollInfo";

  -- https://web.archive.org/web/20160610020032/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644950(v=vs.85).aspx            
  function SendMessageW (hWnd   : Ptr;               -- HWND   
                         Msg    : Int_32_Unsigned_C; -- UINT  
                         wParam : Int_Ptr;           -- WPARAM 
                         lParam : Int_Ptr)           -- LPARAM 
                         return Int_C                -- LRESULT 
                         with Import => True, Convention => StdCall, External_Name => "SendMessageW";
                       
  -- https://web.archive.org/web/20160202112019/https://msdn.microsoft.com/en-us/library/windows/desktop/dd183499(v=vs.85).aspx           
  function CreateFontW (nHeight            : Int_C;             -- int     
                        nWidth             : Int_C;             -- int     
                        nEscapement        : Int_C;             -- int     
                        nOrientation       : Int_C;             -- int     
                        fnWeight           : Int_C;             -- int     
                        fdwItalic          : Int_32_Unsigned_C; -- DWORD   
                        fdwUnderline       : Int_32_Unsigned_C; -- DWORD   
                        fdwStrikeOut       : Int_32_Unsigned_C; -- DWORD   
                        fdwCharSet         : Int_32_Unsigned_C; -- DWORD   
                        fdwOutputPrecision : Int_32_Unsigned_C; -- DWORD   
                        fdwClipPrecision   : Int_32_Unsigned_C; -- DWORD   
                        fdwQuality         : Int_32_Unsigned_C; -- DWORD   
                        fdwPitchAndFamily  : Int_32_Unsigned_C; -- DWORD   
                        lpszFace           : Ptr_Str_16_C)      -- DWORD   
                        return Ptr                              -- HFONT
                        with Import => True, Convention => StdCall, External_Name => "CreateFontW";

  -- https://web.archive.org/web/20140209093950/http://msdn.microsoft.com/en-us/library/windows/desktop/dd183500(v=vs.85).aspx
  function CreateFontIndirectW (lplf : access LOGFONT) -- const LOGFONT*
                                return Ptr             -- HFONT
                                with Import => True, Convention => StdCall, External_Name => "CreateFontIndirectW";

  -- https://web.archive.org/web/20160729035042/https://msdn.microsoft.com/en-us/library/windows/desktop/dd144941(v=vs.85).aspx        
  function GetTextMetricsW (hdc  :        Ptr;        -- HDC          
                            lptm : access TEXTMETRIC) -- LPTEXTMETRIC 
                            return Int_C              -- BOOL
                            with Import => True, Convention => StdCall, External_Name => "GetTextMetricsW";
                           
  -- https://web.archive.org/web/20150117220711/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162957(v=vs.85).aspx
  function SelectObject (hdc     : Ptr; -- HDC     
                         hgdiobj : Ptr) -- HGDIOBJ 
                         return Ptr     -- HGDIOBJ
                         with Import => True, Convention => Stdcall, External_Name => "SelectObject";
                         
  -- https://web.archive.org/web/20150110233051/http://msdn.microsoft.com/en-us/library/windows/desktop/dd144938(v=vs.85).aspx 
  function GetTextExtentPoint32W (hdc      :        Ptr;          -- HDC     
                                  lpString :        Ptr_Str_16_C; -- LPCTSTR 
                                  c        :        Int_C;        -- int     
                                  lpSize   : access SIZE)         -- LPSIZE  
                                  return Int_C                    -- BOOL 
                                  with Import => True, Convention => StdCall, External_Name => "GetTextExtentPoint32W";
                                    
  -- https://web.archive.org/web/20140620200556/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145093(v=vs.85).aspx    
  function SetTextColor (hdc     : Ptr;               -- HDC      
                         crColor : Int_32_Unsigned_C) -- COLORREF 
                         return Int_32_Unsigned_C     -- COLORREF
                         with Import => True, Convention => StdCall, External_Name => "SetTextColor";
                       
  -- https://web.archive.org/web/20150112014144/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162964(v=vs.85).aspx                       
  function SetBkColor (hdc     : Ptr;               -- HDC      
                       crColor : Int_32_Unsigned_C) -- COLORREF 
                       return Int_32_Unsigned_C     -- COLORREF
                       with Import => True, Convention => StdCall, External_Name => "SetBkColor";
                     
  -- https://web.archive.org/web/20150111020406/http://msdn.microsoft.com/en-us/library/windows/desktop/dd183518(v=vs.85).aspx     
  function CreateSolidBrush (crColor : Int_32_Unsigned_C) -- COLORREF 
                             return Ptr                   -- HBRUSH
                             with Import => True, Convention => StdCall, External_Name => "CreateSolidBrush";
                           
  -- https://web.archive.org/web/20160525074306/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx        
  function PeekMessageW (lpMsg         : access MSG;               -- LPMSG 
                         hWnd          :        Ptr;               -- HWND  
                         wMsgFilterMin :        Int_32_Unsigned_C; -- UINT  
                         wMsgFilterMax :        Int_32_Unsigned_C; -- UINT  
                         wRemoveMsg    :        Int_32_Unsigned_C) -- UINT  
                         return Int_C                              -- BOOL 
                         with Import => True, Convention => Stdcall, External_Name => "PeekMessageW";

  -- https://web.archive.org/web/20160607234745/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633520(v=vs.85).aspx
  function GetWindowTextW (hWnd      : Ptr;          -- HWND   
                           lpString  : Ptr_Str_16_C; -- LPTSTR 
                           nMaxCount : Int_C)        -- int    
                           return Int_C              -- int
                           with Import => True, Convention => StdCall, External_Name => "GetWindowTextW";

  -- https://web.archive.org/web/20150114085321/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644974(v=vs.85).aspx
  function CallNextHookEx (hhk    : Ptr;     -- HHOOK  
                           nCode  : Int_C;   -- nCode
                           wParam : Int_Ptr; -- WPARAM
                           lParam : Int_Ptr) -- LPARAM
                           return Int_Ptr    -- LRESULT 
                           with Import => True, Convention => StdCall, External_Name => "CallNextHookEx";

  -- https://web.archive.org/web/20160501214001/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633548(v=vs.85).aspx
  function ShowWindow (hWnd     : Ptr;   -- HWND 
                       nCmdShow : Int_C) -- int  
                       return Int_C      -- BOOL 
                       with Import => True, Convention => StdCall, External_Name => "ShowWindow";

  -- https://web.archive.org/web/20150110222635/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648383(v=vs.85).aspx
  function ClipCursor (lpRect : access RECT) -- const RECT*
                       return Int_C          -- BOOL 
                       with Import => True, Convention => StdCall, External_Name => "ClipCursor";

  -- https://web.archive.org/web/20160115050931/https://msdn.microsoft.com/en-us/library/windows/desktop/dd145167(v=vs.85).aspx
  function UpdateWindow (hWnd : Ptr)  -- HWND 
                         return Int_C -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "UpdateWindow";

  -- https://web.archive.org/web/20160609212317/https://msdn.microsoft.com/en-us/library/windows/desktop/ms632682(v=vs.85).aspx
  function DestroyWindow (hWnd : Ptr)  -- HWND 
                          return Int_C -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "DestroyWindow";

  -- https://web.archive.org/web/20160131033416/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644899(v=vs.85).aspx
  function UnregisterClassW (lpClassName : Ptr_Str_16_C; -- LPCTSTR   
                             hInstance   : Ptr)          -- HINSTANCE 
                             return Int_C                -- BOOL 
                             with Import => True, Convention => StdCall, External_Name => "UnregisterClassW";

  -- https://web.archive.org/web/20160527143411/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644955(v=vs.85).aspx
  function TranslateMessage (lpMsg : access MSG) -- const MSG*
                             return Int_C        -- BOOL 
                             with Import => True, Convention => StdCall, External_Name => "TranslateMessage";

  -- https://web.archive.org/web/20160113002241/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644934(v=vs.85).aspx
  function DispatchMessageW (lpmsg : access MSG) -- const MSG*
                             return Int_C        -- LRESULT
                             with Import => True, Convention => StdCall, External_Name => "DispatchMessageW";

  -- https://web.archive.org/web/20150117222354/http://msdn.microsoft.com/en-us/library/windows/desktop/ms646294(v=vs.85).aspx
  function GetFocus return Ptr  -- HWND
                    with Import => True, Convention => StdCall, External_Name => "GetFocus";

  -- https://web.archive.org/web/20150110222633/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633494(v=vs.85).aspx
  function EnumChildWindows (hWndParent : Ptr;   -- HWND        
                             lpEnumFunc : Ptr;   -- WNDENUMPROC 
                             lParam     : Int_C) -- LPARAM      
                             return Int_C        -- BOOL
                             with Import => True, Convention => StdCall, External_Name => "EnumChildWindows";

  -- http://web.archive.org/web/20131206060702/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633588(v=vs.85).aspx
  function SetClassLongW (hWnd      : Ptr;               -- HWND 
                          nIndex    : Int_C;             -- int 
                          dwNewLong : Int_32_Unsigned_C) -- LONG 
                          return Int_32_Unsigned_C       -- DWORD
                          with Import => True, Convention => StdCall, External_Name => "SetClassLongW";

  -- http://web.archive.org/web/20160623235135/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633589(v=vs.85).aspx
  function SetClassLongPtrW (hWnd      : Ptr;     -- hWnd
                             nIndex    : Int_C;   -- nIndex
                             dwNewLong : Int_Ptr) -- LONG_PTR
                             return Int_Ptr       -- ULONG_PTR
                             with Import => True, Convention => StdCall, External_Name => "SetClassLongPtrW";

  -- http://web.archive.org/web/20160610054506/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648394(v=vs.85).aspx
  function SetCursorPos (X : Int_C;   -- int
                         Y : Int_C)   -- int
                         return Int_C -- BOOL
                         with Import => True, Convention => StdCall, External_Name => "SetCursorPos";

  -- http://web.archive.org/web/20160425021739/https://msdn.microsoft.com/en-us/library/windows/desktop/ms684175(v=vs.85).aspx
  function LoadLibraryW (lpFileName : Str_C) -- LPCTSTR 
                         return Ptr          -- LPCTSTRHMODULE
                         with Import =>True, Convention => StdCall, External_Name => "LoadLibraryW";
end;
