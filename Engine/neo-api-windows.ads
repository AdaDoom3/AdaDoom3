
--                                                                                                                               --
--                                                      N E O  E N G I N E                                                       --
--                                                                                                                               --
--                                               Copyright (C) 2020 Justin Squirek                                               --
--                                                                                                                               --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                      --
--                                                                                                                               --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of         --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                     --
--                                                                                                                               --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                --
--                                                                                                                               --

with Unchecked_Conversion;

-- Binding to the Win32 API: http://web.archive.org/web/20150115092801/http://msdn.microsoft.com/en-us/library/windows/apps/dn424765.aspx
package Neo.API.Windows is

  -----------
  -- Types --
  -----------

  -- LPVOID    Ptr
  -- HGLOBAL   Ptr
  -- HWND      Ptr
  -- HANDLE    Ptr
  -- HICON     Ptr
  -- HINSTANCE Ptr
  -- HHOOK     Ptr
  -- HMODULE   Ptr
  -- LPTSTR    Ptr_Char_16_C
  -- LPCTSTR   Ptr_Char_16_C
  -- WPARAM    Int_Ptr
  -- LPARAM    Int_Ptr
  -- BOOL      Int_C
  -- LONG      Int_C
  -- UINT      Int_Unsigned_C
  -- DWORD     Int_Unsigned_C
  -- BYTE      Int_8_Unsigned_C
  -- SHORT     Int_16_Signed_C
  -- SIZE_T    Int_Size_C

  -----------
  -- Flags --
  -----------

  -- https://web.archive.org/web/20150111095828/http://msdn.microsoft.com/en-us/library/930f87yf.aspx
  MAX_PATH : constant Int_Size_C := 32_768; 

  -- https://web.archive.org/web/20160830082310/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633591(v=vs.85).aspx
  GWL_STYLE : constant Int_C := -16; -- 0

  -- https://web.archive.org/web/20160830082310/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633591(v=vs.85).aspx
  GWL_EXSTYLE : constant Int_C := -20; -- 0

  -- http://web.archive.org/web/20160623235135/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633589(v=vs.85).aspx
  GCLP_HCURSOR : constant Int_C := -12; -- 0
  
  -- https://web.archive.org/web/20150118045535/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633574(v=vs.85).aspx
  DIALOG_CLASS : constant Str_16_C := "#32770"; -- "#0"

  -- https://web.archive.org/web/20150109072835/http://msdn.microsoft.com/en-us/library/windows/desktop/ms646274(v=vs.85).aspx
  WA_CLICKACTIVE : constant Int_Ptr := 16#0000_0002#; -- 0

  -- https://web.archive.org/web/20160525074306/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx
  PM_REMOVE : constant Int_Unsigned_C := 16#0000_0001#; -- 0x0000

  -- https://web.archive.org/web/20161003234148/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648072(v=vs.85).aspx
  IDI_APPLICATION : constant Int_Ptr := 16#7F00#; -- MAKEINTRESOURCE (0)

  -- https://web.archive.org/web/20150101200053/http://msdn.microsoft.com/en-us/library/windows/desktop/ms648391(v=vs.85).aspx
  IDC_APPSTARTING : constant Int_Ptr := 16#7F00#; -- MAKEINTRESOURCE (0)
  
  -- https://web.archive.org/web/20160828015700/https://msdn.microsoft.coSm/en-us/library/windows/desktop/dd144909(v=vs.85).aspx
  CLR_INVALID : constant Int_Unsigned_C := 16#FFFF_FFFF#; 

  -- https://web.archive.org/web/20151028210132/https://msdn.microsoft.com/en-us/library/windows/desktop/ff729168(v=vs.85).aspx
  CF_UNICODETEXT : constant Int_Unsigned_C := 16#0000_000D#; -- 0

  -- https://web.archive.org/web/20160526201612/https://msdn.microsoft.com/en-us/library/windows/desktop/aa366574(v=vs.85).aspx
  GMEM_MOVEABLE : constant Int_Unsigned_C := 16#0000_0002#; -- 0x0000

  -- https://web.archive.org/web/20160808212528/https://msdn.microsoft.com/en-us/library/windows/desktop/ff729175(v=vs.85).aspx
  SPI_GETNONCLIENTMETRICS : constant Int_Unsigned_C := 16#0000_0029#; 

  -- https://web.archive.org/web/20160902020447/https://msdn.microsoft.com/en-us/library/windows/desktop/ms681382(v=vs.85).aspx
  ERROR_INSUFFICIENT_BUFFER : constant Int_Unsigned_C := 16#0000_007A#; -- 0

  -- https://web.archive.org/web/20160625030912/https://msdn.microsoft.com/en-us/library/windows/desktop/bb775951(v=vs.85).aspx
  BS_GROUPBOX : constant Int_Unsigned_C := 16#0000_0007#; 

  -- https://web.archive.org/web/20121031105705/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787537(v=vs.85).aspx
  SIF_ALL : constant Int_Unsigned_C := 16#0000_001F#; -- 0x0000
  
  -- https://web.archive.org/web/20140320170251/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787583(v=vs.85).aspx 
  SB_VERT : constant Int_C := 1; -- 0
  
  -- https://msdn.microsoft.com/en-us/library/windows/desktop/bb773480(v=vs.85).aspx
  KEY_QUERY_VALUE : constant := 16#0001#; -- 0x0000
  KEY_READ        : constant := 16#020019#; -- 0x0000
  
  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms724836(v=vs.85).aspx
  HKEY_CURRENT_USER  : constant Int_Ptr := 16#8000_0001#; -- 0x00000000
  HKEY_LOCAL_MACHINE : constant Int_Ptr := 16#8000_0002#; -- 0x00000000
  
  -- https://web.archive.org/web/20161121002327/https://msdn.microsoft.com/en-us/library/windows/desktop/bb787577(v=vs.85).aspx
  SB_ENDSCROLL : constant Int_Ptr := 16#0000_0007#; 
  SB_LINEDOWN  : constant Int_Ptr := 16#0000_0001#; 

  -- https://web.archive.org/web/20131006214136/http://msdn.microsoft.com/en-us/library/windows/desktop/bb775464(v=vs.85).aspx
  ES_MULTILINE : constant Int_Unsigned_C := 16#0000_0004#; 
  ES_READONLY  : constant Int_Unsigned_C := 16#0000_0800#; 
  
  -- https://web.archive.org/web/20141228152842/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644977(v=vs.85).aspx
  WH_CBT        : constant Int_C := 5; -- 0
  HCBT_ACTIVATE : constant Int_C := 5; -- 0
  
  -- https://web.archive.org/web/20160808212320/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724371(v=vs.85).aspx
  COLOR_WINDOW   : constant Int_Ptr := 16#0005#; -- 0
  COLOR_GRAYTEXT : constant Int_Ptr := 16#0011#; -- 0

  -- http://web.archive.org/web/20141224060057/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632646(v=vs.85).aspx
  SIZE_MAXIMIZED : constant Int_Ptr := 16#0000_0002#; -- 0
  SIZE_MINIMIZED : constant Int_Ptr := 16#0000_0001#; -- 0

  -- http://web.archive.org/web/20160722055429/https://msdn.microsoft.com/en-us/library/windows/desktop/ms646360(v=vs.85).aspx
  SC_SCREENSAVE : constant Int_Ptr := 16#0000_F140#; -- 0x0000
  SC_KEYMENU    : constant Int_Ptr := 16#0000_F100#; -- 0x0000

  -- https://web.archive.org/web/20140321085736/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645571(v=vs.85).aspx
  RIM_TYPEKEYBOARD : constant Int_Unsigned_C := 16#0000_0001#; -- 0
  RIM_TYPEMOUSE    : constant Int_Unsigned_C := 16#0000_0000#; -- 0

  -- https://web.archive.org/web/20120410201544/http://msdn.microsoft.com/en-us/library/windows/desktop/ms679348(v=vs.85).aspx
  FLASHW_TIMER     : constant Int_Unsigned_C := 16#0000_0004#; -- 0x00000000
  FLASHW_TIMERNOFG : constant Int_Unsigned_C := 16#0000_000C#; -- 0x00000000
  FLASHW_ALL       : constant Int_Unsigned_C := 16#0000_0003#; -- 0x00000000
  FLASHW_STOP      : constant Int_Unsigned_C := 16#0000_0000#; -- 0

  -- https://blogs.msdn.microsoft.com/openspecification/2010/04/01/about-the-access_mask-structure/
  GENERIC_READ  : constant Int_Unsigned_C := 16#8000_0000#; -- 0x00000000
  GENERIC_WRITE : constant Int_Unsigned_C := 16#4000_0000#; -- 0x00000000

  -- https://web.archive.org/web/20161024170359/https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
  FILE_SHARE_READ  : constant Int_Unsigned_C := 16#0000_0001#; -- 0x00000000
  FILE_SHARE_WRITE : constant Int_Unsigned_C := 16#0000_0002#; -- 0x00000000
  OPEN_EXISTING    : constant Int_Unsigned_C := 3;             -- 0
    
  -- https://web.archive.org/web/20160821131741/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645597%28v=vs.85%29.aspx
  RIDI_DEVICENAME    : constant Int_Unsigned_C := 16#2000_0007#; -- 0x00000000
  RIDI_DEVICEINFO    : constant Int_Unsigned_C := 16#2000_000b#; -- 0x00000000
  RIDI_PREPARSEDDATA : constant Int_Unsigned_C := 16#2000_0005#; -- 0x00000000

  -- https://web.archive.org/web/20150110160845/http://msdn.microsoft.com/en-us/library/windows/desktop/ff485923(v=vs.85).aspx
  EM_GETSEL              : constant Int_Unsigned_C := 16#0000_00B0#; -- 0x0000
  EM_SETSEL              : constant Int_Unsigned_C := 16#0000_00B1#; -- 0x0000
  EM_GETFIRSTVISIBLELINE : constant Int_Unsigned_C := 16#0000_00CE#; -- 0x0000

  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms633548(v=vs.85).aspx
  SW_SHOWNORMAL    : constant Int_C := 1; -- 0
  SW_HIDE          : constant Int_C := 0; -- 0
  SW_SHOWMINIMIZED : constant Int_C := 2; -- 0
  SW_RESTORE       : constant Int_C := 9; -- 0

  -- http://web.archive.org/web/20160713003824/https://msdn.microsoft.com/en-us/library/windows/desktop/dd375731(v=vs.85).aspx
  VK_LBUTTON   : constant Int_16_Unsigned_C := 16#0001#; -- 0x00
  VK_OEM_CLEAR : constant Int_16_Unsigned_C := 16#00FE#; -- 0x00
  VK_V_KEY     : constant Int_16_Unsigned_C := 16#0056#; -- 0x00
  VK_CONTROL   : constant Int_16_Unsigned_C := 16#0011#; -- 0x00

  -- https://web.archive.org/web/20150816061650/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648045(v=vs.85).aspx
  LR_LOADFROMFILE : constant Int_Unsigned_C := 16#0000_0010#; -- 0x00000000
  LR_DEFAULTSIZE  : constant Int_Unsigned_C := 16#0000_0040#; -- 0x00000000
  IMAGE_CURSOR    : constant Int_Unsigned_C := 16#0000_0002#; -- 0
  IMAGE_ICON      : constant Int_Unsigned_C := 16#0000_0001#; -- 0
  
  -- http://web.archive.org/web/20160808214158/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724385(v=vs.85).aspx
  SM_CYDLGFRAME : constant Int_C := 8;  -- 0 
  SM_CYVTHUMB   : constant Int_C := 9;  -- 0 
  SM_CXHTHUMB   : constant Int_C := 10; -- 0
  SM_CYSIZE     : constant Int_C := 31; -- 0
  SM_CXFRAME    : constant Int_C := 32; -- 0
  SM_CYFRAME    : constant Int_C := 33; -- 0
 
  -- https://web.archive.org/web/20160202112019/https://msdn.microsoft.com/en-us/library/windows/desktop/dd183499(v=vs.85).aspx
  FF_MODERN           : constant Int_Unsigned_C := 16#0000_0030#; 
  DEFAULT_CHARSET     : constant Int_Unsigned_C := 16#0000_0001#; 
  DEFAULT_QUALITY     : constant Int_Unsigned_C := 16#0000_0000#; 
  OUT_DEFAULT_PRECIS  : constant Int_Unsigned_C := 16#0000_0000#; 
  CLIP_DEFAULT_PRECIS : constant Int_Unsigned_C := 16#0000_0000#; 

  -- https://web.archive.org/web/20140209214910/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632647(v=vs.85).aspx
  WMSZ_BOTTOMRIGHT : constant Int_Ptr := 16#0000_0008#; -- 0
  WMSZ_BOTTOMLEFT  : constant Int_Ptr := 16#0000_0007#; -- 0
  WMSZ_TOPRIGHT    : constant Int_Ptr := 16#0000_0005#; -- 0
  WMSZ_TOPLEFT     : constant Int_Ptr := 16#0000_0004#; -- 0
  WMSZ_BOTTOM      : constant Int_Ptr := 16#0000_0006#; -- 0
  WMSZ_RIGHT       : constant Int_Ptr := 16#0000_0002#; -- 0
  WMSZ_LEFT        : constant Int_Ptr := 16#0000_0001#; -- 0
  WMSZ_TOP         : constant Int_Ptr := 16#0000_0003#; -- 0

  -- https://web.archive.org/web/20150109064332/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645565(v=vs.85).aspx
  RIDEV_INPUTSINK                : constant Int_Unsigned_C    := 16#0000_0100#; -- 0x00000000
  GENERIC_DESKTOP                : constant Int_16_Unsigned_C := 16#0001#;      
  USE_RAW_KEYBOARD               : constant Int_16_Unsigned_C := 16#0006#;      
  USE_RAW_MOUSE                  : constant Int_16_Unsigned_C := 16#0002#;      
  GET_DEVICE_HEADER              : constant Int_Unsigned_C    := 16#1000_0005#; 
  GET_DEVICE_DATA                : constant Int_Unsigned_C    := 16#1000_0003#; 
  STOP_READING_TOP_LEVEL_DEVICES : constant Int_Unsigned_C    := 16#0000_0001#; 
  KEY_MAKE_CODE_FOR_LEFT         : constant Int_16_Unsigned_C := 16#002A#;      
  KEY_IS_RIGHT_SIDED             : constant Int_16_Unsigned_C := 16#0002#;      
  MOUSE_WHEEL_DELTA              : constant Int_16_Signed     := 120;           

  -- https://web.archive.org/web/20160605071718/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505(v=vs.85).aspx
  MB_SYSTEMMODAL     : constant Int_Unsigned_C := 16#0000_1000#; -- 0x00000000L
  MB_ICONINFORMATION : constant Int_Unsigned_C := 16#0000_0040#; -- 0x00000000L
  MB_ICONWARNING     : constant Int_Unsigned_C := 16#0000_0030#; -- 0x00000000L
  MB_ICONERROR       : constant Int_Unsigned_C := 16#0000_0010#; -- 0x00000000L
  MB_OK              : constant Int_Unsigned_C := 16#0000_0000#; -- 0x00000000L
  MB_YESNO           : constant Int_Unsigned_C := 16#0000_0004#; -- 0x00000000L
  MB_OKCANCEL        : constant Int_Unsigned_C := 16#0000_0001#; -- 0x00000000L
  MB_RETRYCANCEL     : constant Int_Unsigned_C := 16#0000_0005#; -- 0x00000000L
  IDOK               : constant Int_C          := 1;             -- 0
  IDRETRY            : constant Int_C          := 4;             -- 0
  IDYES              : constant Int_C          := 6;             -- 0

  -- https://web.archive.org/web/20150816054622/https://msdn.microsoft.com/en-us/library/windows/desktop/ms632600(v=vs.85).aspx
  WS_TOPMOST       : constant Int_Unsigned_C := 16#0000_0008#; -- 0x00000000L
  WS_POPUP         : constant Int_Unsigned_C := 16#8000_0000#; -- 0x00000000L
  WS_CAPTION       : constant Int_Unsigned_C := 16#00C0_0000#; -- 0x00000000L
  WS_VSCROLL       : constant Int_Unsigned_C := 16#0020_0000#; -- 0x00000000L
  WS_EX_COMPOSITED : constant Int_Unsigned_C := 16#0200_0000#; -- 0x00000000L
  WS_MAXIMIZEBOX   : constant Int_Unsigned_C := 16#0002_0000#; -- 0x00000000L
  WS_SYSMENU       : constant Int_Unsigned_C := 16#0008_0000#; -- 0x00000000L
  WS_BORDER        : constant Int_Unsigned_C := 16#0080_0000#; -- 0x00000000L
  WS_MINIMIZEBOX   : constant Int_Unsigned_C := 16#0001_0000#; -- 0x00000000L
  WS_CHILD         : constant Int_Unsigned_C := 16#4000_0000#; -- 0x00000000L
  WS_DISABLED      : constant Int_Unsigned_C := 16#0800_0000#; -- 0x00000000L
  WS_MINIMIZE      : constant Int_Unsigned_C := 16#2000_0000#; -- 0x00000000L
  WS_VISIBLE       : constant Int_Unsigned_C := 16#1000_0000#; -- 0x00000000L
  WS_SIZEBOX       : constant Int_Unsigned_C := 16#0004_0000#; -- 0x00000000L

  -- https://web.archive.org/web/20140714222448/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx
  RI_MOUSE_LEFT_BUTTON_DOWN   : constant Int_Unsigned_C := 16#0000_0001#; -- 0x0000
  RI_MOUSE_LEFT_BUTTON_UP     : constant Int_Unsigned_C := 16#0000_0002#; -- 0x0000
  RI_MOUSE_RIGHT_BUTTON_DOWN  : constant Int_Unsigned_C := 16#0000_0004#; -- 0x0000
  RI_MOUSE_RIGHT_BUTTON_UP    : constant Int_Unsigned_C := 16#0000_0008#; -- 0x0000
  RI_MOUSE_MIDDLE_BUTTON_UP   : constant Int_Unsigned_C := 16#0000_0020#; -- 0x0000
  RI_MOUSE_MIDDLE_BUTTON_DOWN : constant Int_Unsigned_C := 16#0000_0010#; -- 0x0000
  RI_MOUSE_HORIZONTAL_WHEEL   : constant Int_Unsigned_C := 16#0000_0800#; -- 0x0000
  RI_MOUSE_VERTICAL_WHEEL     : constant Int_Unsigned_C := 16#0000_0400#; -- 0x0000
  RI_MOUSE_BUTTON_4_DOWN      : constant Int_Unsigned_C := 16#0000_0040#; -- 0x0000
  RI_MOUSE_BUTTON_4_UP        : constant Int_Unsigned_C := 16#0000_0080#; -- 0x0000
  RI_MOUSE_BUTTON_5_DOWN      : constant Int_Unsigned_C := 16#0000_0100#; -- 0x0000
  RI_MOUSE_BUTTON_5_UP        : constant Int_Unsigned_C := 16#0000_0200#; -- 0x0000   

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

  -- https://web.archive.org/web/20150109034335/http://msdn.microsoft.com/en-us/library/windows/desktop/ff468925(v=vs.85).aspx
  WM_SIZING         : constant Int_Unsigned_C := 16#0000_0214#; -- 0x0000
  WM_CLOSE          : constant Int_Unsigned_C := 16#0000_0010#; -- 0x0000
  WM_CHAR           : constant Int_Unsigned_C := 16#0000_0102#; -- 0x0000
  WM_CREATE         : constant Int_Unsigned_C := 16#0000_0001#; -- 0x0000
  WM_CTLCOLORSTATIC : constant Int_Unsigned_C := 16#0000_0138#; -- 0x0000
  WM_GETMINMAXINFO  : constant Int_Unsigned_C := 16#0000_0024#; -- 0x0000
  WM_INPUT          : constant Int_Unsigned_C := 16#0000_00FF#; -- 0x0000
  WM_QUIT           : constant Int_Unsigned_C := 16#0000_0012#; -- 0x0000
  WM_COMMAND        : constant Int_Unsigned_C := 16#0000_0111#; -- 0x0000
  WM_VSCROLL        : constant Int_Unsigned_C := 16#0000_0115#; -- 0x0000
  WM_SETREDRAW      : constant Int_Unsigned_C := 16#0000_000B#; -- 0x0000 
  WM_MOUSEWHEEL     : constant Int_Unsigned_C := 16#0000_020A#; -- 0x0000
  WM_GETTEXT        : constant Int_Unsigned_C := 16#0000_000D#; -- 0x0000
  WM_KEYDOWN        : constant Int_Unsigned_C := 16#0000_0100#; -- 0x0000 
  WM_SYSKEYDOWN     : constant Int_Unsigned_C := 16#0000_0104#; -- 0x0000 
  WM_SETTEXT        : constant Int_Unsigned_C := 16#0000_000C#; -- 0x0000
  WM_SETFONT        : constant Int_Unsigned_C := 16#0000_0030#; -- 0x0000 
  WM_SIZE           : constant Int_Unsigned_C := 16#0000_0005#; -- 0x0000
  WM_ACTIVATE       : constant Int_Unsigned_C := 16#0000_0006#; -- 0x0000
  WM_SYSCOMMAND     : constant Int_Unsigned_C := 16#0000_0112#; -- 0x0000
  WM_CTLCOLOREDIT   : constant Int_Unsigned_C := 16#0000_0133#; -- 0x0000
  WM_SETICON        : constant Int_Unsigned_C := 16#0000_0080#; -- 0x0000
  WM_GETFONT        : constant Int_Unsigned_C := 16#0000_0031#; -- 0x0000
  WM_DEVICECHANGE   : constant Int_Unsigned_C := 16#0000_0219#; -- 0x0000
  WM_DESTROY        : constant Int_Unsigned_C := 16#0000_0002#; -- 0x0000

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
  type WNDPROC is access function (hwnd   : Ptr;            -- HWND   
                                   uMsg   : Int_Unsigned_C; -- UINT   
                                   wParam : Int_Ptr;        -- WPARAM 
                                   lParam : Int_Ptr)        -- LPARAM 
                                   return Int_Ptr           -- LRESULT 
                                   with Convention => Stdcall;

  -- https://web.archive.org/web/20140625144949/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633498(v=vs.85).aspx
  type WNDENUMPROC is access function (hwnd   : Ptr;     -- HWND   
                                       lParam : Int_Ptr) -- LPARAM 
                                       return Int_C      -- BOOL 
                                       with Convention => Stdcall;

  -- https://web.archive.org/web/20150109225815/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145061(v=vs.85).aspx
  type Ptr_RECT;
  type MONITORENUMPROC is access function (hMonitor    : Ptr;      -- HMONITOR 
                                           hdcMonitor  : Ptr;      -- HDC      
                                           lprcMonitor : Ptr_RECT; -- LPRECT   
                                           dwData      : Int_Ptr)  -- LPARAM   
                                           return Int_C            -- BOOL 
                                           with Convention => Stdcall;

  ----------------
  -- Structures --
  ----------------

  -- https://web.archive.org/web/20161012142025/https://msdn.microsoft.com/en-us/library/windows/desktop/aa379561(v=vs.85).aspx
  type SECURITY_ATTRIBUTES is record
       nLength              : Int_Unsigned_C := 0;        -- DWORD
       lpSecurityDescriptor : Ptr            := NULL_PTR; -- LPVOID
       bInheritHandle       : Int_C          := 0;        -- BOOL
    end record with Convention => C; 

  -- https://web.archive.org/web/20120511143301/http://msdn.microsoft.com/en-us/library/windows/desktop/ms679348(v=vs.85).aspx
  type FLASHWINFO is record
      cbSize    : Int_Unsigned_C := FLASHWINFO'Size / Byte'Object_Size; -- UINT  
      hwnd      : Ptr            := NULL_PTR; -- HWND  
      dwFlags   : Int_Unsigned_C := 0;        -- DWORD 
      uCount    : Int_Unsigned_C := 1000;        -- UINT  
      dwTimeout : Int_Unsigned_C := 1000000;        -- DWORD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20150114021053/http://msdn.microsoft.com/en-us/library/windows/desktop/ms686331(v=vs.85).aspx
  type STARTUPINFO is record
      cb              : Int_Unsigned_C       := STARTUPINFO'Size / Byte'Object_Size; -- DWORD  
      lpReserved      : Ptr_Char_16_C        := null;     -- LPTSTR 
      lpDesktop       : Ptr_Char_16_C        := null;     -- LPTSTR 
      lpTitle         : Ptr_Char_16_C        := null;     -- LPTSTR 
      dwX             : Int_Unsigned_C       := 0;        -- DWORD
      dwY             : Int_Unsigned_C       := 0;        -- DWORD
      dwXSize         : Int_Unsigned_C       := 0;        -- DWORD
      dwYSize         : Int_Unsigned_C       := 0;        -- DWORD
      dwXCountChars   : Int_Unsigned_C       := 0;        -- DWORD
      dwYCountChars   : Int_Unsigned_C       := 0;        -- DWORD
      dwFillAttribute : Int_Unsigned_C       := 0;        -- DWORD
      dwFlags         : Int_Unsigned_C       := 0;        -- DWORD
      wShowWindow     : Int_16_Unsigned_C    := 0;        -- WORD   
      cbReserved2     : Int_16_Unsigned_C    := 0;        -- WORD   
      lpReserved2     : Ptr_Int_8_Unsigned_C := null;     -- LPBYTE 
      hStdInput       : Ptr                  := NULL_PTR; -- HANDLE
      hStdOutput      : Ptr                  := NULL_PTR; -- HANDLE
      hStdError       : Ptr                  := NULL_PTR; -- HANDLE
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160302060800/https://msdn.microsoft.com/en-us/library/windows/desktop/ms684873(v=vs.85).aspx
  type PROCESS_INFORMATION is record
      hProcess    : Ptr            := NULL_PTR; -- HANDLE 
      hThread     : Ptr            := NULL_PTR; -- HANDLE 
      dwProcessId : Int_Unsigned_C := 0;        -- DWORD  
      dwThreadId  : Int_Unsigned_C := 0;        -- DWORD  
    end record with Convention => C;
    
  -- https://web.archive.org/web/20141225053346/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162897(v=vs.85).aspx
  type RECT is record
      left   : Int_C := 0; -- LONG
      top    : Int_C := 0; -- LONG
      right  : Int_C := 0; -- LONG
      bottom : Int_C := 0; -- LONG
    end record with Convention => C;
  type Ptr_RECT is access all RECT;
  function To_Ptr_RECT is new Unchecked_Conversion (Int_Ptr, Ptr_RECT);
    
  -- https://web.archive.org/web/20150114082405/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145065(v=vs.85).aspx
  type MONITORINFO is record
      cbSize    : Int_Unsigned_C := MONITORINFO'Size / Byte'Object_Size; -- DWORD 
      rcMonitor : RECT           := (others => <>); -- RECT  
      rcWork    : RECT           := (others => <>); -- RECT  
      dwFlags   : Int_Unsigned_C := 0;              -- DWORD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160129165906/https://msdn.microsoft.com/en-us/library/windows/desktop/dd162805(v=vs.85).aspx
  type POINT is record
      x : Int_C := 0; -- LONG 
      y : Int_C := 0; -- LONG 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160131052119/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644958(v=vs.85).aspx
  type MSG is record
      hwnd    : Ptr            := NULL_PTR;       -- HWND   
      message : Int_Unsigned_C := 0;              -- UINT   
      wParam  : Int_Ptr        := 0;              -- WPARAM 
      lParam  : Int_Ptr        := 0;              -- LPARAM 
      time    : Int_Unsigned_C := 0;              -- DWORD  
      pt      : POINT          := (others => <>); -- POINT  
    end record with Convention => C;
  type Ptr_MSG is access all MSG;
    
  -- https://web.archive.org/web/20160527143421/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633577(v=vs.85).aspx
  type WNDCLASSEX is record
      cbSize        : Int_Unsigned_C := WNDCLASSEX'Size / Byte'Object_Size; -- UINT      
      style         : Int_Unsigned_C := 0;        -- UINT      
      lpfnWndProc   : Address        := NULL_PTR; -- WNDPROC   
      cbClsExtra    : Int_C          := 0;        -- int       
      cbWndExtra    : Int_C          := 0;        -- int       
      hInstance     : Ptr            := NULL_PTR; -- HINSTANCE 
      hIcon         : Ptr            := NULL_PTR; -- HICON     
      hCursor       : Ptr            := NULL_PTR; -- HCURSOR   
      hbrBackground : Int_Ptr        := 0;        -- HBRUSH    
      lpszMenuName  : Ptr_Char_16_C  := null;     -- LPCTSTR   
      lpszClassName : Ptr_Char_16_C  := null;     -- LPCTSTR   
      hIconSm       : Ptr            := NULL_PTR; -- HICON     
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160404075807/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_gamepad(v=vs.85).aspx
  type XINPUT_GAMEPAD is record
      wButtons      : Int_16_Unsigned_C := 0; -- WORD  
      bLeftTrigger  : Int_8_Unsigned_C  := 0; -- BYTE  
      bRightTrigger : Int_8_Unsigned_C  := 0; -- BYTE  
      sThumbLX      : Int_16_Signed_C   := 0; -- SHORT 
      sThumbLY      : Int_16_Signed_C   := 0; -- SHORT 
      sThumbRX      : Int_16_Signed_C   := 0; -- SHORT 
      sThumbRY      : Int_16_Signed_C   := 0; -- SHORT 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20150101223349/http://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_state(v=vs.85).aspx
  type XINPUT_STATE is record
      dwPacketNumber : Int_Unsigned_C := 0;              -- DWORD          
      Gamepad        : XINPUT_GAMEPAD := (others => <>); -- XINPUT_GAMEPAD 
    end record with Convention => C;
    
  -- https://web.archive.org/web/20160509011549/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinput_vibration(v=vs.85).aspx
  type XINPUT_VIBRATION is record
      wLeftMotorSpeed  : Int_16_Unsigned_C := 0; -- WORD 
      wRightMotorSpeed : Int_16_Unsigned_C := 0; -- WORD 
    end record with Convention => C;
    
 -- https://web.archive.org/web/20150109064332/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645565(v=vs.85).aspx
 type RAWINPUTDEVICE is record
      usUsagePage : Int_16_Unsigned_C := 0;        -- USHORT 
      usUsage     : Int_16_Unsigned_C := 0;        -- USHORT 
      dwFlags     : Int_Unsigned_C    := 0;        -- DWORD  
      hwndTarget  : Ptr               := NULL_PTR; -- HWND
    end record with Convention => C;
    
  -- https://web.archive.org/web/20140321090313/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645568(v=vs.85).aspx
  type RAWINPUTDEVICELIST is record
      hDevice : Ptr            := NULL_PTR; -- HANDLE 
      dwType  : Int_Unsigned_C := 0;        -- DWORD  
    end record with Convention => C;
 
  -- https://web.archive.org/web/20140321085736/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645571(v=vs.85).aspx
  type RAWINPUTHEADER is record
      dwType  : Int_Unsigned_C := 0;        -- DWORD  
      dwSize  : Int_Unsigned_C := 0;        -- DWORD  
      hDevice : Ptr            := NULL_PTR; -- HANDLE 
      wParam  : Int_C          := 0;        -- WPARAM 
    end record with Convention => C;
  
  -- https://web.archive.org/web/20150107130951/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645575(v=vs.85).aspx
  type RAWKEYBOARD is record
      Header           : RAWINPUTHEADER    := (others => <>); -- RAWINPUTHEADER
      MakeCode         : Int_16_Unsigned_C := 0;              -- USHORT
      Flags            : Int_16_Unsigned_C := 0;              -- USHORT
      Reserved         : Int_16_Unsigned_C := 0;              -- USHORT
      VKey             : Int_16_Unsigned_C := 0;              -- USHORT
      Message          : Int_Unsigned_C    := 0;              -- UINT
      ExtraInformation : Int_Unsigned_C    := 0;              -- ULONG
    end record with Convention => C;
  
  -- https://web.archive.org/web/20140714222448/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645578(v=vs.85).aspx
  type RAWMOUSE is record
      Header             : RAWINPUTHEADER    := (others => <>); -- RAWINPUTHEADER
      usFlags            : Int_16_Unsigned_C := 0;              -- USHORT 
      usButtons          : Int_Unsigned_C    := 0;              -- ULONG 
      ulRawButtons       : Int_Unsigned_C    := 0;              -- ULONG  
      lLastX             : Int_C             := 0;              -- LONG   
      lLastY             : Int_C             := 0;              -- LONG   
      ulExtraInformation : Int_Unsigned_C    := 0;              -- ULONG  
    end record with Convention => C;
  
  -- https://web.archive.org/web/20141109015511/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145106(v=vs.85).aspx
  type SIZE is record
      cx : Int_Unsigned_C := 0; -- LONG 
      cy : Int_Unsigned_C := 0; -- LONG 
    end record with Convention => C;
  
  -- https://web.archive.org/web/20121031105705/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787537(v=vs.85).aspx
  type SCROLLINFO is record
      cbSize    : Int_Unsigned_C := SCROLLINFO'Size / Byte'Object_Size; -- UINT 
      fMask     : Int_Unsigned_C := 0; -- UINT 
      nMin      : Int_C          := 0; -- int  
      nMax      : Int_C          := 0; -- int  
      nPage     : Int_Unsigned_C := 0; -- UINT 
      nPos      : Int_C          := 0; -- int  
      nTrackPos : Int_C          := 0; -- int  
    end record with Convention => C;
  
  -- https://web.archive.org/web/20140902090739/http://msdn.microsoft.com/en-us/library/windows/desktop/ms632605(v=vs.85).aspx
  type MINMAXINFO is record
      ptReserved     : POINT := (others => <>); -- POINT 
      ptMaxSize      : POINT := (others => <>); -- POINT 
      ptMaxPosition  : POINT := (others => <>); -- POINT 
      ptMinTrackSize : POINT := (others => <>); -- POINT 
      ptMaxTrackSize : POINT := (others => <>); -- POINT 
    end record with Convention => C;
  type Ptr_MINMAXINFO        is access all MINMAXINFO;
  function To_Ptr_MINMAXINFO is new Unchecked_Conversion (Int_Ptr, Ptr_MINMAXINFO);
   
  -- https://web.archive.org/web/20160901051039/https://msdn.microsoft.com/en-us/library/windows/desktop/dd145037(v=vs.85).aspx
  type LOGFONT is record  
      lfHeight         : Int_C            := 0; -- LONG  
      lfWidth          : Int_C            := 0; -- LONG  
      lfEscapement     : Int_C            := 0; -- LONG  
      lfOrientation    : Int_C            := 0; -- LONG  
      lfWeight         : Int_C            := 0; -- LONG  
      lfItalic         : Int_8_Unsigned_C := 0; -- BYTE  
      lfUnderline      : Int_8_Unsigned_C := 0; -- BYTE  
      lfStrikeOut      : Int_8_Unsigned_C := 0; -- BYTE  
      lfCharSet        : Int_8_Unsigned_C := 0; -- BYTE  
      lfOutPrecision   : Int_8_Unsigned_C := 0; -- BYTE  
      lfClipPrecision  : Int_8_Unsigned_C := 0; -- BYTE  
      lfQuality        : Int_8_Unsigned_C := 0; -- BYTE  
      lfPitchAndFamily : Int_8_Unsigned_C := 0; -- BYTE  
      lfFaceName       : Str_16_C (1..32) := (others => NULL_CHAR_16_C); -- TCHAR [LF_FACESIZE]
    end record with Convention => C;
  
  -- https://web.archive.org/web/20160808212528/https://msdn.microsoft.com/en-us/library/windows/desktop/ff729175(v=vs.85).aspx
  type NONCLIENTMETRICS is record
      cbSize             :         Int_Unsigned_C := NONCLIENTMETRICS'Size / Byte'Object_Size; -- UINT
      iBorderWidth       :         Int_C   := 0;              -- int     
      iScrollWidth       :         Int_C   := 0;              -- int     
      iScrollHeight      :         Int_C   := 0;              -- int     
      iCaptionWidth      :         Int_C   := 0;              -- int     
      iCaptionHeight     :         Int_C   := 0;              -- int     
      lfCaptionFont      : aliased LOGFONT := (others => <>); -- LOGFONT 
      iSmCaptionWidth    :         Int_C   := 0;              -- int     
      iSmCaptionHeight   :         Int_C   := 0;              -- int     
      lfSmCaptionFont    : aliased LOGFONT := (others => <>); -- LOGFONT 
      iMenuWidth         :         Int_C   := 0;              -- int     
      iMenuHeight        :         Int_C   := 0;              -- int     
      lfMenuFont         : aliased LOGFONT := (others => <>); -- LOGFONT 
      lfStatusFont       : aliased LOGFONT := (others => <>); -- LOGFONT 
      lfMessageFont      : aliased LOGFONT := (others => <>); -- LOGFONT 
      iPaddedBorderWidth :         Int_C   := 0;              -- int     
    end record with Convention => C;
  
  -- https://web.archive.org/web/20150113013334/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145132(v=vs.85).aspx
  type TEXTMETRIC is record
      tmHeight           : Int_C            := 0;              -- LONG  
      tmAscent           : Int_C            := 0;              -- LONG  
      tmDescent          : Int_C            := 0;              -- LONG  
      tmInternalLeading  : Int_C            := 0;              -- LONG  
      tmExternalLeading  : Int_C            := 0;              -- LONG  
      tmAveCharWidth     : Int_C            := 0;              -- LONG  
      tmMaxCharWidth     : Int_C            := 0;              -- LONG  
      tmWeight           : Int_C            := 0;              -- LONG  
      tmOverhang         : Int_C            := 0;              -- LONG  
      tmDigitizedAspectX : Int_C            := 0;              -- LONG  
      tmDigitizedAspectY : Int_C            := 0;              -- LONG  
      tmFirstChar        : Char_16_C        := NULL_CHAR_16_C; -- TCHAR 
      tmLastChar         : Char_16_C        := NULL_CHAR_16_C; -- TCHAR 
      tmDefaultChar      : Char_16_C        := NULL_CHAR_16_C; -- TCHAR 
      tmBreakChar        : Char_16_C        := NULL_CHAR_16_C; -- TCHAR 
      tmItalic           : Int_8_Unsigned_C := 0;              -- BYTE  
      tmUnderlined       : Int_8_Unsigned_C := 0;              -- BYTE  
      tmStruckOut        : Int_8_Unsigned_C := 0;              -- BYTE  
      tmPitchAndFamily   : Int_8_Unsigned_C := 0;              -- BYTE  
      tmCharSet          : Int_8_Unsigned_C := 0;              -- BYTE  
    end record with Convention => C;
 
  -----------------
  -- Subprograms --
  -----------------
    
  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms724837(v=vs.85).aspx
  function RegOpenKeyExW (hKey       : Int_Ptr;        -- HKEY
                          lpSubKey   : Ptr_Char_16_C;  -- LPCTSTR
                          ulOptions  : Int_Unsigned_C; -- DWORD
                          samDesired : Int_Unsigned_C; -- REGSAM
                          phkResult  : Ptr_Ptr)        -- PHKEY
                          return Int_Unsigned_C        -- LONG
                          with Import => True, Convention => StdCall, External_Name => "RegOpenKeyExW";
                        
  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms724837(v=vs.85).aspx
  function RegCloseKey (hKey : Ptr)           -- HKEY
                        return Int_Unsigned_C -- LONG
                        with Import => True, Convention => StdCall, External_Name => "RegCloseKey";

  -- https://msdn.microsoft.com/en-us/library/windows/desktop/ms724911(v=vs.85).aspx
  function RegQueryValueExW (hKey        : Ptr;                -- HKEY
                             lpValueName : Ptr_Char_16_C;      -- LPCTSTR
                             lpReserved  : Ptr_Int_Unsigned_C; -- LPDWORD
                             lpType      : Ptr_Int_Unsigned_C; -- LPDWORD
                             lpData      : Ptr;                -- LPBYTE
                             lpcbData    : Ptr_Int_Unsigned_C) -- LPDWORD
                             return Int_Unsigned_C             -- BOOL
                             with Import => True, Convention => StdCall, External_Name => "RegQueryValueExW";

  -- https://web.archive.org/web/20161024170359/https://msdn.microsoft.com/en-us/library/windows/desktop/aa363858(v=vs.85).aspx
  function CreateFileW (lpFileName            :        Ptr_Char_16_C;       -- LPCTSTR
                        dwDesiredAccess       :        Int_Unsigned_C;      -- DWORD
                        dwShareMode           :        Int_Unsigned_C;      -- DWORD
                        lpSecurityAttributes  : access SECURITY_ATTRIBUTES; -- LPSECURITY_ATTRIBUTES
                        dwCreationDisposition :        Int_Unsigned_C;      -- DWORD
                        dwFlagsAndAttributes  :        Int_Unsigned_C;      -- DWORD
                        hTemplateFile         :        Ptr)                 -- HANDLE
                        return Ptr                                          -- HANDLE
                        with Import => True, Convention => StdCall, External_Name => "CreateFileW";

  -- https://web.archive.org/web/20140310215229/http://msdn.microsoft.com/en-us/library/windows/hardware/ff539681(v=vs.85).aspx
  function HidD_GetProductString (HidDeviceObject : Ptr;            -- HANDLE 
                                  Buffer          : Ptr;            -- PVOID  
                                  BufferLength    : Int_Unsigned_C) -- ULONG  
                                  return Int_C                      -- BOOLEAN
                                  with Import => True, Convention => StdCall, External_Name => "HidD_GetProductString";

  -- https://web.archive.org/web/20140529093020/http://msdn.microsoft.com/en-us/library/windows/hardware/ff538959(v=vs.85).aspx
  function HidD_GetManufacturerString (HidDeviceObject : Ptr;            -- HANDLE 
                                       Buffer          : Ptr;            -- PVOID  
                                       BufferLength    : Int_Unsigned_C) -- ULONG  
                                       return Int_C                      -- BOOLEAN
                                       with Import => True, Convention => StdCall, External_Name => "HidD_GetManufacturerString";

  -- https://web.archive.org/web/20160821131741/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645597%28v=vs.85%29.aspx
  function GetRawInputDeviceInfoW (hDevice   : Ptr;            -- HANDLE
                                   uiCommand : Int_Unsigned_C; -- UINT
                                   pData     : Ptr;            -- LPVOID
                                   pcbSize   : Ptr)            -- PUINT 
                                   return Int_Unsigned_C       -- UINT 
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
  function GetLastError return Int_Unsigned_C -- DWORD 
                        with Import => True, Convention => StdCall, External_Name => "GetLastError";
       
  -- https://web.archive.org/web/20141228222857/http://msdn.microsoft.com/en-us/library/windows/desktop/ms724432(v=vs.85).aspx                             
  function GetUserNameW (lpBuffer :        Ptr_Char_16_C; -- LPTSTR  
                         lpnSize  : access Int_C)         -- LPDWORD 
                         return Int_C                     -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "GetUserNameW";

  -- https://web.archive.org/web/20160722062551/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683197(v=vs.85).aspx    
  function GetModuleFileNameW (hModule    : Ptr;            -- HMODULE 
                               lpFilename : Ptr_Char_16_C;  -- LPTSTR  
                               nSize      : Int_Unsigned_C) -- DWORD   
                               return Int_Unsigned_C        -- DWORD 
                               with Import => True, Convention => StdCall, External_Name => "GetModuleFileNameW";
                                    
  -- https://web.archive.org/web/20150325193818/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683179(v=vs.85).aspx                       
  function GetCurrentProcess return Ptr -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "GetCurrentProcess";

  -- https://web.archive.org/web/20160830030005/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724947(v=vs.85).aspx         
  function SystemParametersInfoW (uiAction : Int_Unsigned_C; -- UINT  
                                  uiParam  : Int_Unsigned_C; -- UINT  
                                  pvParam  : Ptr;            -- PVOID 
                                  fWinIni  : Int_Unsigned_C) -- UINT  
                                  return Int_C               -- BOOL 
                                  with Import => True, Convention => StdCall, External_Name => "SystemParametersInfoW";

  -- https://web.archive.org/web/20120711225707/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648403(v=vs.85).aspx
  function HideCaret (hWnd : Ptr) -- HWND
                     return Int_C -- BOOL
                     with Import => True, Convention => StdCall, External_Name => "HideCaret";
  
  -- https://web.archive.org/web/20160526201612/https://msdn.microsoft.com/en-us/library/windows/desktop/aa366574(v=vs.85).aspx                  
  function GlobalAlloc (uFlags  : Int_Unsigned_C; -- UINT   
                        dwBytes : Int_Size_C)     -- SIZE_T 
                        return Ptr                -- HGLOBAL 
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
  function SetClipboardData (uFormat : Int_Unsigned_C; -- UINT   
                             hMem    : Ptr)            -- HANDLE 
                             return Ptr                -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "SetClipboardData";

  -- https://web.archive.org/web/20150109231948/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649035(v=vs.85).aspx
  function CloseClipboard return Int_C -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "CloseClipboard";
                      
  -- https://web.archive.org/web/20150118081856/http://msdn.microsoft.com/en-us/library/windows/desktop/ms649039(v=vs.85).aspx              
  function GetClipboardData (uFormat : Int_Unsigned_C) -- UINT 
                             return Ptr                -- HANDLE 
                             with Import => True, Convention => StdCall, External_Name => "GetClipboardData";
                                    
  -- https://web.archive.org/web/20150325220213/https://msdn.microsoft.com/en-us/library/windows/desktop/ms679347(v=vs.85).aspx
  function FlashWindowEx (pfwi : access FLASHWINFO) -- PFLASHWINFO 
                          return Int_C              -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "FlashWindowEx";
                     
  -- https://web.archive.org/web/20160607101505/https://msdn.microsoft.com/en-us/library/windows/desktop/ms682425%28v=vs.85%29.aspx               
  function CreateProcessW (lpApplicationName    :        Ptr_Char_16_C;       -- LPCTSTR               
                           lpCommandLine        :        Ptr_Char_16_C;       -- LPTSTR                
                           lpProcessAttributes  : access SECURITY_ATTRIBUTES; -- LPSECURITY_ATTRIBUTES 
                           lpThreadAttributes   : access SECURITY_ATTRIBUTES; -- LPSECURITY_ATTRIBUTES 
                           bInheritHandles      :        Int_C;               -- BOOL                  
                           dwCreationFlags      :        Int_Unsigned_C;      -- DWORD                 
                           lpEnvironment        :        Ptr;                 -- LPVOID                
                           lpCurrentDirectory   :        Ptr_Char_16_C;       -- LPCTSTR               
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
  function GetCurrentThreadId return Int_Unsigned_C -- DWORD 
                              with Import => True, Convention => StdCall, External_Name => "GetCurrentThreadId";
                                    
  -- https://web.archive.org/web/20160603220225/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633582(v=vs.85).aspx
  function GetClassNameW (hWnd        : Ptr;           -- HWND
                          lpClassName : Ptr_Char_16_C; -- LPTSTR 
                          nMaxCount   : Int_C)         -- int    
                          return Int_C                 -- int
                          with Import => True, Convention => StdCall, External_Name => "GetClassNameW";
                           
  -- https://web.archive.org/web/20150816061650/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648045(v=vs.85).aspx         
  function LoadImageW (hinst     : Ptr;            -- HINSTANCE 
                       lpszName  : Ptr_Char_16_C;  -- LPCTSTR   
                       uType     : Int_Unsigned_C; -- UINT      
                       cxDesired : Int_C;          -- int       
                       cyDesired : Int_C;          -- int       
                       fuLoad    : Int_Unsigned_C) -- UINT      
                       return Ptr                  -- HANDLE 
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
  function GetClipCursor (lpRect : Ptr_RECT) -- LPRECT 
                          return Int_C       -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetClipCursor";
                     
  -- https://web.archive.org/web/20160624094706/https://msdn.microsoft.com/en-us/library/windows/desktop/ms648390(v=vs.85).aspx               
  function GetCursorPos (lpPoint : access POINT) -- LPPOINT
                         return Int_C            -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "GetCursorPos";
                                    
  -- http://web.archive.org/web/20160213181912/https://msdn.microsoft.com/en-us/library/windows/desktop/ms683199(v=vs.85).aspx                 
  function GetModuleHandleW (lpModuleName : Ptr_Char_16_C) -- LPCTSTR 
                             return Ptr                    -- HMODULE 
                             with Import => True, Convention => Stdcall, External_Name => "GetModuleHandleW";
      
  -- https://web.archive.org/web/20140402070151/http://msdn.microsoft.com/en-us/library/windows/desktop/ms644993(v=vs.85).aspx
  function UnhookWindowsHookEx (hhk : Ptr)   -- HHOOK 
                                return Int_C -- BOOL 
                                with Import => True, Convention => StdCall, External_Name => "UnhookWindowsHookEx";
                              
  -- https://web.archive.org/web/20160804094136/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644990(v=vs.85).aspx          
  function SetWindowsHookExW (idHook     : Int_C;          -- int       
                              lpfn       : Ptr;            -- HOOKPROC  
                              hMod       : Ptr;            -- HINSTANCE 
                              dwThreadId : Int_Unsigned_C) -- DWORD     
                              return Ptr                   -- HHOOK 
                              with Import => True, Convention => StdCall, External_Name => "SetWindowsHookExW";
                                    
  -- https://web.archive.org/web/20160605071718/https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505(v=vs.85).aspx
  function MessageBoxW (hWnd      : Ptr;               -- HWND    
                        lpText    : Ptr_Char_16_C;     -- LPCTSTR 
                        lpCaption : Ptr_Char_16_C;     -- LPCTSTR 
                        uType     : Int_Unsigned_C)    -- UINT    
                        return Int_C                   -- int 
                        with Import => True, Convention => StdCall, External_Name => "MessageBoxW";
                             
  -- https://web.archive.org/web/20150815182728/https://msdn.microsoft.com/en-us/library/windows/desktop/ms632680(v=vs.85).aspx       
  function CreateWindowExW (dwExStyle    : Int_Unsigned_C; -- DWORD     
                            lpClassName  : Ptr_Char_16_C;  -- LPCTSTR 
                            lpWindowName : Ptr_Char_16_C;  -- LPCTSTR   
                            dwStyle      : Int_Unsigned_C; -- DWORD     
                            x            : Int_C;          -- int       
                            y            : Int_C;          -- int       
                            nWidth       : Int_C;          -- int       
                            nHeight      : Int_C;          -- int       
                            hWndParent   : Ptr;            -- HWND      
                            hMenu        : Int_Ptr;        -- HMENU     
                            hInstance    : Ptr;            -- HINSTANCE 
                            lpParam      : Ptr)            -- LPVOID    
                            return Ptr                     -- HWND 
                            with Import => True, Convention => StdCall, External_Name => "CreateWindowExW";
                                  
  -- https://web.archive.org/web/20160808214158/https://msdn.microsoft.com/en-us/library/windows/desktop/ms724385(v=vs.85).aspx  
  function GetSystemMetrics (nIndex : Int_C) -- int 
                             return Int_C    -- int 
                             with Import => True, Convention => StdCall, External_Name => "GetSystemMetrics";
                                 
  -- https://web.archive.org/web/20160223131327/https://msdn.microsoft.com/en-us/library/windows/desktop/ms682411(v=vs.85).aspx   
  function CreateMutexW (lpMutexAttributes : access SECURITY_ATTRIBUTES; -- LPSECURITY_ATTRIBUTES 
                         bInitialOwner     :        Int_C;               -- BOOL                  
                         lpName            :        Ptr_Char_16_C)       -- LPCTSTR               
                         return Ptr                                      -- HANDLE 
                         with Import => True, Convention => StdCall, External_Name => "CreateMutexW";
                                    
  -- https://web.archive.org/web/20160505131151/https://msdn.microsoft.com/en-us/library/windows/desktop/ms685066(v=vs.85).aspx             
  function ReleaseMutex (hMutex : Ptr) -- HANDLE 
                         return Int_C  -- BOOL 
                         with Import => True, Convention => StdCall, External_Name => "ReleaseMutex";
                                    
  -- https://web.archive.org/web/20160605125803/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633499(v=vs.85).aspx
  function FindWindowW (lpClassName  : Ptr_Char_16_C; -- LPCTSTR 
                        lpWindowName : Ptr_Char_16_C) -- LPCTSTR 
                        return Ptr                    -- HWND 
                        with Import => True, Convention => StdCall, External_Name => "FindWindowW";
                                  
  -- https://web.archive.org/web/20160131040502/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633545(v=vs.85).aspx
  function SetWindowPos (hWnd            : Ptr;            -- HWND 
                         hWndInsertAfter : Ptr;            -- HWND 
                         X               : Int_C;          -- int  
                         Y               : Int_C;          -- int  
                         cx              : Int_C;          -- int  
                         cy              : Int_C;          -- int  
                         uFlags          : Int_Unsigned_C) -- UINT 
                         return Int_C                      -- BOOL
                         with Import => True, Convention => StdCall, External_Name => "SetWindowPos";
                                    
  -- https://web.archive.org/web/20150118082712/http://msdn.microsoft.com/en-us/library/windows/desktop/dd144901(v=vs.85).aspx
  function GetMonitorInfoW (hMonitor :        Ptr;         -- HMONITOR      
                            lpmi     : access MONITORINFO) -- LPMONITORINFO 
                            return Int_C                   -- BOOL 
                            with Import => True, Convention => StdCall, External_Name => "GetMonitorInfoW";
                       
  -- https://web.archive.org/web/20150107150037/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162610(v=vs.85).aspx             
  function EnumDisplayMonitors (hdc      : Ptr;      -- HDC             
                                lprcClip : Ptr_RECT; -- LPCRECT         
                                lpfnEnum : Ptr;      -- MONITORENUMPROC 
                                dwData   : Int_C)    -- LPARAM          
                                return Int_C         -- BOOL 
                                with Import => True, Convention => StdCall, External_Name => "EnumDisplayMonitors";
                                 
  -- https://web.archive.org/web/20150110222635/http://msdn.microsoft.com/en-us/library/windows/desktop/ms633519(v=vs.85).aspx  
  function GetWindowRect (hWnd   : Ptr;      -- HWND   
                          lpRect : Ptr_RECT) -- LPRECT 
                          return Int_C       -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetWindowRect";
                                
  -- https://web.archive.org/web/20160519224341/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633591(v=vs.85).aspx
  function SetWindowLongW (hWnd      : Ptr;            -- HWND 
                           nIndex    : Int_C;          -- int  
                           dwNewLong : Int_Unsigned_C) -- LONG 
                           return Int_Unsigned_C       -- LONG 
                           with Import => True, Convention => StdCall, External_Name => "SetWindowLongW";
                                  
  -- https://web.archive.org/web/20150311211447/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633504%28v=vs.85%29.aspx
  function GetDesktopWindow return Ptr -- HWND 
                            with Import => True, Convention => StdCall, External_Name => "GetDesktopWindow";
                                    
  -- https://web.archive.org/web/20151125225543/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633572(v=vs.85).aspx    
  function DefWindowProcW (hWnd   : Ptr;            -- HWND   
                           Msg    : Int_Unsigned_C; -- UINT   
                           wParam : Int_Ptr;        -- WPARAM 
                           lParam : Int_Ptr)        -- LPARAM 
                           return Int_Ptr           -- LRESULT 
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
  function RedrawWindow (hWnd       : Ptr;            -- HWND 
                         lprcUpdate : Ptr_RECT;       -- const RECT*
                         hrgnUpdate : Ptr;            -- HRGN 
                         flags      : Int_Unsigned_C) -- UINT 
                         return Int_C                 -- BOOL
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
  function XInputSetState (dwUserIndex :        Int_Unsigned_C;   -- DWORD            
                           pVibration  : Ptr) -- access XINPUT_VIBRATION) -- XINPUT_VIBRATION*
                           return Int_Unsigned_C                  -- DWORD
                           with Import => True, Convention => StdCall, External_Name => "XInputSetState";
                                
  -- https://web.archive.org/web/20160404075716/https://msdn.microsoft.com/en-us/library/windows/desktop/microsoft.directx_sdk.reference.xinputgetstate(v=vs.85).aspx    
  function XInputGetState (dwUserIndex :        Int_Unsigned_C; -- DWORD        
                           pState      : Ptr) -- access XINPUT_STATE)   -- XINPUT_STATE*
                           return Int_Unsigned_C                -- DWORD
                           with Import => True, Convention => StdCall, External_Name => "XInputGetState";
                                   
  -- https://web.archive.org/web/20140321145747/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645596(v=vs.85).aspx
  function GetRawInputData (hRawInput    : Ptr;                   -- HRAWINPUT 
                            uiCommand    : Int_Unsigned_C;        -- UINT      
                            pData        : Ptr;                   -- LPVOID    
                            pcbSize      : Ptr_Int_32_Unsigned_C; -- PUINT     
                            cbSizeHeader : Int_Unsigned_C)        -- UINT      
                            return Int_Unsigned_C                 -- UINT 
                            with Import => True, Convention => StdCall, External_Name => "GetRawInputData";
                                    
  -- https://web.archive.org/web/20150114022224/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645600(v=vs.85).aspx
  function RegisterRawInputDevices (pRawInputDevices : Ptr;            -- PCRAWINPUTDEVICE 
                                    uiNumDevices     : Int_Unsigned_C; -- UINT             
                                    cbSize           : Int_Unsigned_C) -- UINT             
                                    return Int_C                       -- BOOL 
                                    with Import => True, Convention => StdCall, External_Name => "RegisterRawInputDevices";
                           
  -- https://web.archive.org/web/20140608172723/http://msdn.microsoft.com/en-us/library/windows/desktop/ms645598(v=vs.85).aspx         
  function GetRawInputDeviceList (pRawInputDeviceList : Ptr;                -- PRAWINPUTDEVICELIST 
                                  puiNumDevices       : Ptr_Int_Unsigned_C; -- PUINT               
                                  cbSize              : Int_Unsigned_C)     -- UINT                
                                  return Int_C                              -- UINT 
                                  with Import => True, Convention => StdCall, External_Name => "GetRawInputDeviceList";
         
  -- https://web.archive.org/web/20140320170251/http://msdn.microsoft.com/en-us/library/windows/desktop/bb787583(v=vs.85).aspx                           
  function GetScrollInfo (hwnd  :        Ptr;        -- HWND         
                          fnBar :        Int_C;      -- int          
                          lpsi  : access SCROLLINFO) -- LPSCROLLINFO 
                          return Int_C               -- BOOL 
                          with Import => True, Convention => StdCall, External_Name => "GetScrollInfo";

  -- https://web.archive.org/web/20160610020032/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644950(v=vs.85).aspx            
  function SendMessageW (hWnd   : Ptr;            -- HWND   
                         Msg    : Int_Unsigned_C; -- UINT  
                         wParam : Int_Ptr;        -- WPARAM 
                         lParam : Int_Ptr)        -- LPARAM 
                         return Int_Ptr           -- LRESULT 
                         with Import => True, Convention => StdCall, External_Name => "SendMessageW";
                       
  -- https://web.archive.org/web/20160202112019/https://msdn.microsoft.com/en-us/library/windows/desktop/dd183499(v=vs.85).aspx           
  function CreateFontW (nHeight            : Int_C;          -- int     
                        nWidth             : Int_C;          -- int     
                        nEscapement        : Int_C;          -- int     
                        nOrientation       : Int_C;          -- int     
                        fnWeight           : Int_C;          -- int     
                        fdwItalic          : Int_Unsigned_C; -- DWORD   
                        fdwUnderline       : Int_Unsigned_C; -- DWORD   
                        fdwStrikeOut       : Int_Unsigned_C; -- DWORD   
                        fdwCharSet         : Int_Unsigned_C; -- DWORD   
                        fdwOutputPrecision : Int_Unsigned_C; -- DWORD   
                        fdwClipPrecision   : Int_Unsigned_C; -- DWORD   
                        fdwQuality         : Int_Unsigned_C; -- DWORD   
                        fdwPitchAndFamily  : Int_Unsigned_C; -- DWORD   
                        lpszFace           : Ptr_Str_16_C)   -- lpszFace   
                        return Ptr                           -- HFONT
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
  function GetTextExtentPoint32W (hdc      :        Ptr;           -- HDC     
                                  lpString :        Ptr_Char_16_C; -- LPCTSTR 
                                  c        :        Int_C;         -- int     
                                  lpSize   : access SIZE)          -- LPSIZE  
                                  return Int_C                     -- BOOL 
                                  with Import => True, Convention => StdCall, External_Name => "GetTextExtentPoint32W";
                                    
  -- https://web.archive.org/web/20140620200556/http://msdn.microsoft.com/en-us/library/windows/desktop/dd145093(v=vs.85).aspx    
  function SetTextColor (hdc     : Ptr;            -- HDC      
                         crColor : Int_Unsigned_C) -- COLORREF 
                         return Int_Unsigned_C     -- COLORREF
                         with Import => True, Convention => StdCall, External_Name => "SetTextColor";
                       
  -- https://web.archive.org/web/20150112014144/http://msdn.microsoft.com/en-us/library/windows/desktop/dd162964(v=vs.85).aspx                       
  function SetBkColor (hdc     : Ptr;            -- HDC      
                       crColor : Int_Unsigned_C) -- COLORREF 
                       return Int_Unsigned_C     -- COLORREF
                       with Import => True, Convention => StdCall, External_Name => "SetBkColor";
                     
  -- https://web.archive.org/web/20150111020406/http://msdn.microsoft.com/en-us/library/windows/desktop/dd183518(v=vs.85).aspx     
  function CreateSolidBrush (crColor : Int_Unsigned_C) -- COLORREF 
                             return Ptr                -- HBRUSH
                             with Import => True, Convention => StdCall, External_Name => "CreateSolidBrush";
                           
  -- https://web.archive.org/web/20160525074306/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644943(v=vs.85).aspx        
  function PeekMessageW (lpMsg         : access MSG;            -- LPMSG 
                         hWnd          :        Ptr;            -- HWND  
                         wMsgFilterMin :        Int_Unsigned_C; -- UINT  
                         wMsgFilterMax :        Int_Unsigned_C; -- UINT  
                         wRemoveMsg    :        Int_Unsigned_C) -- UINT  
                         return Int_C                           -- BOOL 
                         with Import => True, Convention => Stdcall, External_Name => "PeekMessageW";

  -- https://web.archive.org/web/20160607234745/https://msdn.microsoft.com/en-us/library/windows/desktop/ms633520(v=vs.85).aspx
  function GetWindowTextW (hWnd      : Ptr;           -- HWND   
                           lpString  : Ptr_Char_16_C; -- LPTSTR 
                           nMaxCount : Int_C)         -- int    
                           return Int_C               -- int
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
  function ClipCursor (lpRect : Ptr_RECT) -- const RECT*
                       return Int_C       -- BOOL 
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
  function UnregisterClassW (lpClassName : Ptr_Char_16_C; -- LPCTSTR   
                             hInstance   : Ptr)           -- HINSTANCE 
                             return Int_C                 -- BOOL 
                             with Import => True, Convention => StdCall, External_Name => "UnregisterClassW";

  -- https://web.archive.org/web/20160527143411/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644955(v=vs.85).aspx
  function TranslateMessage (lpMsg : Ptr_MSG) -- const MSG*
                             return Int_C     -- BOOL 
                             with Import => True, Convention => StdCall, External_Name => "TranslateMessage";

  -- https://web.archive.org/web/20160113002241/https://msdn.microsoft.com/en-us/library/windows/desktop/ms644934(v=vs.85).aspx
  function DispatchMessageW (lpmsg : Ptr_MSG) -- const MSG*
                             return Int_C     -- LRESULT
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
  function SetClassLongW (hWnd      : Ptr;            -- HWND 
                          nIndex    : Int_C;          -- int 
                          dwNewLong : Int_Unsigned_C) -- LONG 
                          return Int_Unsigned_C       -- DWORD
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
  function LoadLibraryW (lpFileName : Ptr_Char_16_C) -- LPCTSTR 
                         return Ptr                  -- LPCTSTRHMODULE
                         with Import =>True, Convention => StdCall, External_Name => "LoadLibraryW";
end;
