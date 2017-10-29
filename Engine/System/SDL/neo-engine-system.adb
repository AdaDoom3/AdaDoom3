
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

separate (Neo.Engine) package System is

  -------------
  -- Globals --
  -------------

  -- Window titles
  APP_NAME           : constant Str   := S (Game_Info.Name_ID);
  GAME_NAME          : aliased  Str_C := To_Str_C (APP_NAME);
  INPUT_NAME         : aliased  Str_C := To_Str_C (APP_NAME & " Input");
  CONSOLE_NAME       : aliased  Str_C := To_Str_C (APP_NAME & " Console");
  MULTI_MONITOR_NAME : aliased  Str_C := To_Str_C (APP_NAME & " Multi-monitor");

  -- Asset paths
  SDL_PATH_ICON            : aliased Str_C := To_Str_C (PATH_ICON             & ".bmp");
  SDL_PATH_CURSOR_ACTIVE   : aliased Str_C := To_Str_C (PATH_CURSOR_ACTIVE    & ".bmp");
  SDL_PATH_CURSOR_INACTIVE : aliased Str_C := To_Str_C (PATH_CURSOR_INACTIVE  & ".bmp");

  -- Main "HWND"s for the invisible input window and game window
  Game, Input : aliased Ptr := NULL_PTR;

  -- Window handles for multi-monitor mode
  package Vector_Ptr is new Neo.Core.Vectors (Ptr);
  Multi_Monitor_Windows : Vector_Ptr.Unsafe.Vector;

  -------------
  -- Console --
  -------------

  -- A task-isolated GUI console application for debugging
  procedure Run_Console is null;

  ------------
  -- Vulkan --
  ------------

  -- Pointer to the driver dll
  Vulkan_DLL : Ptr := LoadLibraryW (To_Ptr_Char_16_C (VK_WIN32_DLL_NAME));

  -- Load a pointer to a procedure based on a name
  function Get_Vulkan_Subprogram (Name : Str) return Ptr is (GetProcAddress (Vulkan_DLL, To_Str_8_C (Name)));

  -- Fetch extension strings
  function Get_Vulkan_Extension return Str_8_C is (VK_KHR_WIN32_SURFACE_EXTENSION_NAME);

  -- Finalization and initialization (mostly revolve around loading the dll)
  procedure Finalize_Vulkan_Library is begin Assert (FreeLibrary (Vulkan_DLL)); end;
  procedure Initialize_Vulkan_Library is
    begin
      vkCreateWin32SurfaceKHR := To_Ptr_vkCreateWin32SurfaceKHR (Get_Vulkan_Subprogram ("vkCreateWin32SurfaceKHR"));
    end;

  -- Create a new surface
  function Create_Vulkan_Surface (Instance : Ptr) return Ptr is
    Result       : aliased Ptr                         := NULL_PTR;
    Surface_Info : aliased VkWin32SurfaceCreateInfoKHR := (hWnd => Game, hInstance => GetModuleHandleNULL, others => <>);
    begin
      vkAssert (vkCreateWin32SurfaceKHR (Instance, Surface_Info'Unchecked_Access, null, Result'Unchecked_Access));
      Assert (Result);
      return Result;
    end;

  -- Information
  procedure Copy   (Item : Str);
  function Paste   return Str;
  function OS_Info return OS_Info_State;

  -- Input
  procedure Initialize_Input;
  procedure Finalize_Input;
  procedure Vibrate     (Id : Int_Ptr; Hz_High, Hz_Low : Real_Percent);
  function Update_Input return Bool;

  -- Error Handling
  procedure Run_Console;
  procedure Alert        (Val : Bool);
  procedure Open_Text    (Path : Str);
  procedure Open_Webpage (Path : Str);
  procedure Execute      (Path : Str);
  function Last_Error    return Int_Unsigned;
  function Ok            (Message : Str; Buttons : Buttons_Kind; Icon : Icon_Kind)
                         return Bool;

  -- Windowing
  procedure Initialize_Multi_Monitor;
  procedure Finalize_Multi_Monitor;
  procedure Initialize_Windowing;
  procedure Finalize_Windowing;
  procedure Minimize;
  procedure Maximize;
  procedure Make_Windowed;
  procedure Clip_Cursor      (Do_Clip : Bool := True);
  procedure Hide_Cursor      (Do_Hide : Bool := True);
  procedure Set_Cursor_Style (Kind : Cursor_Kind);
  procedure Set_Cursor       (Pos  : Cursor_State);
  function Get_Cursor        return Cursor_state;
  function Get_Windows       return Vector_Border.Unsafe_Array;
  function Get_Decoration    return Border_State;
  function Fullscreen_Only   return Bool;
  function Only_Instance     return Bool;
  function Update_Windowing  return Bool; -- Set Activated and Mode cvars

  ---------------
  -- Clipboard --
  ---------------

  procedure Copy (Item : Str_2) is
    begin
      Assert (SDL_Set_Clipboard_Text (C.To_C (Text)));
      Check_For_Window;
      if /= Success then
         raise Clipboard_Error with SDL.Error.Get;
      end if;
    end;
  function Paste return Str_2 is
    begin
      Check_For_Window;
      return C.Strings.Value (SDL_Get_Clipboard_Text);
    end;

  ------------
  -- Memory --
  ------------

  function Get_Memory return Memory_State is (others => <>);

  --------------------
  -- Error_Handling --
  --------------------

  procedure Alert        (Value : Bool)                    is null;
  procedure Open_Text    (Path : Str_2)                    is null;
  procedure Open_Webpage (Path : Str_2)                    is null;
  procedure Execute      (Path : Str_2; Fullscreen : Bool) is null;
  function Last_Error return Int_4_Unsigned is
    begin
      printf("SDL_Init failed: %s\n", SDL_GetError());
      return 0;
    end;
  function Ok (Name, Message : Str_2; Buttons : Button_Kind; Icon : Icon_Kind) return Bool is
    begin
      SDL_ShowSimpleMessageBox
      return False;
    end;

  -----------------
  -- Information --
  -----------------

  function Get_Information return Information_State is
    begin
      const char* SDL_GetPlatform(void)
      return (others => <>);
    end;

  ---------------
  -- Windowing --
  ---------------

  procedure Assert_Only_Instance is null;
  procedure Minimize is
    begin
      raise Unimplemented;
    end;
  procedure Fullscreen is
    begin
      raise Unimplemented;
    end;
  procedure Initialize_Multi_Monitor is
    begin
      raise Unsupported;
    end;
  procedure Finalize_Multi_Monitor is
    begin
      raise Unsupported;
    end;
  procedure Initialize_Windowing is
    begin
      raise Unimplemented;
    end;
  procedure Finalize_Windowing is
    begin
      raise Unimplemented;
    end;
  procedure Clip_Cursor (Undo : Bool := False; Hide : Bool := False) is
    begin
      raise Unimplemented;
    end;
  procedure Set_Cursor (X, Y : Int_8_Signed) is
    begin
      raise Unimplemented;
    end;
  procedure Adjust (X, Y : Int_4_Signed; Width, Height : Int_4_Positive; Fullscreen : Bool) is
    begin
      raise Unimplemented;
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
  function Get_Decoration return Border_State is
    begin
      raise Unimplemented;
      return (others => <>);
    end;
  function Fullscreen_Only return Bool is
    begin
      raise Unimplemented;
      return False;
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