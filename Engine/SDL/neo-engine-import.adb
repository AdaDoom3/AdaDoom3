
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

separate (Neo.Engine) package Import is

  ---------------
  -- Clipboard --
  ---------------

  procedure Copy (Item : Str_2) is
    begin
      Check_For_Window;
      if SDL_Set_Clipboard_Text (C.To_C (Text)) /= Success then
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