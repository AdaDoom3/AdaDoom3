
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

package body Neo.World.Impulses is

  ----------------
  -- Enter_Game --
  ----------------

  procedure Callback_Enter_Game (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and then In_Main_Window then
        if In_Menu.Get then In_Menu.Set (False);
        else Game_Entry_Status.Occupied (True); end if;
      end if;
    end;

  ------------------
  -- Exit_To_Menu --
  ------------------

  procedure Callback_Exit_To_Menu (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then In_Menu.Set (True); end if;
    end;

  ----------------
  -- Fullscreen --
  ----------------

  procedure Callback_Fullscreen (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then
        Mode.Set ((case Mode.Get is
                     when Multi_Monitor_Mode | Fullscreen_Mode => Windowed_Mode,
                     when Windowed_Mode => Fullscreen_Mode));
      end if;
    end;
end;
