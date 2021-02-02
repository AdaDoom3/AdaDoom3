
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

package body Impulses is

  --------------
  -- Movement --
  --------------

  procedure  Callback_Move_Backward (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and not In_Menu.Get then
        Player_One.Move (Backward_Direction, Args (Args'First).Press.Last);
      end if;
    end;
  procedure Callback_Move_Forward (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and not In_Menu.Get then
        Player_One.Move (Forward_Direction, Args (Args'First).Press.Last);
      end if;
    end;
  procedure  Callback_Move_Right (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and not In_Menu.Get then
        Player_One.Move (Right_Direction, Args (Args'First).Press.Last);
      end if;
    end;
  procedure Callback_Move_Left (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and not In_Menu.Get then
        Player_One.Move (Left_Direction, Args (Args'First).Press.Last);
      end if;
    end;

  ----------
  -- Look --
  ----------

  procedure Callback_Look (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if not In_Menu.Get then
        Player_One.Look (Args (Args'First).Cursor.X, Args (Args'First).Cursor.Y);
      end if;
    end;
end;
