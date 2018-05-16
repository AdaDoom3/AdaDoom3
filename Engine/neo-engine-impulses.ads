

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

with Neo.Engine.CVars; use Neo.Engine.CVars;

-- ???
package Neo.Engine.Impulses is

  ----------------
  -- Enter_Game --
  ----------------
  --
  -- Impulse for enter or exit menu mode
  --

  procedure Callback_Enter_Game (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Enter_Game is new Impulse ("entergame",  Callback_Enter_Game);

  ------------------
  -- Exit_To_Menu --
  ------------------
  --
  -- ???
  --

  procedure Callback_Exit_To_Menu (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Exit_To_Menu is new Impulse ("exittomenu", Callback_Exit_To_Menu);

  ----------------
  -- Fullscreen --
  ----------------
  --
  -- ???
  --

  procedure Callback_Fullscreen (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Fullscreen is new Impulse ("togglemode", Callback_Fullscreen);
end;
