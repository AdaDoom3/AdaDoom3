
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

with Neo.Core.Console; use Neo.Core.Console;
with Neo.Core.Math; use Neo.Core.Math;
with Neo.Engine;    use Neo.Engine;
with Neo;           use Neo;
with Neo.World.CVars; use Neo.World.CVars;
with Neo.World;       use Neo.World;
use Neo.Core.Math.Calc_32;

with Ada.Numerics; use Ada.Numerics;
with Ada.Calendar; use Ada.Calendar;

package Impulses is

  --------------
  -- Movement --
  --------------
  --

  --

  procedure Callback_Move_Forward (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Move_Forward is new Impulse ("moveforward", Callback_Move_Forward, Rapid => True);

  procedure Callback_Move_Left (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Move_Left is new Impulse ("moveleft", Callback_Move_Left, Rapid => True);

  procedure Callback_Move_Right (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Move_Right is new Impulse ("moveright", Callback_Move_Right, Rapid => True);

  procedure Callback_Move_Backward (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Move_Backward is new Impulse ("moveback", Callback_Move_Backward, Rapid => True);

  ----------
  -- Look --
  ----------

  procedure Callback_Look (Args : Vector_Impulse_Arg.Unsafe_Array);
  package Look is new Impulse ("look", Callback_Look);
end;
