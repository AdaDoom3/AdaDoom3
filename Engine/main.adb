
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

with Neo;              use Neo;
with Neo.Engine;       use Neo.Engine;
with Neo.Core;         use Neo.Core;
with Neo.Core.Strings; use Neo.Core.Strings;
with Neo.Core.Console; use Neo.Core.Console;
with Ada.Command_Line; use Ada.Command_Line;

procedure Main is
  begin
    for I in 1..Argument_Count loop Submit (Replace (To_Str (Argument (I)), ".", " ")); end loop;
    Run;
  exception when others => Set_Exit_Status (Failure); end;
