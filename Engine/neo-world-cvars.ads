
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

package Neo.World.CVars is

  ---------------
  -- Rendering --
  ---------------

  -- Renderer
  CVAR_R : constant Str := "r_";
  package Max_GPU_Memory     is new CVar (CVAR_R & "maxgpumb",    "GPU local memory in MB",         Int_Ptr,        128);
  package Max_Visible_Memory is new CVar (CVAR_R & "maxhostmb",   "CPU visible memory in MB",       Int_Ptr,        64);
  package Max_Upload_Buffer  is new CVar (CVAR_R & "maxuploadmb", "GPU upload buffer size in MB",   Int_Ptr,        64);
  package Sampling           is new CVar (CVAR_R & "sampling",    "Number of antialiasing samples", Sampling_Kind,  No_Sampling);

  -- System
  CVAR_S : constant Str := "s_";
  package Task_Count is new CVar (CVAR_S & "tasks",     "Number of running tasks",  Positive,       1,               False);
  package Activated  is new CVar (CVAR_S & "activated", "Query last window action", Activated_Kind, Other_Activated, False);
  package Cursor     is new CVar (CVAR_S & "cursor",    "Cursor style",             Cursor_Kind,    Inactive_Cursor, False);
  package In_Menu    is new CVar (CVAR_S & "inmenu",    "Cursor capture",           Bool,           True,            False);

  -- Windowing
  CVAR_W : constant Str := "w_";
  package Mode            is new CVar (CVAR_W & "mode",    "Window mode",                  Mode_Kind, Windowed_Mode);
  package Aspect_Narrow_X is new CVar (CVAR_W & "narrowx", "Windowed min narrow aspect x", Positive,  16);
  package Aspect_Narrow_Y is new CVar (CVAR_W & "narrowy", "Windowed min narrow aspect y", Positive,  9);
  package Aspect_Wide_X   is new CVar (CVAR_W & "widex",   "Windowed min wide aspect x",   Positive,  4);
  package Aspect_Wide_Y   is new CVar (CVAR_W & "widey",   "Windowed min wide aspect y",   Positive,  3);
  package Window_Height   is new CVar (CVAR_W & "height",  "Height of game window",        Positive,  600);
  package Window_Width    is new CVar (CVAR_W & "width",   "Width of game window",         Positive,  800);

  -- Physics
  CVAR_PHYSX : constant Str := "phyx_";
end;
