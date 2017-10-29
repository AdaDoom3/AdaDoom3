
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

package Neo.Engine.Interactions is

  -----------
  -- CVars --
  -----------

  -- Task safe global console variables                              
  package Activated       is new CVar ("activated", "Query last window activation action", Activated_Kind, Other_Activated, False);
  package Cursor          is new CVar ("cursor",    "Cursor style",                        Cursor_Kind,    Inactive_Cursor, False);    
  package Menu            is new CVar ("menu",      "Cursor capture",                      Bool,           True,            False);
  package Mode            is new CVar ("mode",      "Window mode",                         Mode_Kind,      Windowed_Mode);
  package Aspect_Narrow_X is new CVar ("narrowx",   "Windowed min narrow aspect x",        Positive,       16);
  package Aspect_Narrow_Y is new CVar ("narrowy",   "Windowed min narrow aspect y",        Positive,       9);
  package Aspect_Wide_X   is new CVar ("widex",     "Windowed min wide aspect x",          Positive,       4);
  package Aspect_Wide_Y   is new CVar ("widey",     "Windowed min wide aspect y",          Positive,       3);
  package Windowed_Height is new CVar ("winheight", "Height in windowed mode",             Positive,       600);
  package Windowed_Width  is new CVar ("winwidth",  "Width in windowed mode",              Positive,       800);
  package GPU_Memory      is new CVar ("devicemb",  "GPU local memory in MB",              Positive,       128);
  package Visible_Memory  is new CVar ("hostmb",    "CPU visible memory in MB",            Positive,       64);    
  package Upload_Buffer   is new CVar ("uploadmb",  "GPU upload buffer size in MB",        Positive,       64);   

  --------------
  -- Impulses --
  --------------

  -- Enter or exit menu mode 
  Game_Entry_Check_Status : Safe_Status;
  procedure Callback_Enter_Game (Args : Vector_Impulse_Arg.Unsafe_Array) is
  package Enter_Game is new Impulse ("entergame",  Callback_Enter_Game);

  -- Enter or exit menu mode 
  procedure Callback_Exit_To_Menu (Args : Vector_Impulse_Arg.Unsafe_Array) is
  package Exit_To_Menu is new Impulse ("exittomenu", Callback_Exit_To_Menu);

  -- Toggle fullscreen mode
  procedure Callback_Fullscreen (Args : Vector_Impulse_Arg.Unsafe_Array) is
  package Fullscreen is new Impulse ("togglemode", Callback_Fullscreen);

  -------------
  -- Command --
  -------------

  procedure Command_Map (Server : in out Server_State; Name : Str) is
  package Map is new Command (Name     => "bind",
                              Info     => "Bind an impulse to an input value",
                              Usage    => "bind [player#] [impulse] [value]",
                              Callback => Command_Map);

  procedure Command_Kick (Args : Array_Str_Unbound) is
  package Kick is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Kick);

  procedure Command_Ban (Args : Array_Str_Unbound) is
  package Ban is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Ban);

  procedure Command_Status (Args : Array_Str_Unbound) is
  package Status is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Status);

  procedure Command_Info (Args : Array_Str_Unbound) is
  package Info is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Info);

  procedure Command_Say (Args : Array_Str_Unbound) is
  package Say is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Say);

  procedure Command_Say_Team  (Args : Array_Str_Unbound) is
  package Say_Team is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Say_Team);

  function Save_Binds return Str is (NULL_STR);
  procedure Command_Bind (Args : Array_Str_Unbound) is
  package Bind is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Bind,
                               Save     => Save_Binds'Access);
  
  procedure Command_Unbind (Args : Array_Str_Unbound) is
  package Unbind is new Command (Name     => "unbind",
                                 Info     => "Unbind an impulse to an input value",
                                 Usage    => "unbind [player#] [impulse] [value]",
                                 Callback => Command_Unbind);

  procedure Command_Restart_Map (Args : Array_Str_Unbound) is
  package Restart is new Command (Name     => "bind",
                                  Info     => "Bind an impulse to an input value",
                                  Usage    => "bind [player#] [impulse] [value]",
                                  Callback => Command_Restart_Map;

  procedure Command_Load_Map (Args : Array_Str_Unbound) is
  package Map is new Command (Name     => "unbind",
                              Info     => "Unbind an impulse to an input value",
                              Usage    => "unbind [player#] [impulse] [value]",
                              Callback => Command_Unbind);

  procedure Command_Host_Map (Args : Array_Str_Unbound) is
  package Host is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Host_Map;

  procedure Command_Join_Map (Args : Array_Str_Unbound) is
  package Join is new Command (Name     => "unbind",
                               Info     => "Unbind an impulse to an input value",
                               Usage    => "unbind [player#] [impulse] [value]",
                               Callback => Command_Unbind);

  procedure Command_Server_List (Args : Array_Str_Unbound) is
  package Servers is new Command (Name     => "unbind",
                                  Info     => "Unbind an impulse to an input value",
                                  Usage    => "unbind [player#] [impulse] [value]",
                                  Callback => Command_Server_List);

  procedure Command_Recent_Server_List (Args : Array_Str_Unbound) is
  package Recent is new Command (Name     => "unbind",
                                 Info     => "Unbind an impulse to an input value",
                                 Usage    => "unbind [player#] [impulse] [value]",
                                 Callback => Command_Recent_Server_List);
end;

