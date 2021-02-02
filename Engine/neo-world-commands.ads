
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

-- User facing interactive settings (e.g. console variables and commands)
package Neo.World.Commands is

  ----------
  -- Time --
  ----------
  --
  
  --

  procedure Command_The_Time (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package The_Time is new Command (Name     => "thetime",
                                   Info     => "Bind an impulse to an input value",
                                   Usage    => "bind [player#] [impulse] [value]",
                                   Callback => Command_The_Time);

  ----------
  -- Kick --
  ----------
  --
  
  --

  procedure Command_Kick (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Kick is new Command (Name     => "kick",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Kick);

  ---------
  -- Ban --
  ---------
  --
  
  --

  procedure Command_Ban (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Ban is new Command (Name     => "ban",
                              Info     => "Bind an impulse to an input value",
                              Usage    => "bind [player#] [impulse] [value]",
                              Callback => Command_Ban);

  ----------
  -- Info --
  ----------
  --
  
  --

  procedure Command_Info (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Info is new Command (Name     => "info",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Info);

  ---------
  -- Say --
  ---------
  --
  
  --

  procedure Command_Say (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Say is new Command (Name     => "say",
                              Info     => "Bind an impulse to an input value",
                              Usage    => "bind [player#] [impulse] [value]",
                              Callback => Command_Say);

  ----------
  -- Bind --
  ----------
  --
  
  --

  function Save_Binds return Str;
  procedure Command_Bind (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Bind is new Command (Name     => "bind",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Bind,
                               Save     => Save_Binds'Access);

  ------------
  -- Unbind --
  ------------
  --
  
  --

  procedure Command_Unbind (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Unbind is new Command (Name     => "unbind",
                                 Info     => "Unbind an impulse to an input value",
                                 Usage    => "unbind [player#] [impulse] [value]",
                                 Callback => Command_Unbind);

  -------------
  -- Restart --
  -------------
  --
  
  --

  procedure Command_Restart_Map (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Restart is new Command (Name     => "restart",
                                  Info     => "Bind an impulse to an input value",
                                  Usage    => "bind [player#] [impulse] [value]",
                                  Callback => Command_Restart_Map);

  ---------
  -- Map --
  ---------
  --
  
  --

  procedure Command_Load_Map (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Map is new Command (Name     => "map",
                              Info     => "Unbind an impulse to an input value",
                              Usage    => "unbind [player#] [impulse] [value]",
                              Callback => Command_Unbind);

  ----------
  -- Host --
  ----------
  --
  
  --

  procedure Command_Host_Map (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Host is new Command (Name     => "host",
                               Info     => "Bind an impulse to an input value",
                               Usage    => "bind [player#] [impulse] [value]",
                               Callback => Command_Host_Map);

  ----------
  -- Join --
  ----------
  --
  
  --

  procedure Command_Join_Map (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Join is new Command (Name     => "join",
                               Info     => "Unbind an impulse to an input value",
                               Usage    => "unbind [player#] [impulse] [value]",
                               Callback => Command_Unbind);

  -------------
  -- Servers --
  -------------
  --
  
  --

  procedure Command_Server_List (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Servers is new Command (Name     => "servers",
                                  Info     => "Unbind an impulse to an input value",
                                  Usage    => "unbind [player#] [impulse] [value]",
                                  Callback => Command_Server_List);

  ------------
  -- Recent --
  ------------
  --
  
  --

  procedure Command_Recent_Server_List (Args : Array_Str_Unbound); --  := NULL_ARRAY_STR_UNBOUND);
  package Recent is new Command (Name     => "recent",
                                 Info     => "Unbind an impulse to an input value",
                                 Usage    => "unbind [player#] [impulse] [value]",
                                 Callback => Command_Recent_Server_List);
end;
