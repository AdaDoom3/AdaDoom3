
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

package body Neo.Engine.Interactions is

  --------------
  -- Impulses --
  --------------

  -- Enter or exit menu mode 
  procedure Callback_Enter_Game (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down and then In_Main_Window then
        if Menu.Get then Menu.Set (False);
        else Game_Entry_Check_Status.Occupied (True); end if;
      end if;
    end;

  -- Enter or exit menu mode 
  procedure Callback_Exit_To_Menu (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then Menu.Set (True); end if;
    end;

  -- Toggle fullscreen mode
  procedure Callback_Fullscreen (Args : Vector_Impulse_Arg.Unsafe_Array) is
    begin
      if Args (Args'First).Press.Down then
        Mode.Set ((case Mode.Get is
                     when Multi_Monitor_Mode | Fullscreen_Mode => Windowed_Mode,
                     when Windowed_Mode => Fullscreen_Mode));
      end if;
    end;
    
  --------------
  -- Commands --
  --------------

  procedure Command_Map (Server : in out Server_State; Name : Str) is
    begin
      null;
    end;

  procedure Command_Kick (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Ban (Args : Array_Str_Unbound) is
    begin
      null;
    end; 

  procedure Command_Status (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Info (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Say (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Say_Team  (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  function Save_Binds return Str is (NULL_STR);
  procedure Command_Bind (Args : Array_Str_Unbound) is
    begin
      null;
    end;
  
  procedure Command_Unbind (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Restart_Map (Args : Array_Str_Unbound) is
    begin
      null;
    end; 

  procedure Command_Load_Map (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Host_Map (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Join_Map (Args : Array_Str_Unbound) is
    begin
      null;
    end;

  procedure Command_Server_List (Args : Array_Str_Unbound) is
    begin
      null;
    end; 

  procedure Command_Recent_Server_List (Args : Array_Str_Unbound) is
    begin
      null;
    end;
end;



  -- procedure Map is
  --   begin
  --     Server_Map := Load (Arg (1));
  --     if Arg (2) = "Single_Player_Game" then
  --       Initialize_Single_Player (Server_Map);
  --     else 
  --     char    *cmd;
  --     char    *map;
  --     qboolean  killBots, cheat;
  --     char    expanded[MAX_QPATH];
  --     char    mapname[MAX_QPATH];
  --     map = Cmd_Argv(1);
  --     if ( !map ) {
  --       return;
  --     }
  --     -- make sure the level exists before trying to change, so that
  --     -- a typo at the server console won't end the game
  --     Com_sprintf (expanded, sizeof(expanded), "maps/%s.bsp", map);
  --     if ( FS_ReadFile (expanded, NULL) == -1 ) {
  --       Com_Printf ("Can't find map %s\n", expanded);
  --       return;
  --     }
  --     -- force latched values to get set
  --     Cvar_Get ("g_gametype", "0", CVAR_SERVERINFO | CVAR_USERINFO | CVAR_LATCH );
  --     cmd = Cmd_Argv(0);
  --     if( Q_stricmpn( cmd, "sp", 2 ) == 0 ) {
  --       Cvar_SetValue( "g_gametype", GT_SINGLE_PLAYER );
  --       Cvar_SetValue( "g_doWarmup", 0 );
  --       -- may not set sv_maxclients directly, always set latched
  --       Cvar_SetLatched( "sv_maxclients", "8" );
  --       cmd += 2;
  --       cheat = qfalse;
  --       killBots = qtrue;
  --     }
  --     else {
  --       if ( !Q_stricmp( cmd, "devmap" ) || !Q_stricmp( cmd, "spdevmap" ) ) {
  --         cheat = qtrue;
  --         killBots = qtrue;
  --       } else {
  --         cheat = qfalse;
  --         killBots = qfalse;
  --       }
  --       if( sv_gametype->integer == GT_SINGLE_PLAYER ) {
  --         Cvar_SetValue( "g_gametype", GT_FFA );
  --       }
  --     }
  --     -- save the map name here cause on a map restart we reload the q3config.cfg
  --     -- and thus nuke the arguments of the map command
  --     Q_strncpyz(mapname, map, sizeof(mapname));
  --     -- start up the map
  --     SV_SpawnServer( mapname, killBots );
  --     -- set the cheat value
  --     -- if the level was started with "map <levelname>", then
  --     -- cheats will not be allowed.  If started with "devmap <levelname>"
  --     -- then cheats will be allowed
  --     if ( cheat ) {
  --       Cvar_Set( "sv_cheats", "1" );
  --     } else {
  --       Cvar_Set( "sv_cheats", "0" );
  --     }
  --   end;
  -- procedure Ban is
  --   begin
  --     client_t  *cl;
  --     -- make sure server is running
  --     if ( !com_sv_running->integer ) {
  --       Com_Printf( "Server is not running.\n" );
  --       return;
  --     }
  --     cl = SV_GetPlayerByName();
  --     if (!cl) {
  --       return;
  --     }
  --     if( cl->netchan.remoteAddress.type == NA_LOOPBACK ) {
  --       SV_SendServerCommand(NULL, "print \"%s\"", "Cannot kick host player\n");
  --       return;
  --     }
  --     -- look up the authorize server's IP
  --     if ( !svs.authorizeAddress.ip[0] && svs.authorizeAddress.type != NA_BAD ) {
  --       Com_Printf( "Resolving %s\n", AUTHORIZE_SERVER_NAME );
  --       if ( !NET_StringToAdr( AUTHORIZE_SERVER_NAME, &svs.authorizeAddress ) ) {
  --         Com_Printf( "Couldn't resolve address\n" );
  --         return;
  --       }
  --       svs.authorizeAddress.port = BigShort( PORT_AUTHORIZE );
  --       Com_Printf( "%s resolved to %i.%i.%i.%i:%i\n", AUTHORIZE_SERVER_NAME,
  --         svs.authorizeAddress.ip[0], svs.authorizeAddress.ip[1],
  --         svs.authorizeAddress.ip[2], svs.authorizeAddress.ip[3],
  --         BigShort( svs.authorizeAddress.port ) );
  --     }
  --     -- otherwise send their ip to the authorize server
  --     if ( svs.authorizeAddress.type != NA_BAD ) {
  --       NET_OutOfBandPrint( NS_SERVER, svs.authorizeAddress,
  --         "banUser %i.%i.%i.%i", cl->netchan.remoteAddress.ip[0], cl->netchan.remoteAddress.ip[1], 
  --                      cl->netchan.remoteAddress.ip[2], cl->netchan.remoteAddress.ip[3] );
  --       Com_Printf("%s was banned from coming back\n", cl->name);
  --     }
  --   end;
  -- procedure Kick is
  --   begin
  --     if not Server.Running then
  --       Line ("Server is not running");
  --       return;
  --     end if;
  --     Client := Get_Client (Args (1));
  --     if Client.IP = LOOPBACK_IP then
  --       SV_SendServerCommand(NULL, "print \"%s\"", "Cannot kick host player\n");
  --       return;
  --     end if;
  --     SV_DropClient( Client, "was kicked" );
  --     Client.Packet_Time := svs.time;  -- in case there is a funny zombie
  --   end;
  -- procedure Status is
  --   begin
  --     if not Server.Running then
  --       Line ("Server is not running");
  --       return;
  --     end if;
  --     Line ("Map: " & Server.Map_Name);
  --     for Client in Clients loop
  --       Put (TAB & "#: "    & Client.Id'Img & ": " & Client.Name);
  --       Put (TAB & "IP: "   & Client.IP & ":" & Client.Port);
  --       Put (TAB & "Rate: " & Client.Rate'Img);
  --       Put (TAB & "Ping: " & Client.Pint'Img);
  --       Put (TAB & "Last: " & Client.Last_Packet_Time);
  --       if Client.State = Connected_State then
  --         Put (TAB & "Connecting...");
  --       elsif Client.State = Zombie_State then
  --         Put (TAB & "ZOMBIE!!!1!");
  --       else
  --       Line;
  --     end loop;
  --   end;
  -- procedure Say is
  --   begin
  --     char  *p;
  --     char  text[1024];
  --     -- make sure server is running
  --     if ( !com_sv_running->integer ) {
  --       Com_Printf( "Server is not running.\n" );
  --       return;
  --     }
  --     if ( Cmd_Argc () < 2 ) {
  --       return;
  --     }
  --     strcpy (text, "console: ");
  --     p = Cmd_Args();
  --     if ( *p == '"' ) {
  --       p++;
  --       p[strlen(p)-1] = 0;
  --     }
  --     strcat(text, p);
  --     SV_SendServerCommand(NULL, "chat \"%s\n\"", text);
  --   end;
