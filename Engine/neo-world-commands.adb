
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

package body Neo.World.Commands is

  procedure Command_The_Time (Args : Array_Str_Unbound) is
    begin null;
    end;
    
  procedure Command_Info (Args : Array_Str_Unbound) is
--      procedure Show_Clients (Client_Kind : Connection_Kind) is
--        begin
--          for Client of Clients loop if Client.Kind = Client_Kind then
--            Line (TAB & Client.Name);
--            Line (TAB & TAB & Client.IP & ":" & Client.Port
--                      & TAB & Client.Rate'Wide_Image
--                      & TAB & Client.Pint'Wide_Image
--                      & TAB & Client.Last_Packet_Time);
--          end if; end loop;
--        end;
    begin null;
--        if Client.Hosted  then Line_Info ("Host: "        & Client.Host); end if;
--        if Client.Running then Line_Info ("Map: "         & Server.Map);  end if;
--        if Server.Running then Line_Info ("Server: "      & Server.Name); 
--                               Line_Info ("Zombies: ");   Show_Clients (Zombie_Status);
--                               Line_Info ("Incomming: "); Show_Clients (Incoming_Status);
--                               Line_Info ("Connected: "); Show_Clients (Connected_Status); end if;
    end;
    
  procedure Command_Map (Args : Array_Str_Unbound) is
    begin null; end;
      --if Args (2) /= "multi" then Initialize_Single_Player (Args (2)); end if;
      --else 
      
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
  
  procedure Command_Game_Type (Args : Array_Str_Unbound) is
    begin null;
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
    end;

  procedure Command_Kick (Args : Array_Str_Unbound) is
    begin null;
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
    end;

  procedure Command_Ban (Args : Array_Str_Unbound) is
    begin null;
  --    if not Server_Task.Running then
  --      Line ("Server is not running!");
  --      return;
  --    end if;

  --     cl = SV_GetPlayerByName();
  --     if (!cl) {
  --       return;
  --     }

  --    if Client.Channel.Remote_Address.Kind = Loopback_Address then
  --      Line ("Cannot kick host player!");
  --      return;
  --    end if;


  --     client_t  *cl;
  --     -- make sure server is running
  --     if ( !com_sv_running->integer ) {
  --       Com_Printf( "Server is not running.\n" );
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
    end; 

  procedure Command_Say (Args : Array_Str_Unbound) is
    begin null;
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
    end;

  function Save_Binds return Str is
    Result : Str_Unbound;
    begin
      for Impulse of Impulses.Get loop
        for Binding of Impulse.Bindings.Get loop
          Result := Result & EOL & "bind " & Impulse.Name & " " &
            (case Binding.Kind is
              when Mouse_Impulse   => Binding.Mouse'Wide_Image, --"mouse",
              when Key_Impulse     => Binding.Key'Wide_Image,
              when Stick_Impulse   => Binding.Stick'Wide_Image,
              when Gamepad_Impulse => Binding.Gamepad'Wide_Image,
              when Trigger_Impulse => Binding.Trigger'Wide_Image,
              when Text_Impulse    => "text",
              when Cursor_Impulse  => "cursor");
        end loop;
      end loop;
      return Result;
    end;
    
  
  procedure Command_Bind (Args : Array_Str_Unbound) is
    Impulse : Impulse_State := Impulses.Get (Args (1));
    Device  : Device_Kind   := Device_Kind'Val (To_Str_8 (Args (2)) & "_device");
    begin
    
      -- Gamepad stick
      if Index (Args (2), "stick") /= 0 then
        Impulse.Bindings.Append (Gamepad (Stick_Kind'Val (To_Str_8 (Args (2)))));
        
      -- A button could be from the gamepad or the mouse
      elsif Index (Args (2), "button") /= 0 then
        begin
          Impulse.Bindings.Append (Gamepad (Gamepad_Kind'Val (To_Str_8 (Args (2)))));
        exception when Contraint_Error =>
          Impulse.Bindings.Append (Mouse (Mouse_Kind'Val (To_Str_8 (Args (2)))));
        end;
        
      -- Gamepad trigger
      elsif Index (Args (2), "trigger") /= 0 then
        Impulse.Bindings.Append (Gamepad (Trigger_Kind'Val (To_Str_8 (Args (2)))));
        
      -- Keyboard
      elsif Index (Args (2), "key") /= 0 then
        Impulse.Bindings.Append (Keyboard (Key_Kind'Val (To_Str_8 (Args (2)))));
        
      -- Text
      elsif Index (Args (2), "text") /= 0 then
        Impulse.Bindings.Append (Mouse);
        
      -- Mouse cursor
      elsif Index (Args (2), "cursor") /= 0 then
        Impulse.Bindings.Append (Mouse);
      end if;
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
    --!helpmenu -- This brings up a special help menu with information for you to view as a player on the server.
--maprate -- This brings up specific information for you to view regarding the servers map rate settings.
--maprating -- This brings up a menu for you to rate the map currently being played.
--!die -- This cause you to forefit your life or commit sucide.
--hlx -- This brings up your player statistics for the servers that run the HlStatsX system.
--!achievement -- This brings up information regarding your specific achievements towards the achievements on the server.
--    votekick -- This starts a vote to kick a specific player from the server if the server has the option enabled to allow public votes to kick players.
--voteban -- This starts a vote to ban a specific player from the server if the server has the option enabled to allow public votes to ban players.
--votemap -- This starts a vote to change the map from the current map to another map
--rtv -- This starts a vote which queries the players on the server to determine if they want to conduct a vote to change the map.
--nominate -- This nominates a specific map in regards to pending map vote started at the end of the round or due to a rtv vote being initiated.
end;
