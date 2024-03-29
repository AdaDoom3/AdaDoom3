
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

package body Neo.World.Network is

  -- -----------
  -- -- Frame --
  -- -----------

  -- function Runlength_Encode_Delta (Orignal, Current : Frame_State) return Stream is
  --   begin
  --     for I in Original_Bits loop
  --     end loop;
  --   end;
  -- function Decode_Delta (Current : Frame_State; Next : Stream) return Frame_State is
  --   begin
  --   end;

  -- -------------
  -- -- Message --
  -- -------------

  -- type Message_State is record
  --     4 sequence number (high bit set if an oversize fragment)
  --     <optional reliable commands>
  --     1 svc_snapshot
  --     4 last client reliable command
  --     4 serverTime
  --     1 lastframe for delta compression
  --     1 snapFlags
  --     1 areaBytes
  --     <areabytes>
  --     <playerstate>
  --     <packetentities>
  --   end record;
    
  -- ----------------
  -- -- Connection --
  -- ----------------

  -- Netchan_TransmitNextFragment

  ------------
  -- Client --
  ------------

  procedure Run_Client is
    
    begin
      while Is_Running.Get loop
      
        -- Fetch and decompress all recieved messages
        for Message of Messages loop
        
          -- Handle messages accordingly
          case Message.Kind is
          
            -- Send challenge repsonses instead of request packets and address changes
            when Challenge_Message =>
              if Client.Server_View.Status /= Connecting_State then
                Line ("Ignoring spurious challenge from server");
              else
                Client.Challenge         := Message.Arg1;
                Client.Server_View.State := Challenging_State;
                Client.Packet_Count      := 0;
                Client.Connection_Time   := NULL_TIME;
                Client.Server_Address    := Message.From; -- Allow a server proxy to hand off connections to multiple servers
              end if;
              
            -- 
            when Connection_Response_Message =>
              if Client.Server_View.Status then
                Line ("Ignoring spurious connection reply from server");
              elsif Message.From /= Client.Server_View.Address then
                Line ("Ignoring connection reply from 
              
              case  is
                when Connected_Status.._Status =>
                  Line ("Ignoring duplicate connection reply from server");
                when others =>
              
            
           if  cls.state >= CA_CONNECTED ) {
             Com_Printf ("Dup connect received.  Ignored.\n");
             return;
           end if;
           if  cls.state != CA_CHALLENGING ) {
             Com_Printf ("connectResponse packet while not connecting.  Ignored.\n");
             return;
           end if;
           if  !NET_CompareBaseAdr( from, clc.serverAddress ) ) {
             Com_Printf( "connectResponse from a different address.  Ignored.\n" );
             Com_Printf( "%s should have been %s\n", NET_AdrToString( from ), 
               NET_AdrToString( clc.serverAddress ) );
             return;
           end if;
           Netchan_Setup (NS_CLIENT, &clc.netchan, from, Cvar_VariableValue( "net_qport" ) );
           cls.state = CA_CONNECTED;
           clc.lastPacketSentTime = -9999;   -- send first packet immediately
            
            
          end case;
        end loop;
      end loop;
    end;
    
    
    
   while Is_Running.Get loop

     -- Load the scene and submit server messages
     if not Local_Server.Is_Running then
       for Packet of Server_Packets loop
         Current_Packet := Decompress (Decompress (Current_Packet), Last_Packet);
         Frame.Set (Current_Packet.Frame);
         Submit (Current_Packet.Message);
         Last_Packet := Current_Packet;
       end loop;
       Send (Local_Server, Compress ());
       Prediction := (others => <>);
     else
       Prediction := Server (Current_Packet.Frame);
     end if;

     -- Delay a bit
     delay 0.0;
   end loop;
  end;




       while GetLoopPacketNS_SERVER, &evFrom, &buf ) loop
         -- if the server just shut down, flush the events
         if Server_Running.Get then
           Com_RunAndTimeServerPacket&evFrom, &buf );
          end if;

       int   headerBytes;
       clc.lastPacketTime = Client.realtime;
       if msg->cursize >= 4 && *(int *)msg->data == -1 then
         char  *s;
         char  *c;
         MSG_BeginReadingOOB( msg );
         MSG_ReadLong( msg );  -- skip the -1
         s = MSG_ReadStringLine( msg );
         Cmd_TokenizeString( s );
         c = Cmd_Argv(0);
         Com_DPrintf ("CL packet %s: %s\n", NET_AdrToString(from), c);

         -- challenge from the server we are connecting to
         if  !Q_stricmp(c, "challengeResponse") ) {
           if  cls.state != CA_CONNECTING ) {
             Com_Printf( "Unwanted challenge response received.  Ignored.\n" );
           end if; else {
             -- start sending challenge repsonse instead of challenge request packets
             clc.challenge = atoi(Cmd_Argv(1));
             cls.state = CA_CHALLENGING;
             clc.connectPacketCount = 0;
             clc.connectTime = -99999;

             -- take this address as the new server address.  This allows
             -- a server proxy to hand off connections to multiple servers
             clc.serverAddress = from;
             Com_DPrintf ("challengeResponse: %d\n", clc.challenge);
           end if;
           return;
         end if;

         -- server connection
         if  !Q_stricmp(c, "connectResponse") ) {
           return;
         end if;

         -- 
         case Message.Kind is
           when Info_Response_Message =>
             int   i, type;
             char  info[MAX_INFO_STRING];
             char* str;
             char  *infoString;
             int   prot;
             infoString = MSG_ReadString( msg );
             -- if this isn't the correct protocol version, ignore it
             prot = atoi( Info_ValueForKey( infoString, "protocol" ) );
             if ( prot != PROTOCOL_VERSION ) {
               Com_DPrintf( "Different protocol info packet: %s\n", infoString );
               return;
             }
             -- iterate servers waiting for ping response
             for (i=0; i<MAX_PINGREQUESTS; i++)
             {
               if ( cl_pinglist[i].adr.port && !cl_pinglist[i].time && NET_CompareAdr( from, cl_pinglist[i].adr ) )
               {
                 -- calc ping time
                 cl_pinglist[i].time = cls.realtime - cl_pinglist[i].start + 1;
                 Com_DPrintf( "ping time %dms from %s\n", cl_pinglist[i].time, NET_AdrToString( from ) );
                 -- save of info
                 Q_strncpyz( cl_pinglist[i].info, infoString, sizeof( cl_pinglist[i].info ) );
                 -- tack on the net type
                 -- NOTE: make sure these types are in sync with the netnames strings in the UI
                 switch (from.type)
                 {
                   case NA_BROADCAST:
                   case NA_IP:
                     str = "udp";
                     type = 1;
                     break;
                   case NA_IPX:
                   case NA_BROADCAST_IPX:
                     str = "ipx";
                     type = 2;
                     break;
                   default:
                     str = "???";
                     type = 0;
                     break;
                 }
                 Info_SetValueForKey( cl_pinglist[i].info, "nettype", va("%d", type) );
                 CL_SetServerInfoByAddress(from, infoString, cl_pinglist[i].time);
                 return;
               }
             }
             -- if not just sent a local broadcast or pinging local servers
             if (cls.pingUpdateSource != AS_LOCAL) {
               return;
             }
             for ( i = 0 ; i < MAX_OTHER_SERVERS ; i++ ) {
               -- empty slot
               if ( cls.localServers[i].adr.port == 0 ) {
                 break;
               }
               -- avoid duplicate
               if ( NET_CompareAdr( from, cls.localServers[i].adr ) ) {
                 return;
               }
             }
             if ( i == MAX_OTHER_SERVERS ) {
               Com_DPrintf( "MAX_OTHER_SERVERS hit, dropping infoResponse\n" );
               return;
             }
             -- add this to the list
             cls.numlocalservers = i+1;
             cls.localServers[i].adr = from;
             cls.localServers[i].clients = 0;
             cls.localServers[i].hostName[0] = '\0';
             cls.localServers[i].mapName[0] = '\0';
             cls.localServers[i].maxClients = 0;
             cls.localServers[i].maxPing = 0;
             cls.localServers[i].minPing = 0;
             cls.localServers[i].ping = -1;
             cls.localServers[i].game[0] = '\0';
             cls.localServers[i].gameType = 0;
             cls.localServers[i].netType = from.type;
             cls.localServers[i].punkbuster = 0;
             Q_strncpyz( info, MSG_ReadString( msg ), MAX_INFO_STRING );
             if (strlen(info)) {
               if (info[strlen(info)-1] != '\n') {
                 strncat(info, "\n", sizeof(info));
               }
               Com_Printf( "%s: %s", NET_AdrToString( from ), info );
             }
           when Status_Response_Message =>
             char  *s;
             char  info[MAX_INFO_STRING];
             int   i, l, score, ping;
             int   len;
             serverStatus_t *serverStatus;
             serverStatus = NULL;
             for (i = 0; i < MAX_SERVERSTATUSREQUESTS; i++) {
               if ( NET_CompareAdr( from, cl_serverStatusList[i].address ) ) {
                 serverStatus = &cl_serverStatusList[i];
                 break;
               }
             }
             -- if we didn't request this server status
             if (!serverStatus) {
               return;
             }
             s = MSG_ReadStringLine( msg );
             len = 0;
             Com_sprintf(&serverStatus->string[len], sizeof(serverStatus->string)-len, "%s", s);
             if (serverStatus->print) {
               Com_Printf("Server settings:\n");
               -- print cvars
               while (*s) {
                 for (i = 0; i < 2 && *s; i++) {
                   if (*s == '\\')
                     s++;
                   l = 0;
                   while (*s) {
                     info[l++] = *s;
                     if (l >= MAX_INFO_STRING-1)
                       break;
                     s++;
                     if (*s == '\\') {
                       break;
                     }
                   }
                   info[l] = '\0';
                   if (i) {
                     Com_Printf("%s\n", info);
                   }
                   else {
                     Com_Printf("%-24s", info);
                   }
                 }
               }
             }
             len = strlen(serverStatus->string);
             Com_sprintf(&serverStatus->string[len], sizeof(serverStatus->string)-len, "\\");
             if (serverStatus->print) {
               Com_Printf("\nPlayers:\n");
               Com_Printf("num: score: ping: name:\n");
             }
             for (i = 0, s = MSG_ReadStringLine( msg ); *s; s = MSG_ReadStringLine( msg ), i++) {
               len = strlen(serverStatus->string);
               Com_sprintf(&serverStatus->string[len], sizeof(serverStatus->string)-len, "\\%s", s);
               if (serverStatus->print) {
                 score = ping = 0;
                 sscanf(s, "%d %d", &score, &ping);
                 s = strchr(s, ' ');
                 if (s)
                   s = strchr(s+1, ' ');
                 if (s)
                   s++;
                 else
                   s = "unknown";
                 Com_Printf("%-2d   %-3d    %-3d   %s\n", i, score, ping, s );
               }
             }
             len = strlen(serverStatus->string);
             Com_sprintf(&serverStatus->string[len], sizeof(serverStatus->string)-len, "\\");
             serverStatus->time = Com_Milliseconds();
             serverStatus->address = from;
             serverStatus->pending = qfalse;
             if (serverStatus->print) {
               serverStatus->retrieved = qtrue;
             }
           when Disconnect_Message => 
             if ( cls.state < CA_AUTHORIZING ) {
               return;
             }
             -- if not from our server, ignore it
             if ( !NET_CompareAdr( from, clc.netchan.remoteAddress ) ) {
               return;
             }
             -- if we have received packets within three seconds, ignore it
             -- (it might be a malicious spoof)
             if ( cls.realtime - clc.lastPacketTime < 3000 ) {
               return;
             }
             -- drop the connection
             Com_Printf( "Server disconnected for unknown reason\n" );
             Cvar_Set("com_errorMessage", "Server disconnected for unknown reason\n" );
             CL_Disconnect( qtrue );
           when Echo_Message =>
             s = MSG_ReadString( msg );
             Q_strncpyz( clc.serverMessage, s, sizeof( clc.serverMessage ) );
             Com_Printf( "%s", s );
             return;
           when Response_Packet_Message =>
             int       i, count, max, total;
             serverAddress_t addresses[MAX_SERVERSPERPACKET];
             int       numservers;
             byte*     buffptr;
             byte*     buffend;
             Com_Printf("CL_ServersResponsePacket\n");
             if (cls.numglobalservers == -1) {
               -- state to detect lack of servers or lack of response
               cls.numglobalservers = 0;
               cls.numGlobalServerAddresses = 0;
             }
             if (cls.nummplayerservers == -1) {
               cls.nummplayerservers = 0;
             }
             -- parse through server response string
             numservers = 0;
             buffptr    = msg->data;
             buffend    = buffptr + msg->cursize;
             while (buffptr+1 < buffend) {
               -- advance to initial token
               do {
                 if (*buffptr++ == '\\')
                   break;    
               }
               while (buffptr < buffend);
               if ( buffptr >= buffend - 6 ) {
                 break;
               }
               -- parse out ip
               addresses[numservers].ip[0] = *buffptr++;
               addresses[numservers].ip[1] = *buffptr++;
               addresses[numservers].ip[2] = *buffptr++;
               addresses[numservers].ip[3] = *buffptr++;
               -- parse out port
               addresses[numservers].port = (*buffptr++)<<8;
               addresses[numservers].port += *buffptr++;
               addresses[numservers].port = BigShort( addresses[numservers].port );
               -- syntax check
               if (*buffptr != '\\') {
                 break;
               }
               Com_DPrintf( "server: %d ip: %d.%d.%d.%d:%d\n",numservers,
                   addresses[numservers].ip[0],
                   addresses[numservers].ip[1],
                   addresses[numservers].ip[2],
                   addresses[numservers].ip[3],
                   addresses[numservers].port );
               numservers++;
               if (numservers >= MAX_SERVERSPERPACKET) {
                 break;
               }
               -- parse out EOT
               if (buffptr[1] == 'E' && buffptr[2] == 'O' && buffptr[3] == 'T') {
                 break;
               }
             }
             if (cls.masterNum == 0) {
               count = cls.numglobalservers;
               max = MAX_GLOBAL_SERVERS;
             } else {
               count = cls.nummplayerservers;
               max = MAX_OTHER_SERVERS;
             }
             for (i = 0; i < numservers && count < max; i++) {
               -- build net address
               serverInfo_t *server = (cls.masterNum == 0) ? &cls.globalServers[count] : &cls.mplayerServers[count];
               CL_InitServerInfo( server, &addresses[i] );
               -- advance to next slot
               count++;
             }
             -- if getting the global list
             if (cls.masterNum == 0) {
               if ( cls.numGlobalServerAddresses < MAX_GLOBAL_SERVERS ) {
                 -- if we couldn't store the servers in the main list anymore
                 for (; i < numservers && count >= max; i++) {
                   serverAddress_t *addr;
                   -- just store the addresses in an additional list
                   addr = &cls.globalServerAddresses[cls.numGlobalServerAddresses++];
                   addr->ip[0] = addresses[i].ip[0];
                   addr->ip[1] = addresses[i].ip[1];
                   addr->ip[2] = addresses[i].ip[2];
                   addr->ip[3] = addresses[i].ip[3];
                   addr->port  = addresses[i].port;
                 }
               }
             }
             if (cls.masterNum == 0) {
               cls.numglobalservers = count;
               total = count + cls.numGlobalServerAddresses;
             } else {
               cls.nummplayerservers = count;
               total = count;
             }
             Com_Printf("%d servers parsed (total %d)\n", numservers, total);
         end case;
         Com_DPrintf ("Unknown connectionless packet command.\n");
         exit;
       end if;
       if Client.state < CA_CONNECTED then
         exit;   -- can't be a valid sequenced packet
       end if;
       if msg->cursize < 4 then
         Com_Printf ("%s: Runt packet\n",NET_AdrToStringfrom ));
         exit;
       end if;
       -- packet from server
       if !NET_CompareAdrfrom, clc.netchan.remoteAddress ) then
         Com_DPrintf ("%s:sequenced packet without connection\n",NET_AdrToStringfrom ) );
         -- FIXME: send a client disconnect?
         exit;
       end if;
       if !CL_Netchan_Process&clc.netchan, msg) then
         exit;   -- out of order, duplicated, etc
       end if;
       -- the header is different lengths for reliable and unreliable messages
       headerBytes = msg->readcount;
       -- track the last message received so it can be returned in 
       -- client messages, allowing the server to detect a dropped
       -- gamestate
       clc.serverMessageSequence = LittleLong*(int *)msg->data );
       clc.lastPacketTime = Client.realtime;
       CL_ParseServerMessagemsg;
       -- we don't know if it is ok to save a demo message until
       -- after we have parsed the frame
       if clc.demorecording && !clc.demowaiting then
         CL_WriteDemoMessage msg, headerBytes );
         -- write the packet sequence
         len = clc.serverMessageSequence;
         swlen = LittleLong( len );
         FS_Write (&swlen, 4, clc.demofile);

         -- skip the packet sequencing information
         len = msg->cursize - headerBytes;
         swlen = LittleLong(len);
         FS_Write (&swlen, 4, clc.demofile);
         FS_Write ( msg->data + headerBytes, len, clc.demofile );
       end if;        
     end loop;
     while GetLoopPacketNS_SERVER, &evFrom, &buf ) loop
       -- if the server just shut down, flush the events
       if Server_Running.Get then
         Com_RunAndTimeServerPacket&evFrom, &buf );
        end if;
     end loop;

     -- Recieve

   end loop;
  end;
  
  ------------
  -- Server --
  ------------
  
/*
	Simple udp server
*/
#include&lt;stdio.h&gt;	//printf
#include&lt;string.h&gt; //memset
#include&lt;stdlib.h&gt; //exit(0);
#include&lt;arpa/inet.h&gt;
#include&lt;sys/socket.h&gt;

#define BUFLEN 512	//Max length of buffer
#define PORT 8888	//The port on which to listen for incoming data

void die(char *s)
{
	perror(s);
	exit(1);
}

int main(void)
{
	struct sockaddr_in si_me, si_other;
	
	int s, i, slen = sizeof(si_other) , recv_len;
	char buf[BUFLEN];
	
	//create a UDP socket
	if ((s=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP)) == -1)
	{
		die(&quot;socket&quot;);
	}
	
	// zero out the structure
	memset((char *) &amp;si_me, 0, sizeof(si_me));
	
	si_me.sin_family = AF_INET;
	si_me.sin_port = htons(PORT);
	si_me.sin_addr.s_addr = htonl(INADDR_ANY);
	
	//bind socket to port
	if( bind(s , (struct sockaddr*)&amp;si_me, sizeof(si_me) ) == -1)
	{
		die(&quot;bind&quot;);
	}
	
	//keep listening for data
	while(1)
	{
		printf(&quot;Waiting for data...&quot;);
		fflush(stdout);
		
		//try to receive some data, this is a blocking call
		if ((recv_len = recvfrom(s, buf, BUFLEN, 0, (struct sockaddr *) &amp;si_other, &amp;slen)) == -1)
		{
			die(&quot;recvfrom()&quot;);
		}
		
		//print details of the client/peer and the data received
		printf(&quot;Received packet from %s:%d\n&quot;, inet_ntoa(si_other.sin_addr), ntohs(si_other.sin_port));
		printf(&quot;Data: %s\n&quot; , buf);
		
		//now reply the client with the same data
		if (sendto(s, buf, recv_len, 0, (struct sockaddr*) &amp;si_other, slen) == -1)
		{
			die(&quot;sendto()&quot;);
		}
	}

	close(s);
	return 0;
}

  
  -- ------------
  -- -- Server --
  -- ------------

  -- procedure Run_Server is
  --   beginprocedure Server is

  -- begin
  --   loop

  --     -- Do nothing if its not the time for the next frame
  --     if Server.FPS < 0 then
  --       Server.FPS.Set (10);
  --     end if;

  --     -- Do the frame
  --     Frame_Time := 1000 / Server.FPS.Get;
  --     Server.Residual_Time := Server.Residual_Timel + msec;
  --     if Dedicated.Get then Bot_Frame (svs.time + Server.Residual_Time);

  --     -- Wait for a packet or if we are ready for a frame
  --     elsif sv.timeResidual < Frame_Time then
  --       NET_Sleep (Frame_Time - sv.timeResidual);
  --     end if;

  --     -- Update infostrings and send the data to all relevent clients ???

  --     -- Update ping
  --     for Client in Clients loop
  --       if Client.State /= Active_State or not Client.GEntity then Client.Ping := 999;
  --       elsif Client.Is_Bot then Client.Ping := 0;
  --       else
  --         for Frame in Client.Frames loop
  --           Total := Total + Frame.Message_Acked - Frame.Message_Sent;
  --         end loop;
  --         Client.Ping := (if Total = 0 then 999 else Total / Client.Frames.Length);
  --       end if;
  --     end loop;

  --     -- Check timeouts
  --     for Client in Clients loop

  --       -- Correct message times across map changes
  --       if Client.Last_Packet_Time > Server.Time then Client.Last_Packet_Time := Server.Time; end if;

  --       -- Cure a zombie if it is not past the point of no return
  --       if Client.State = Zombie_State and Client.Last_Packet_Time < Zombie_Point then
  --         Line ("Switching " & Client.Name & " to Free_State from Zombie_State.")
  --         Client.State := Free_State;
  --       end if;

  --       -- Drop a timeout
  --       if Client.State = Connected_State and Client.Last_Packet_Time < Drop_Point then
  --         SV_DropClient (cl, "timed out"); 
  --         Client.State := Free_State;
  --       else
  --         Client.Timeout_Count;
  --       end if;
  --     end loop;

  --     -- Send messages
  --     for Client of Clients loop
  --       if Server.Time >= Client.Next_Snapshot_Time then
  --         if Client.Channel.Has_Unsent_Fragments then
  --           Client.Next_Snapshot_Time := Server.Time + RateMSec (Client, Client.Channal.Unsent_Length - Client.Channel.Unsent_Fragment_Start);
  --           Netchan_TransmitNextFragment( &client->netchan );
  --           if (!client->netchan.unsentFragments)
  --           {
  --             -- make sure the netchan queue has been properly initialized (you never know)
  --             if (!client->netchan_end_queue) {
  --               Com_Error(ERR_DROP, "netchan queue is not properly initialized in SV_Netchan_TransmitNextFragment\n");
  --             }
  --             -- the last fragment was transmitted, check wether we have queued messages
  --             if (client->netchan_start_queue) {
  --               netchan_buffer_t *netbuf;
  --               Com_DPrintf("#462 Netchan_TransmitNextFragment: popping a queued message for transmit\n");
  --               netbuf = client->netchan_start_queue;
  --               SV_Netchan_Encode( client, &netbuf->msg );
  --               Netchan_Transmit( &client->netchan, netbuf->msg.cursize, netbuf->msg.data );
  --               -- pop from queue
  --               client->netchan_start_queue = netbuf->next;
  --               if (!client->netchan_start_queue) {
  --                 Com_DPrintf("#462 Netchan_TransmitNextFragment: emptied queue\n");
  --                 client->netchan_end_queue = &client->netchan_start_queue;
  --               }
  --               else
  --                 Com_DPrintf("#462 Netchan_TransmitNextFragment: remaining queued message\n");
  --               Z_Free(netbuf);
  --             } 
  --           } 
  --         else
  --           byte    msg_buf[MAX_MSGLEN];
  --           msg_t   msg;
  --           -- build the snapshot
  --           SV_BuildClientSnapshot( client );
  --           -- bots need to have their snapshots build, but
  --           -- the query them directly without needing to be sent
  --           if ( client->gentity && client->gentity->r.svFlags & SVF_BOT ) {
  --             return;
  --           }
  --           MSG_Init (&msg, msg_buf, sizeof(msg_buf));
  --           msg.allowoverflow = qtrue;
  --           -- NOTE, MRE: all server->client messages now acknowledge
  --           -- let the client know which reliable clientCommands we have received
  --           MSG_WriteLong( &msg, client->lastClientCommand );
  --           -- (re)send any reliable server commands
  --           SV_UpdateServerCommandsToClient( client, &msg );
  --           -- send over all the relevant entityState_t
  --           -- and the playerState_t
  --           SV_WriteSnapshotToClient( client, &msg );
  --           -- Add any download data if the client is downloading
  --           SV_WriteDownloadToClient( client, &msg );
  --           -- check for overflow
  --           if ( msg.overflowed ) {
  --             Com_Printf ("WARNING: msg overflowed for %s\n", client->name);
  --             MSG_Clear (&msg);
  --           }
  --           SV_SendMessageToClient( &msg, client );
  --         end if;
  --       end if;
  --     end loop;

  --     -- Send heartbeat to master
  --     if Server_Mode.Get = Dedicated_Mode and Server.Time >= Server.Next_Heartbeat then
  --       for Master_Server of Master_Servers loop

  --         -- Resolve if needed
  --         if Master.Modified then
  --           Master.Modified := False;
  --           Line ("Resolving " & Master.Name & "...");

  --           -- Failed to resolve
  --           if not String_To_Address (Master.Name, Master.Address) then
  --             Line ("Couldn't resolve address: " & Master.Name);
  --             Master.Name.Set ("");

  -- goto Bad_Master;
  --           else
  --             if Index (":", Master.Name) then
  --               Address.Port := Short (PORT_MASTER);
  --             end if;
  --             Line (Master.Name & " resolved to " & Master.IP & ":" & Master.Port);
  --           end if;
  --         end if;
  --         Line ("Sending heartbeat to " & Master.Name);
  --         Out_Of_Band_Print (Server, Master.Address, "heartbeat " & HEARTBEAT_GAME);
  -- <<Bad_Master>>
  -- --!

  --       end loop;
  --     end if;
  --   end loop;
  -- end;
  --   end;
