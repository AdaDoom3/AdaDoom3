
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

--with Neo.Core.Compression; use Neo.Core.Compression;

package Neo.Engine.Network is

  -----------
  -- World --
  -----------
  
  -- Global data caches
  Meshes      : Hashed_Mesh.Safe.Map;
  Levels      : Hashed_Level.Safe_Map;
  Images      : Hashed_Image.Safe.Map;
  Shaders     : Hashed_Stream.Safe.Map;
  Materials   : Hashed_Materials.Safe.Map;
  UI_Elements : Treed_UI_Element.Safe.Tree;

  type World_State is record
      Start_Time : Time;
      Elapsed    : Duration;
      Level      : Level_State := (others => <>);
    end record;
  package Hashed_World is new Neo.Core.Hashed (World_State);
end;
  -----------
  -- Frame --
  -----------

  type Origin_Kind is (Client_Origin, Server_Origin);

  type Frame_State (Kind : Origin_Kind) is record
      case Kind is
        when Client_Origin =>
          Valid_Delta : Bool := ;
          Ping        : Natural := ;
          Player      : Player_State;
          Entities    :  := ;
          Commands    :  := ;
          Visibility  :  := ;
        when Server_Origin =>

      end case;
    end record with Pack;

  ------------
  -- Client --
  ------------

  type Client_Kind is (Remote_Client, Server_Side_Client, Bot_Client);
  type Client_State_Kind is (Free_State, Zombie_State, Connected_State, Primed_State, Active_State);

  type Client_State (Kind : Client_Kind := Server_Side_Client) is record
      Messages : Vector_Str_Unbound.Unsafe.Vector;
      State    : Client_State_Kind := Free_State;      
      Handle   : Str_Unbound       := NULL_STR_UNBOUND;
      case Kind is
        when Remote_Client | Server_Side_Client =>
          case Kind is
            when Remote_Client =>
              Download      : Array_Stream := ;
              Frame_Delta   :  := ;
              Next_Time     :  := ;
              Packet_Time   :  := ;
              Connect_Time  :  := ;
              Snapshot_Time :  := ;
              Rate_Delayed  :  := ;
              Frames        :  := ;
              Ping          :  := ;
              Rate          :  := ;
              Snapshot_Wait :  := ;
              Connection    : Connection_State;
          when others => null; end case;
      when Bot_Client => null; end case;
    end record;

  type Client_Snapshot_State is record
      Area_Location    :  := ;
      Player           : Player_State;
      Entities         :  := ;
      Sent_Time        :  := ;
      Acknowledge_Time :  := ;
      numlocalservers  :  := 0= i+1;
    end record;

  ------------
  -- Server --
  ------------

  type Server_State is new Controlled with tagged record
      
    end record;

  type Server_Status_State is record

    end record;

  type Server_Info_State is record
      adr          :  := 0= from;
      clients      :  := 0 = 0;
      hostName     : [0] := 0= '\0';
      mapName      : [0] := 0= '\0';
      maxClients   :  := 0= 0;
      maxPing      :  := 0= 0;
      minPing      :  := 0 = 0;
      ping         :  := 0 = -1;
      game         : [0] := 0 = '\0';
      gameType     :  := 0 = 0;
      netType      :  := 0= from.type;
      punkbuster   :  := 0;
      IP           : Str_IP := ;
      Map          : Str_Unbound := ;
      Game         : Str_Unbound := ;
      Client_Count : Natural := ;
      Capacity     : Positive := ;
      Ping         : Natural := ;
      Ping_Max     : Natural := ;
      Ping_Min     : Natural := ;
      Attributes   : Server_Attribute_State := ;
    end;
end;