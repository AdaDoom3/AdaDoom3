
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

generic
  type Player_State is private;
  type Server_Attribute_State is private;
package Neo.Engine.Network is

  ------------
  -- Entity --
  ------------
  
  -----------
  -- Frame --
  -----------

  type Frame_Kind is (Client_Frame, Server_Frame); -- Denotes origin

  type Frame_State (Kind : Frame_Kind) is record
      case Kind is
        when Client_Kind =>
          Valid_Delta : ;
          Ping        : ;
          Player      : Player_State;
          Entities    : ;
          Commands    : ;
          Visibility  : ;
        when Server_Kind =>
      end case;
    end record with Pack;

  type Frame_Header_Kind (Kind : Frame_Kind := Client_Kind; Multi_Frame : Bool := False) is record
      Frame_Id : 
      case Kind is
        when Client_Kind => Client_Id : Int_32_Unsigned := 0;
        when Server_Kind => null;
      end case;
    end record with Pack;

  ------------
  -- Client --
  ------------

  type Client_State is record
      Frame_Count : Natural;
      Snapshots : Vector_Snapshot_State;
      Baselines : Vector_Entity_State;
      Entities  : Vector_Entity_State;
    end record;

  type Client_State_Kind is (Free_State, Zombie_State, Connected_State, Primed_State, Active_State);

  type Client_Status_State (Is_Bot : Bool := False) is record
      State         : Client_State_Kind;      
      Handle        : Str_Unbound;
      Commands      : Vector_Str_Unbound;
      case Is_Bot is
        when False =>
          Download      : Array_Stream;
          Frame_Delta   : ;
          Next_Time     : ;
          Packet_Time   : ;
          Connect_Time  : ;
          Snapshot_Time : ;
          Rate_Delayed  : ;
          Frames        : ;
          Ping          : ;
          Rate          : ;
          Snapshot_Wait : ;
          Connection    : Connection_State;
        when True =>
          null;
      end case;
    end record;

  type Client_Snapshot_State is record
      Area_Location    : ;
      Player           : Player_State;
      Entities         : ;
      Sent_Time        : ;
      Acknowledge_Time : ;

    end record;

  ------------
  -- Server --
  ------------

  type Server_State is new Controlled with tagged record
      
    end record;

  type Server_Status_State is record

    end record;

  type Server_Info_State is record
      IP           : Str_IP;
      Map          : Str_Unbound;
      Game         : Str_Unbound;
      Client_Count : Natural;
      Capacity     : Positive;
      Ping         : Natural;
      Ping_Max     : Natural;
      Ping_Min     : Natural;
      Attributes   : Server_Attribute_State;
    end;

  -- Server commands
  procedure Map (Server : in out Server_State; Name : Str);
  procedure Kick
  procedure Ban
  procedure Status
  procedure Info
  procedure Say
end;