--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  Neo.POSIX;
use
  Neo.POSIX;
separate(Neo.System.Network)
package body Implementation
  is
  ----------------
  -- Initialize --
  ----------------
    procedure Initialize
      is
      begin
        Initialize(False);
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
        GNAT.Sockets.Finalize;
      end Finalize;
    procedure Finalize(
      Item : in out Record_Connection)
      is
      begin
        if 
      end Finalize;
  --------------------------
  -- Get_Local_Primary_IP --
  --------------------------
    function Get_Local_Primary_IP
      return String_2
      is
      begin
        --Mutex
        ---------------
        Fetch_The_Data:
        ---------------
          declare
          Result : String_2 := To_String_2(Image(Addresses(Get_Host_By_Name(Host_Name), 1)));
          begin
            -- Un Mutex
            return Result;
          end Fetch_The_Data;
      end Get_Local_Primary_IP;
  -----------------
  -- Set_Address --
  -----------------
    procedure Set_Address(
      Connection      : in out Record_Connection;
      Network_Address : in     String_2 := LOOPBACK_NETWORK_ADDRESS;
      Port )
      is
      Address : Sock_Addr_Type := (Inet_Addr(IP_Address), Port);
      Socket  : Socket_Type;
      begin
        Create_Socket(Socket, Family_Inet, Socket_Datagram);
        Set_Socket_Option(Socket, Socket_Level, Broadcast);
        if Network_Address = LOOPBACK_NETWORK_ADDRESS then
          Address.Addr := ANY_inET_ADDR;
        end if;
        if Network_Address.Port = PORT_ANY then
          Address.Port := 0;
        end if;
        Bind_Socket(Socket, Address);
        if Socket <= 0 then
          bound_to := ;
          raise SYSTEM_CALL_FAILURE;
        elsif Port = PORT_ANY then --Bound_To then
          bound_to := Net_SockadrToNetadr(Get_Socket_Name(Socket));
        end if;
        return Socket;
      end Set_Address;
  ----------
  -- Send --
  ----------
    procedure Send(
      Connection : in Record_Connection;
      To         : in String_2;
      Item       : in Array_Integer_1_Unsigned)
      is
      int       ret;
      sockaddr_in   addr;
      Net_NetadrToSockadr( &to, &addr );
      begin
        Connection.Number_Of_Written_Packets := Connection.Number_Of_Written_Packets + 1;
        Connection.Number_Of_Written_Bytes   := Connection.Number_Of_Written_Bytes   + Size;
        if not Connection.Is_Silenced then
          ret = sendto( netSocket, (const char *)data, length, 0, (sockaddr *)&addr, sizeof(addr) );
        end if;
      EXCEPTION
        WHEN WSAEADDRNOTAVAIL =>
          return;
      end Send;
  -------------
  -- Receive --
  -------------
    function Receive(
      Connection : in  Record_Connection;
      From       : out String_2;
      Timeout    : in  Duration := 0.0)
      return Array_Integer_1_Unsigned
      is
      From_Socket : Sock_Addr_Type;
      begin
        Receive_Socket(Connection.Socket, Data, Last, From);
        Pad_Null(From, Image(From.Addr));
        -- if Get_Socket_Name(Connection.Socket).Family = Family_Inet then
        --   From.IP := (OTHERS => 0);
        -- end if;
        if Last > MAXIMUM_PACKET_SIZE_in_BYTES then
          raise OVERSIZE_PACKET_RECEIVED;
        end if;
        if Data'Length = 0 then
          raise Empty_Packet;
        end if;
        Connection.Number_Of_Read_Packets := Connection.Number_Of_Read_Packets + 1;
        Connection.Number_Of_Read_Bytes   := Connection.Number_Of_Read_Bytes   + Size;
        return Data;
      end Receive;
    function Receive(
      Connection : in     Record_Connection;
      From       : in out String_2;
      Timeout    : in     Duration)
      return Array_Integer_1_Unsigned
      is
      Selector : Selector_Type;
      Reads    : Socket_Set_Type;
      Writes   : Socket_Set_Type;
      begin
        Empty(Reads);
        Empty(Writes);
        Set(Reads, Connection.Socket);
        Create_Selector(Selector);
        Check_Selector(Selector, Reads, Writes, Timeout);
        Close_Selector(Selector);
        if Status /= Completed then
          raise PACKET_RECEPTION_TIMEout;
        end if;
        return Receive(Connection, From);
      end Receive;
  end Implementation;
