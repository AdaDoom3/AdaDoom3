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
WITH
  GNAT.Sockets;
USE
  GNAT.Sockets;
SEPARATE(Neo.System.Network)
PACKAGE BODY Implementation
  IS
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      IS
      BEGIN
        GNAT.Sockets.Initialize(False);
      END Initialize;
  --------------
  -- Finalize --
  --------------
    PROCEDURE Finalize
      IS
      BEGIN
        GNAT.Sockets.Finalize;
      END Finalize;
    PROCEDURE Finalize(
      Item : IN OUT Record_Connection)
      IS
      BEGIN
        IF 
      END Finalize;
  --------------------------
  -- Get_Local_Primary_IP --
  --------------------------
    FUNCTION Get_Local_Primary_IP
      RETURN String_2
      IS
      BEGIN
        --Mutex
        ---------------
        Fetch_The_Data:
        ---------------
          DECLARE
          Result : String_2 := To_String_2(Image(Addresses(Get_Host_By_Name(Host_Name), 1)));
          BEGIN
            -- Un Mutex
            RETURN Result;
          END Fetch_The_Data;
      END Get_Local_Primary_IP;
  -----------------
  -- Set_Address --
  -----------------
    PROCEDURE Set_Address(
      Connection      : IN OUT Record_Connection;
      Network_Address : IN     String_2 := LOOPBACK_NETWORK_ADDRESS;
      Port )
      IS
      Address : Sock_Addr_Type := (Inet_Addr(IP_Address), Port);
      Socket  : Socket_Type;
      BEGIN
        Create_Socket(Socket, Family_Inet, Socket_Datagram);
        Set_Socket_Option(Socket, Socket_Level, Broadcast);
        IF Network_Address = LOOPBACK_NETWORK_ADDRESS THEN
          Address.Addr := ANY_INET_ADDR;
        END IF;
        IF Network_Address.Port = PORT_ANY THEN
          Address.Port := 0;
        END IF;
        Bind_Socket(Socket, Address);
        IF Socket <= 0 THEN
          bound_to := ;
          RAISE SYSTEM_CALL_FAILURE;
        ELSIF Port = PORT_ANY THEN --Bound_To THEN
          bound_to := Net_SockadrToNetadr(Get_Socket_Name(Socket));
        END IF;
        RETURN Socket;
      END Set_Address;
  ----------
  -- Send --
  ----------
    PROCEDURE Send(
      Connection : IN Record_Connection;
      To         : IN String_2;
      Item       : IN Array_Integer_1_Unsigned)
      IS
      int       ret;
      sockaddr_in   addr;
      Net_NetadrToSockadr( &to, &addr );
      BEGIN
        Connection.Number_Of_Written_Packets := Connection.Number_Of_Written_Packets + 1;
        Connection.Number_Of_Written_Bytes   := Connection.Number_Of_Written_Bytes   + Size;
        IF NOT Connection.Is_Silenced THEN
          ret = sendto( netSocket, (const char *)data, length, 0, (sockaddr *)&addr, sizeof(addr) );
        END IF;
      EXCEPTION
        WHEN WSAEADDRNOTAVAIL =>
          RETURN;
      END Send;
  -------------
  -- Receive --
  -------------
    FUNCTION Receive(
      Connection : IN  Record_Connection;
      From       : OUT String_2;
      Timeout    : IN  Duration := 0.0)
      RETURN Array_Integer_1_Unsigned
      IS
      From_Socket : Sock_Addr_Type;
      BEGIN
        Receive_Socket(Connection.Socket, Data, Last, From);
        Pad_Null(From, Image(From.Addr));
        -- IF Get_Socket_Name(Connection.Socket).Family = Family_Inet THEN
        --   From.IP := (OTHERS => 0);
        -- END IF;
        IF Last > MAXIMUM_PACKET_SIZE_IN_BYTES THEN
          RAISE OVERSIZE_PACKET_RECEIVED;
        END IF;
        IF Data'Length = 0 THEN
          RAISE Empty_Packet;
        END IF;
        Connection.Number_Of_Read_Packets := Connection.Number_Of_Read_Packets + 1;
        Connection.Number_Of_Read_Bytes   := Connection.Number_Of_Read_Bytes   + Size;
        RETURN Data;
      END Receive;
    FUNCTION Receive(
      Connection : IN     Record_Connection;
      From       : IN OUT String_2;
      Timeout    : IN     Duration)
      RETURN Array_Integer_1_Unsigned
      IS
      Selector : Selector_Type;
      Reads    : Socket_Set_Type;
      Writes   : Socket_Set_Type;
      BEGIN
        Empty(Reads);
        Empty(Writes);
        Set(Reads, Connection.Socket);
        Create_Selector(Selector);
        Check_Selector(Selector, Reads, Writes, Timeout);
        Close_Selector(Selector);
        IF Status /= Completed THEN
          RAISE PACKET_RECEPTION_TIMEOUT;
        END IF;
        RETURN Receive(Connection, From);
      END Receive;
  END Implementation;

