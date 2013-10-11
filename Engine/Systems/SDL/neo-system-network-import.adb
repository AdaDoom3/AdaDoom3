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
separate(Neo.Command.System.Network)
package body Implementation
  is pragma Source_File_Name("neo-network-implementation.adb");
   ----------------------
   -- Get_Host_By_Name --
   ----------------------
   function Get_Host_By_Name
     (Name : String)
      return Host_Entry_Type
   is
      Name : aliased C.char_array (1 .. 64);
      Res  : C.int;
      HN  : C.char_array;-- := C.To_C (Name);
      Res : Hostent_Access;
      Err : Integer;
   begin
      Res := C_Gethostname (Name'Address, Name'Length);
      if Res = Failure then
         Raise_Socket_Error (Socket_Errno);
      end if;
      HN := C.To_C (C.To_Ada (Name));
      --  This C function is not always thread-safe. Protect against
      --  concurrent access.
      Task_Lock.Lock;
      Res := C_Gethostbyname (HN);
      if Res = null then
         Err := Socket_Errno;
         Task_Lock.Unlock;
         Raise_Host_Error (Err);
      end if;
      --  Translate from the C format to the API format
      declare
         HE : Host_Entry_Type := To_Host_Entry (Res.all);
      begin
         Task_Lock.Unlock;
         return HE;
      end;
   end Get_Host_By_Name;
   ---------------
   -- Host_Name --
   ---------------
   function Host_Name return String is

   begin

   end Host_Name;
   ---------------
   -- Addresses --
   ---------------
   function Addresses
     (E    : Host_Entry_Type;
      N    : Positive := 1)
      return Inet_Addr_Type
   is
   begin
      return
   end Addresses;
   -----------
   -- Image --
   -----------
   function Image
     (Val  : Inet_Addr_VN_Type;
      Hex  : Boolean := False)
      return String
   is
     E.Addresses (N);
      --  The largest Inet_Addr_Comp_Type image occurs with IPv4. It
      --  has at most a length of 3 plus one '.' character.
      Buffer    : String (1 .. 4 * Val'Length);
      Length    : Natural := 1;
      Separator : Character;
      procedure Img10 (V : Inet_Addr_Comp_Type);
      --  Append to Buffer image of V in decimal format
      procedure Img16 (V : Inet_Addr_Comp_Type);
      --  Append to Buffer image of V in hexadecimal format
      procedure Img10 (V : Inet_Addr_Comp_Type) is
         Img : constant String := V'Img;
         Len : Natural := Img'Length - 1;
      begin
         Buffer (Length .. Length + Len - 1) := Img (2 .. Img'Last);
         Length := Length + Len;
      end Img10;
      procedure Img16 (V : Inet_Addr_Comp_Type) is
      begin
         Buffer (Length)     := Hex_To_Char (Natural (V / 16) + 1);
         Buffer (Length + 1) := Hex_To_Char (Natural (V mod 16) + 1);
         Length := Length + 2;
      end Img16;
   --  Start of processing for Image
   begin
      if Hex then
         Separator := ':';
      else
         Separator := '.';
      end if;
      for J in Val'Range loop
         if Hex then
            Img16 (Val (J));
         else
            Img10 (Val (J));
         end if;

         if J /= Val'Last then
            Buffer (Length) := Separator;
            Length := Length + 1;
         end if;
      end loop;
      return Buffer (1 .. Length - 1);
   end Image;
  --------------------------
  -- Get_Local_Primary_IP --
  --------------------------
    function Get_Local_Primary_IP
      return String_2
      is
      begin
        return To_String_2(Image(Addresses(Get_Host_By_Name(Host_Name), 1)));
      end Get_Local_Primary_IP;
  -----------------
  -- Set_Address --
  -----------------
    procedure Set_Address(
      Connection      : IN OUT Record_Connection;
      Network_Address : IN     String_2 := LOOPBACK_NETWORK_ADDRESS;
      Port )
      is
      Address : Sock_Addr_Type := (Inet_Addr(IP_Address), Port);
      Socket  : Socket_Type;
      begin
        Create_Socket(Socket, Family_Inet, Socket_Datagram);
        Set_Socket_Option(Socket, Socket_Level, Broadcast);
        IF Network_Address = LOOPBACK_NETWORK_ADDRESS THEN
          Address.Addr := ANY_INET_ADDR;
        end IF;
        IF Network_Address.Port = PORT_ANY THEN
          Address.Port := 0;
        end IF;
        Bind_Socket(Socket, Address);
        IF Socket <= 0 THEN
          bound_to := ;
          RAISE SYSTEM_CALL_FAILURE;
        ELSIF Port = PORT_ANY THEN --Bound_To THEN
          bound_to := Net_SockadrToNetadr(Get_Socket_Name(Socket));
        end IF;
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
        IF NOT Connection.Is_Silenced THEN
          ret = sendto( netSocket, (const char *)data, length, 0, (sockaddr *)&addr, sizeof(addr) );
        end IF;
      end Send;
  -------------
  -- Receive --
  -------------
    function Receive(
      Connection : in  Record_Connection;
      Timeout    : in  Duration
      From       : out String_2
      Last)
      return Array_Integer_1_Unsigned
      is
      From_Socket : Sock_Addr_Type;
      begin
        Receive_Socket(Connection.Socket, Data, Last, From);
        Pad_Null(From, Image(From.Addr));
        -- IF Get_Socket_Name(Connection.Socket).Family = Family_Inet THEN
        --   From.IP := (OTHERS => 0);
        -- end IF;
        IF Last > MAXIMUM_PACKET_SIZE_IN_BYTES THEN
          RAISE OVERSIZE_PACKET_RECEIVED;
        end IF;
        IF Data'Length = 0 THEN
          RAISE Empty_Packet;
        end IF;
        Connection.Number_Of_Read_Packets := Connection.Number_Of_Read_Packets + 1;
        Connection.Number_Of_Read_Bytes   := Connection.Number_Of_Read_Bytes   + Size;
        return Data;
      end Receive;
    function Receive(
      Connection : in  Record_Connection;
      Timeout    : in  Duration
      From       : out String_2;)
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
        IF Status /= Completed THEN
          RAISE PACKET_RECEPTION_TIMEOUT;
        end IF;
        return Receive(Connection, From);
      end Receive;
  end Implementation;

