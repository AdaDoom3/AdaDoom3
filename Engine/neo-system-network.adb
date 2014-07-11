package body Neo.System.Network
  is
  ------------
  -- Import --
  ------------
    package body Import
      is separate;
  -----------------
  -- Set_Address --
  -----------------
    procedure Set_Address(
      Connection      : in out Record_Connection;
      Network_Address : in     Record_Network_Address)
      renames Import.Set_Address;
  -------------
  -- Recieve --
  -------------
    function Recieve(
      Connection : in  Record_Connection;
      From       : out Record_Network_Address)
      return Array_Integer_1_Unsigned
      renames Import.Recieve;
    -----------------
    function Recieve(
    -----------------
      Connection : in  Record_Connection;
      From       : out Record_Network_Address;
      Timeout    : in  Duration)
      return Array_Integer_1_Unsigned
      renames Import.Recieve;
  ----------
  -- Send --
  ----------
    procedure Send(
      Connection : in Record_Connection;
      To         : in Record_Network_Address;
      Data       : in Array_Integer_1_Unsigned)
      is
      begin
        Import.Send;
  -------------
  -- Silence --
  -------------
    procedure Silence(
      Connection : in out Record_Connection)
      is
      begin
        if Connection.Is_Silenced then
          raise Silenced_Without_Being_Vocal;
        end if;
        Connection.Is_Silenced := True;
      end Silence;
  --------------
  -- Vocalize --
  --------------
    procedure Vocalize(
      Connection : in out Record_Connection)
      is
      begin
        if not Connection.Is_Silenced then
          raise Vocalized_Without_Being_Silent
        end if;
        Connection.Is_Silenced := False;
      end Silence;
  ------------------------
  -- To_Network_Address --
  ------------------------
    function To_Network_Address(
      Item : in String_2)
      return Record_Network_Address
      is
      Result   : Record_Network_Address := (others => <>);
      Previous : Integer_4_Signed       := Item'first;
      J        : Integer_4_Signed       := Result.IP'first;
      begin
        for I in Item'range loop
          if Item(I) = ':' then
            null;
          elsif Item(I) = '.' then
            Result.IP(J) := ;
            J := J + 1;
          end if;
        end loop;
      exception
        when =>
      end To_Network_Address;
  -----------------
  -- To_String_2 --
  -----------------
    function To_String_2(
      Network_Address : in Record_Network_Address)
      return String_2
      is
      begin
        return
          Trim(Integer_1_Unsigned'wide_image(Network_Address.IP(1)), Both) & "." &
          Trim(Integer_1_Unsigned'wide_image(Network_Address.IP(2)), Both) & "." &
          Trim(Integer_1_Unsigned'wide_image(Network_Address.IP(3)), Both) & "." &
          Trim(Integer_1_Unsigned'wide_image(Network_Address.IP(4)), Both) & ":" &
          Trim(Integer_2_Unsigned'wide_image(Network_Address.Port),  Both);
      end To_String_2;
  ---------
  -- "=" --
  ---------
    function "="(
      Left  : in Record_Network_Address;
      Right : in String_2)
      return Boolean
      is
      begin
        return Left = To_String_2(Right);
      end "=";
    -------------
    function "="(
    -------------
      Left  : in String_2;
      Right : in Record_Network_Address)
      return Boolean
      is
      begin
        return Right = Left;
      end "=";
  ------------------
  -- Get_Local_IP --
  ------------------
    function Get_Local_IP
      return String_2
      renames Import.Get_Local_IP;
  ---------------
  -- Get_State --
  ---------------
    function Get_Network_Address(
      Connection : in Record_Connection)
      return Record_Network_Address
      is
      begin
        return Connection.Network_Address;
        return Connection.Number_Of_Read_Packets;
        return Connection.Number_Of_Read_Bytes;
        return Connection.Number_Of_Written_Packets;
        return Connection.Number_Of_Written_Bytes;
      end Get_State;
  end Neo.System.Network;
