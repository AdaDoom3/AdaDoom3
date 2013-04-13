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
PACKAGE BODY Neo.System.Network
  IS
  --------------------
  -- IMPLEMENTATION --
  --------------------
    PACKAGE BODY Implementation
      IS SEPARATE;
  ----------------
  -- Initialize --
  ----------------
    PROCEDURE Initialize
      RENAMES Implementation.Initialize;
  --------------
  -- Finalize --
  --------------
    PROCEDURE Finalize
      RENAMES Implementation.Finalize;
  -----------------
  -- Set_Address --
  -----------------
    PROCEDURE Set_Address(
      Connection      : IN OUT Record_Connection;
      Network_Address : IN     Record_Network_Address)
      RENAMES Implementation.Set_Address;
  -------------
  -- Recieve --
  -------------
    FUNCTION Recieve(
      Connection : IN  Record_Connection;
      From       : OUT Record_Network_Address)
      RETURN Array_Integer_1_Unsigned
      RENAMES Implementation.Recieve;
    FUNCTION Recieve(
      Connection : IN  Record_Connection;
      From       : OUT Record_Network_Address;
      Timeout    : IN  Duration)
      RETURN Array_Integer_1_Unsigned
      RENAMES Implementation.Recieve;
  ----------
  -- Send --
  ----------
    PROCEDURE Send(
      Connection : IN Record_Connection;
      To         : IN Record_Network_Address;
      Data       : IN Array_Integer_1_Unsigned)
      RENAMES Implementation.Send;
  ------------------
  -- Get_Local_IP --
  ------------------
    FUNCTION Get_Local_IP
      RETURN String_2
      RENAMES Implementation.Get_Local_IP;
  -------------------------
  -- Get_Network_Address --
  -------------------------
    FUNCTION Get_Network_Address(
      Connection : IN Record_Connection)
      RETURN Record_Network_Address
      IS
      BEGIN
        RETURN Connection.Network_Address;
      END Get_Network_Address;
  --------------------------------
  -- Get_Number_Of_Read_Packets --
  --------------------------------
    FUNCTION Get_Number_Of_Read_Packets(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural
      IS
      BEGIN
        RETURN Connection.Number_Of_Read_Packets;
      END Get_Number_Of_Read_Packets;
  ------------------------------
  -- Get_Number_Of_Read_Bytes --
  ------------------------------
    FUNCTION Get_Number_Of_Read_Bytes(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural
      IS
      BEGIN
        RETURN Connection.Number_Of_Read_Bytes;
      END Get_Number_Of_Read_Bytes;
  -----------------------------------
  -- Get_Number_Of_Written_Packets --
  -----------------------------------
    FUNCTION Get_Number_Of_Written_Packets(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural
      IS
      BEGIN
        RETURN Connection.Number_Of_Written_Packets;
      END Get_Number_Of_Written_Packets;
  ---------------------------------
  -- Get_Number_Of_Written_Bytes --
  ---------------------------------
    FUNCTION Get_Number_Of_Written_Bytes(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural
      IS
      BEGIN
        RETURN Connection.Number_Of_Written_Bytes;
      END Get_Number_Of_Written_Bytes;
  -------------
  -- Silence --
  -------------
    PROCEDURE Silence(
      Connection : IN OUT Record_Connection)
      IS
      BEGIN
        IF Connection.Is_Silenced THEN
          RAISE SILENCED_WITHOUT_BEING_VOCAL;
        END IF;
        Connection.Is_Silenced := True;
      END Silence;
  --------------
  -- Vocalize --
  --------------
    PROCEDURE Vocalize(
      Connection : IN OUT Record_Connection)
      IS
      BEGIN
        IF NOT Connection.Is_Silenced THEN
          RAISE VOCALIZED_WITHOUT_BEING_SILENT;
        END IF;
        Connection.Is_Silenced := False;
      END Silence;
  ------------------------
  -- To_Network_Address --
  ------------------------
    FUNCTION To_Network_Address(
      Item : IN String_2)
      RETURN Record_Network_Address
      IS
      Result   : Record_Network_Address := (OTHERS => <>);
      Previous : Integer_4_Signed       := Item'First;
      J        : Integer_4_Signed       := Result.IP'First;
      BEGIN
        FOR I IN Item'Range LOOP
          IF Item(I) = ':' THEN

          ELSIF Item(I) = '.' THEN
            Result.IP(J) := ;
            J := J + 1;
          END IF;
        END LOOP;
      EXCEPTION
        WHEN 
      END To_Network_Address;
  -----------------
  -- To_String_2 --
  -----------------
    FUNCTION To_String_2(
      Network_Address : IN Record_Network_Address)
      RETURN String_2
      IS
      BEGIN
        RETURN
          Trim(Integer_1_Unsigned'Wide_Image(Network_Address.IP(1)), Both) & "." &
          Trim(Integer_1_Unsigned'Wide_Image(Network_Address.IP(2)), Both) & "." & 
          Trim(Integer_1_Unsigned'Wide_Image(Network_Address.IP(3)), Both) & "." & 
          Trim(Integer_1_Unsigned'Wide_Image(Network_Address.IP(4)), Both) & ":" &
          Trim(Integer_2_Unsigned'Wide_Image(Network_Address.Port),  Both);
      END To_String_2;
  ---------
  -- "=" --
  ---------
    FUNCTION "="(
      Left  : IN Record_Network_Address;
      Right : IN String_2)
      RETURN Boolean
      IS
      BEGIN
        RETURN Left = To_String_2(Right);
      END "=";
    FUNCTION "="(
      Left  : IN String_2;
      Right : IN Record_Network_Address)
      RETURN Boolean
      IS
      BEGIN
        RETURN Right = Left;
      END "=";
  END Neo.System.Network;
