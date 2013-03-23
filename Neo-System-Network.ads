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
  Ada.Finalization.Controlled,
  Neo.Foundation.Data_Types;
USE
  Neo.Foundation.Data_Types;
PACKAGE Neo.System.Network
  IS
  ----------------
  -- EXCEPTIONS --
  ----------------
    SILENCED_WITHOUT_BEING_VOCAL   : CONSTANT Exception;
    VOCALIZED_WITHOUT_BEING_SILENT : CONSTANT Exception;
    INVALID_NETWORK_ADDRESS        : CONSTANT Exception;
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Network_Address
      IS RECORD
        IP   : Array_Integer_1_Unsigned(1..4) := (OTHERS => 0);
        Port : Integer_2_Unsigned             := 0;
      END RECORD;
    TYPE Record_Connection
      IS PRIVATE;
  -----------------
  -- SUBPROGRAMS --
  -----------------
    PROCEDURE Initialize;
    PROCEDURE Finalize;
    FUNCTION Get_Local_IP
      RETURN String_2;
    FUNCTION Get_Network_Address(
      Connection : IN Record_Connection)
      RETURN Record_Network_Address;
    FUNCTION Get_Number_Of_Read_Packets(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural;
    FUNCTION Get_Number_Of_Read_Bytes(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural;
    FUNCTION Get_Number_Of_Written_Packets(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural;
    FUNCTION Get_Number_Of_Written_Bytes(
      Connection : IN Record_Connection)
      RETURN Integer_4_Natural;
    PROCEDURE Set_Address(
      Connection      : IN OUT Record_Connection;
      Network_Address : IN     Record_Network_Address);
    PROCEDURE Silence(
      Connection : IN OUT Record_Connection);
    PROCEDURE Vocalize(
      Connection : IN OUT Record_Connection);
    FUNCTION To_Network_Address(
      Item : IN String_2)
      RETURN Record_Network_Address;
    FUNCTION To_String_2(
      Network_Address : IN Record_Network_Address)
      RETURN String_2;
    FUNCTION "="(
      Left  : IN Record_Network_Address;
      Right : IN String_2)
      RETURN Boolean;
    FUNCTION "="(
      Left  : IN String_2;
      Right : IN Record_Network_Address)
      RETURN Boolean;
    FUNCTION Recieve(
      Connection : IN  Record_Connection;
      From       : OUT Record_Network_Address)
      RETURN Array_Integer_1_Unsigned;
    FUNCTION Recieve(
      Connection : IN  Record_Connection;
      From       : OUT Record_Network_Address;
      Timeout    : IN  Duration)
      RETURN Array_Integer_1_Unsigned;
    PROCEDURE Send(
      Connection : IN Record_Connection;
      To         : IN Record_Network_Address;
      Data       : IN Array_Integer_1_Unsigned);
-------
PRIVATE
-------
  -------------
  -- RECORDS --
  -------------
    TYPE Record_Connection
      IS Ada.Controlled.Limited_Controlled
      WITH RECORD
        Network_Address           : Record_Network_Address := (OTHERS => <>);
        Socket                    : Integer_4_Natural      := 0;
        Is_Silenced               : Boolean                := False;
        Number_Of_Read_Packets    : Integer_4_Natural      := 0;
        Number_Of_Read_Bytes      : Integer_4_Natural      := 0;
        Number_Of_Written_Packets : Integer_4_Natural      := 0;
        Number_Of_Written_Bytes   : Integer_4_Natural      := 0;
      END RECORD;
  -----------------
  -- SUBPROGRAMS --
  -----------------
    PROCEDURE Finalize(
      Item : IN OUT Record_Connection);
  --------------------
  -- IMPLEMENTATION --
  --------------------
    PACKAGE Implementation
      IS
      PROCEDURE Initialize;
      PROCEDURE Finalize;
      FUNCTION Get_Local_IP
        RETURN String_2;
      PROCEDURE Set_Address(
        Connection      : IN OUT Record_Connection;
        Network_Address : IN     Record_Network_Address);
      FUNCTION Recieve(
        Connection : IN Record_Connection;
        From       : IN Record_Network_Address)
        RETURN Array_Integer_1_Unsigned;
      FUNCTION Recieve(
        Connection : IN Record_Connection;
        From       : IN Record_Network_Address;
        Timeout    : IN Duration)
        RETURN Array_Integer_1_Unsigned;
      PROCEDURE Send(
        Connection : IN Record_Connection;
        To         : IN Record_Network_Address);
      END Implementation;
  END Neo.System.Network;

