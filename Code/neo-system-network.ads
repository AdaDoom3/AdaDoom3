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
  Ada.Finalization.Controlled,
  Neo.Foundation.Data_Types;
use
  Neo.Foundation.Data_Types;
package Neo.System.Network
  is
  ----------------
  -- Exceptions --
  ----------------
    Silenced_Without_Being_Vocal   : Exception;
    Vocalized_without_Begin_Silent : Exception;
    Invalid_Network_Address_Passed : Exception;
  -----------
  -- Types --
  -----------
    type Record_Connection
      is private;
  -----------------
  -- Subprograms --
  -----------------
    procedure Initialize;
    procedure Finalize;
    function Get_Local_IP
      return String_2;
    function Get_Network_Address(
      Connection : in Record_Connection)
      return Record_Network_Address;
    function Get_Number_Of_Read_Packets(
      Connection : in Record_Connection)
      return Integer_4_Natural;
    function Get_Number_Of_Read_Bytes(
      Connection : in Record_Connection)
      return Integer_4_Natural;
    function Get_Number_Of_Written_Packets(
      Connection : in Record_Connection)
      return Integer_4_Natural;
    function Get_Number_Of_Written_Bytes(
      Connection : in Record_Connection)
      return Integer_4_Natural;
    procedure Set_Address(
      Connection      : in out Record_Connection;
      Network_Address : in     String_2);
    procedure Silence(
      Connection : in out Record_Connection);
    procedure Vocalize(
      Connection : in out Record_Connection);
    function Recieve(
      Connection : in  Record_Connection;
      From       : out Record_Network_Address;
      Timeout    : in  Duration := 0.0)
      return Array_Integer_1_Unsigned;
    procedure Send(
      Connection : in Record_Connection;
      To         : in Record_Network_Address;
      Data       : in Array_Integer_1_Unsigned);
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Connection
      is Ada.Controlled.Limited_Controlled
      with record
        Network_Address           : String_1(1..64)   := (others => NULL_CHARACTER_1);
        Socket                    : Integer_4_Natural := 0;
        Is_Silenced               : Boolean           := False;
        Number_Of_Read_Packets    : Integer_4_Natural := 0;
        Number_Of_Read_Bytes      : Integer_4_Natural := 0;
        Number_Of_Written_Packets : Integer_4_Natural := 0;
        Number_Of_Written_Bytes   : Integer_4_Natural := 0;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Finalize(
      Item : in out Record_Connection);
  --------------------
  -- Implementation --
  --------------------
    package Implementation
      is
        procedure Initialize;
        procedure Finalize;
        function Get_Local_IP
          return String_2;
        procedure Set_Address(
          Connection      : in out Record_Connection;
          Network_Address : in     Record_Network_Address);
        function Recieve(
          Connection : in Record_Connection;
          From       : in Record_Network_Address)
          return Array_Integer_1_Unsigned;
        function Recieve(
          Connection : in Record_Connection;
          From       : in Record_Network_Address;
          Timeout    : in Duration)
          return Array_Integer_1_Unsigned;
        procedure Send(
          Connection : in Record_Connection;
          To         : in Record_Network_Address);
      end Implementation;
  end Neo.System.Network;

