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
    Connection_Silenced_Without_Being_Vocal   : Exception;
    Connection_Vocalized_without_Begin_Silent : Exception;
    Invalid_Network_Address_Passed            : Exception;
  -----------
  -- Types --
  -----------
    type Record_Connection
      is private;
  --------------
  -- Packages --
  --------------
    generic
      type Array_Type_To_Message
        is array--???private;
    package Messenger
      is
        function Recieve(
          Connection : in  Record_Connection;
          From       : out String_2;
          Timeout    : in  Duration := 0.0)
          return Array_Type_To_Message;
        procedure Send(
          Connection : in Record_Connection;
          To         : in String_2;
          Data       : in Array_Type_To_Message);
      end Messenger;
  -------------
  -- Records --
  -------------
    type Record_Connection_Status
      is record
        Network_Address           : String_2(1..64)    := (others => NULL_CHARACTER_2);
        Socket                    : Integer_8_Unsigned := 0;
        Number_Of_Read_Packets    : Integer_8_Unsigned := 0;
        Number_Of_Read_Bytes      : Integer_8_Unsigned := 0;
        Number_Of_Written_Packets : Integer_8_Unsigned := 0;
        Number_Of_Written_Bytes   : Integer_8_Unsigned := 0;
      end record;
  -----------------
  -- Subprograms --
  -----------------
    procedure Initialize;
    procedure Finalize;
    function Get_Local_IP
      return String_2;
    function Get_Connection_Status(
      Connection : in Record_Connection)
      return Record_Status;
    procedure Set_Address(
      Connection      : in out Record_Connection;
      Network_Address : in     String_2);
    procedure Silence(
      Connection : in out Record_Connection);
    procedure Vocalize(
      Connection : in out Record_Connection);
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Connection
      is Ada.Controlled.Limited_Controlled
      with record
        Vocal_Status      : Protected_Status;
        Connection_Status : Record_Connection_Status := (others => <>);
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
          Network_Address : in     String_2);
        function Recieve(
          Connection : in Record_Connection;
          From       : in String_2)
          return Array_Integer_1_Unsigned;
        function Recieve(
          Connection : in Record_Connection;
          From       : in String_2;
          Timeout    : in Duration)
          return Array_Integer_1_Unsigned;
        procedure Send(
          Data       : in Array_Integer_1_Unsigned;
          Connection : in Record_Connection;
          To         : in String_2);
      end Implementation;
  end Neo.System.Network;
