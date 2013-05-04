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
  Ada.Streams,
  Ada.Finalization.Controlled,
  Neo.Foundation.Data_Types;
use
  Ada.Streams,
  Neo.Foundation.Data_Types;
package Neo.System.Network
  is
  ----------------
  -- Exceptions --
  ----------------
    Connection_Silenced_Without_Being_Vocal   : Exception;
    Connection_Vocalized_without_Begin_Silent : Exception;
    Invalid_Network_Address_String_Passed     : Exception;
  -----------
  -- Types --
  -----------
    type Record_Connection
      is private;
  -------------
  -- Records --
  -------------
    type Record_State
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
    function Get_State(
      Connection : in Record_Connection)
      return Record_State;
    procedure Set_Address(
      Connection      : in out Record_Connection;
      Network_Address : in     String_2);
    procedure Silence(
      Connection : in out Record_Connection);
    procedure Vocalize(
      Connection : in out Record_Connection);
    function Recieve(
      Connection : in  Record_Connection;
      From       : out String_2;
      Timeout    : in  Duration := 0.0)
      return Stream_Element_Array;
    procedure Send(
      Connection : in Record_Connection;
      To         : in String_2;
      Data       : in Stream_Element_Array);
-------
private
-------
  -------------
  -- Records --
  -------------
    type Record_Connection
      is Ada.Controlled.Limited_Controlled
      with record
        Vocal_Status : Protected_Status;
        State        : Record_State := (others => <>);
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
          return Address;
        procedure Send(
          Data       : in Address;
          Connection : in Record_Connection;
          To         : in String_2);
      end Implementation;
  end Neo.System.Network;
