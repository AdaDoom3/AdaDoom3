with Ada.Finalization.Controlled;
with Ada.Streams; use Ada.Streams;
package Neo.System.Network is
    Connection_Silenced_Without_Being_Vocal   : Exception;
    Connection_Vocalized_without_Being_Silent : Exception;
    Invalid_Network_Address_String_Passed     : Exception;
    String_Buffer_Too_Small_To_Store_Address  : Exception;
    type Record_Connection is private;
    type Record_State is record
        Network_Address           : String_2(1..64)    := (others => NULL_CHARACTER_2);
        Number_Of_Packets_Read    : Integer_8_Unsigned := 0;
        Number_Of_Packets_Written : Integer_8_Unsigned := 0;
        Number_Of_Bytes_Read      : Integer_8_Unsigned := 0;
        Number_Of_Bytes_Written   : Integer_8_Unsigned := 0;
      end record;
    procedure Test;
    procedure Silence     (Connection : in out Record_Connection);
    procedure Vocalize    (Connection : in out Record_Connection);
    procedure Set_Address (Connection : in out Record_Connection; Network_Address : in String_2);
    procedure Send        (Connection : in     Record_Connection; Recipient : in String_2; Data : in Stream_Element_Array);
    function Recieve      (Connection : in     Record_Connection; Sender : out String_2_Unbounded; Timeout : in Duration := 0.0) return Stream_Element_Array;
    function Get_State    (Connection : in     Record_Connection) return Record_State;
    function Get_IP                                               return String_2;
    IP : constant String_2 := Get_IP;
private
    type Record_Connection is Ada.Controlled.Limited_Controlled with record
        Vocal_Status : Protected_Status;
        Socket       : Integer_8_Unsigned := 0; --???
        State        : Record_State       := (others => <>);
      end record;
    procedure Finalize(Item : in out Record_Connection);
    package Import is
        procedure Set_Address (Connection : in out Record_Connection; Value : in String_2);
        procedure Send        (Connection : in     Record_Connection; To    : in String_2; Data : in Address);
        function Recieve      (Connection : in     Record_Connection; From  : in String_2; Timeout : in Duration) return Address;
        function Get_Local_IP return String_2;
      end Import;
  end Neo.System.Network;