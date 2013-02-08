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
  Neo.Foundation.Data_Types;
use
  Neo.Foundation.Data_Types;
package Neo.System.Network
  is
  ---------------
  -- Constants --
  ---------------
  -----------------
  -- Subprograms --
  -----------------
    function Wait_For_UDP_Packet(
      Network_Socket : in Integer_4_Positive;
      Time_Out       : in Integer_4_Natural)
      return Boolean
    function Recieve_UDP_Packet(
      Socket       : in Integer_4_Positive;
      From_Address : in Record_Network_Address;
      Maximum_Size : in Integer_4_Positive := DEFAULT_MAXIMUM_UDP_PACKET_SIZE)
      return Address;
    procedure Send_UDP_Packet(
      Socket            : in Integer_4_Unsigned;
      Data              : in Array_Integer_1_Unsigned;
      Recipiant_Address : in Record_Network_Address);
    function Create_Socket(
      Network_Interface : in String_2;
      Port              : in Integer_2_Unsigned_C;
      Bound_To          : in Access_Network_Address)
      return Integer_4_Unsigned_C;
    procedure Open_Connection(
      Port : in Integer_4_signed)
    function Is_Local_Address(
      Address : in Record_Network_Address) 
      return Boolean;
-------
private
-------
  end Neo.System.Network;
