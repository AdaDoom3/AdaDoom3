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
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
use
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing;
generic
  type Type_To_Order
    is private;
package Neo.Library.Byte_Ordering
  is
  -----------------
  -- Subprograms --
  -----------------
    procedure Test;
    function To_Low_Order_Byte_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_Low_Order_Byte_First(
      Item : in out Type_To_Order);
    function To_Low_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_Low_Order_Byte_Then_Low_Order_Bit_First(
      Item : in out Type_To_Order);
    function To_Low_Order_Byte_Then_High_Order_Bit_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_Low_Order_Byte_Then_High_Order_Bit_First(
      Item : in out Type_To_Order);
    function To_High_Order_Byte_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_High_Order_Byte_First(
      Item : in out Type_To_Order);
    function To_High_Order_Byte_Then_Low_Order_Bit_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_High_Order_Byte_Then_Low_Order_Bit_First(
      Item : in out Type_To_Order);
    function To_High_Order_Byte_Then_High_Order_Bit_First(
      Item : in Type_To_Order)
      return Type_To_Order;
    procedure To_High_Order_Byte_Then_High_Order_Bit_First(
      Item : in out Type_To_Order);
  end Neo.Library.Byte_Ordering;
