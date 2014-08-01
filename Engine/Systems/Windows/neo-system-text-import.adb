with Ada.Unchecked_Conversion;
with Interfaces;   use Interfaces; 
with Interfaces.C; use Interfaces.C;
with Neo.Windows;  use Neo.Windows;
separate(Neo.System.Text) package body Import is
    procedure Set_Clipboard(Item : in String_2) is
      type Array_Text is array(Item'first..Item'last + 1) of Character_2_C;
      type Access_Array_Text is access all Array_Text;
      function To_Unchecked_Access_Array_Text is new Ada.Unchecked_Conversion(Address, Access_Array_Text);
      Data     : Address           := NULL_ADDRESS;
      Accessor : Access_Array_Text := null;
      begin
        Data := Global_Allocate(MEMORY_MOVEABLE or MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE, Array_Text'size / Byte'size);
        Assert(Data);
        Accessor := To_Unchecked_Access_Array_Text(Global_Lock(Data));
        Assert(Accessor /= null);
        Accessor(Accessor.all'last) := NULL_CHARACTER_2_C;
        for I in Item'range loop Accessor(I) := Character_2_C'val(Character_2'pos(Item(I))); end loop;
        Assert(Global_Unlock(Data) = 0);
        Assert(not(Open_Clipboard(NULL_ADDRESS) = FAILED and then Global_Free(Data) /= NULL_ADDRESS));
        Assert(Empty_Clipboard);
        Assert(Set_Clipboard_Data(CLIPBOARD_UNICODE_TEXT, Data));
        Assert(Close_Clipboard);
      end Set_Clipboard;
    function Get_Clipboard return String_2 is
      Data     : Address                       := NULL_ADDRESS;
      Accessor : Access_Constant_Character_2_C := null;
      begin
        Assert(Open_Clipboard(NULL_ADDRESS));
        Data     := Get_Clipboard_Data(CLIPBOARD_UNICODE_TEXT); Assert(Data);
        Accessor := To_Unchecked_Access_Constant_Character_2_C(Global_Lock(Data)); Assert_Dummy(Accessor /= null);
          declare Result : String_2 := To_String_2(Accessor); begin
            Assert_Dummy(Global_Unlock(Data) /= 0); -- Why does this fail???
            Assert(Close_Clipboard);
            return Result;
          end;
      end Get_Clipboard;
  end Import;
