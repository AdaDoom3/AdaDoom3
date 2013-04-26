--
--
--
--
--
--
--
-- After first booting up the clipboard is empty and if Get_Clipboard is called a system call
-- failure is raised when instead a null string should be returned.
--
--
--
--
--
--
--
with
  Interfaces,
  Interfaces.C,
  Ada.Unchecked_Conversion,
  Neo.Windows;
use
  Interfaces,
  Interfaces.C,
  Neo.Windows;
separate(Neo.System.Text)
package body Implementation
  is
  ---------------
  -- Constants --
  ---------------
    DO_TEST_FOR_END_OF_FILE : constant Boolean := True;
  -------------------
  -- Set_Clipboard --
  -------------------
    procedure Set_Clipboard(
      Text : in String_2)
      is
      type Array_Text
        is array(Text'First..Text'Last + 1)
        of Character_2_C;
      type Access_Array_Text
        is access all Array_Text;
      function To_Unchecked_Access_Array_Text
        is new Ada.Unchecked_Conversion(Address, Access_Array_Text);
      Data     : Address           := NULL_ADDRESS;
      Accessor : Access_Array_Text := null;
      begin
        Data :=
	  Global_Allocate(
	    Flags => MEMORY_MOVEABLE or MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE,
	    Bytes => Array_Text'Size / Byte'Size);
        if Data = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        Accessor := To_Unchecked_Access_Array_Text(Global_Lock(Data));
        if Accessor = null then
          raise System_Call_Failure;
        end if;
        Accessor(Accessor.All'Last) := Character_2_C'Val(0);
        for I in Text'Range loop
          Accessor(I) := Character_2_C'Val(Character_2'Pos(Text(I)));
        end loop;
        if Global_Unlock(Data) /= 0 then
          raise System_Call_Failure;
        end if;
        if Open_Clipboard(NULL_ADDRESS) = FAILED and then Global_Free(Data) /= NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if Empty_Clipboard = FAILED then
          raise System_Call_Failure;
        end if;
        if Set_Clipboard_Data(CLIPBOARD_UNICODE_TEXT, Data) = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        if Close_Clipboard = FAILED then
          raise System_Call_Failure;
        end if;
      end Set_Clipboard;
  -------------------
  -- Get_Clipboard --
  -------------------
    function Get_Clipboard
      return String_2
      is
      Data     : Address                       := NULL_ADDRESS;
      Accessor : Access_Constant_Character_2_C := null;
      begin
        if Open_Clipboard(NULL_ADDRESS) = FAILED then
          raise System_Call_Failure;
        end if;
        Data := Get_Clipboard_Data(CLIPBOARD_UNICODE_TEXT);
        if Data = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        Accessor := To_Unchecked_Access_Constant_Character_2_C(Global_Lock(Data));
        if Accessor = null then
          raise System_Call_Failure;
        end if;
        ---------------
        Compose_Result:
        ---------------
          declare
          Result : String_2 := To_String_2(Accessor);
          begin
            if Global_Unlock(Data) /= 0 then
              null;--raise System_Call_Failure; Why does this fail???
            end if;
            if Close_Clipboard = FAILED then
              raise System_Call_Failure;
            end if;
            return Result;
          end Compose_Result;
      end Get_Clipboard;
  end Implementation;
