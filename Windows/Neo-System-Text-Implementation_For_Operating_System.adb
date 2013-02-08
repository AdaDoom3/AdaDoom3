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
  Neo.Windows;
use
  Neo.Windows;
separate(Neo.System.Text)
package body Implementation_For_Operating_System
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
      Data     : Address                     := NULL_ADDRESS;
      Accessor : Access_Integer_2_Unsigned_C := null;
      begin
        Data := Global_Allocate(MEMORY_MOVEABLE or MEMORY_DDE_SHARE, (Text'Length + 1) * 2);
        if Data = NULL_ADDRESS then
          raise System_Call_Failure;
        end if;
        Accessor := To_Access_Integer_2_Unsigned_C(Global_Lock(Data));
        if Accessor = null then
          raise System_Call_Failure;
        end if;
        for I in 1..Text'Length loop
          Accessor.All := Integer_2_Unsigned_C(Character_2'Pos(Text(I)));
          Accessor     := To_Access_Integer_2_Unsigned_C(To_Integer_4_Unsigned(Accessor) + 2);
        end loop;
        Accessor.All := 0;
        if Global_Unlock(Data) /= Integer_4_Signed_C(NO_ERROR) then
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
        Accessor := To_Access_Constant_Character_2_C(Global_Lock(Data));
        if Accessor = null then
          raise System_Call_Failure;
        end if;
        ---------------
        Compose_Result:
        ---------------
          declare
          Result : String_2 := To_String_2(Accessor);
          begin
            if Global_Unlock(Data) /= Integer_4_Signed_C(NO_ERROR) then 
              null;--raise System_Call_Failure;
            end if;
            if Close_Clipboard = FAILED then
              raise System_Call_Failure;
            end if;
            return Result;
          end Compose_Result;
      end Get_Clipboard;
  --------------
  -- To_Lower --
  --------------
    function To_Lower(
      Item : in Character_2)
      return Character_2
      is
      Result : Integer_2_Unsigned_C := To_Lower(Integer_2_Unsigned_C(Character_2'Val(Item)));
      begin
        if DO_TEST_FOR_END_OF_FILE and then Result = END_OF_FILE_ON_WINDOWS then
          raise System_Call_Failure;
        end if;
        return Character_2'Pos(Integer_4_Signed(Result));
      end To_Lower;
  --------------
  -- To_Upper --
  --------------
    function To_Upper(
      Item : in Character_2)
      return Character_2
      Result : Integer_2_Unsigned_C := To_Upper(Integer_2_Unsigned_C(Character_2'Val(Item)));
      begin
        if DO_TEST_FOR_END_OF_FILE and then Result = END_OF_FILE_ON_WINDOWS then
          raise System_Call_Failure;
        end if;
        return Character_2'Pos(Integer_4_Signed(Result));
      end To_Upper;
  --------------
  -- Is_Lower --
  --------------
    function Is_Lower(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Lower(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Lower;
  --------------
  -- Is_Upper --
  --------------
    function Is_Upper(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Upper(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Upper;
  ------------------
  -- Is_Printable --
  ------------------
    function Is_Printable(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Printable(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Printable;
  --------------------
  -- Is_White_Space --
  --------------------
    function Is_White_Space(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_White_Space(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_White_Space;
  ----------------
  -- Is_Control --
  ----------------
    function Is_Control(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Control(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Control;
  -------------------
  -- Is_Alphabetic --
  -------------------
    function Is_Alphabetic(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Alphabetic(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Alphabetic;
  ---------------------
  -- Is_Alphanumeric --
  ---------------------
    function Is_Alphanumeric(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Alphanumeric(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Alphanumeric;
  --------------------
  -- Is_Punctuation --
  --------------------
    function Is_Punctuation(
      Item : in Character_2)
      return Boolean
      is
      begin
        return Is_Punctuation(Integer_2_Unsigned_C(Item)) = C_TRUE;
      end Is_Punctuation;
  end Implementation_For_Operating_System;
