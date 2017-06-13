
--                                                                                                                                      --
--                                                         N E O  E N G I N E                                                           --
--                                                                                                                                      --
--                                                 Copyright (C) 2016 Justin Squirek                                                    --
--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Neo is

  ------------
  -- Status --
  ------------

  protected body Safe_Status is
      function Occupied return Bool is (Status);
      procedure Occupied (Val : Bool) is begin Status := Val; end;
    end;
    
  ----------
  -- Path --
  ----------
  
  protected body App_Path is
  
      -- Path separator for OS
      function Sep return Char is (Current_Sep);
      procedure Set_Sep (Sep : Char) is begin Current_Sep := Sep; end;
      
      -- Executable path
      procedure Set (Path : Str_Unbound) is begin Current_Path := Path;     end;
      procedure Set (Path : Str)         is begin Current_Path := U (Path); end;
      function Get return Str_Unbound    is (Current_Path);
      function Get return Str            is (S (Current_Path));
    end;

  ---------------
  -- Debugging --
  ---------------

  procedure Assert (Val : Int_16_Unsigned_C) is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_Unsigned_C)    is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_C)             is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Ptr)               is begin Assert (Val /= NULL_PTR); end;
  procedure Assert (Val : Bool)              is begin if not Val then raise Program_Error; end if; end;--  pragma Assert (Val);      end;

  ------------
  -- Timing --
  ------------

  -- Fetch the time when the application was started
  START_TIME : Time := Clock;
  function Get_Start_Time return Time is (START_TIME);

  -- Get the duration of the current timer
  function Get_Duration (Timer : Timer_State) return Duration is ((if Timer.Is_Stopped then Timer.Last else Clock - Timer.Start));

  -- Start and start the timer and raise an error if it not being used properly
  procedure Start (Timer : in out Timer_State) is begin Assert (not Timer.Is_Stopped); Timer := (Is_Stopped => False, Start => Clock,               others => <>); end;
  procedure Stop  (Timer : in out Timer_State) is begin Assert (Timer.Is_Stopped);     Timer := (Is_Stopped => True,  Last  => Timer.Start - Clock, others => <>); end;
  
  -----------------
  -- Conversions --
  -----------------

  -- Character conversions
  function To_Char_8 (Item : Char_16) return Char_8 is
    (if Char_16'Pos (Item) > Char_8'Pos (Char_8'Last) then CHAR_16_REPLACEMENT else Char_8'Val (Char_16'Pos (Item)));

  -- String conversions
  function To_Str_8 (Item : Str) return Str_8 is
    Result : Str_8 (Item'First..Item'Length);
    begin
      for I in Item'Range loop Result (I) := To_Char_8 (Item (I)); end loop;
      return Result;
    end;
  function To_Str_16_C (Item : Str_8_C) return Str_16_C is
    Result : Str_16_C (Item'First..Item'Length);
    begin
      for I in Item'Range loop Result (I) := Char_16_C'Val (Char_8_C'Pos (Item (I))); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_8) return Str is
    Result : Str (Item'Range);
    begin
      for I in Item'Range loop Result (I) := Char_16'Val (Char_8'Pos (Item (I))); end loop;
      return Result;
    end;
  function To_Str_16 (Item : Str_16_C) return Str is
    Last   : Int_Size_C := Item'First;
    Buffer : Str_Unbound;
    begin
      for I in Item'Range loop
        exit when Item (I) = NULL_CHAR_16_C;
        Buffer := Buffer & Char_16'Val (Char_16_C'Pos (Item (Int_Size_C (I))));
      end loop;
      return S (Buffer);
    end;
  function To_Str_16 (Item : Ptr_Const_Char_16_C) return Str is
    Length : Int := 0;
    Buffer : Str_Unbound;
    Temp   : Ptr_Const_Char_16_C := Item;
    begin
      while Temp.all /= NULL_CHAR_16_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_16 (Temp.all);
        Temp   := To_Ptr_Const_Char_16_C (To_Ptr (To_Int_Ptr (Temp) + Char_16_C'Size / Byte'Size));
      end loop;
      return S (Buffer);
    end;          
  function To_Str_8 (Item : Ptr_Const_Char_8_C) return Str_8 is 
    Length : Int := 0;
    Buffer : Str_8_Unbound;
    Temp   : Ptr_Const_Char_8_C := Item;
    begin
      while Temp.all /= NULL_CHAR_8_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_8 (Temp.all);
        Temp   := To_Ptr_Const_Char_8_C (To_Ptr (To_Int_Ptr (Temp) + Char_8_C'Size / Byte'Size));
      end loop;
      return To_Str_8 (Buffer);
    end;

  -- Integer to string with a changable base (e.g. decimal to binary or hex)
  function Generic_To_Str_16 (Item : Num_T; Base : Positive; Do_Pad_Zeros : Bool := True) return Str is
    package Num_T_Text_IO is new Ada_IO.Modular_IO (Num_T);
    Buffer : Str_Unbound;
    Input  : Str (1..4096);
    begin
      Num_T_Text_IO.Put (Input, Item, Ada_IO.Number_Base (Base));
      if Base = 10 then return Trim (Input, Both); end if;
      Buffer := To_Str_Unbound (Trim (Input, Both));
      Delete (Buffer, 1, Trim (Base'Img, Both)'Length + 1);
      Delete (Buffer, Length (Buffer), Length (Buffer));
      if Item /= Num_T'Last and Do_Pad_Zeros then
        for I in 1..Generic_To_Str_16 (Num_T'Last, Base)'Length - Length (Buffer) loop Insert (Buffer, 1, "0"); end loop;
      end if;
      return S (Buffer);
    end;
end;
