
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
  
  ---------------
  -- Debugging --
  ---------------

  procedure Debug_Assert (Val : Int_Unsigned_C)    is begin if Is_Debugging then Assert (Val /= 0);        end if; end;
  procedure Debug_Assert (Val : Int_16_Unsigned_C) is begin if Is_Debugging then Assert (Val /= 0);        end if; end;
  procedure Debug_Assert (Val : Int_C)             is begin if Is_Debugging then Assert (Val /= 0);        end if; end;
  procedure Debug_Assert (Val : Int_Ptr)           is begin if Is_Debugging then Assert (Val /= 0);        end if; end;
  procedure Debug_Assert (Val : Ptr)               is begin if Is_Debugging then Assert (Val /= NULL_PTR); end if; end;
  procedure Debug_Assert (Val : Bool)              is begin if Is_Debugging then Assert (Val);             end if; end;
  
  procedure Assert (Val : Int_16_Unsigned_C) is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_Unsigned_C)    is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_C)             is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Int_Ptr)           is begin Assert (Val /= 0);        end;
  procedure Assert (Val : Ptr)               is begin Assert (Val /= NULL_PTR); end;
  procedure Assert (Val : Bool) is
    begin
      if not Val then
      
        -- Get the call stack at the point of failure so we can return it through an exception message
        declare
        Traces        : Tracebacks_Array (1..128);
        Result        : Str_8_Unbound := NULL_STR_8_UNBOUND;
        Chain_Length  : Natural       := 0;
        Hit_At, In_At : Boolean       := False;
        begin
          Call_Chain (Traces, Chain_Length);
          
          -- Change line endings and ignore subprogram names due to dated exception message length restrictions - see RM 11.4.1(18)
          for Item of Symbolic_Traceback (Traces) loop
            if Item = ASCII.LF then
              Append (Result, EOL_8);
              Hit_At := False;
            elsif Hit_At then Append (Result, Item);
            elsif Item = ' ' then
              Hit_At := In_At;
              In_At  := not In_At;
            end if;
          end loop;
          
          -- Trim irrelevant bits
          Head (Result, Index (Result, "b__main") - EOL_8'Length);
          Tail (Result, Length (Result) - Index (Result, "neo.adb:"));
          Tail (Result, Length (Result) - Index (Result, EOL_8) - 1);
          
          -- Propagate the exception with the trimmed stack trace as its message
          raise Program_Error with To_Str_8 (Result);
        end;
      end if;
    end;
        
  ------------
  -- Status --
  ------------

  protected body Safe_Status is
      function Occupied return Bool is (Status);
      procedure Occupied (Val : Bool) is begin Status := Val; end;
    end;

  -------------
  -- Counter --
  -------------
  
  protected body Safe_Counter is
      function Get        return Int     is (Count);
      procedure Set       (Val : Int)    is begin Count := Val;            end;
      procedure Increment (Amount : Int) is begin Count := Count - Amount; end;
      procedure Decrement (Amount : Int) is begin Count := Count + Amount; end;
      procedure Increment                is begin Count := Count + 1;      end;
      procedure Decrement                is begin Count := Count - 1;      end;
    end;

  ------------
  -- Timing --
  ------------

  -- Fetch the time when the application was started
  START_TIME : Time := Clock;
  function Get_Start_Time return Time is (START_TIME);

  -- Get the duration of the current timer
  function Get_Duration (Timer : Timer_State) return Duration is ((if Timer.Is_Stopped then Timer.Last else Clock - Timer.Start));

  -- Start and start the timer and raise an error if it not being used properly
  procedure Start (Timer : in out Timer_State) is
    begin
      pragma Assert (not Timer.Is_Stopped); Timer := (Is_Stopped => False, Start => Clock, others => <>);
    end;
  procedure Stop (Timer : in out Timer_State) is
    begin
      pragma Assert (Timer.Is_Stopped); Timer := (Is_Stopped => True,  Last  => Timer.Start - Clock, others => <>);
    end;
  
  -----------------
  -- Conversions --
  -----------------

  -- Character conversions
  function To_Char_8 (Item : Char_16) return Char_8 is
    (if Char_16'Pos (Item) > Char_8'Pos (Char_8'Last) then CHAR_16_REPLACEMENT else Char_8'Val (Char_16'Pos (Item)));

  -- String conversions
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
    Last   : Int_Size_C  := Item'First;
    Buffer : Str_Unbound := NULL_STR_UNBOUND;
    begin
      for I in Item'Range loop
        exit when Item (I) = NULL_CHAR_16_C;
        Buffer := Buffer & Char_16'Val (Char_16_C'Pos (Item (Int_Size_C (I))));
      end loop;
      return S (Buffer);
    end;
  function To_Str_16 (Item : Ptr_Const_Char_16_C) return Str is
    Length : Int                 := 0;
    Buffer : Str_Unbound         := NULL_STR_UNBOUND;
    Temp   : Ptr_Const_Char_16_C := Item;
    begin
      while Temp.all /= NULL_CHAR_16_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_16 (Temp.all);
        Temp   := To_Ptr_Const_Char_16_C (To_Ptr (To_Int_Ptr (Temp) + Char_16_C'Size / Byte'Size));
      end loop;
      return S (Buffer);
    end;          
  function To_Str_8 (Item : Ptr_Char_8_C) return Str_8 is 
    Length : Int                := 0;
    Buffer : Str_8_Unbound      := NULL_STR_8_UNBOUND;
    Temp   : Ptr_Char_8_C := Item;
    begin
      while Temp.all /= NULL_CHAR_8_C loop
        Length := Length + 1;
        Buffer := Buffer & Char_8 (Temp.all);
        Temp   := To_Ptr_Char_8_C (To_Ptr (To_Int_Ptr (Temp) + Char_8_C'Size / Byte'Size));
      end loop;
      return To_Str_8 (Buffer);
    end;

  -- Integer to string with a changable base (e.g. decimal to binary or hex)
  function Generic_To_Str_16_Int (Item : Num_T; Base : Positive; Do_Pad_Zeros : Bool := True) return Str is
    package Num_T_Text_IO is new Ada_IO.Modular_IO (Num_T);
    Buffer : Str_Unbound   := NULL_STR_UNBOUND;
    Input  : Str (1..4096) := (others => NULL_CHAR);
    begin
      Num_T_Text_IO.Put (Input, Item, Ada_IO.Number_Base (Base));
      if Base = 10 then return Trim (Input, Both); end if;
      Buffer := To_Str_Unbound (Trim (Input, Both));
      Delete (Buffer, 1, Trim (Base'Img, Both)'Length + 1);
      Delete (Buffer, Length (Buffer), Length (Buffer));
      if Item /= Num_T'Last and Do_Pad_Zeros then
        for I in 1..Generic_To_Str_16_Int (Num_T'Last, Base)'Length - Length (Buffer) loop Insert (Buffer, 1, "0"); end loop;
      end if;
      return S (Buffer);
    end;
    
  -- Float to string
  function Generic_To_Str_16_Real (Item : Num_T) return Str_16 is
    package Var_T_IO is new Ada.Text_IO.Float_IO (Num_T);
    Result : Str_8 (1..Num_T'Digits * 2) := (others => ' ');
    begin
    
      -- Obtain Val's image and decide whether to use exponent notation if we encounter something ridiculous
      if Item > 100_000_000_000_000.0 or Item < -0.000000000001 then
        Var_T_IO.Put (Result, Item);
      else
        Var_T_IO.Put (Result, Item, Exp => 0);
      end if;
      
      -- Trim zeros
      for I in reverse Result'Range loop
        if Result (I) = '.' then Result (I) := ' '; exit; end if;
        exit when Result (I) /= '0';
        Result (I) := ' ';
      end loop;
      
      return To_Str (Trim (Result, Both));
    end;
end;
