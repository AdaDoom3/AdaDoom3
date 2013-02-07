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
  GNAT.Traceback,
  GNAT.Traceback.Symbolic,
  Ada.Strings.Wide_Fixed;
use
  GNAT.Traceback,
  GNAT.Traceback.Symbolic,
  Ada.Strings.Wide_Fixed;
separate(Neo.System.Processor)
package body Implementation_For_Compiler
  is
  --------------------
  -- Put_Call_Stack --
  --------------------
    function Put_Call_Stack
      return Boolean
      is
      Trace   : Tracebacks_Array(1..CALLBACK_TRACE_LIMIT) := (others => NULL_ADDRESS);
      Length  : Integer_4_Natural                         := 0;
      Text_IO : Record_Input_Output                       := Protected_Text_IO.Get;
      begin
        Call_Chain(Trace, Length);
        Put_Line("Call stack:");
        ----------------
        Sweep_Traceback:
        ----------------
          declare
          Traceback  : String_2         := To_String_2(Symbolic_Traceback(Trace(1..Length)));
          Skip_Next  : Boolean          := False;
          To_Skip    : Integer_4_Signed := 3;
          Skips      : Integer_4_Signed := 1;
          Current    : Integer_4_Signed := 1;
          Index      : Integer_4_Signed := 1;
          begin
            while Traceback(Current) /= Character_2'Val(Character_1'Pos(Ascii.LF)) loop
              if Current = Traceback'Last then
                exit;
              end if;
              if Index /= Length then
                New_Line;
              end if;
              ----------------------
              Put_With_Padded_Index:
              ----------------------
                declare
                Index_Image : String_2 := Trim(Integer_4_Signed'Wide_Image(Index), Both);
                begin
                  Put_Tab(2);
                  for I in 2..Integer_4_Natural'Image(Length)'Length - Index_Image'Length loop
                    Put(" ");
                  end loop;
                  Put(Index_Image & ": " & Traceback(Current + 2..Current + 10));
                end Put_With_Padded_Index;
              for I in Current + 11..Traceback'Last loop
                if I = Traceback'Last then
                  Current := I;
                elsif Traceback(I) = Character_2'Val(Character_1'Pos(Ascii.LF)) then
                  Current := I + 1;
                  exit;
                end if;
                if Traceback(Current + 11) /= ' ' then
                  if I + 2 <= Traceback'Last and then Traceback(I..I + 2) = "at " then
                    Skip_Next := True;
                    New_Line;
                    Put_Tab(2);
                    for I in 1..Integer_4_Natural'Image(Length)'Length loop
                      Put(" ");
                    end loop;
                    Put("       At ");
                  end if;
                  if not Skip_Next then
                    Put(Traceback(I));
                  else
                    if Skips = To_Skip then
                      Skip_Next := False;
                      Skips     := 1;
                    else
                      Skips := Skips + 1;
                    end if;                  
                  end if;
                end if;
              end loop;
              Index := Index - 1;
            end loop;
            New_Line;
          end Sweep_Traceback;
        return True;
      end Put_Call_Stack;
  end Implementation_For_Compiler;
