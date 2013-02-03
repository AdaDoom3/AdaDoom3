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
package body Neo.Foundation.Package_Testing
  is
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
        Display_Title("PACKAGE TESTING TEST", True);
        Display_Title("SECOND PART OF THE PACKAGE TESTING TEST FEATURING A VERY LARGE TITLE");
        Hang_Window;
      end Test;
  ---------------
  -- Put_Title --
  ---------------
    procedure Put_Title(
      Title              : in String_2;
      Do_Show_Directions : in Boolean := False)
      is
      Space_Count : Integer_4_Signed := 0;
      begin
        New_Line;
        for I in 1..Get.Line_Size loop
          Put(TESTING_SEPORATOR);
        end loop;
        New_Line;
        New_Line;
        for I in 1..Title'Length loop
          if Title(I) = ' ' then
            Space_Count := Space_Count + 1;
          end if;
        end loop;
        for I in 1..(Get.Line_Size / 2 - (Title'Length * 3 - Title'Length / 3 + Space_Count) / 2 - 1) loop
          Put(" ");
        end loop;
        for I in 1..Title'Length loop
          Put(Title(I) & "  ");
          if Title(I) = ' ' then
            Put(" ");
          end if;
        end loop;
        New_Line;
        for I in 1..Get.Line_Size loop
          Put(TESTING_SEPORATOR);
        end loop;
        New_Line;
        New_Line;
        if Do_Show_Directions then
          Put_Line(TESTING_INSTRUCTIONS);
          New_Line;
        end if;
      end Put_Title;
    -----------------
    -- Hang_Window --
    -----------------
      procedure Hang_Window
        is
        begin
          New_Line;
          Put(TESTING_INPUT_CURSOR);
          Get_Line;
        end Hang_Window;
  end Neo.Foundation.Package_Testing;