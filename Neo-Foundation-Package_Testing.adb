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
        Put_Title("PACKAGE TESTING TEST");
        Put_Title("SECOND PART OF THE PACKAGE_TESTING TEST FEATURING A VERY LARGE TITLE123456123456");
        Hang_Window;
        --Set_Line_Size(10);
        --Put_Title("12345678901"); -- Should raise exception
      end Test;
  ---------------
  -- Put_Title --
  ---------------
    procedure Put_Title(
      Title : in String_2)
      is
      Space_Count : Integer_4_Signed := 0;
      Count       : Integer_4_Signed := Title'Length * 3 - Title'Length / 3;
      begin
        if Title'Length > Get_Line_Size then
          raise Title_Is_Too_Large_To_Fit_On_A_Single_Line;
        end if;
        New_Line;
        for I in 1..Get_Line_Size loop
          Put(TESTING_SEPORATOR);
        end loop;
        New_Line(2);
        for I in 1..Title'Length loop
          if Title(I) = ' ' then
            Space_Count := Space_Count + 1;
          end if;
        end loop;
        if Count + Space_Count >= Get_Line_Size then
          for I in 1..(Get_Line_Size / 2 - (Title'Length) / 2 - 1) loop
            Put(" ");
          end loop;
          Put(Title);
        else
          for I in 1..(Get_Line_Size / 2 - (Count + Space_Count) / 2 - 1) loop
            Put(" ");
          end loop;
          for I in 1..Title'Length loop
            Put(Title(I) & "  ");
            if Title(I) = ' ' then
              Put(" ");
            end if;
          end loop;
        end if;
        New_Line;
        for I in 1..Get_Line_Size loop
          Put(TESTING_SEPORATOR);
        end loop;
        New_Line(2);
      end Put_Title;
    -----------------
    -- Hang_Window --
    -----------------
      procedure Hang_Window
        is
        Item : String_2(1..1)    := (others => NULL_CHARACTER_2);
        Last : Integer_4_Natural := 0;
        begin
          New_Line;
          Put(TESTING_INPUT_CURSOR);
          Get_Line(Item, Last);
        end Hang_Window;
  end Neo.Foundation.Package_Testing;
