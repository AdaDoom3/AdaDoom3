
--  The procedure below shows how you can hide all the columns but one
--  in the clist.
--  Since Gtk_Clist prevents you to hide the last visible column, the following
--  code does not work:
--
--     --  Hide all the columns
--     for J in 0 .. Get_Columns (Clist) loop
--        Set_Column_Visibility (Clist, J, False);
--     end loop;
--
--     --  Show the one you want
--     Set_Column_Visibility (Clist, New_Column, True);
--
--  The following code should be used instead:

package body Clist is

   procedure Hide_All_But_One (Clist : access Gtk_Clist_Record'Class;
                               New_Column : Gint)
   is
   begin
      --  Make sure that at least one column is visible
      Set_Column_Visibility (Clist, New_Column, True);

      --  Hide all the other columns.
      for J in 0 .. Get_Columns (Clist) loop
         if J /= New_Column then
            Set_Column_Visibility (Clist, J, False);
         end if;
      end loop;
   end Hide_All_But_One;

end Clist;

