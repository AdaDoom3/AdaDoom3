with Glib; use Glib;
with Gtk.Enums;
with Ada.Text_IO; use Ada.Text_IO;

procedure Glist_Traverse is
   use Gtk.Enums.Gint_List;
   List : Gtk.Enums.Gint_List.Glist;
   Temp : Gtk.Enums.Gint_List.Glist;
begin

   --  First step: create a new list.

   Prepend (List, 2);                       -- add at the beginning of the list
   Append (List, 3);                        -- add at the end of the list
   Insert (List, Data => 1, Position => 1); -- in the middle of the list

   --  Traverse the list (first way)

   Temp := First (List);
   while Temp /= Null_List loop
      Put_Line (Gint'Image (Get_Data (Temp)));
      Temp := Next (Temp);
   end loop;

   --  Traverse the list (second way)

   for I in 1 .. Length (List) loop
      Put_Line (Gint'Image (Nth_Data (List, I - 1)));
   end loop;

end Glist_Traverse;
