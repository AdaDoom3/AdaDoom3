-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;           use Glib;
with Gtk.Box;        use Gtk.Box;
with Gtk.Button;     use Gtk.Button;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk;            use Gtk;

with Ada.Text_IO;
with Interfaces.C.Strings;

package body Create_Status is

   package Status_Cb is new Handlers.Callback (Gtk_Status_Bar_Record);

   Counter : Gint := 1;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Status_Bar@B is generally found at the bottom of"
        & " application windows. It displays some help text, generally"
        & " depending on the current context in the application."
        & ASCII.LF
        & "In GtkAda, @bGtk_Status_Bar@B are organized as stacks: you push"
        & " some text onto the stack, and the top of the stack is always"
        & " displayed. You can then pop part of the stak to display the older"
        & " content of the @bGtk_Status_Bar@B. This behavior is especially"
        & " for temporarily displaying help in the status bar, but still being"
        & " able to show some other informations.";
   end Help;

   ----------
   -- Push --
   ----------

   procedure Push (Status : access Gtk_Status_Bar_Record'Class) is
      Id : Message_Id;
      pragma Unreferenced (Id);
   begin
      Id := Push (Status, 1, "Something" & Gint'Image (Counter));
      Counter := Counter + 1;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Status : access Gtk_Status_Bar_Record'Class) is
   begin
      Pop (Status, 1);
   end Pop;

   ------------
   -- Popped --
   ------------

   procedure Popped (Status : access Gtk_Status_Bar_Record'Class) is
      use type Messages_List.GSlist;
   begin
      if Get_Messages (Status) = Messages_List.Null_List then
         Counter := 1;
      end if;
   end Popped;

   -----------
   -- Steal --
   -----------

   procedure Steal (Status : access Gtk_Status_Bar_Record'Class) is
   begin
      Remove (Status, 1, 4);
   end Steal;

   --------------
   -- Contexts --
   --------------

   procedure Contexts (Status : access Gtk_Status_Bar_Record'Class) is
   begin
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "any context"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "any context")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "idle messages"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "idle message")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "some text"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "some text")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "hit the mouse")));
      Ada.Text_IO.Put_Line ("Status_Bar : Context : "
                            & "hit the mouse2"
                            & "  Id="
                            & Context_Id'Image (Get_Context_Id
                                          (Status, "hit the mouse2")));
   end Contexts;

   ----------
   -- Dump --
   ----------

   procedure Dump (Status : access Gtk_Status_Bar_Record'Class) is
      List : Messages_List.GSlist := Get_Messages (Status);
      use type Messages_List.GSlist;
   begin
      while List /= Messages_List.Null_List loop
         declare
            Msg : constant Status_Bar_Msg := Messages_List.Get_Data (List);
         begin
            Ada.Text_IO.Put_Line ("Context Id = "
                                  & Context_Id'Image (Msg.Context)
                                  & " Message_Id = "
                                  & Message_Id'Image (Msg.Message)
                                  & " Text = "
                                  & Interfaces.C.Strings.Value (Msg.Text));
         end;
         List := Messages_List.Next (List);
      end loop;
   end Dump;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1,
        Box2    : Gtk_Box;
      Status    : Gtk_Status_Bar;
      Button    : Gtk_Button;

   begin
      Set_Label (Frame, "Status Bar");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Vbox (Box2, False, 10);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Status);
      Pack_End (Box1, Status, False, False, 0);
      Status_Cb.Object_Connect (Status, "text_popped",
                                Status_Cb.To_Marshaller (Popped'Access),
                                Slot_Object => Status);

      Gtk_New (Button, "Push Something");
      Pack_Start (Box2, Button, False, False, 0);
      Status_Cb.Object_Connect (Button, "clicked",
                                Status_Cb.To_Marshaller (Push'Access),
                                Slot_Object => Status);

      Gtk_New (Button, "Pop");
      Pack_Start (Box2, Button, False, False, 0);
      Status_Cb.Object_Connect (Button, "clicked",
                                Status_Cb.To_Marshaller (Pop'Access),
                                Slot_Object => Status);

      Gtk_New (Button, "Steal Message_Id #4");
      Pack_Start (Box2, Button, False, False, 0);
      Status_Cb.Object_Connect (Button, "clicked",
                                Status_Cb.To_Marshaller (Steal'Access),
                                Slot_Object => Status);

      Gtk_New (Button, "Dump stack");
      Pack_Start (Box2, Button, False, False, 0);
      Status_Cb.Object_Connect (Button, "clicked",
                                Status_Cb.To_Marshaller (Dump'Access),
                                Slot_Object => Status);

      Gtk_New (Button, "Test contexts");
      Pack_Start (Box2, Button, False, False, 0);
      Status_Cb.Object_Connect (Button, "clicked",
                                Status_Cb.To_Marshaller (Contexts'Access),
                                Slot_Object => Status);

      Show_All (Frame);
   end Run;

end Create_Status;

