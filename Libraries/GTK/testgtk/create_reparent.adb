-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                  Copyright (C) 2001 ACT-Europe                    --
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

with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;    use Gtk.Button;
with Gtk.Label;     use Gtk.Label;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Widget;    use Gtk.Widget;
with Gtk;           use Gtk;

with Ada.Text_IO;

package body Create_Reparent is

   type My_Button_Record is new Gtk_Button_Record with record
      Label : Gtk_Label;
   end record;
   type My_Button is access all My_Button_Record'Class;

   package Box_Cb is new Handlers.User_Callback (My_Button_Record, Gtk_Box);
   package Int_Cb is new Handlers.User_Callback
     (Gtk_Label_Record, Gint);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo does not demonstrate a widget. Instead, it shows how"
        & " you can dynamically change the parent of a widget. The widget is"
        & " automatically redisplayed inside its new parent, and delete from"
        & " the old one."
        & ASCII.LF
        & "This demo also shows how to extend an existing @bGtk_Button@B to"
        & " include specific data to it.";
   end Help;

   -----------------------
   -- Set_Parent_Signal --
   -----------------------

   procedure Set_Parent_Signal (Child      : access Gtk_Label_Record'Class;
                                Old_Parent : access Gtk_Widget_Record'Class;
                                Data       : in Gint)
   is
   begin
      Ada.Text_IO.Put ("Set_Parent for ");
      if Is_Created (Child.all) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Child))
                          & " : new parent : ");
         if Get_Parent (Child) /= null then
            Ada.Text_IO.Put (Type_Name (Get_Type (Get_Parent (Child))));
         else
            Ada.Text_IO.Put ("NULL");
         end if;
      else
         Ada.Text_IO.Put ("NULL ");
      end if;
      Ada.Text_IO.Put ("  old parent : ");
      if Is_Created (Old_Parent.all) then
         Ada.Text_IO.Put (Type_Name (Get_Type (Old_Parent)));
      else
         Ada.Text_IO.Put ("NULL");
      end if;
      Ada.Text_IO.Put_Line (" data = " & Gint'Image (Data));
   end Set_Parent_Signal;

   --------------------
   -- Reparent_Label --
   --------------------

   procedure Reparent_Label (Widget     : access My_Button_Record'Class;
                             New_Parent : in Gtk_Box)
   is
   begin
      Reparent (Widget.Label, New_Parent);
   end Reparent_Label;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1      : Gtk_Box;
      Box2      : Gtk_Box;
      Box3      : Gtk_Box;
      Label     : Gtk_Label;
      Frame2    : Gtk_Frame;
      Myb       : My_Button;

   begin
      Set_Label (Frame, "Reparent");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New_Hbox (Box2, False, 5);
      Set_Border_Width (Box2, 10);
      Pack_Start (Box1, Box2, False, False, 0);

      Gtk_New (Label, "hello world");

      Gtk_New (Frame2, "Frame 1");
      Pack_Start (Box2, Frame2, True, True, 0);
      Gtk_New_Vbox (Box3, False, 5);
      Set_Border_Width (Box3, 5);
      Add (Frame2, Box3);

      Myb := new My_Button_Record;
      Initialize (Myb, "switch");
      Box_Cb.Connect (Myb, "clicked",
                      Box_Cb.To_Marshaller (Reparent_Label'Access),
                      Box3);
      Myb.Label := Label;
      Pack_Start (Box3, Myb, False, True, 0);

      Pack_Start (Box3, Label, False, True, 0);
      Int_Cb.Connect (Label, "parent_set",
                      Int_Cb.To_Marshaller (Set_Parent_Signal'Access),
                      42);
      Show (Label);

      Gtk_New (Frame2, "Frame 2");
      Pack_Start (Box2, Frame2, True, True, 0);
      Gtk_New_Vbox (Box3, False, 5);
      Set_Border_Width (Box3, 5);
      Add (Frame2, Box3);

      Myb := new My_Button_Record;
      Initialize (Myb, "switch");
      Box_Cb.Connect (Myb, "clicked",
                      Box_Cb.To_Marshaller (Reparent_Label'Access),
                      Box3);
      Myb.Label := Label;
      Pack_Start (Box3, Myb, False, True, 0);

      Show_All (Frame);
   end Run;

end Create_Reparent;

