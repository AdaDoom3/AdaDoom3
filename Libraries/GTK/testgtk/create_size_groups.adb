-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2000-2006 AdaCore               --
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

with Glib;              use Glib;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Label;         use Gtk.Label;
with Gtk.Size_Group;    use Gtk.Size_Group;
with Gtk.Table;         use Gtk.Table;

package body Create_Size_Groups is

   procedure Add_Row
     (Table : access Gtk_Table_Record'Class;
      Row   : Guint;
      Group : Gtk_Size_Group;
      Text  : String);
   --  Add a new row in Table, with a label Text .
   --  The option menu in that row is added to the group Group.

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Group        : Gtk_Size_Group);
   --  Toggle whether the size group is active.

   package Toggle_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Check_Button_Record, Gtk_Size_Group);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "@bGtk_Size_Group@B provides a mechanism for grouping a number of"
        & " widgets together so they all request the same amount of space."
        & " This is typically useful when you want a column of widgets to have"
        & " the same size, but you can't use a @bGtk_Table@B widget."
        & ASCII.LF
        & "Note that size groups only affect the amount of space requested,"
        & " not the size that the widgets finally receive. If you want the"
        & " widgets in a @bGtk_Size_Group@B to actually be the same size,"
        & " you need to pack them in such a way that they get the size they"
        & " request and not more. For example, if you are packing your"
        & " widgets into a table, you would not include the FILL flag.";
   end Help;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Table : access Gtk_Table_Record'Class;
      Row   : Guint;
      Group : Gtk_Size_Group;
      Text  : String)
   is
      Label  : Gtk_Label;
      Button : Gtk_Button;
   begin
      Gtk_New (Label, Text);
      Set_Alignment (Label, 0.0, 1.0);
      Attach
        (Table         => Table,
         Child         => Label,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => Row,
         Bottom_Attach => Row + 1,
         Xoptions      => Expand or Fill,
         Yoptions      => 0);

      Gtk_New (Button, Text);
      Gtk.Size_Group.Add_Widget (Group, Button);
      Attach
        (Table         => Table,
         Child         => Button,
         Left_Attach   => 1,
         Right_Attach  => 2,
         Top_Attach    => Row,
         Bottom_Attach => Row + 1,
         Xoptions      => 0,  --  !!! Do not force any size, see Help
         Yoptions      => 0);
   end Add_Row;

   ---------------------
   -- Toggle_Grouping --
   ---------------------

   procedure Toggle_Grouping
     (Check_Button : access Gtk_Check_Button_Record'Class;
      Group        : Gtk_Size_Group) is
   begin
      --  Note: we use both the properties and the directy function call only
      --  to demonstrate the two technics. No other technical reason.

      if Get_Active (Check_Button) then
         Set_Property (Group, Mode_Property, Horizontal);
      else
         Gtk.Size_Group.Set_Mode (Group, None);
      end if;
   end Toggle_Grouping;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Vbox   : Gtk_Box;
      Group  : Gtk_Size_Group;
      Table  : Gtk_Table;
      F      : Gtk_Frame;
      Toggle : Gtk_Check_Button;

   begin
      Gtk_New_Vbox (Vbox, Homogeneous => False);
      Add (Frame, Vbox);
      Set_Label (Frame, "Size group");

      Gtk_New (Group, Horizontal);

      Gtk_New (F, "Options1");
      Pack_Start (Vbox, F, Expand => False, Fill => False);

      Gtk_New (Table, 2, 2, False);
      Add (F, Table);
      Set_Border_Width (Table, 5);
      Set_Row_Spacings (Table, 5);
      Set_Col_Spacings (Table, 10);

      Add_Row (Table, 0, Group, "foofoofoofoofoofoofoofoofoo");
      Add_Row (Table, 1, Group, "foofoofoo");


      Gtk_New (F, "Options2");
      Pack_Start (Vbox, F, Expand => False, Fill => False);

      Gtk_New (Table, 2, 2, False);
      Add (F, Table);
      Set_Border_Width (Table, 5);
      Set_Row_Spacings (Table, 5);
      Set_Col_Spacings (Table, 10);

      Add_Row (Table, 0, Group, "foo");
      Add_Row (Table, 1, Group, "foofoofoofoofoofoo");


      Gtk_New (Toggle, "Enable grouping");
      Pack_Start (Vbox, Toggle, Expand => False, Fill => False);
      Set_Active (Toggle, True);
      Toggle_Cb.Connect
        (Toggle, "toggled",
         Toggle_Cb.To_Marshaller (Toggle_Grouping'Access), Group);

      Show_All (Frame);
   end Run;

end Create_Size_Groups;
