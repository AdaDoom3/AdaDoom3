-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2003 ACT Europe                 --
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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtk.Menu_Bar;
with Ada.Text_IO; use Ada.Text_IO;

package body Create_Item_Factory is

   package Factory_Data is new Data_Item (Integer);
   use Factory_Data;

   procedure Ifactory_Cb
     (Callback_Data   : Data_Type_Access;
      Callback_Action : Guint;
      Widget          : Limited_Widget);

   Menu_Items : constant Gtk_Item_Factory_Entry_Array :=
     (Gtk_New ("/_File", Item_Type => Branch),
      Gtk_New ("/File/tearoff1", "", Ifactory_Cb'Access, Tearoff),
      Gtk_New ("/File/_New", "<control>N", Ifactory_Cb'Access),
      Gtk_New ("/File/_Open", "<control>O", Ifactory_Cb'Access),
      Gtk_New ("/File/_Save", "<control>S", Ifactory_Cb'Access),
      Gtk_New ("/File/Save _As...", Callback => Ifactory_Cb'Access),
      Gtk_New ("/File/sep1", "", Ifactory_Cb'Access, Separator),
      Gtk_New ("/File/_Quit", "<control>Q", Ifactory_Cb'Access),
      Gtk_New ("/_Preferences", Item_Type => Branch),
      Gtk_New ("/_Preferences/_Color", Item_Type => Branch),
      Gtk_New ("/_Preferences/_Color/_Red", "",
               Ifactory_Cb'Access, Radio_Item),
      Gtk_New ("/_Preferences/_Color/_Green", "", Ifactory_Cb'Access,
               "/Preferences/Color/Red"),
      Gtk_New ("/_Preferences/_Color/_Blue", "", Ifactory_Cb'Access,
               "/Preferences/Color/Red"),
      Gtk_New ("/_Preferences/_Shape", Item_Type => Branch),
      Gtk_New ("/_Preferences/Shape/_Square", "", Ifactory_Cb'Access,
               Radio_Item),
      Gtk_New ("/_Preferences/Shape/_Rectangle", "", Ifactory_Cb'Access,
               "/Preferences/Shape/Square"),
      Gtk_New ("/_Preferences/Shape/_Oval", "", Ifactory_Cb'Access,
               "/Preferences/Shape/Rectangle"),
      Gtk_New ("/_Help", Item_Type => Last_Branch),
      Gtk_New ("/Help/_About", "", Ifactory_Cb'Access));

   procedure Ifactory_Cb
     (Callback_Data   : Data_Type_Access;
      Callback_Action : Guint;
      Widget          : Limited_Widget)
   is
      pragma Warnings (Off, Callback_Data);
      pragma Warnings (Off, Callback_Action);
   begin
      Put_Line ("ItemFactory: activated " &
        Path_From_Widget (To_Widget (Widget)));
   end Ifactory_Cb;

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1         : Gtk_Box;
      Accel_Group  : Gtk_Accel_Group;
      Item_Factory : Gtk_Item_Factory;

   begin
      --  gtk_signal_connect (GTK_OBJECT (window), "destroy",
      --                      GTK_SIGNAL_FUNC(gtk_widget_destroyed),
      --                      &window);
      --  gtk_signal_connect (GTK_OBJECT (window), "delete-event",
      --                      GTK_SIGNAL_FUNC (gtk_true),
      --                      NULL);

      Gtk_New (Accel_Group);
      Gtk_New (Item_Factory, Gtk.Menu_Bar.Get_Type, "<main>", Accel_Group);
      --  gtk_object_set_data_full (GTK_OBJECT (window),
      --                            "<main>",
      --                            item_factory,
      --                            (GtkDestroyNotify) gtk_object_unref);
      Create_Items (Item_Factory, Menu_Items, null);

      --  preselect /Preferences/Shape/Oval over the other radios
      Set_Active (Gtk_Check_Menu_Item (Get_Item
        (Item_Factory, "/Preferences/Shape/Oval")), True);

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Pack_Start
        (Box1, Get_Widget (Item_Factory, "<main>"), False, False, 0);

      Show_All (Frame);
   end Run;

   function Help return String is
   begin
      return "No help available";
   end Help;

end Create_Item_Factory;
