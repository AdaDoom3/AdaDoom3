-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2006-2013, AdaCore              --
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

with Ada.Text_IO;      use Ada.Text_IO;
with Glib;             use Glib;
with Glib.Error;       use Glib.Error;
with Gtk.Box;          use Gtk.Box;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Action;       use Gtk.Action;
with Gtk.Action_Group; use Gtk.Action_Group;
with Gtk.Stock;        use Gtk.Stock;
with Gtk.UI_Manager;   use Gtk.UI_Manager;
with Gtk.Window;       use Gtk.Window;
with System;           use System;

package body Create_UI_Manager is

   procedure Activate_Action (Action, User_Data : System.Address);
   pragma Convention (C, Activate_Action);
   --  Called when one of the actions has been selected

   procedure Activate_Radio_Action
     (Group, Current : access Gtk_Action_Record'Class;
      User_Data      : System.Address);
   --  Called when a radio action is selected

   --  First define all the actions that our application can perform. This is
   --  unrelated to the layout of menus

   Entries : constant Action_Entry_Array :=
     (1 => Create (Name => "FileMenu",        Label => "_File"),
      2 => Create (Name => "PreferencesMenu", Label => "_Preferences"),
      3 => Create (Name => "ColorMenu",       Label => "_Color"),
      4 => Create (Name => "ShapeMenu",       Label => "_Shape"),
      5 => Create (Name => "HelpMenu",        Label => "_Help"),
      6 => Create (Name => "New",
                   Stock_Id    => Stock_New,
                   Label       => "_New",
                   Accelerator => "<control>N",
                   Tooltip     => "Create a new file",
                   Callback    => Activate_Action'Access),
      7 => Create (Name => "Open",
                   Stock_Id    => Stock_Open,
                   Label       => "_Open",
                   Accelerator => "<control>O",
                   Tooltip     => "Open a file",
                   Callback    => Activate_Action'Access),
      8 => Create (Name => "Save",
                   Stock_Id    => Stock_Save,
                   Label       => "_Save",
                   Accelerator => "<control>S",
                   Tooltip     => "Save current file",
                   Callback    => Activate_Action'Access),
      9 => Create (Name => "SaveAs",
                   Stock_Id    => Stock_Save,
                   Label       => "Save _As...",
                   Tooltip     => "Save to a file",
                   Callback    => Activate_Action'Access),
      10 => Create (Name => "Quit",
                    Stock_Id    => Stock_Quit,
                    Label       => "_Quit",
                    Accelerator => "<control>Q",
                    Tooltip     => "Quit",
                    Callback    => Activate_Action'Access),
      11 => Create (Name => "About",
                    Label       => "_About",
                    Accelerator => "<control>A",
                    Tooltip     => "About",
                    Callback    => Activate_Action'Access),
      12 => Create (Name => "Logo",
                    Stock_Id    => "demo-gtk-logo",
                    Tooltip     => "GTK+",
                    Callback    => Activate_Action'Access));

   Toggle_Entries : constant Toggle_Action_Entry_Array :=
     (1 => Create (Name        => "Bold",
                   Stock_Id    => Stock_Bold,
                   Label       => "_Bold",
                   Accelerator => "<control>B",
                   Tooltip     => "Bold",
                   Callback    => Activate_Action'Access,
                   Is_Active   => True));

   subtype Color is Gint;
   Color_Red   : constant Color := 1;
   Color_Green : constant Color := 2;
   Color_Blue  : constant Color := 3;

   Color_Entries : constant Radio_Action_Entry_Array :=
     (1 => Create (Name        => "Red",
                   Label       => "_Red",
                   Accelerator => "<control>R",
                   Tooltip     => "Blood",
                   Value       => Color_Red),
      2 => Create (Name        => "Green",
                   Label       => "_Green",
                   Accelerator => "<control>G",
                   Tooltip     => "Grass",
                   Value       => Color_Green),
      3 => Create (Name        => "Blue",
                   Label       => "_Blue",
                   Accelerator => "<control>B",
                   Tooltip     => "Sky",
                   Value       => Color_Blue));

   subtype Shape is Gint;
   Shape_Square : constant Shape := 1;
   Shape_Oval   : constant Shape := 2;

   Shape_Entries : constant Radio_Action_Entry_Array :=
     (1 => Create (Name        => "Square",
                   Label       => "_Square",
                   Accelerator => "<control>S",
                   Tooltip     => "Square",
                   Value       => Shape_Square),
      2 => Create (Name        => "Oval",
                   Label       => "_Oval",
                   Accelerator => "<control>O",
                   Tooltip     => "Egg",
                   Value       => Shape_Oval));

   UI_Info : constant String :=
     "<ui>"
     & "  <menubar name='MenuBar'>"
     & "    <menu action='FileMenu'>"
     & "      <menuitem action='New'/>"
     & "      <menuitem action='Open'/>"
     & "      <menuitem action='Save'/>"
     & "      <menuitem action='SaveAs'/>"
     & "      <separator/>"
     & "      <menuitem action='Quit'/>"
     & "    </menu>"
     & "    <menu action='PreferencesMenu'>"
     & "      <menu action='ColorMenu'>"
     & "       <menuitem action='Red'/>"
     & "       <menuitem action='Green'/>"
     & "       <menuitem action='Blue'/>"
     & "      </menu>"
     & "      <menu action='ShapeMenu'>"
     & "        <menuitem action='Square'/>"
     & "        <menuitem action='Oval'/>"
     & "      </menu>"
     & "      <menuitem action='Bold'/>"
     & "    </menu>"
     & "    <menu action='HelpMenu'>"
     & "      <menuitem action='About'/>"
     & "    </menu>"
     & "  </menubar>"
     & "  <toolbar  name='ToolBar'>"
     & "    <toolitem action='Open'/>"
     & "    <toolitem action='Quit'/>"
     & "    <separator action='Sep1'/>"
     & "    <toolitem action='Logo'/>"
     & "  </toolbar>"
     & "</ui>";

   ---------------------------
   -- Activate_Radio_Action --
   ---------------------------

   procedure Activate_Radio_Action
     (Group, Current : access Gtk_Action_Record'Class;
      User_Data      : System.Address)
   is
      pragma Unreferenced (User_Data, Group);
   begin
      Put_Line ("Radio action " & Get_Name (Current) & " selected");
   end Activate_Radio_Action;

   ---------------------
   -- Activate_Action --
   ---------------------

   procedure Activate_Action (Action, User_Data : System.Address) is
      Act : constant Gtk_Action := Convert (Action);
      pragma Unreferenced (User_Data);
   begin
      Put_Line ("Action " & Get_Name (Act) & " was activated");
   end Activate_Action;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bGtk_UI_Manager@B widget is a convenient facility to"
        & " create menu bars, toolbars and other high level widgets through"
        & " a set of simple API calls, or through an XML description";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      UI      : Gtk_UI_Manager;
      Actions : Gtk_Action_Group;
      Box     : Gtk_Box;
      Error   : aliased GError := null;
   begin
      Set_Label (Frame, "UI Manager");

      Gtk_New (Actions, "Actions");
      Add_Actions        (Actions, Entries);
      Add_Toggle_Actions (Actions, Toggle_Entries);
      Add_Radio_Actions  (Actions, Color_Entries, Color_Red,
                          Activate_Radio_Action'Access);
      Add_Radio_Actions  (Actions, Shape_Entries, Shape_Oval,
                          Activate_Radio_Action'Access);

      Gtk_New (UI);
      Insert_Action_Group (UI, Actions, 0);
      Add_Accel_Group
        (Gtk_Window (Get_Toplevel (Frame)), Get_Accel_Group (UI));

      if Add_UI_From_String (UI, UI_Info, Error'Unchecked_Access) = 0 then
         Put_Line ("Building menus failed: " & Get_Message (Error));
         Error_Free (Error);
      end if;

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Pack_Start (Box, Get_Widget (UI, "/MenuBar"), Expand => False);

--
--
--
--        label = gtk_label_new ("Type\n<alt>\nto start");
--        gtk_widget_set_size_request (label, 200, 200);
--        gtk_misc_set_alignment (GTK_MISC (label), 0.5, 0.5);
--        gtk_box_pack_start (GTK_BOX (box1), label, TRUE, TRUE, 0);
--
--
--        separator = gtk_hseparator_new ();
--        gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
--
--
--        box2 = gtk_vbox_new (FALSE, 10);
--        gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
--        gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
--
--        button = gtk_button_new_with_label ("close");
--        g_signal_connect_swapped (button, "clicked",
--                                  G_CALLBACK (gtk_widget_destroy), window);
--        gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
--        GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
--        gtk_widget_grab_default (button);
--
--        gtk_widget_show_all (window);

      Show_All (Frame);

   end Run;

end Create_UI_Manager;
