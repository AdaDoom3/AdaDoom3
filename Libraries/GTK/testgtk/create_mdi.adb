-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2003                            --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                     Copyright (C) 2004-2013, AdaCore              --
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

with Gdk.Bitmap;         use Gdk.Bitmap;
with Gdk.Color;          use Gdk.Color;
with Gdk.Pixmap;         use Gdk.Pixmap;
with Glib.Xml_Int;       use Glib.Xml_Int;
with Gtk;                use Gtk;
with Gtk.Accel_Group;    use Gtk.Accel_Group;
with Gtk.Box;            use Gtk.Box;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Label;          use Gtk.Label;
with Gtk.Menu;           use Gtk.Menu;
with Gtk.Menu_Tool_Button; use Gtk.Menu_Tool_Button;
with Gtk.Widget;         use Gtk.Widget;
with Gtkada.MDI;         use Gtkada.MDI;
with Gtk.Toolbar;        use Gtk.Toolbar;
with Gtkada.Handlers;    use Gtkada.Handlers;
with Gtk.Toggle_Tool_Button;  use Gtk.Toggle_Tool_Button;
with Gtk.Tool_Button;    use Gtk.Tool_Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Window;         use Gtk.Window;
with Gtk.Image;          use Gtk.Image;

package body Create_MDI is

   package Desktops is new Gtkada.MDI.Desktop (Integer);

   function Create_Child (Index : Natural) return MDI_Child;
   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class);
   procedure On_Snapshot  (Button : access Gtk_Widget_Record'Class);
   procedure Do_Configure (MDI : access MDI_Window_Record'Class);

   procedure On_Save_Desktop (Button : access Gtk_Widget_Record'Class);
   procedure Load_Desktop;
   --  Load the desktop (and all known perspectives) from an external
   --  XML file. Or create that file from the current desktop.

   function Load_From_Desktop
      (MDI  : MDI_Window;
       Node : Glib.Xml_Int.Node_Ptr;
       User : Integer) return MDI_Child;
   --  This function recreates a MDI_Child from its XML description as
   --  given in a desktop.xml file (see the documentation for package
   --  Desktop).

   function Save_To_Desktop
      (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       User   : Integer) return Glib.Xml_Int.Node_Ptr;
   --  This is the opposite of Load_From_Desktop, and saves an existing
   --  window into an XML node (with all information needed so that
   --  Load_From_Desktop can recreate it later).

   procedure Setup
      (Frame : access Gtk_Frame_Record'Class; Independent : Boolean);
   --  Create the demo, either in "Independent Perspectives" mode or not.

   type My_Window_Record is new Gtk_Box_Record with record
      Index : Natural;
   end record;
   type My_Window is access all My_Window_Record'Class;
   --  The type of windows this example is using. In practive, you would
   --  likely put trees, editors, and other kinds of complex windows.

   MDI    : MDI_Window;
   Opaque : Boolean := False;

   My_Window_Name : constant String := "my_window_";

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A Gtkada specific widget." & ASCII.LF
        & "This is based on the GtkAda multi panned widget. You should"
        & " try the corresponding demo to find out about other capabilities"
        & " like splitting windows or using fixed sizes." & ASCII.LF
        & "In the MDI, windows can be dragged around to be reorganized."
        & " The MDI also supports the notion of perspectives: when you"
        & " select another perspective, some of the windows (the ones in the"
        & " central area) will be preserved. All other windows will only"
        & " remain visible if they also are in the other perspective."
        & " This example loads the perspectives from an XML file, and you"
        & " can then switch between perspectives by using the menu"
        & " <b>Perspectives</b>. By default, the MDI includes a central area"
        & " whose content is preserved when you switch perspectives. For"
        & " instance, this could be used in an IDE so that the editors are"
        & " the same in the development and debugging perspectives. However,"
        & " it can sometimes be confusing to users, so you can force"
        & " the perspectives to be <b>independent</b>. In such a case, no"
        & " window is preserved when switching perspectives."
        & ASCII.LF
        & "The MDI also provides the notion of desktop (the current layout"
        & " of your windows in all the perspectives). Such a desktop can be"
        & " saved to the disk, and restored when the application is"
        & " restarted (not shown in this demo). This is actually how"
        & " the default perspectives themselves can be defined initially"
        & " when you package your application."
        & ASCII.LF
        & "Windows can be @bfloated@B, ie put outside of the MDI, by"
        & " the user. This provides a convenient use for many users who"
        & " prefer having multiple windows. It is also a convenient way"
        & " to use multiple screens. You can either float a window"
        & " programmatically (and provide a menu to the user to do so),"
        & " or the user can drag a window outside of the MDI to float it"
        & ASCII.LF
        & "A contextual menu exists in the notebook tabs to close windows,"
        & " or change the location of tabs.";
   end Help;

   ------------------
   -- Do_Configure --
   ------------------

   procedure Do_Configure (MDI : access MDI_Window_Record'Class) is
      Bg_Color, Title_Color, Focus_Color : Gdk_Color;
   begin
      Bg_Color := Parse ("#8A8A8A");
      Alloc (Get_Colormap (MDI), Bg_Color);

      Title_Color := Parse ("#7D7D7D");
      Alloc (Get_Colormap (MDI), Title_Color);

      Focus_Color := Parse ("#5894FA");
      Alloc (Get_Colormap (MDI), Focus_Color);

      Configure (MDI,
                 Background_Color  => Bg_Color,
                 Title_Bar_Color   => Title_Color,
                 Focus_Title_Color => Focus_Color,
                 Opaque_Resize     => Opaque,
                 Show_Tabs_Policy  => Automatic,
                 Tabs_Position     => Gtk.Enums.Pos_Bottom,
                 Draw_Title_Bars   => Always);
   end Do_Configure;

   ---------------
   -- On_Opaque --
   ---------------

   procedure On_Opaque  (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Opaque := not Opaque;
      Do_Configure (MDI);
   end On_Opaque;

   -----------------
   -- On_Snapshot --
   -----------------

   Iterator : Child_Iterator;

   procedure On_Snapshot (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
      Child : MDI_Child;
      Pixmap : Gdk_Pixmap;
      Window : Gtk_Window;
      Image  : Gtk_Image;
   begin
      Child := Get (Iterator);

      if Child = null then
         Iterator := First_Child (MDI);
         Child := Get (Iterator);
      else
         Next (Iterator);
      end if;

      Pixmap := Get_Snapshot (Child, null);

      Gtk_New (Window);
      Set_Default_Size (Window, 800, 600);

      Gtk_New (Image, Pixmap, Null_Bitmap);
      Add (Window, Image);
      Show_All (Window);
   end On_Snapshot;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);

      Perspectives, Central : Node_Ptr;
   begin
      Desktops.Save_Desktop (MDI, 0, Perspectives, Central);

      --  These could also be saved in the same XML file. We keep this
      --  example simpler by avoiding the extra XML manipulation this requires

      if Independent_Perspectives (MDI) then
         Print (Perspectives, "perspectives_indep.xml");
      else
         Print (Perspectives, "perspectives.xml");
         Print (Central, "central.xml");
      end if;
   end On_Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   procedure Load_Desktop is
      Perspectives, Central : Node_Ptr;
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if Independent_Perspectives (MDI) then
         Perspectives := Parse ("perspectives_indep.xml");
      else
         Perspectives := Parse ("perspectives.xml");
         Central      := Parse ("central.xml");
      end if;

      Success := Desktops.Restore_Desktop (MDI, Perspectives, Central, 0);
   end Load_Desktop;

   ------------------
   -- Create_Child --
   ------------------

   function Create_Child (Index : Natural) return MDI_Child is
      Child : MDI_Child;
      Box   : My_Window;
      Label : Gtk_Label;
   begin
      Box := new My_Window_Record;
      Box.Index := Index;
      Initialize_Vbox (Box);

      Gtk_New (Child, Box, Flags => All_Buttons, Group => Group_Default);

      Gtk_New (Label, "This is the" & Integer'Image (Index) & " window");
      Pack_Start (Box, Label);

      Set_Title (Child, "Window" & Integer'Image (Index));

      Show_All (Child);
      return Child;
   end Create_Child;

   ---------------------
   -- Save_To_Desktop --
   ---------------------

   function Save_To_Desktop
      (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
       User   : Integer) return Glib.Xml_Int.Node_Ptr
   is
      pragma Unreferenced (User);
      Box : constant My_Window := My_Window (Widget);
      Index : constant String := Box.Index'Img;
      N   : Node_Ptr;
   begin
      --  We store the index directly in the XML tag, so that all windows
      --  appear to be different.
      N := new Node;
      N.Tag := new String'
         (My_Window_Name & Index (Index'First + 1 .. Index'Last));
      return N;
   end Save_To_Desktop;

   -----------------------
   -- Load_From_Desktop --
   -----------------------

   function Load_From_Desktop
      (MDI  : MDI_Window;
       Node : Glib.Xml_Int.Node_Ptr;
       User : Integer) return MDI_Child
   is
      pragma Unreferenced (User);
      Child : MDI_Child;
      Index : constant Integer := Integer'Value
        (Node.Tag (Node.Tag'First + My_Window_Name'Length .. Node.Tag'Last));
   begin
      Child := Create_Child (Index);
      Put (MDI, Child, Position_Automatic);
      return Child;
   end Load_From_Desktop;

   -----------
   -- Setup --
   -----------

   procedure Setup
      (Frame : access Gtk_Frame_Record'Class; Independent : Boolean)
   is
      Bar    : Gtk_Toolbar;
      Box    : Gtk_Box;
      Toggle : Gtk_Toggle_Tool_Button;
      Button : Gtk_Tool_Button;
      Group  : Gtk_Accel_Group;

      Menu   : Gtk_Menu;
      Menu_Button : Gtk_Menu_Tool_Button;

      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New (Bar);
      Pack_Start (Box, Bar, Expand => False);

      Gtk_New (Group);
      Gtk_New (MDI, Group => Group, Independent_Perspectives => Independent);
      Do_Configure (MDI);
      Pack_End (Box, MDI, Expand => True);

      Menu := Desktops.Create_Menu (MDI, User => 1);  --  User irrelevant here

      Gtk_New (Toggle);
      Set_Label (Toggle, "Opaque Resizing");
      Insert (Bar, Toggle);
      Widget_Callback.Connect (Toggle, "toggled", On_Opaque'Access);

      Gtk_New (Button, Label => "Save Desktop");
      Insert (Bar, Button);
      Widget_Callback.Connect (Button, "clicked", On_Save_Desktop'Access);

      Gtk_New (Menu_Button, Label => "Menu");
      Set_Menu (Menu_Button, Menu);
      Insert (Bar, Menu_Button);

      Gtk_New (Button, Label => "Snapshot");
      Insert (Bar, Button);
      Widget_Callback.Connect (Button, "clicked", On_Snapshot'Access);
      --  Pressing "snapshot will cycle through the MDI children and take
      --  a pixmap of the MDI children.

      --  Load the desktop from external XML files

      Desktops.Register_Desktop_Functions
         (Save_To_Desktop'Access, Load_From_Desktop'Access);

      Load_Desktop;
      --  Put (MDI, Create_Child (1), Position_Automatic);
      --  Put (MDI, Create_Child (2), Position_Automatic);
      --  Put (MDI, Create_Child (3), Position_Automatic);

      Show_All (Frame);
   end Setup;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Setup (Frame, Independent => False);
   end Run;

   ---------------------
   -- Run_Independent --
   ---------------------

   procedure Run_Independent
      (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
   begin
      Setup (Frame, Independent => True);
   end Run_Independent;
end Create_MDI;
