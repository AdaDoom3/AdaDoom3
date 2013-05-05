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

with Ada.Text_IO;          use Ada.Text_IO;
with GNAT.Strings;         use GNAT.Strings;
with Glib.Values;          use Glib.Values;
with Gtk.About_Dialog;     use Gtk.About_Dialog;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Window;           use Gtk.Window;
with Gtkada.Handlers;
with Gtk.Widget;           use Gtk.Widget;

package body Create_About is

   function On_Activate_Link
      (About  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Params : Glib.Values.GValues) return Boolean;
   --  Called when a link is clicked

   ----------------------
   -- On_Activate_Link --
   ----------------------

   function On_Activate_Link
      (About  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Params : Glib.Values.GValues) return Boolean
   is
      URI   : constant String := Get_String (Nth (Params, 1));
      pragma Unreferenced (About);
   begin
      Put_Line ("Url clicked: " & URI);
      return True;
   end On_Activate_Link;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "The @bGtk_About_Dialog@B is used to display information about"
        & " your application, like its name, version, developers, website";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Dialog : Gtk_About_Dialog;
   begin
      Set_Label (Frame, "About dialog");

      Gtk_New (Dialog);
      Set_Transient_For (Dialog, Gtk_Window (Get_Toplevel (Frame)));
      Set_Destroy_With_Parent (Dialog, True);
      Set_Modal (Dialog, True);

      Gtkada.Handlers.Return_Callback.Connect
         (Dialog, Gtk.About_Dialog.Signal_Activate_Link,
          On_Activate_Link'Access);

      --  In real applications, you will need to free the allocate strings
      Set_Artists (Dialog, (1 => new String'("Artist1 <artist1@foo.com>"),
                            2 => new String'("Artist2 <artist2@foo.com>")));
      Set_Authors (Dialog, (1 => new String'("Author1 <author1@foo.com>"),
                            2 => new String'("Author2 <author2@foo.com>")));
      Set_Documenters
        (Dialog, (1 => new String'("Documenter1"),
                   2 => new String'("Documenter2 <doc@foo.com>")));
      Set_Comments  (Dialog, "Comment about the application");
      Set_Copyright (Dialog, "Copyright (c) 2006, AdaCore");
      Set_License
        (Dialog,
         "This library is free software; you can redistribute it and/or"
         & " modify it under the terms of the GNU General Public"
         & " License as published by the Free Software Foundation; either"
         & " version 2 of the License, or (at your option) any later version."
         & ASCII.LF
         & "This library is distributed in the hope that it will be useful,"
         & " but WITHOUT ANY WARRANTY; without even the implied warranty of"
         & " MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU"
         & " General Public License for more details.");
      Set_Wrap_License (Dialog, True);
      Set_Program_Name (Dialog, "Testgtk");
      Set_Version (Dialog, "2.8.17");
      Set_Website (Dialog, "http://www.adacore.com");
      Set_Website_Label (Dialog, "AdaCore");

      --  Just checking that this works correctly
      declare
         Artists : String_List := Get_Artists (Dialog);
      begin
         for A in Artists'Range loop
            Put_Line ("Artist: " & Artists (A).all);
            Free (Artists (A));
         end loop;
      end;

      --  In real application, you might not want to call Run here, just
      --     Present (Dialog)
      --  so that it doesn't block the rest of your application
      if Run (Dialog) /= Gtk_Response_Close then
         --  Dialog was destroyed by user, not closed through Close button
         null;
      end if;
      Destroy (Dialog);
   end Run;

end Create_About;
