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

with Ada.Text_IO;               use Ada.Text_IO;
with Glib.Error;                use Glib.Error;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.File_Chooser;          use Gtk.File_Chooser;
with Gtk.File_Chooser_Button;   use Gtk.File_Chooser_Button;
with Gtk.File_Filter;           use Gtk.File_Filter;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Stock;                 use Gtk.Stock;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.Properties;         use Gtkada.Properties;
with Gtk.Widget;                use Gtk.Widget;

package body Create_File_Chooser is

   procedure Show_Properties (Widget : access Gtk_Widget_Record'Class);
   --  Opens a properties editor for Widget

   -----------------
   -- Help_Button --
   -----------------

   function Help_Button return String is
   begin
      return "The Gtk_File_Chooser_Button is a widget that lets the user"
        & " select a file." & ASCII.LF
        & "It can exist in several modes, which influence its behavior.";
   end Help_Button;

   ---------------------
   -- Show_Properties --
   ---------------------

   procedure Show_Properties (Widget : access Gtk_Widget_Record'Class) is
   begin
      Popup_Properties_Editor (Widget);
   end Show_Properties;

   ----------------
   -- Run_Button --
   ----------------

   procedure Run_Button (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Hbox   : Gtk_Box;
      Button : Gtk_Button;
      File   : Gtk_File_Chooser_Button;
      Error  : GError;
      Filter1, Filter2 : Gtk_File_Filter;
   begin
      Set_Label (Frame, "File Chooser Button");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      --  File chooser

      Gtk_New (Filter1);
      Add_Pattern (Filter1, "*");
      Set_Name (Filter1, "All Files");

      Gtk_New (Filter2);
      Add_Pattern (Filter2, "*.ad[bs]");
      Set_Name (Filter2, "Ada Files");

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Fill => False, Expand => False);
      Gtk_New (File,
               Title   => "Select a file (Open mode)",
               Action  => Action_Open);
      Pack_Start (Hbox, File, Expand => True);

      Add_Filter (+File, Filter1);
      Add_Filter (+File, Filter2);

      Gtk_New_From_Stock (Button, Stock_Properties);
      Pack_Start (Hbox, Button, Expand => False);
      Widget_Callback.Object_Connect
        (Button, "clicked", Show_Properties'Access, File);

      --  Add a shortcut to the current directory

      Error := Add_Shortcut_Folder (+File, Get_Current_Dir);
      if Error /= null then
         Put_Line (Get_Message (Error));
      end if;

      --  Directory chooser

      Gtk_New (Filter1);
      Add_Mime_Type (Filter1, "x-directory/normal");
      Set_Name (Filter1, "Directories only");

      Gtk_New_Hbox (Hbox, Homogeneous => False);
      Pack_Start (Box, Hbox, Fill => False, Expand => False);
      Gtk_New (File,
               Title   => "Select a file (Open mode)",
               Action  => Action_Open);
      Pack_Start (Hbox, File, Expand => True);

      Set_Action (+File, Action_Select_Folder);
      Add_Filter (+File, Filter1);

      Show_All (Frame);
   end Run_Button;

end Create_File_Chooser;
