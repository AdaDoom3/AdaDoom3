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

with Gtk.Box;            use Gtk.Box;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Widget;         use Gtk.Widget;
with Common;             use Common;
with Gtk;                use Gtk;

with Ada.Text_IO;

package body Create_File_Selection is

   type Gtk_File_Selection_Access is access all Gtk_File_Selection;
   package Destroy_Cb is new Handlers.User_Callback
     (Gtk_File_Selection_Record, Gtk_File_Selection_Access);
   package Files_Cb is new Handlers.Callback
     (Gtk_File_Selection_Record);

   Window : aliased Gtk_File_Selection;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "@bGtk_File_Selection@B is the basic widget to ask the user"
        & " for a file name. It can also give access to basic file and"
        & " directory manipulation, such as create, rename, delete.";
   end Help;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Win : access Gtk_File_Selection_Record'Class;
                        Ptr : in Gtk_File_Selection_Access) is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroyed;

   --------
   -- Ok --
   --------

   procedure OK (Files : access Gtk_File_Selection_Record'Class) is
   begin
      Ada.Text_IO.Put_Line ("Selected " & Get_Filename (Files));
      Destroy (Files);
   end OK;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Win : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Win);
   end Cancel;

   -----------------
   -- Show_Fileop --
   -----------------

   procedure Show_Fileop (Files : access Gtk_File_Selection_Record'Class) is
   begin
      Show_Fileop_Buttons (Files);
   end Show_Fileop;

   -----------------
   -- Hide_Fileop --
   -----------------

   procedure Hide_Fileop (Files : access Gtk_File_Selection_Record'Class) is
   begin
      Hide_Fileop_Buttons (Files);
   end Hide_Fileop;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Button : Gtk_Button;

   begin
      Set_Label (Frame, "File Selection");

      if Window = null then
         Gtk_New (Window, Title => "File Selection Dialog");
         Hide_Fileop_Buttons (Window);
         Set_Position (Window, Win_Pos_Mouse);
         Destroy_Cb.Connect
           (Window, "destroy",
            Destroy_Cb.To_Marshaller (Destroyed'Access), Window'Access);
         Files_Cb.Object_Connect
           (Get_Ok_Button (Window), "clicked",
            Files_Cb.To_Marshaller (OK'Access),
            Slot_Object => Window);
         Widget_Handler.Object_Connect
           (Get_Cancel_Button (Window), "clicked",
            Widget_Handler.To_Marshaller (Cancel'Access),
            Slot_Object => Window);

         Gtk_New (Button, Label => "Hide Fileops");
         Files_Cb.Object_Connect
           (Button, "clicked",
            Files_Cb.To_Marshaller (Hide_Fileop'Access),
            Slot_Object => Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);

         Gtk_New (Button, Label => "Show Fileops");
         Files_Cb.Object_Connect
           (Button, "clicked",
            Files_Cb.To_Marshaller (Show_Fileop'Access),
            Slot_Object => Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_File_Selection;

