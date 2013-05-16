-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--      Copyright (C) 2000 E. Briot, J. Brobecker and A. Charlet     --
--               Copyright (C) 2001-2003 ACT-Europe                  --
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

with Gtk.Main;           use Gtk.Main;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtkada.Handlers;    use Gtkada.Handlers;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Gtk.GEntry;         use Gtk.GEntry;

package body Gtkada.File_Selection is

   type Gtkada_File_Selection_Record is new Gtk_File_Selection_Record with
   record
      File_Selected : Boolean := False;
   end record;
   type Gtkada_File_Selection is access all Gtkada_File_Selection_Record'Class;

   function Delete_Cb (Win : access Gtk_Widget_Record'Class) return Boolean;
   procedure Clicked_Ok_Cb (Button : access Gtk_Widget_Record'Class);
   procedure Clicked_Cancel_Cb (Button : access Gtk_Widget_Record'Class);

   ---------------
   -- Delete_Cb --
   ---------------

   function Delete_Cb (Win : access Gtk_Widget_Record'Class) return Boolean is
      pragma Unreferenced (Win);
   begin
      Main_Quit;
      return True;
   end Delete_Cb;

   -------------------
   -- Clicked_Ok_Cb --
   -------------------

   procedure Clicked_Ok_Cb (Button : access Gtk_Widget_Record'Class) is
   begin
      Gtkada_File_Selection (Get_Toplevel (Button)).File_Selected := True;
      Main_Quit;
   end Clicked_Ok_Cb;

   -----------------------
   -- Clicked_Cancel_Cb --
   -----------------------

   procedure Clicked_Cancel_Cb (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
   begin
      Main_Quit;
   end Clicked_Cancel_Cb;

   --------------------
   -- Message_Dialog --
   --------------------

   function File_Selection_Dialog
     (Title       : Glib.UTF8_String := "Select File";
      Default_Dir : String := "";
      Dir_Only    : Boolean := False;
      Must_Exist  : Boolean := False) return String
   is
      Dialog : Gtkada_File_Selection;
      Button : Gtk_Button;

   begin
      Dialog := new Gtkada_File_Selection_Record;
      Initialize (Dialog, Title);

      if Default_Dir /= "" then
         Set_Filename (Dialog, Default_Dir);
      end if;

      Set_Modal (Dialog);
      Set_Show_File_Op_Buttons (Dialog, False);
      Set_Position (Dialog, Win_Pos_Mouse);
      Return_Callback.Connect
        (Dialog, "delete_event",
         Return_Callback.To_Marshaller (Delete_Cb'Access));
      Button := Get_Ok_Button (Dialog);
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Ok_Cb'Access));
      Button := Get_Cancel_Button (Dialog);
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Cancel_Cb'Access));
      Show_All (Dialog);

      if Dir_Only then
         Hide_All (Get_Parent (Get_File_List (Dialog)));
      end if;

      Main;

      loop
         if Dialog.File_Selected then
            declare
               S    : constant String := Get_Filename (Dialog);
               Last : Natural := S'Last;

            begin
               while Last > S'First
                 and then S (Last) = Directory_Separator
               loop
                  Last := Last - 1;
               end loop;

               if S = ""
                 or else not Must_Exist
                 or else
                   (Dir_Only and then Is_Directory (S (S'First .. Last)))
                 or else Is_Regular_File (S (S'First .. Last))
               then
                  Destroy (Dialog);
                  return S;
               elsif Is_Directory (S (S'First .. Last)) then
                  --  The user might have entered a name in the text field
                  --  and pressed enter, so we simply change the current
                  --  directory, and try again
                  Set_Filename (Dialog, S & Directory_Separator);
                  Set_Text (Gtk_Entry (Get_Selection_Entry (Dialog)), "");
                  Dialog.File_Selected := False;
                  Main;
               else
                  Destroy (Dialog);
                  return "";
               end if;
            end;
         else
            Destroy (Dialog);
            return "";
         end if;
      end loop;
   end File_Selection_Dialog;

end Gtkada.File_Selection;
