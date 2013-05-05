-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                Copyright (C) 2000-2013, AdaCore                   --
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

with Gdk;                         use Gdk;
with Gdk.Color;                   use Gdk.Color;
with Gtk;                         use Gtk;
with Glib;                        use Glib;
with Glib.Properties;
with Gtk.Color_Selection;         use Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;  use Gtk.Color_Selection_Dialog;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Settings;                use Gtk.Settings;
with Common;                      use Common;
with Ada.Text_IO;                 use Ada.Text_IO;
with Gtk.Widget;                  use Gtk.Widget;

package body Create_Color_Selection is

   type Gtk_Color_Dialog_Access is access all Gtk_Color_Selection_Dialog;
   package Destroy_Dialog_Cb is new Handlers.User_Callback
     (Gtk_Color_Selection_Dialog_Record, Gtk_Color_Dialog_Access);

   Dialog : aliased Gtk_Color_Selection_Dialog;

   package Color_Sel_Cb is new Handlers.Callback
     (Gtk_Color_Selection_Dialog_Record);

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Selection_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access);
   --  Called when the dialog is destroyed

   procedure On_Palette_Changed
     (Screen : Gdk.Gdk_Screen; Colors : Gdk.Color.Gdk_Color_Array);
   --  Called when the palette is changed

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This widget provides an easy way to select new colors."
        & " This is a very specific widget, and most applications won't"
        & " need it. There are two versions, one with a dialog, and one"
        & " without.";
   end Help;

   --------------------
   -- Destroy_Dialog --
   --------------------

   procedure Destroy_Dialog
     (Win : access Gtk_Color_Selection_Dialog_Record'Class;
      Ptr : Gtk_Color_Dialog_Access)
   is
      pragma Warnings (Off, Win);
   begin
      Ptr.all := null;
   end Destroy_Dialog;

   ------------------
   -- Close_Window --
   ------------------

   procedure Close_Window (Win : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Destroy (Win);
   end Close_Window;

   ------------------------
   -- On_Palette_Changed --
   ------------------------

   procedure On_Palette_Changed
     (Screen : Gdk.Gdk_Screen; Colors : Gdk.Color.Gdk_Color_Array)
   is
      pragma Unreferenced (Screen);
      Str : constant String := Palette_To_String (Colors);
   begin
      Put_Line ("Palette has changed: ");
      Put_Line ("Palette=" & Str);

      Glib.Properties.Set_Property
        (Get_Default, Gtk_Color_Palette,
         Palette_To_String (Colors));
   end On_Palette_Changed;

   --------------
   -- Color_Ok --
   --------------

   procedure Color_Ok
     (Dialog : access Gtk_Color_Selection_Dialog_Record'Class)
   is
      Color : Gdk_Color;
   begin
      Get_Current_Color (Get_Colorsel (Dialog), Color);

      Put_Line ("Selected color is: ");
      Put ("Red=" & Guint16'Image (Red (Color)));
      Put (" Green=" & Guint16'Image (Green (Color)));
      Put (" Blue=" & Guint16'Image (Blue (Color)));
      Put_Line (" Alpha="
           & Guint16'Image (Get_Current_Alpha (Get_Colorsel (Dialog))));
   end Color_Ok;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      pragma Warnings (Off, Frame);
      Old : Gtk_Color_Selection_Change_Palette_With_Screen_Func;
      pragma Unreferenced (Old);
   begin
      if Dialog = null then
         Gtk_New (Dialog, Title => "Color Selection Dialog");
         Set_Position (Dialog, Enums.Win_Pos_Mouse);

         Set_Has_Palette (Get_Colorsel (Dialog), True);
         Set_Has_Opacity_Control (Get_Colorsel (Dialog), True);
         Old :=
           Set_Change_Palette_With_Screen_Hook (On_Palette_Changed'Access);

         Destroy_Dialog_Cb.Connect
           (Dialog, "destroy",
            Destroy_Dialog_Cb.To_Marshaller (Destroy_Dialog'Access),
            Dialog'Access);

         Color_Sel_Cb.Object_Connect
           (Get_OK_Button (Dialog),
            "clicked",
            Color_Sel_Cb.To_Marshaller (Color_Ok'Access),
            Slot_Object => Dialog);
         Widget_Handler.Object_Connect
           (Get_Cancel_Button (Dialog),
            "clicked",
            Widget_Handler.To_Marshaller (Close_Window'Access),
            Slot_Object => Dialog);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;
   end Run;

end Create_Color_Selection;
