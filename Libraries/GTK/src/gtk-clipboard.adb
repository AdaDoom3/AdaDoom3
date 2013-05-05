-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                  Copyright (C) 2002-2013, AdaCore                 --
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

with Gtkada.C;              use Gtkada.C;
with Gdk.Types;             use Gdk.Types;
with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Gtk.Selection;         use Gtk.Selection;
with Gtkada.Types;
with System;                use System;

package body Gtk.Clipboard is

   package Atom_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Atom, Gdk.Types.Gdk_None,
      Natural, Gdk.Types.Gdk_Atom_Array);

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Clipboard : Gtk_Clipboard;
      Pixbuf    : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal (Clipboard : Gtk_Clipboard; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_set_image");

   begin
      Internal (Clipboard, Get_Object (Pixbuf));
   end Set_Image;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Clipboard : Gtk_Clipboard;
      Text      : UTF8_String)
   is
      procedure Internal
        (Clipboard : Gtk_Clipboard;
         Str       : UTF8_String;
         Len       : Integer);
      pragma Import (C, Internal, "gtk_clipboard_set_text");

   begin
      Internal (Clipboard, Text, Text'Length);
   end Set_Text;

   --------------------
   -- Wait_For_Image --
   --------------------

   function Wait_For_Image
     (Clipboard : Gtk_Clipboard)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Clipboard : Gtk_Clipboard) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_image");

   begin
      return Gdk.Pixbuf.Convert (Internal (Clipboard));
   end Wait_For_Image;

   -------------------
   -- Wait_For_Text --
   -------------------

   function Wait_For_Text (Clipboard : Gtk_Clipboard) return UTF8_String is
      function Internal (Clipboard : Gtk_Clipboard) return chars_ptr;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_text");

      S : constant chars_ptr := Internal (Clipboard);

   begin
      if S /= Null_Ptr then
         declare
            Result : constant UTF8_String := Value (S);
         begin
            Gtkada.Types.g_free (S);
            return Result;
         end;
      end if;

      return "";
   end Wait_For_Text;

   ----------------------------
   -- Wait_Is_Text_Available --
   ----------------------------

   function Wait_Is_Text_Available
     (Clipboard : Gtk_Clipboard) return Boolean
   is
      function Internal (Clipboard : Gtk_Clipboard) return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_text_available");
   begin
      return Internal (Clipboard) /= 0;
   end Wait_Is_Text_Available;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Clipboard : Gtk_Clipboard) return Glib.Object.GObject
   is
      function Internal (Clipboard : Gtk_Clipboard) return System.Address;
      pragma Import (C, Internal, "gtk_clipboard_get_owner");
      Stub : GObject_Record;
   begin
      return Get_User_Data (Internal (Clipboard), Stub);
   end Get_Owner;

   -------------------
   -- Set_Can_Store --
   -------------------

   procedure Set_Can_Store
     (Clipboard : Gtk_Clipboard;
      Targets   : Gtk.Selection.Target_Entry_Array)
   is
      procedure Internal
        (Clipboard : Gtk_Clipboard;
         Targets   : System.Address;
         N_Targets : Gint);
      pragma Import (C, Internal, "gtk_clipboard_set_can_store");
   begin
      if Targets'Length = 0 then
         Internal (Clipboard, System.Null_Address, 0);
      else
         Internal (Clipboard, Targets (Targets'First)'Address, Targets'Length);
      end if;
   end Set_Can_Store;

   -------------------
   -- Set_With_Data --
   -------------------

   function Set_With_Data
     (Clipboard  : Gtk_Clipboard;
      Targets    : Target_Entry_Array;
      Get_Func   : Gtk_Clipboard_Get_Func;
      Clear_Func : Gtk_Clipboard_Clear_Func;
      User_Data  : System.Address)
      return Boolean
   is
      function Internal
        (Clipboard  : Gtk_Clipboard;
         Targets    : System.Address;
         N_Targets  : Guint;
         Get_Func   : Gtk_Clipboard_Get_Func;
         Clear_Func : Gtk_Clipboard_Clear_Func;
         User_Data  : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_set_with_data");
   begin
      return Boolean'Val
        (Internal (Clipboard, Targets (Targets'First)'Address,
         Targets'Length, Get_Func, Clear_Func, User_Data));
   end Set_With_Data;

   --------------------
   -- Set_With_Owner --
   --------------------

   function Set_With_Owner
     (Clipboard  : Gtk_Clipboard;
      Targets    : Target_Entry_Array;
      Get_Func   : Gtk_Clipboard_Get_Func;
      Clear_Func : Gtk_Clipboard_Clear_Func;
      Owner      : access Glib.Object.GObject_Record'Class)
      return Boolean
   is
      function Internal
        (Clipboard  : Gtk_Clipboard;
         Targets    : System.Address;
         N_Targets  : Guint;
         Get_Func   : Gtk_Clipboard_Get_Func;
         Clear_Func : Gtk_Clipboard_Clear_Func;
         Owner      : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_set_with_owner");
   begin
      return Boolean'Val
        (Internal (Clipboard, Targets (Targets'First)'Address,
         Targets'Length, Get_Func, Clear_Func, Get_Object (Owner)));
   end Set_With_Owner;

   -------------------
   -- Get_Clipboard --
   -------------------

   function Get_Clipboard
     (Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Selection : Gdk.Types.Gdk_Atom) return Gtk_Clipboard
   is
      function Internal
        (Widget : System.Address; Selection : Gdk_Atom) return Gtk_Clipboard;
      pragma Import (C, Internal, "gtk_widget_get_clipboard");
      --  External binding: gtk_widget_get_clipboard
   begin
      return Internal (Get_Object (Widget), Selection);
   end Get_Clipboard;

   -----------------------------
   -- Wait_Is_Image_Available --
   -----------------------------

   function Wait_Is_Image_Available
     (Clipboard : Gtk_Clipboard) return Boolean
   is
      function Internal (Clipboard : Gtk_Clipboard) return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_image_available");
   begin
      return Boolean'Val (Internal (Clipboard));
   end Wait_Is_Image_Available;

   ------------------------------
   -- Wait_Is_Target_Available --
   ------------------------------

   function Wait_Is_Target_Available
     (Clipboard : Gtk_Clipboard;
      Target    : Gdk_Atom)
      return Boolean
   is
      function Internal
        (Clipboard : Gtk_Clipboard;
         Target    : Gdk_Atom)
         return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_is_target_available");
   begin
      return Boolean'Val (Internal (Clipboard, Target));
   end Wait_Is_Target_Available;

   ----------------------
   -- Wait_For_Targets --
   ----------------------

   function Wait_For_Targets
     (Clipboard : Gtk_Clipboard) return Gdk.Types.Gdk_Atom_Array
   is
      use Atom_Arrays;
      function Internal
        (Clipboard : Gtk_Clipboard;
         Targets   : access Unbounded_Array_Access;
         N_Targets : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_clipboard_wait_for_targets");

      Output    : aliased Unbounded_Array_Access;
      N_Targets : aliased Gint;
   begin
      if Internal
        (Clipboard, Output'Unchecked_Access, N_Targets'Unchecked_Access) = 0
      then
         G_Free (Output);
         Output := null;
      end if;

      declare
         Result : constant Gdk_Atom_Array :=
           To_Array (Output, Integer (N_Targets));
      begin
         if Output /= null then
            G_Free (Output);
         end if;

         return Result;
      end;
   end Wait_For_Targets;

   ----------------------
   -- Request_Contents --
   ----------------------

   procedure Request_Contents
     (Clipboard : Gtk_Clipboard;
      Target    : Gdk_Atom;
      Callback  : Gtk_Clipboard_Received_Func;
      User_Data : System.Address)
   is
      procedure Internal
        (Clipboard : Gtk_Clipboard;
         Target    : Gdk_Atom;
         Callback  : Gtk_Clipboard_Received_Func;
         User_Data : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_request_contents");
   begin
      Internal (Clipboard, Target, Callback, User_Data);
   end Request_Contents;

   ---------------------
   -- Request_Targets --
   ---------------------

   procedure Request_Targets
     (Clipboard : Gtk_Clipboard;
      Callback  : Gtk_Clipboard_Targets_Received_Func;
      User_Data : System.Address)
   is
      procedure Internal
        (Clipboard : Gtk_Clipboard;
         Callback  : Gtk_Clipboard_Targets_Received_Func;
         User_Data : System.Address);
      pragma Import (C, Internal, "gtk_clipboard_request_targets");
   begin
      Internal (Clipboard, Callback, User_Data);
   end Request_Targets;

end Gtk.Clipboard;
