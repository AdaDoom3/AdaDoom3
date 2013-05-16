-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Status_Bar is
   function Convert (Msg : Status_Bar_Msg) return System.Address is
   begin
      return Msg'Address;
      --  This function is anyway not supposed to be used
   end Convert;

   function Convert (Msg : System.Address) return Status_Bar_Msg is
      type Status_Bar_Msg_Access is access all Status_Bar_Msg;
      function Internal is new
      Ada.Unchecked_Conversion (System.Address, Status_Bar_Msg_Access);
   begin
      return Internal (Msg).all;
   end Convert;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Status_Bar_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar) is
   begin
      Statusbar := new Gtk_Status_Bar_Record;
      Gtk.Status_Bar.Initialize (Statusbar);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Statusbar : access Gtk_Status_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_new");
   begin
      Set_Object (Statusbar, Internal);
   end Initialize;

   --------------------
   -- Get_Context_Id --
   --------------------

   function Get_Context_Id
      (Statusbar           : access Gtk_Status_Bar_Record;
       Context_Description : UTF8_String) return Context_Id
   is
      function Internal
         (Statusbar           : System.Address;
          Context_Description : Interfaces.C.Strings.chars_ptr)
          return Context_Id;
      pragma Import (C, Internal, "gtk_statusbar_get_context_id");
      Tmp_Context_Description : Interfaces.C.Strings.chars_ptr := New_String (Context_Description);
      Tmp_Return              : Context_Id;
   begin
      Tmp_Return := Internal (Get_Object (Statusbar), Tmp_Context_Description);
      Free (Tmp_Context_Description);
      return Tmp_Return;
   end Get_Context_Id;

   -------------------------
   -- Get_Has_Resize_Grip --
   -------------------------

   function Get_Has_Resize_Grip
      (Statusbar : access Gtk_Status_Bar_Record) return Boolean
   is
      function Internal (Statusbar : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_statusbar_get_has_resize_grip");
   begin
      return Boolean'Val (Internal (Get_Object (Statusbar)));
   end Get_Has_Resize_Grip;

   ----------------------
   -- Get_Message_Area --
   ----------------------

   function Get_Message_Area
      (Statusbar : access Gtk_Status_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Statusbar : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_statusbar_get_message_area");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Statusbar)), Stub));
   end Get_Message_Area;

   ---------
   -- Pop --
   ---------

   procedure Pop
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id)
   is
      procedure Internal (Statusbar : System.Address; Context : Context_Id);
      pragma Import (C, Internal, "gtk_statusbar_pop");
   begin
      Internal (Get_Object (Statusbar), Context);
   end Pop;

   ----------
   -- Push --
   ----------

   function Push
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Text      : UTF8_String) return Message_Id
   is
      function Internal
         (Statusbar : System.Address;
          Context   : Context_Id;
          Text      : Interfaces.C.Strings.chars_ptr) return Message_Id;
      pragma Import (C, Internal, "gtk_statusbar_push");
      Tmp_Text   : Interfaces.C.Strings.chars_ptr := New_String (Text);
      Tmp_Return : Message_Id;
   begin
      Tmp_Return := Internal (Get_Object (Statusbar), Context, Tmp_Text);
      Free (Tmp_Text);
      return Tmp_Return;
   end Push;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Message   : Message_Id)
   is
      procedure Internal
         (Statusbar : System.Address;
          Context   : Context_Id;
          Message   : Message_Id);
      pragma Import (C, Internal, "gtk_statusbar_remove");
   begin
      Internal (Get_Object (Statusbar), Context, Message);
   end Remove;

   ----------------
   -- Remove_All --
   ----------------

   procedure Remove_All
      (Statusbar : access Gtk_Status_Bar_Record;
       Context   : Context_Id)
   is
      procedure Internal (Statusbar : System.Address; Context : Context_Id);
      pragma Import (C, Internal, "gtk_statusbar_remove_all");
   begin
      Internal (Get_Object (Statusbar), Context);
   end Remove_All;

   -------------------------
   -- Set_Has_Resize_Grip --
   -------------------------

   procedure Set_Has_Resize_Grip
      (Statusbar : access Gtk_Status_Bar_Record;
       Setting   : Boolean)
   is
      procedure Internal (Statusbar : System.Address; Setting : Integer);
      pragma Import (C, Internal, "gtk_statusbar_set_has_resize_grip");
   begin
      Internal (Get_Object (Statusbar), Boolean'Pos (Setting));
   end Set_Has_Resize_Grip;

   ------------------
   -- Get_Messages --
   ------------------

   function Get_Messages
      (Statusbar : access Gtk_Status_Bar_Record)
       return Gtk.Status_Bar.Messages_List.GSlist
   is
      function Internal (Statusbar : System.Address) return System.Address;
      pragma Import (C, Internal, "gtkada_GtkStatusbar_get_messages");
      Tmp_Return : Messages_List.GSlist;
   begin
      Gtk.Status_Bar.Messages_List.Set_Object (Tmp_Return, Internal (Get_Object (Statusbar)));
      return Tmp_Return;
   end Get_Messages;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Status_Bar_Record) return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : access Gtk_Status_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Status_Bar;
