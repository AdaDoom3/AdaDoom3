-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                 Copyright (C) 2006-2013, AdaCore                  --
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

with Gtk.Dialog;   use Gtk.Dialog;
with Gtk.Window;   use Gtk.Window;

with Glib.Type_Conversion_Hooks;

package body Gtk.Message_Dialog is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Message_Dialog_Record);
   pragma Warnings (Off, Type_Conversion);

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Str            : String)
   is
      procedure Internal
        (Message_Dialog : System.Address;
         Str            : String);
      pragma Import (C, Internal, "gtk_message_dialog_set_markup");
   begin
      Internal (Get_Object (Message_Dialog), Str & ASCII.NUL);
   end Set_Markup;

   -----------------------------
   -- Format_Secondary_Markup --
   -----------------------------

   procedure Format_Secondary_Markup
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Message        : String)
   is
      procedure Internal
        (Message_Dialog : System.Address;
         Message        : String);
      pragma Import
        (C, Internal, "ada_gtk_message_dialog_format_secondary_markup");
   begin
      Internal (Get_Object (Message_Dialog), Message);
   end Format_Secondary_Markup;

   ---------------------------
   -- Format_Secondary_Text --
   ---------------------------

   procedure Format_Secondary_Text
     (Message_Dialog : access Gtk_Message_Dialog_Record;
      Message        : String)
   is
      procedure Internal
        (Message_Dialog : System.Address;
         Message        : String);
      pragma Import
        (C, Internal, "ada_gtk_message_dialog_format_secondary_text");
   begin
      Internal (Get_Object (Message_Dialog), Message);
   end Format_Secondary_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog         : out Gtk_Message_Dialog;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String) is
   begin
      Dialog := new Gtk_Message_Dialog_Record;
      Initialize (Dialog, Parent, Flags, Typ, Buttons, Message);
   end Gtk_New;

   -------------------------
   -- Gtk_New_With_Markup --
   -------------------------

   procedure Gtk_New_With_Markup
     (Dialog         : out Gtk_Message_Dialog;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String) is
   begin
      Dialog := new Gtk_Message_Dialog_Record;
      Initialize_With_Markup (Dialog, Parent, Flags, Typ, Buttons, Message);
   end Gtk_New_With_Markup;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Dialog         : access Gtk_Message_Dialog_Record'Class;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String)
   is
      function Internal
        (Parent         : System.Address;
         Flags          : Gtk_Dialog_Flags;
         Typ            : Gtk_Message_Type;
         Buttons        : Gtk_Buttons_Type;
         Message        : String) return System.Address;
      pragma Import (C, Internal, "ada_gtk_message_dialog_new");
   begin
      Set_Object
        (Dialog,
         Internal (Get_Object (Parent), Flags, Typ, Buttons,
           Message & ASCII.NUL));
   end Initialize;

   ----------------------------
   -- Initialize_With_Markup --
   ----------------------------

   procedure Initialize_With_Markup
     (Dialog         : access Gtk_Message_Dialog_Record'Class;
      Parent         : Gtk.Window.Gtk_Window := null;
      Flags          : Gtk.Dialog.Gtk_Dialog_Flags := 0;
      Typ            : Gtk_Message_Type := Message_Info;
      Buttons        : Gtk_Buttons_Type := Buttons_Close;
      Message        : String)
   is
      function Internal
        (Parent         : System.Address;
         Flags          : Gtk_Dialog_Flags;
         Typ            : Gtk_Message_Type;
         Buttons        : Gtk_Buttons_Type;
         Message        : String) return System.Address;
      pragma Import (C, Internal, "ada_gtk_message_dialog_new_with_markup");
   begin
      Set_Object
        (Dialog,
         Internal (Get_Object (Parent), Flags, Typ, Buttons,
           Message & ASCII.NUL));
   end Initialize_With_Markup;

   ---------------
   -- Get_Image --
   ---------------

   function Get_Image
     (Dialog : access Gtk_Message_Dialog_Record)
      return Gtk.Widget.Gtk_Widget
   is
      function Internal (Dialog : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_message_dialog_get_image");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Dialog)), Stub));
   end Get_Image;

   ---------------
   -- Set_Image --
   ---------------

   procedure Set_Image
     (Dialog : access Gtk_Message_Dialog_Record;
      Image  : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Dialog : System.Address; Image : System.Address);
      pragma Import (C, Internal, "gtk_message_dialog_set_image");
   begin
      Internal (Get_Object (Dialog), Get_Object (Image));
   end Set_Image;

end Gtk.Message_Dialog;
