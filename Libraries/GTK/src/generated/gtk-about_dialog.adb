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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.About_Dialog is
   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_About_Dialog_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (About : out Gtk_About_Dialog) is
   begin
      About := new Gtk_About_Dialog_Record;
      Gtk.About_Dialog.Initialize (About);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (About : access Gtk_About_Dialog_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_new");
   begin
      Set_Object (About, Internal);
   end Initialize;

   -----------------
   -- Get_Artists --
   -----------------

   function Get_Artists
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_artists");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Artists;

   -----------------
   -- Get_Authors --
   -----------------

   function Get_Authors
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_authors");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Authors;

   ------------------
   -- Get_Comments --
   ------------------

   function Get_Comments
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_comments");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Comments;

   -------------------
   -- Get_Copyright --
   -------------------

   function Get_Copyright
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_copyright");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Copyright;

   ---------------------
   -- Get_Documenters --
   ---------------------

   function Get_Documenters
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (About : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_about_dialog_get_documenters");
   begin
      return To_String_List (Internal (Get_Object (About)).all);
   end Get_Documenters;

   -----------------
   -- Get_License --
   -----------------

   function Get_License
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_license");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_License;

   --------------
   -- Get_Logo --
   --------------

   function Get_Logo
      (About : access Gtk_About_Dialog_Record) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (About : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (About)), Stub));
   end Get_Logo;

   ------------------------
   -- Get_Logo_Icon_Name --
   ------------------------

   function Get_Logo_Icon_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_logo_icon_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Logo_Icon_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Name;

   ----------------------
   -- Get_Program_Name --
   ----------------------

   function Get_Program_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_program_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Program_Name;

   ----------------------------
   -- Get_Translator_Credits --
   ----------------------------

   function Get_Translator_Credits
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_translator_credits");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Translator_Credits;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_version");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Version;

   -----------------
   -- Get_Website --
   -----------------

   function Get_Website
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Website;

   -----------------------
   -- Get_Website_Label --
   -----------------------

   function Get_Website_Label
      (About : access Gtk_About_Dialog_Record) return UTF8_String
   is
      function Internal
         (About : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_about_dialog_get_website_label");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (About)));
   end Get_Website_Label;

   ----------------------
   -- Get_Wrap_License --
   ----------------------

   function Get_Wrap_License
      (About : access Gtk_About_Dialog_Record) return Boolean
   is
      function Internal (About : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_about_dialog_get_wrap_license");
   begin
      return Boolean'Val (Internal (Get_Object (About)));
   end Get_Wrap_License;

   -----------------
   -- Set_Artists --
   -----------------

   procedure Set_Artists
      (About   : access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List)
   is
      procedure Internal
         (About   : System.Address;
          Artists : Interfaces.C.Strings.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_artists");
      Tmp_Artists : Interfaces.C.Strings.chars_ptr_array := From_String_List (Artists);
   begin
      Internal (Get_Object (About), Tmp_Artists);
      GtkAda.Types.Free (Tmp_Artists);
   end Set_Artists;

   -----------------
   -- Set_Authors --
   -----------------

   procedure Set_Authors
      (About   : access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List)
   is
      procedure Internal
         (About   : System.Address;
          Authors : Interfaces.C.Strings.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_authors");
      Tmp_Authors : Interfaces.C.Strings.chars_ptr_array := From_String_List (Authors);
   begin
      Internal (Get_Object (About), Tmp_Authors);
      GtkAda.Types.Free (Tmp_Authors);
   end Set_Authors;

   ------------------
   -- Set_Comments --
   ------------------

   procedure Set_Comments
      (About    : access Gtk_About_Dialog_Record;
       Comments : UTF8_String)
   is
      procedure Internal
         (About    : System.Address;
          Comments : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_comments");
      Tmp_Comments : Interfaces.C.Strings.chars_ptr := New_String (Comments);
   begin
      Internal (Get_Object (About), Tmp_Comments);
      Free (Tmp_Comments);
   end Set_Comments;

   -------------------
   -- Set_Copyright --
   -------------------

   procedure Set_Copyright
      (About     : access Gtk_About_Dialog_Record;
       Copyright : UTF8_String)
   is
      procedure Internal
         (About     : System.Address;
          Copyright : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_copyright");
      Tmp_Copyright : Interfaces.C.Strings.chars_ptr := New_String (Copyright);
   begin
      Internal (Get_Object (About), Tmp_Copyright);
      Free (Tmp_Copyright);
   end Set_Copyright;

   ---------------------
   -- Set_Documenters --
   ---------------------

   procedure Set_Documenters
      (About       : access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List)
   is
      procedure Internal
         (About       : System.Address;
          Documenters : Interfaces.C.Strings.chars_ptr_array);
      pragma Import (C, Internal, "gtk_about_dialog_set_documenters");
      Tmp_Documenters : Interfaces.C.Strings.chars_ptr_array := From_String_List (Documenters);
   begin
      Internal (Get_Object (About), Tmp_Documenters);
      GtkAda.Types.Free (Tmp_Documenters);
   end Set_Documenters;

   -----------------
   -- Set_License --
   -----------------

   procedure Set_License
      (About   : access Gtk_About_Dialog_Record;
       License : UTF8_String)
   is
      procedure Internal
         (About   : System.Address;
          License : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_license");
      Tmp_License : Interfaces.C.Strings.chars_ptr := New_String (License);
   begin
      Internal (Get_Object (About), Tmp_License);
      Free (Tmp_License);
   end Set_License;

   --------------
   -- Set_Logo --
   --------------

   procedure Set_Logo
      (About : access Gtk_About_Dialog_Record;
       Logo  : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (About : System.Address; Logo : System.Address);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo");
   begin
      Internal (Get_Object (About), Get_Object (Logo));
   end Set_Logo;

   ------------------------
   -- Set_Logo_Icon_Name --
   ------------------------

   procedure Set_Logo_Icon_Name
      (About     : access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String)
   is
      procedure Internal
         (About     : System.Address;
          Icon_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_logo_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (About), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Logo_Icon_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (About : access Gtk_About_Dialog_Record;
       Name  : UTF8_String)
   is
      procedure Internal
         (About : System.Address;
          Name  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (About), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name
      (About : access Gtk_About_Dialog_Record;
       Name  : UTF8_String)
   is
      procedure Internal
         (About : System.Address;
          Name  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_program_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (About), Tmp_Name);
      Free (Tmp_Name);
   end Set_Program_Name;

   ----------------------------
   -- Set_Translator_Credits --
   ----------------------------

   procedure Set_Translator_Credits
      (About              : access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String)
   is
      procedure Internal
         (About              : System.Address;
          Translator_Credits : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_translator_credits");
      Tmp_Translator_Credits : Interfaces.C.Strings.chars_ptr := New_String (Translator_Credits);
   begin
      Internal (Get_Object (About), Tmp_Translator_Credits);
      Free (Tmp_Translator_Credits);
   end Set_Translator_Credits;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
      (About   : access Gtk_About_Dialog_Record;
       Version : UTF8_String)
   is
      procedure Internal
         (About   : System.Address;
          Version : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_version");
      Tmp_Version : Interfaces.C.Strings.chars_ptr := New_String (Version);
   begin
      Internal (Get_Object (About), Tmp_Version);
      Free (Tmp_Version);
   end Set_Version;

   -----------------
   -- Set_Website --
   -----------------

   procedure Set_Website
      (About   : access Gtk_About_Dialog_Record;
       Website : UTF8_String)
   is
      procedure Internal
         (About   : System.Address;
          Website : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website");
      Tmp_Website : Interfaces.C.Strings.chars_ptr := New_String (Website);
   begin
      Internal (Get_Object (About), Tmp_Website);
      Free (Tmp_Website);
   end Set_Website;

   -----------------------
   -- Set_Website_Label --
   -----------------------

   procedure Set_Website_Label
      (About         : access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String)
   is
      procedure Internal
         (About         : System.Address;
          Website_Label : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_about_dialog_set_website_label");
      Tmp_Website_Label : Interfaces.C.Strings.chars_ptr := New_String (Website_Label);
   begin
      Internal (Get_Object (About), Tmp_Website_Label);
      Free (Tmp_Website_Label);
   end Set_Website_Label;

   ----------------------
   -- Set_Wrap_License --
   ----------------------

   procedure Set_Wrap_License
      (About        : access Gtk_About_Dialog_Record;
       Wrap_License : Boolean)
   is
      procedure Internal (About : System.Address; Wrap_License : Integer);
      pragma Import (C, Internal, "gtk_about_dialog_set_wrap_license");
   begin
      Internal (Get_Object (About), Boolean'Pos (Wrap_License));
   end Set_Wrap_License;

   --------------------
   -- Set_Email_Hook --
   --------------------

   function Set_Email_Hook
      (Func    : Activate_Link_Func;
       Data    : System.Address;
       Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func
   is
      function Internal
         (Func    : Activate_Link_Func;
          Data    : System.Address;
          Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func;
      pragma Import (C, Internal, "gtk_about_dialog_set_email_hook");
   begin
      return Internal (Func, Data, Destroy);
   end Set_Email_Hook;

   ------------------
   -- Set_Url_Hook --
   ------------------

   function Set_Url_Hook
      (Func    : Activate_Link_Func;
       Data    : System.Address;
       Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func
   is
      function Internal
         (Func    : Activate_Link_Func;
          Data    : System.Address;
          Destroy : Glib.G_Destroy_Notify_Address) return Activate_Link_Func;
      pragma Import (C, Internal, "gtk_about_dialog_set_url_hook");
   begin
      return Internal (Func, Data, Destroy);
   end Set_Url_Hook;

end Gtk.About_Dialog;
