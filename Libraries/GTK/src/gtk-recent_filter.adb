-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010-2013, AdaCore               --
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

with Interfaces.C.Strings;

package body Gtk.Recent_Filter is

   -------------
   -- Add_Age --
   -------------

   procedure Add_Age
     (Filter : access Gtk_Recent_Filter_Record;
      Days   : Gint)
   is
      procedure Internal
        (Filter : System.Address;
         Days   : Gint);
      pragma Import (C, Internal, "gtk_recent_filter_add_age");
   begin
      Internal (Get_Object (Filter), Days);
   end Add_Age;

   ---------------------
   -- Add_Application --
   ---------------------

   procedure Add_Application
     (Filter      : access Gtk_Recent_Filter_Record;
      Application : UTF8_String)
   is
      procedure Internal
        (Filter      : System.Address;
         Application : UTF8_String);
      pragma Import (C, Internal, "gtk_recent_filter_add_application");
   begin
      Internal (Get_Object (Filter), Application & ASCII.NUL);
   end Add_Application;

   ---------------
   -- Add_Group --
   ---------------

   procedure Add_Group
     (Filter : access Gtk_Recent_Filter_Record;
      Group  : UTF8_String)
   is
      procedure Internal
        (Filter : System.Address;
         Group  : UTF8_String);
      pragma Import (C, Internal, "gtk_recent_filter_add_group");
   begin
      Internal (Get_Object (Filter), Group & ASCII.NUL);
   end Add_Group;

   -------------------
   -- Add_Mime_Type --
   -------------------

   procedure Add_Mime_Type
     (Filter    : access Gtk_Recent_Filter_Record;
      Mime_Type : UTF8_String)
   is
      procedure Internal
        (Filter    : System.Address;
         Mime_Type : UTF8_String);
      pragma Import (C, Internal, "gtk_recent_filter_add_mime_type");
   begin
      Internal (Get_Object (Filter), Mime_Type & ASCII.NUL);
   end Add_Mime_Type;

   -----------------
   -- Add_Pattern --
   -----------------

   procedure Add_Pattern
     (Filter  : access Gtk_Recent_Filter_Record;
      Pattern : UTF8_String)
   is
      procedure Internal
        (Filter  : System.Address;
         Pattern : UTF8_String);
      pragma Import (C, Internal, "gtk_recent_filter_add_pattern");
   begin
      Internal (Get_Object (Filter), Pattern & ASCII.NUL);
   end Add_Pattern;

   ------------------------
   -- Add_Pixbuf_Formats --
   ------------------------

   procedure Add_Pixbuf_Formats (Filter : access Gtk_Recent_Filter_Record) is
      procedure Internal (Filter : System.Address);
      pragma Import (C, Internal, "gtk_recent_filter_add_pixbuf_formats");
   begin
      Internal (Get_Object (Filter));
   end Add_Pixbuf_Formats;

   -------------
   -- Convert --
   -------------

   function Convert (Widget : Gtk_Recent_Filter) return System.Address is
   begin
      return Get_Object (Widget);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (Widget : System.Address) return Gtk_Recent_Filter is
      Stub : Gtk_Recent_Filter_Record;
   begin
      return Gtk_Recent_Filter (Get_User_Data (Widget, Stub));
   end Convert;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Filter : access Gtk_Recent_Filter_Record) return UTF8_String
   is
      use Interfaces.C.Strings;

      function Internal (Filter : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_recent_filter_get_name");
   begin
      return Value (Internal (Get_Object (Filter)));
   end Get_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Recent_Filter) is
   begin
      Widget := new Gtk_Recent_Filter_Record;
      Gtk.Recent_Filter.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Recent_Filter_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_recent_filter_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Filter : access Gtk_Recent_Filter_Record;
      Name   : UTF8_String)
   is
      procedure Internal
        (Filter : System.Address;
         Name   : UTF8_String);
      pragma Import (C, Internal, "gtk_recent_filter_set_name");
   begin
      Internal (Get_Object (Filter), Name & ASCII.NUL);
   end Set_Name;

end Gtk.Recent_Filter;
