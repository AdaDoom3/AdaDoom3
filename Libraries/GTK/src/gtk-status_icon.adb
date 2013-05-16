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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Status_Icon is

   ------------------
   -- Get_Blinking --
   ------------------

   function Get_Blinking
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_blinking");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Get_Blinking;

   ------------------
   -- Get_Geometry --
   ------------------

   procedure Get_Geometry
     (Status_Icon : access Gtk_Status_Icon_Record;
      Screen      : in out Gdk.Screen.Gdk_Screen;
      Area        : out Gdk.Rectangle.Gdk_Rectangle;
      Orientation : out Gtk.Enums.Gtk_Orientation;
      Success     : out Boolean)
   is
      function Internal
        (Status_Icon : System.Address;
         Screen      : System.Address;
         Area        : System.Address;
         Orientation : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_geometry");
   begin
      Success := Boolean'Val (Internal
        (Get_Object (Status_Icon),
         Get_Object (Screen),
         Area'Address,
         Orientation'Address));
   end Get_Geometry;

   ---------------
   -- Get_Gicon --
   ---------------

   function Get_Gicon
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Glib.G_Icon.G_Icon
   is
      function Internal
        (Status_Icon : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_status_icon_get_gicon");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Gicon;

   ---------------------
   -- Get_Has_Tooltip --
   ---------------------

   function Get_Has_Tooltip
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_has_tooltip");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Get_Has_Tooltip;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
     (Status_Icon : access Gtk_Status_Icon_Record) return String
   is
      function Internal (Status_Icon : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_icon_name");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Status_Icon));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Icon_Name;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Status_Icon : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_get_pixbuf");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf
        (Get_User_Data (Internal (Get_Object (Status_Icon)), Stub));
   end Get_Pixbuf;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gdk.Screen.Gdk_Screen
   is
      function Internal (Status_Icon : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_get_screen");
      Stub : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen
        (Get_User_Data (Internal (Get_Object (Status_Icon)), Stub));
   end Get_Screen;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
     (Status_Icon : access Gtk_Status_Icon_Record) return Gint
   is
      function Internal (Status_Icon : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_status_icon_get_size");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Size;

   ---------------
   -- Get_Stock --
   ---------------

   function Get_Stock
     (Status_Icon : access Gtk_Status_Icon_Record) return String
   is
      function Internal (Status_Icon : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_stock");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Status_Icon));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Stock;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
     (Status_Icon : access Gtk_Status_Icon_Record)
      return Gtk.Image.Gtk_Image_Type
   is
      function Internal
        (Status_Icon : System.Address) return Gtk.Image.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_status_icon_get_storage_type");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Storage_Type;

   ------------------------
   -- Get_Tooltip_Markup --
   ------------------------

   function Get_Tooltip_Markup
     (Status_Icon : access Gtk_Status_Icon_Record) return String
   is
      function Internal (Status_Icon : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_tooltip_markup");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Status_Icon));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Tooltip_Markup;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   function Get_Tooltip_Text
     (Status_Icon : access Gtk_Status_Icon_Record) return String
   is
      function Internal (Status_Icon : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_tooltip_text");
      C : chars_ptr;
   begin
      C := Internal (Get_Object (Status_Icon));
      if C = Null_Ptr then
         return "";
      else
         return Value (C);
      end if;
   end Get_Tooltip_Text;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Get_Visible;

   -----------------------
   -- Get_X11_Window_Id --
   -----------------------

   function Get_X11_Window_Id
     (Status_Icon : access Gtk_Status_Icon_Record) return Guint32
   is
      function Internal (Status_Icon : System.Address) return Guint32;
      pragma Import (C, Internal, "gtk_status_icon_get_x11_window_id");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_X11_Window_Id;

   -----------------
   -- Is_Embedded --
   -----------------

   function Is_Embedded
     (Status_Icon : access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_is_embedded");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Is_Embedded;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Status_Icon) is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Status_Icon_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
     (Widget   : out Gtk_Status_Icon;
      Filename : String)
   is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Initialize_From_File (Widget, Filename);
   end Gtk_New_From_File;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
     (Widget   : access Gtk_Status_Icon_Record'Class;
      Filename : String)
   is
      function Internal (Filename : String) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_file");
   begin
      Set_Object (Widget, Internal (Filename & ASCII.NUL));
   end Initialize_From_File;

   ------------------------
   -- Gtk_New_From_Gicon --
   ------------------------

   procedure Gtk_New_From_Gicon
     (Widget : out Gtk_Status_Icon;
      Icon   : Glib.G_Icon.G_Icon)
   is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Initialize_From_Gicon (Widget, Icon);
   end Gtk_New_From_Gicon;

   ---------------------------
   -- Initialize_From_Gicon --
   ---------------------------

   procedure Initialize_From_Gicon
     (Widget : access Gtk_Status_Icon_Record'Class;
      Icon   : Glib.G_Icon.G_Icon)
   is
      function Internal (Icon : Glib.G_Icon.G_Icon) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_gicon");
   begin
      Set_Object (Widget, Internal (Icon));
   end Initialize_From_Gicon;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
     (Widget    : out Gtk_Status_Icon;
      Icon_Name : String)
   is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Initialize_From_Icon_Name (Widget, Icon_Name);
   end Gtk_New_From_Icon_Name;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
     (Widget    : access Gtk_Status_Icon_Record'Class;
      Icon_Name : String)
   is
      function Internal (Icon_Name : String) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_icon_name");
   begin
      Set_Object (Widget, Internal (Icon_Name & ASCII.NUL));
   end Initialize_From_Icon_Name;

   -------------------------
   -- Gtk_New_From_Pixbuf --
   -------------------------

   procedure Gtk_New_From_Pixbuf
     (Widget : out Gtk_Status_Icon;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Initialize_From_Pixbuf (Widget, Pixbuf);
   end Gtk_New_From_Pixbuf;

   ----------------------------
   -- Initialize_From_Pixbuf --
   ----------------------------

   procedure Initialize_From_Pixbuf
     (Widget : access Gtk_Status_Icon_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_pixbuf");
   begin
      Set_Object (Widget, Internal (Get_Object (Pixbuf)));
   end Initialize_From_Pixbuf;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
     (Widget   : out Gtk_Status_Icon;
      Stock_Id : String)
   is
   begin
      Widget := new Gtk_Status_Icon_Record;
      Initialize_From_Stock (Widget, Stock_Id);
   end Gtk_New_From_Stock;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
     (Widget   : access Gtk_Status_Icon_Record'Class;
      Stock_Id : String)
   is
      function Internal (Stock_Id : String) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_stock");
   begin
      Set_Object (Widget, Internal (Stock_Id & ASCII.NUL));
   end Initialize_From_Stock;

   ------------------
   -- Set_Blinking --
   ------------------

   procedure Set_Blinking
     (Status_Icon : access Gtk_Status_Icon_Record;
      Blinking    : Boolean)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Blinking    : Gboolean);
      pragma Import (C, Internal, "gtk_status_icon_set_blinking");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Blinking));
   end Set_Blinking;

   -------------------
   -- Set_From_File --
   -------------------

   procedure Set_From_File
     (Status_Icon : access Gtk_Status_Icon_Record;
      Filename    : String)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Filename    : String);
      pragma Import (C, Internal, "gtk_status_icon_set_from_file");
   begin
      Internal (Get_Object (Status_Icon), Filename & ASCII.NUL);
   end Set_From_File;

   --------------------
   -- Set_From_Gicon --
   --------------------

   procedure Set_From_Gicon
     (Status_Icon : access Gtk_Status_Icon_Record;
      Icon        : Glib.G_Icon.G_Icon)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Icon        : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_status_icon_set_from_gicon");
   begin
      Internal (Get_Object (Status_Icon), Icon);
   end Set_From_Gicon;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
     (Status_Icon : access Gtk_Status_Icon_Record;
      Icon_Name   : String)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Icon_Name   : String);
      pragma Import (C, Internal, "gtk_status_icon_set_from_icon_name");
   begin
      Internal (Get_Object (Status_Icon), Icon_Name & ASCII.NUL);
   end Set_From_Icon_Name;

   ---------------------
   -- Set_From_Pixbuf --
   ---------------------

   procedure Set_From_Pixbuf
     (Status_Icon : access Gtk_Status_Icon_Record;
      Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf)
   is
      procedure Internal (Status_Icon, Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_from_pixbuf");
   begin
      Internal (Get_Object (Status_Icon), Get_Object (Pixbuf));
   end Set_From_Pixbuf;

   --------------------
   -- Set_From_Stock --
   --------------------

   procedure Set_From_Stock
     (Status_Icon : access Gtk_Status_Icon_Record;
      Stock_Id    : String)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Stock_Id    : String);
      pragma Import (C, Internal, "gtk_status_icon_set_from_stock");
   begin
      Internal (Get_Object (Status_Icon), Stock_Id & ASCII.NUL);
   end Set_From_Stock;

   ---------------------
   -- Set_Has_Tooltip --
   ---------------------

   procedure Set_Has_Tooltip
     (Status_Icon : access Gtk_Status_Icon_Record;
      Has_Tooltip : Boolean)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Has_Tooltip : Gboolean);
      pragma Import (C, Internal, "gtk_status_icon_set_has_tooltip");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Has_Tooltip));
   end Set_Has_Tooltip;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
     (Status_Icon : access Gtk_Status_Icon_Record;
      Screen      : access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Status_Icon, Screen : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_screen");
   begin
      Internal (Get_Object (Status_Icon), Get_Object (Screen));
   end Set_Screen;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip
     (Status_Icon  : access Gtk_Status_Icon_Record;
      Tooltip_Text : String)
   is
      procedure Internal
        (Status_Icon  : System.Address;
         Tooltip_Text : String);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip");
   begin
      Internal (Get_Object (Status_Icon), Tooltip_Text & ASCII.NUL);
   end Set_Tooltip;

   ------------------------
   -- Set_Tooltip_Markup --
   ------------------------

   procedure Set_Tooltip_Markup
     (Status_Icon : access Gtk_Status_Icon_Record;
      Markup      : String)
   is
      procedure Internal (Status_Icon, Markup : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_markup");
      C_Markup : constant String := Markup & ASCII.NUL;
   begin
      if Markup = "" then
         Internal (Get_Object (Status_Icon), System.Null_Address);
      else
         Internal (Get_Object (Status_Icon), C_Markup'Address);
      end if;
   end Set_Tooltip_Markup;

   ----------------------
   -- Set_Tooltip_Text --
   ----------------------

   procedure Set_Tooltip_Text
     (Status_Icon : access Gtk_Status_Icon_Record;
      Text        : String)
   is
      procedure Internal (Status_Icon, Text : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_text");
      C_Text : constant String := Text & ASCII.NUL;
   begin
      if Text = "" then
         Internal (Get_Object (Status_Icon), System.Null_Address);
      else
         Internal (Get_Object (Status_Icon), C_Text'Address);
      end if;
   end Set_Tooltip_Text;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Status_Icon : access Gtk_Status_Icon_Record;
      Visible     : Boolean)
   is
      procedure Internal
        (Status_Icon : System.Address;
         Visible     : Gboolean);
      pragma Import (C, Internal, "gtk_status_icon_set_visible");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Visible));
   end Set_Visible;

end Gtk.Status_Icon;
