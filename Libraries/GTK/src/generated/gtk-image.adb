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
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Image is
   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk_Icon_Size)
   is
      procedure Internal
        (Image : System.Address;
         Name  : out Interfaces.C.Strings.chars_ptr;
         Size  : out Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_icon_name");
      Str : chars_ptr;
   begin
      Internal (Get_Object (Image), Str, Size);
      Name := new String'(Value (Str));
   end Get_Icon_Name;

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String
   is
      procedure Internal
        (Image    : System.Address;
         Stock_Id : out Interfaces.C.Strings.chars_ptr;
         Size     : out Gint);
      pragma Import (C, Internal, "gtk_image_get_stock");

      Stock : Interfaces.C.Strings.chars_ptr;
      Sze   : Gint;

   begin
      Internal (Get_Object (Image), Stock, Sze);
      Size.all := Gtk.Enums.Gtk_Icon_Size'Val (Sze);
      return Interfaces.C.Strings.Value (Stock);
   end Get;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Image_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Image : out Gtk_Image) is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image     : out Gtk_Image;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Animation);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Image : out Gtk_Image; Filename : UTF8_String) is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Filename);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Icon_Set, Size);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image : out Gtk_Image;
       Val   : Gdk.Image.Gdk_Image;
       Mask  : Gdk.Bitmap.Gdk_Bitmap)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Val, Mask);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Pixbuf);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Pixmap, Mask);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize (Image, Stock_Id, Size);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Gicon --
   ------------------------

   procedure Gtk_New_From_Gicon
      (Image : out Gtk_Image;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Gicon (Image, Icon, Size);
   end Gtk_New_From_Gicon;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
      (Image     : out Gtk_Image;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
   begin
      Image := new Gtk_Image_Record;
      Gtk.Image.Initialize_From_Icon_Name (Image, Icon_Name, Size);
   end Gtk_New_From_Icon_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Image : access Gtk_Image_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_image_new");
   begin
      Set_Object (Image, Internal);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image     : access Gtk_Image_Record'Class;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      function Internal
         (Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_animation");
   begin
      Set_Object (Image, Internal (Animation));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Filename : UTF8_String)
   is
      function Internal
         (Filename : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_file");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Filename);
      Free (Tmp_Filename);
      Set_Object (Image, Tmp_Return);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_set");
   begin
      Set_Object (Image, Internal (Icon_Set, Size));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image : access Gtk_Image_Record'Class;
       Val   : Gdk.Image.Gdk_Image;
       Mask  : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
         (Val  : Gdk.Image.Gdk_Image;
          Mask : Gdk.Bitmap.Gdk_Bitmap) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_image");
   begin
      Set_Object (Image, Internal (Val, Mask));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image  : access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixbuf");
   begin
      Set_Object (Image, Internal (Get_Object_Or_Null (GObject (Pixbuf))));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image  : access Gtk_Image_Record'Class;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      function Internal
         (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
          Mask   : Gdk.Bitmap.Gdk_Bitmap) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_pixmap");
   begin
      Set_Object (Image, Internal (Pixmap, Mask));
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Image    : access Gtk_Image_Record'Class;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id, Size);
      Free (Tmp_Stock_Id);
      Set_Object (Image, Tmp_Return);
   end Initialize;

   ---------------------------
   -- Initialize_From_Gicon --
   ---------------------------

   procedure Initialize_From_Gicon
      (Image : access Gtk_Image_Record'Class;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon : Glib.G_Icon.G_Icon;
          Size : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_gicon");
   begin
      Set_Object (Image, Internal (Icon, Size));
   end Initialize_From_Gicon;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
      (Image     : access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
      function Internal
         (Icon_Name : Interfaces.C.Strings.chars_ptr;
          Size      : Gtk.Enums.Gtk_Icon_Size) return System.Address;
      pragma Import (C, Internal, "gtk_image_new_from_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Icon_Name, Size);
      Free (Tmp_Icon_Name);
      Set_Object (Image, Tmp_Return);
   end Initialize_From_Icon_Name;

   -----------
   -- Clear --
   -----------

   procedure Clear (Image : access Gtk_Image_Record) is
      procedure Internal (Image : System.Address);
      pragma Import (C, Internal, "gtk_image_clear");
   begin
      Internal (Get_Object (Image));
   end Clear;

   ---------
   -- Get --
   ---------

   function Get
      (Image : access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf_Animation
   is
      function Internal
         (Image : System.Address) return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
      pragma Import (C, Internal, "gtk_image_get_animation");
   begin
      return Internal (Get_Object (Image));
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image : access Gtk_Image_Record;
       Gicon : out Glib.G_Icon.G_Icon;
       Size  : out Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image : System.Address;
          Gicon : out Glib.G_Icon.G_Icon;
          Size  : out Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_gicon");
   begin
      Internal (Get_Object (Image), Gicon, Size);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image    : access Gtk_Image_Record;
       Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : out Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
          Size     : out Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_get_icon_set");
   begin
      Internal (Get_Object (Image), Icon_Set, Size);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image     : access Gtk_Image_Record;
       Gdk_Image : out Gdk.Image.Gdk_Image;
       Mask      : out Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Image     : System.Address;
          Gdk_Image : out Gdk.Image.Gdk_Image;
          Mask      : out Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_get_image");
   begin
      Internal (Get_Object (Image), Gdk_Image, Mask);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
      (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Image : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_image_get_pixbuf");
      Stub : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Image)), Stub));
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
      (Image  : access Gtk_Image_Record;
       Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
       Mask   : out Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Image  : System.Address;
          Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
          Mask   : out Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_get_pixmap");
   begin
      Internal (Get_Object (Image), Pixmap, Mask);
   end Get;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   function Get_Pixel_Size (Image : access Gtk_Image_Record) return Gint is
      function Internal (Image : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_image_get_pixel_size");
   begin
      return Internal (Get_Object (Image));
   end Get_Pixel_Size;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
      (Image : access Gtk_Image_Record) return Gtk_Image_Type
   is
      function Internal (Image : System.Address) return Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_image_get_storage_type");
   begin
      return Internal (Get_Object (Image));
   end Get_Storage_Type;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image     : access Gtk_Image_Record;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation)
   is
      procedure Internal
         (Image     : System.Address;
          Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
      pragma Import (C, Internal, "gtk_image_set_from_animation");
   begin
      Internal (Get_Object (Image), Animation);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set (Image : access Gtk_Image_Record; Filename : UTF8_String) is
      procedure Internal
         (Image    : System.Address;
          Filename : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_image_set_from_file");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
   begin
      Internal (Get_Object (Image), Tmp_Filename);
      Free (Tmp_Filename);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image : access Gtk_Image_Record;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image : System.Address;
          Icon  : Glib.G_Icon.G_Icon;
          Size  : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_gicon");
   begin
      Internal (Get_Object (Image), Icon, Size);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image    : access Gtk_Image_Record;
       Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
          Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_set");
   begin
      Internal (Get_Object (Image), Icon_Set, Size);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image     : access Gtk_Image_Record;
       Gdk_Image : Gdk.Image.Gdk_Image;
       Mask      : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Image     : System.Address;
          Gdk_Image : Gdk.Image.Gdk_Image;
          Mask      : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_set_from_image");
   begin
      Internal (Get_Object (Image), Gdk_Image, Mask);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image  : access Gtk_Image_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal (Image : System.Address; Pixbuf : System.Address);
      pragma Import (C, Internal, "gtk_image_set_from_pixbuf");
   begin
      Internal (Get_Object (Image), Get_Object (Pixbuf));
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image  : access Gtk_Image_Record;
       Pixmap : Gdk.Pixmap.Gdk_Pixmap;
       Mask   : Gdk.Bitmap.Gdk_Bitmap)
   is
      procedure Internal
         (Image  : System.Address;
          Pixmap : Gdk.Pixmap.Gdk_Pixmap;
          Mask   : Gdk.Bitmap.Gdk_Bitmap);
      pragma Import (C, Internal, "gtk_image_set_from_pixmap");
   begin
      Internal (Get_Object (Image), Pixmap, Mask);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
      (Image    : access Gtk_Image_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image    : System.Address;
          Stock_Id : Interfaces.C.Strings.chars_ptr;
          Size     : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Image), Tmp_Stock_Id, Size);
      Free (Tmp_Stock_Id);
   end Set;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
      (Image     : access Gtk_Image_Record;
       Icon_Name : UTF8_String;
       Size      : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Image     : System.Address;
          Icon_Name : Interfaces.C.Strings.chars_ptr;
          Size      : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_image_set_from_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (Image), Tmp_Icon_Name, Size);
      Free (Tmp_Icon_Name);
   end Set_From_Icon_Name;

   --------------------
   -- Set_Pixel_Size --
   --------------------

   procedure Set_Pixel_Size
      (Image      : access Gtk_Image_Record;
       Pixel_Size : Gint)
   is
      procedure Internal (Image : System.Address; Pixel_Size : Gint);
      pragma Import (C, Internal, "gtk_image_set_pixel_size");
   begin
      Internal (Get_Object (Image), Pixel_Size);
   end Set_Pixel_Size;

end Gtk.Image;
