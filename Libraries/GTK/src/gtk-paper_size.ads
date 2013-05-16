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

--  <description>
--  Gtk_Paper_Size handles paper sizes. It uses the standard called
--  "PWG 5101.1-2002 PWG: Standard for Media Standardized Names" to name the
--  paper sizes (and to get the data for the page sizes). In addition to
--  standard paper sizes, Gtk_Paper_Size allows to construct custom paper
--  sizes with arbitrary dimensions.
--
--  The Gtk_Paper_Size object stores not only the dimensions (width and height)
--  of a paper size and its name, it also provides default print margins.
--  </description>
--  <c_version>2.16.6</c_version>

with Ada.Unchecked_Conversion;

with Glib.Glist;
with Glib.Key_File;
with Gtk.Enums;

package Gtk.Paper_Size is

   type Gtk_Paper_Size is new Glib.C_Proxy;

   function Convert is
     new Ada.Unchecked_Conversion (Gtk_Paper_Size, System.Address);
   function Convert is
     new Ada.Unchecked_Conversion (System.Address, Gtk_Paper_Size);
   package Gtk_Paper_Size_Glist is
     new Glib.Glist.Generic_List (Gtk_Paper_Size);

   function Get_Type return GType;

   procedure Gtk_New
     (Widget : out Gtk_Paper_Size;
      Name   : String);
   --  Creates a new Gtk_Paper_Size object by parsing a PWG 5101.1-2002
   --  paper name.
   --
   --  If Name is "", the default paper size is returned; see Get_Default.

   procedure Gtk_New_Custom
     (Widget       : out Gtk_Paper_Size;
      Name         : String;
      Display_Name : String;
      Width        : Gdouble;
      Height       : Gdouble;
      Unit         : Gtk.Enums.Gtk_Unit);
   --  Creates a new Gtk_Paper_Size object with the given parameters:
   --
   --  Name:         the paper name
   --  Display_Name: the human-readable name
   --  Width:        the paper width, in units of Unit
   --  Height:       the paper height, in units of Unit
   --  Unit:         the unit for Width and Height

   procedure Gtk_New_From_Key_File
     (Widget     : out Gtk_Paper_Size;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "");
   --  Reads a paper size from the group Group_Name in the key file
   --  Key_File.  Will set Widget to null in case of error.

   procedure Gtk_New_From_Ppd
     (Widget           : out Gtk_Paper_Size;
      Ppd_Name         : String;
      Ppd_Display_Name : String := "";
      Width            : Gdouble;
      Height           : Gdouble);
   --  Creates a new Gtk_Paper_Size object by using PPD information:
   --
   --  Ppd_Name:         a PPD paper name
   --  Ppd_Display_Name: the corresponding human-readable name
   --  Width:            the paper width, in points
   --  Height:           the paper height in points
   --
   --  If Ppd_Name is not a recognized PPD paper name,
   --  Ppd_Display_Name, Width and Height are used to
   --  construct a custom Gtk_Paper_Size object.

   procedure To_Key_File
     (Size       : Gtk_Paper_Size;
      Key_File   : Glib.Key_File.G_Key_File;
      Group_Name : String := "");
   --  This function adds the paper size from Size to Key_File,
   --  within the group Group_Name.

   function Copy (Other : Gtk_Paper_Size) return Gtk_Paper_Size;
   --  Copies an existing Gtk_Paper_Size.

   procedure Free (Size : Gtk_Paper_Size);
   --  Free the given Gtk_Paper_Size object.

   function Get_Default return String;
   --  Returns the name of the default paper size, which
   --  depends on the current locale.

   function Get_Name (Size : Gtk_Paper_Size) return String;
   --  Gets the name of the Gtk_Paper_Size.

   function Get_Display_Name
     (Size : Gtk_Paper_Size) return String;
   --  Gets the human-readable name of the Gtk_Paper_Size.

   function Get_Ppd_Name (Size : Gtk_Paper_Size) return String;
   --  Gets the PPD name of the Gtk_Paper_Size.

   function Is_Custom (Size : Gtk_Paper_Size) return Boolean;
   --  Returns True if Size is not a standard paper size.

   -----------
   -- Sizes --
   -----------

   function Get_Width
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Height
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   --  Gets the paper width/height of the Gtk_Paper_Size, in units of Unit.

   procedure Set_Size
     (Size   : Gtk_Paper_Size;
      Width  : Gdouble;
      Height : Gdouble;
      Unit   : Gtk.Enums.Gtk_Unit);
   --  Changes the dimensions of a Size to Width x Height.

   function Get_Paper_Sizes
     (Include_Custom : Boolean)
      return Gtk_Paper_Size_Glist.Glist;
   --  Creates a list of known paper sizes.  The caller must specify whether
   --  to include custom paper sizes as defined in the page setup dialog

   function "=" (Size1, Size2 : Gtk_Paper_Size) return Boolean;
   --  Compares two Gtk_Paper_Size objects.

   -------------
   -- Margins --
   -------------

   function Get_Default_Bottom_Margin
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Default_Left_Margin
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Default_Right_Margin
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   function Get_Default_Top_Margin
     (Size : Gtk_Paper_Size;
      Unit : Gtk.Enums.Gtk_Unit)
      return Gdouble;
   --  Gets the default bottom/left/right/top margin for the Gtk_Paper_Size,
   --  using the units specified by Unit.

   --  Common names, from PWG 5101.1-2002 PWG: Standard for Media Standardized
   --  Names
   Gtk_Paper_Name_A3        : constant String := "iso_a3";
   Gtk_Paper_Name_A4        : constant String := "iso_a4";
   Gtk_Paper_Name_A5        : constant String := "iso_a5";
   Gtk_Paper_Name_B5        : constant String := "iso_b5";
   Gtk_Paper_Name_Letter    : constant String := "na_letter";
   Gtk_Paper_Name_Executive : constant String := "na_executive";
   Gtk_Paper_Name_Legal     : constant String := "na_legal";

private

   type Gtk_Paper_Size_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_paper_size_get_type");
   pragma Import (C, Copy, "gtk_paper_size_copy");
   pragma Import (C, Free, "gtk_paper_size_free");
   pragma Import (C, Get_Default_Bottom_Margin,
                  "gtk_paper_size_get_default_bottom_margin");
   pragma Import (C, Get_Default_Left_Margin,
                  "gtk_paper_size_get_default_left_margin");
   pragma Import (C, Get_Default_Right_Margin,
                  "gtk_paper_size_get_default_right_margin");
   pragma Import (C, Get_Default_Top_Margin,
                  "gtk_paper_size_get_default_top_margin");
   pragma Import (C, Get_Height, "gtk_paper_size_get_height");
   pragma Import (C, Get_Width, "gtk_paper_size_get_width");
   pragma Import (C, Set_Size, "gtk_paper_size_set_size");

end Gtk.Paper_Size;
